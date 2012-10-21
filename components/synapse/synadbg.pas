{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.002 |
|==============================================================================|
| Content: Socket debug tools                                                  |
|==============================================================================|
| Copyright (c)2008-2011, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2008-2011.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(Socket debug tools)

Routines for help with debugging of events on the Sockets.
}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit synadbg;

interface

uses
  blcksock, synsock, synautil, classes, sysutils, synafpc;

type
  TSynaDebug = class(TObject)
    class procedure HookStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
    class procedure HookMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
  end;

procedure AppendToLog(const value: Ansistring);

var
  LogFile: string;

implementation

procedure AppendToLog(const value: Ansistring);
var
  st: TFileStream;
  s: string;
  h, m, ss, ms: word;
  dt: Tdatetime;
begin
  if fileexists(LogFile) then
    st := TFileStream.Create(LogFile, fmOpenReadWrite or fmShareDenyWrite)
  else
    st := TFileStream.Create(LogFile, fmCreate or fmShareDenyWrite);
  try
    st.Position := st.Size;
    dt := now;
    decodetime(dt, h, m, ss, ms);
    s := formatdatetime('yyyymmdd-hhnnss', dt) + format('.%.3d', [ms]) + ' ' + value;
    WriteStrToStream(st, s);
  finally
    st.free;
  end;
end;

class procedure TSynaDebug.HookStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
var
  s: string;
begin
  case Reason of
    HR_ResolvingBegin:
      s := 'HR_ResolvingBegin';
    HR_ResolvingEnd:
      s := 'HR_ResolvingEnd';
    HR_SocketCreate:
      s := 'HR_SocketCreate';
    HR_SocketClose:
      s := 'HR_SocketClose';
    HR_Bind:
      s := 'HR_Bind';
    HR_Connect:
      s := 'HR_Connect';
    HR_CanRead:
      s := 'HR_CanRead';
    HR_CanWrite:
      s := 'HR_CanWrite';
    HR_Listen:
      s := 'HR_Listen';
    HR_Accept:
      s := 'HR_Accept';
    HR_ReadCount:
      s := 'HR_ReadCount';
    HR_WriteCount:
      s := 'HR_WriteCount';
    HR_Wait:
      s := 'HR_Wait';
    HR_Error:
      s := 'HR_Error';
  else
    s := '-unknown-';
  end;
  s := inttohex(PtrInt(Sender), 8) + s + ': ' + value + CRLF;
  AppendToLog(s);
end;

class procedure TSynaDebug.HookMonitor(Sender: TObject; Writing: Boolean; const Buffer: TMemory; Len: Integer);
var
  s, d: Ansistring;
begin
  setlength(s, len);
  move(Buffer^, pointer(s)^, len);
  if writing then
    d := '-> '
  else
    d := '<- ';
  s :=inttohex(PtrInt(Sender), 8) + d + s + CRLF;
  AppendToLog(s);
end;

initialization
begin
  Logfile := changefileext(paramstr(0), '.slog');
end;

end.
