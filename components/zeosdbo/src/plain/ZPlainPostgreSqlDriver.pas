{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for PostgreSQL           }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainPostgreSqlDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZCompatibility, ZPlainDriver;

const
{ Type Lengths }
  NAMEDATALEN  = 32;

{ OIDNAMELEN should be set to NAMEDATALEN + sizeof(Oid) }
  OIDNAMELEN   = 36;

  INV_WRITE    = $00020000;
  INV_READ     = $00040000;

  BLOB_SEEK_SET     = 0;
  BLOB_SEEK_CUR     = 1;
  BLOB_SEEK_END     = 2;

type

{ Application-visible enum types }
  TZPostgreSQLConnectStatusType = (
    CONNECTION_OK,
    CONNECTION_BAD
  );

  TZPostgreSQLFieldCode=( // FirmOS
        {$IFNDEF VER130}// not possible for Delphi5
            PG_DIAG_SEVERITY=ord('S'),
            PG_DIAG_SQLSTATE=ord('C'),
            PG_DIAG_MESSAGE_PRIMARY=ord('M'),
            PG_DIAG_MESSAGE_DETAIL=ord('D'),
            PG_DIAG_MESSAGE_HINT=ord('H'),
            PG_DIAG_STATEMENT_POSITION=ord('P'),
            PG_DIAG_INTERNAL_POSITION=ord('p'),
            PG_DIAG_INTERNAL_QUERY=ord('q'),
            PG_DIAG_CONTEXT=ord('W'),
            PG_DIAG_SOURCE_FILE=ord('F'),
            PG_DIAG_SOURCE_LINE=ord('L'),
            PG_DIAG_SOURCE_FUNCTION=ord('R')
        {$ELSE}
            PG_DIAG_SEVERITY,
            PG_DIAG_SQLSTATE,
            PG_DIAG_MESSAGE_PRIMARY,
            PG_DIAG_MESSAGE_DETAIL,
            PG_DIAG_MESSAGE_HINT,
            PG_DIAG_STATEMENT_POSITION,
            PG_DIAG_INTERNAL_POSITION,
            PG_DIAG_INTERNAL_QUERY,
            PG_DIAG_CONTEXT,
            PG_DIAG_SOURCE_FILE,
            PG_DIAG_SOURCE_LINE,
            PG_DIAG_SOURCE_FUNCTION
        {$ENDIF}
            );

  TZPostgreSQLExecStatusType = (
    PGRES_EMPTY_QUERY,
    PGRES_COMMAND_OK,		{ a query command that doesn't return anything
				  was executed properly by the backend }
    PGRES_TUPLES_OK,		{ a query command that returns tuples
				  was executed properly by the backend,
				  PGresult contains the result tuples }
    PGRES_COPY_OUT,		{ Copy Out data transfer in progress }
    PGRES_COPY_IN,		{ Copy In data transfer in progress }
    PGRES_BAD_RESPONSE,		{ an unexpected response was recv'd from
				  the backend }
    PGRES_NONFATAL_ERROR,
    PGRES_FATAL_ERROR
  );

{ PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  TZPostgreSQLNotify = packed record
    relname: PChar;   { name of relation containing data }
    be_pid:  Integer; { process id of backend }
  end;

  PZPostgreSQLNotify = ^TZPostgreSQLNotify;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  TZPostgreSQLNoticeProcessor = procedure(arg: Pointer; message: PChar); cdecl;

{ Structure for the conninfo parameter definitions returned by PQconndefaults }

  TZPostgreSQLConnectInfoOption = packed record
    keyword:  PChar;	{ The keyword of the option }
    envvar:   PChar;	{ Fallback environment variable name }
    compiled: PChar;	{ Fallback compiled in default value  }
    val:      PChar;	{ Options value	}
    lab:      PChar;	{ Label for field in connect dialog }
    dispchar: PChar;	{ Character to display for this field
			  in a connect dialog. Values are:
			  ""	Display entered value as is
			  "*"	Password field - hide value
			  "D"	Debug options - don't
			  create a field by default }
    dispsize: Integer;	{ Field size in characters for dialog }
  end;

  PZPostgreSQLConnectInfoOption = ^TZPostgreSQLConnectInfoOption;

{ PQArgBlock -- structure for PQfn() arguments }

  TZPostgreSQLArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger);	{ can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PZPostgreSQLArgBlock = ^TZPostgreSQLArgBlock;

  PZPostgreSQLConnect = Pointer;
  PZPostgreSQLResult = Pointer;
  Oid = Integer;

type

  {** Represents a generic interface to PostgreSQL native API. }
  IZPostgreSQLPlainDriver = interface (IZPlainDriver)
    ['{03CD6345-2D7A-4FE2-B03D-3C5656789FEB}']

    function  EncodeBYTEA(Value: string;Handle: PZPostgreSQLConnect): string;
    function  DecodeBYTEA(value: string): string;

    function ConnectDatabase(ConnInfo: PChar): PZPostgreSQLConnect;
    function SetDatabaseLogin(Host, Port, Options, TTY, Db, User,Passwd: PChar): PZPostgreSQLConnect;
    function GetConnectDefaults: PZPostgreSQLConnectInfoOption;

    procedure Finish(Handle: PZPostgreSQLConnect);
    procedure Reset(Handle: PZPostgreSQLConnect);
    function RequestCancel(Handle: PZPostgreSQLConnect): Integer;
    function GetDatabase(Handle: PZPostgreSQLConnect): PChar;
    function GetUser(Handle: PZPostgreSQLConnect): PChar;
    function GetPassword(Handle: PZPostgreSQLConnect): PChar;
    function GetHost(Handle: PZPostgreSQLConnect): PChar;
    function GetPort(Handle: PZPostgreSQLConnect): PChar;
    function GetTTY(Handle: PZPostgreSQLConnect): PChar; cdecl;
    function GetOptions(Handle: PZPostgreSQLConnect): PChar;
    function GetStatus(Handle: PZPostgreSQLConnect):TZPostgreSQLConnectStatusType;

    function GetErrorMessage(Handle: PZPostgreSQLConnect): PChar;
    function GetSocket(Handle: PZPostgreSQLConnect): Integer;
    function GetBackendPID(Handle: PZPostgreSQLConnect): Integer;
    procedure Trace(Handle: PZPostgreSQLConnect; DebugPort: Pointer);
    procedure Untrace(Handle: PZPostgreSQLConnect);
    procedure SetNoticeProcessor(Handle: PZPostgreSQLConnect;Proc: TZPostgreSQLNoticeProcessor; Arg: Pointer);

    function ExecuteQuery(Handle: PZPostgreSQLConnect;Query: PChar): PZPostgreSQLResult;

    function Notifies(Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
    procedure FreeNotify(Handle: PZPostgreSQLNotify);

    function SendQuery(Handle: PZPostgreSQLConnect; Query: PChar): Integer;
    function GetResult(Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
    function IsBusy(Handle: PZPostgreSQLConnect): Integer;
    function ConsumeInput(Handle: PZPostgreSQLConnect): Integer;
    function GetLine(Handle: PZPostgreSQLConnect; Str: PChar;
      Length: Integer): Integer;
    function PutLine(Handle: PZPostgreSQLConnect; Str: PChar): Integer;
    function GetLineAsync(Handle: PZPostgreSQLConnect; Buffer: PChar;
      Length: Integer): Integer;

    function PutBytes(Handle: PZPostgreSQLConnect; Buffer: PChar;
      Length: Integer): Integer;
    function EndCopy(Handle: PZPostgreSQLConnect): Integer;
    function ExecuteFunction(Handle: PZPostgreSQLConnect; fnid: Integer;
      result_buf, result_len: PInteger; result_is_int: Integer;
      args: PZPostgreSQLArgBlock; nargs: Integer): PZPostgreSQLResult;
    function GetResultStatus(Res: PZPostgreSQLResult):
      TZPostgreSQLExecStatusType;

    function GetResultErrorMessage(Res: PZPostgreSQLResult): PChar;
    function GetResultErrorField(Res: PZPostgreSQLResult;FieldCode:TZPostgreSQLFieldCode):Pchar;

    function GetRowCount(Res: PZPostgreSQLResult): Integer;
    function GetFieldCount(Res: PZPostgreSQLResult): Integer;

    function GetBinaryTuples(Res: PZPostgreSQLResult): Integer;
    function GetFieldName(Res: PZPostgreSQLResult;
      FieldNum: Integer): PChar;
    function GetFieldNumber(Res: PZPostgreSQLResult;
      FieldName: PChar): Integer;
    function GetFieldType(Res: PZPostgreSQLResult;
      FieldNum: Integer): Oid;
    function GetFieldSize(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetFieldMode(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetCommandStatus(Res: PZPostgreSQLResult): PChar;
    function GetOidValue(Res: PZPostgreSQLResult): Oid;
    function GetOidStatus(Res: PZPostgreSQLResult): PChar;
    function GetCommandTuples(Res: PZPostgreSQLResult): PChar;

    function GetValue(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): PChar;
    function GetLength(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    function GetIsNull(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    procedure Clear(Res: PZPostgreSQLResult);

    function MakeEmptyResult(Handle: PZPostgreSQLConnect;
      Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;

    function OpenLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      Mode: Integer): Integer;
    function CloseLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function ReadLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PChar; Length: Integer): Integer;
    function WriteLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PChar; Length: Integer): Integer;
    function SeekLargeObject(Handle: PZPostgreSQLConnect;
      Fd, Offset, Whence: Integer): Integer;
    function CreateLargeObject(Handle: PZPostgreSQLConnect;
      Mode: Integer): Oid;
    function TellLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function UnlinkLargeObject(Handle: PZPostgreSQLConnect;
      ObjId: Oid): Integer;
    function ImportLargeObject(Handle: PZPostgreSQLConnect;
      FileName: PChar): Oid;
    function ExportLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      FileName: PChar): Integer;
  end;


  {** Implements a driver for PostgreSQL 7.4 }
  TZPostgreSQL7PlainDriver = class(TZAbstractObject, IZPlainDriver,
    IZPostgreSQLPlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    function  EncodeBYTEA(Value: string;Handle: PZPostgreSQLConnect): string;
    function  DecodeBYTEA(value: string): string;

    function ConnectDatabase(ConnInfo: PChar): PZPostgreSQLConnect;
    function SetDatabaseLogin(Host, Port, Options, TTY, Db, User,
      Passwd: PChar): PZPostgreSQLConnect;
    function GetConnectDefaults: PZPostgreSQLConnectInfoOption;

    procedure Finish(Handle: PZPostgreSQLConnect);
    procedure Reset(Handle: PZPostgreSQLConnect);
    function RequestCancel(Handle: PZPostgreSQLConnect): Integer;
    function GetDatabase(Handle: PZPostgreSQLConnect): PChar;
    function GetUser(Handle: PZPostgreSQLConnect): PChar;
    function GetPassword(Handle: PZPostgreSQLConnect): PChar;
    function GetHost(Handle: PZPostgreSQLConnect): PChar;
    function GetPort(Handle: PZPostgreSQLConnect): PChar;
    function GetTTY(Handle: PZPostgreSQLConnect): PChar; cdecl;
    function GetOptions(Handle: PZPostgreSQLConnect): PChar;
    function GetStatus(Handle: PZPostgreSQLConnect):
      TZPostgreSQLConnectStatusType;

    function GetErrorMessage(Handle: PZPostgreSQLConnect): PChar;
    function GetSocket(Handle: PZPostgreSQLConnect): Integer;
    function GetBackendPID(Handle: PZPostgreSQLConnect): Integer;
    procedure Trace(Handle: PZPostgreSQLConnect; DebugPort: Pointer);
    procedure Untrace(Handle: PZPostgreSQLConnect);
    procedure SetNoticeProcessor(Handle: PZPostgreSQLConnect;
      Proc: TZPostgreSQLNoticeProcessor; Arg: Pointer);

    function ExecuteQuery(Handle: PZPostgreSQLConnect;
      Query: PChar): PZPostgreSQLResult;

    function Notifies(Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
    procedure FreeNotify(Handle: PZPostgreSQLNotify);

    function SendQuery(Handle: PZPostgreSQLConnect; Query: PChar): Integer;
    function GetResult(Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
    function IsBusy(Handle: PZPostgreSQLConnect): Integer;
    function ConsumeInput(Handle: PZPostgreSQLConnect): Integer;
    function GetLine(Handle: PZPostgreSQLConnect; Buffer: PChar;
      Length: Integer): Integer;
    function PutLine(Handle: PZPostgreSQLConnect; Buffer: PChar): Integer;
    function GetLineAsync(Handle: PZPostgreSQLConnect; Buffer: PChar;
      Length: Integer): Integer;

    function PutBytes(Handle: PZPostgreSQLConnect; Buffer: PChar;
      Length: Integer): Integer;
    function EndCopy(Handle: PZPostgreSQLConnect): Integer;
    function ExecuteFunction(Handle: PZPostgreSQLConnect; fnid: Integer;
      result_buf, result_len: PInteger; result_is_int: Integer;
      args: PZPostgreSQLArgBlock; nargs: Integer): PZPostgreSQLResult;
    function GetResultStatus(Res: PZPostgreSQLResult):
      TZPostgreSQLExecStatusType;
    function GetResultErrorMessage(Res: PZPostgreSQLResult): PChar;
    function GetResultErrorField(Res: PZPostgreSQLResult;FieldCode:TZPostgreSQLFieldCode):PChar;

    function GetRowCount(Res: PZPostgreSQLResult): Integer;
    function GetFieldCount(Res: PZPostgreSQLResult): Integer;

    function GetBinaryTuples(Res: PZPostgreSQLResult): Integer;
    function GetFieldName(Res: PZPostgreSQLResult;
      FieldNum: Integer): PChar;
    function GetFieldNumber(Res: PZPostgreSQLResult;
      FieldName: PChar): Integer;
    function GetFieldType(Res: PZPostgreSQLResult;
      FieldNum: Integer): Oid;
    function GetFieldSize(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetFieldMode(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetCommandStatus(Res: PZPostgreSQLResult): PChar;
    function GetOidValue(Res: PZPostgreSQLResult): Oid;
    function GetOidStatus(Res: PZPostgreSQLResult): PChar;
    function GetCommandTuples(Res: PZPostgreSQLResult): PChar;

    function GetValue(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): PChar;
    function GetLength(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    function GetIsNull(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    procedure Clear(Res: PZPostgreSQLResult);

    function MakeEmptyResult(Handle: PZPostgreSQLConnect;
      Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;

    function OpenLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      Mode: Integer): Integer;
    function CloseLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function ReadLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PChar; Length: Integer): Integer;
    function WriteLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PChar; Length: Integer): Integer;
    function SeekLargeObject(Handle: PZPostgreSQLConnect;
      Fd, Offset, Whence: Integer): Integer;
    function CreateLargeObject(Handle: PZPostgreSQLConnect;
      Mode: Integer): Oid;
    function TellLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function UnlinkLargeObject(Handle: PZPostgreSQLConnect;
      ObjId: Oid): Integer;
    function ImportLargeObject(Handle: PZPostgreSQLConnect;
      FileName: PChar): Oid;
    function ExportLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      FileName: PChar): Integer;
  end;


  {** Implements a driver for PostgreSQL 8.1 }
  TZPostgreSQL8PlainDriver = class(TZAbstractObject, IZPlainDriver,IZPostgreSQLPlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    function ConnectDatabase(ConnInfo: PChar): PZPostgreSQLConnect;
    function SetDatabaseLogin(Host, Port, Options, TTY, Db, User,
      Passwd: PChar): PZPostgreSQLConnect;
    function GetConnectDefaults: PZPostgreSQLConnectInfoOption;

    function  EncodeBYTEA(Value: string;Handle: PZPostgreSQLConnect): string;
    function  DecodeBYTEA(value: string): string;

    procedure Finish(Handle: PZPostgreSQLConnect);
    procedure Reset(Handle: PZPostgreSQLConnect);
    function RequestCancel(Handle: PZPostgreSQLConnect): Integer;
    function GetDatabase(Handle: PZPostgreSQLConnect): PChar;
    function GetUser(Handle: PZPostgreSQLConnect): PChar;
    function GetPassword(Handle: PZPostgreSQLConnect): PChar;
    function GetHost(Handle: PZPostgreSQLConnect): PChar;
    function GetPort(Handle: PZPostgreSQLConnect): PChar;
    function GetTTY(Handle: PZPostgreSQLConnect): PChar; cdecl;
    function GetOptions(Handle: PZPostgreSQLConnect): PChar;
    function GetStatus(Handle: PZPostgreSQLConnect):
      TZPostgreSQLConnectStatusType;

    function GetErrorMessage(Handle: PZPostgreSQLConnect): PChar;
    function GetSocket(Handle: PZPostgreSQLConnect): Integer;
    function GetBackendPID(Handle: PZPostgreSQLConnect): Integer;
    procedure Trace(Handle: PZPostgreSQLConnect; DebugPort: Pointer);
    procedure Untrace(Handle: PZPostgreSQLConnect);
    procedure SetNoticeProcessor(Handle: PZPostgreSQLConnect;
      Proc: TZPostgreSQLNoticeProcessor; Arg: Pointer);

    function ExecuteQuery(Handle: PZPostgreSQLConnect;
      Query: PChar): PZPostgreSQLResult;

    function Notifies(Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
    procedure FreeNotify(Handle: PZPostgreSQLNotify);

    function SendQuery(Handle: PZPostgreSQLConnect; Query: PChar): Integer;
    function GetResult(Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
    function IsBusy(Handle: PZPostgreSQLConnect): Integer;
    function ConsumeInput(Handle: PZPostgreSQLConnect): Integer;
    function GetLine(Handle: PZPostgreSQLConnect; Buffer: PChar;
      Length: Integer): Integer;
    function PutLine(Handle: PZPostgreSQLConnect; Buffer: PChar): Integer;
    function GetLineAsync(Handle: PZPostgreSQLConnect; Buffer: PChar;
      Length: Integer): Integer;

    function PutBytes(Handle: PZPostgreSQLConnect; Buffer: PChar;
      Length: Integer): Integer;
    function EndCopy(Handle: PZPostgreSQLConnect): Integer;
    function ExecuteFunction(Handle: PZPostgreSQLConnect; fnid: Integer;
      result_buf, result_len: PInteger; result_is_int: Integer;
      args: PZPostgreSQLArgBlock; nargs: Integer): PZPostgreSQLResult;
    function GetResultStatus(Res: PZPostgreSQLResult):
      TZPostgreSQLExecStatusType;

    function GetResultErrorMessage(Res: PZPostgreSQLResult): PChar;
    function GetResultErrorField(Res: PZPostgreSQLResult;FieldCode:TZPostgreSQLFieldCode):PChar;

    function GetRowCount(Res: PZPostgreSQLResult): Integer;
    function GetFieldCount(Res: PZPostgreSQLResult): Integer;

    function GetBinaryTuples(Res: PZPostgreSQLResult): Integer;
    function GetFieldName(Res: PZPostgreSQLResult;
      FieldNum: Integer): PChar;
    function GetFieldNumber(Res: PZPostgreSQLResult;
      FieldName: PChar): Integer;
    function GetFieldType(Res: PZPostgreSQLResult;
      FieldNum: Integer): Oid;
    function GetFieldSize(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetFieldMode(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetCommandStatus(Res: PZPostgreSQLResult): PChar;
    function GetOidValue(Res: PZPostgreSQLResult): Oid;
    function GetOidStatus(Res: PZPostgreSQLResult): PChar;
    function GetCommandTuples(Res: PZPostgreSQLResult): PChar;

    function GetValue(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): PChar;
    function GetLength(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    function GetIsNull(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    procedure Clear(Res: PZPostgreSQLResult);

    function MakeEmptyResult(Handle: PZPostgreSQLConnect;
      Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;

    function OpenLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      Mode: Integer): Integer;
    function CloseLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function ReadLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PChar; Length: Integer): Integer;
    function WriteLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PChar; Length: Integer): Integer;
    function SeekLargeObject(Handle: PZPostgreSQLConnect;
      Fd, Offset, Whence: Integer): Integer;
    function CreateLargeObject(Handle: PZPostgreSQLConnect;
      Mode: Integer): Oid;
    function TellLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function UnlinkLargeObject(Handle: PZPostgreSQLConnect;
      ObjId: Oid): Integer;
    function ImportLargeObject(Handle: PZPostgreSQLConnect;
      FileName: PChar): Oid;
    function ExportLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      FileName: PChar): Integer;
  end;


implementation

uses SysUtils, ZPlainPostgreSql7,ZPlainPostgreSql8;


//Support for Postgresql 7.4 should also work for 7.3
{ TZPostgreSQL7PlainDriver }

constructor TZPostgreSQL7PlainDriver.Create;
begin
end;

function TZPostgreSQL7PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-7';
end;

function TZPostgreSQL7PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 7.x';
end;

procedure TZPostgreSQL7PlainDriver.Initialize;
begin
  ZPlainPostgreSql7.LibraryLoader.LoadIfNeeded;
end;

procedure TZPostgreSQL7PlainDriver.Clear(Res: PZPostgreSQLResult);
begin
  ZPlainPostgreSql7.PQclear(Res);
end;

function TZPostgreSQL7PlainDriver.CloseLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.lo_close(Handle, Fd);
end;

function TZPostgreSQL7PlainDriver.ConnectDatabase(
  ConnInfo: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql7.PQconnectdb(ConnInfo);
end;

function TZPostgreSQL7PlainDriver.ConsumeInput(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql7.PQconsumeInput(Handle);
end;

function TZPostgreSQL7PlainDriver.CreateLargeObject(
  Handle: PZPostgreSQLConnect; Mode: Integer): Oid;
begin
  Result := ZPlainPostgreSql7.lo_creat(Handle, Mode);
end;

function TZPostgreSQL7PlainDriver.DecodeBYTEA(value: string): string;
begin
 result:=value;
end;

function TZPostgreSQL7PlainDriver.EncodeBYTEA(Value: string;  Handle: PZPostgreSQLConnect): string;
begin
 result:=value;
end;

function TZPostgreSQL7PlainDriver.EndCopy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql7.PQendcopy(Handle);
end;

function TZPostgreSQL7PlainDriver.ExecuteFunction(
  Handle: PZPostgreSQLConnect; fnid: Integer; result_buf,
  result_len: PInteger; result_is_int: Integer; args: PZPostgreSQLArgBlock;
  nargs: Integer): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql7.PQfn(Handle, fnid, result_buf,
    result_len, result_is_int, ZPlainPostgreSql7.PPQArgBlock(args), nargs);
end;

function TZPostgreSQL7PlainDriver.ExecuteQuery(
  Handle: PZPostgreSQLConnect; Query: PChar): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql7.PQexec(Handle, Query);
end;

function TZPostgreSQL7PlainDriver.ExportLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; FileName: PChar): Integer;
begin
  Result := ZPlainPostgreSql7.lo_export(Handle, ObjId, FileName);
end;

procedure TZPostgreSQL7PlainDriver.Finish(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql7.PQfinish(Handle);
end;

procedure TZPostgreSQL7PlainDriver.FreeNotify(Handle: PZPostgreSQLNotify);
begin
  ZPlainPostgreSql7.PQfreeNotify(ZPlainPostgreSql7.PPGnotify(Handle));
end;

function TZPostgreSQL7PlainDriver.GetBackendPID(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql7.PQbackendPID(Handle);
end;

function TZPostgreSQL7PlainDriver.GetBinaryTuples(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql7.PQbinaryTuples(Res);
end;

function TZPostgreSQL7PlainDriver.GetCommandStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql7.PQcmdStatus(Res);
end;

function TZPostgreSQL7PlainDriver.GetCommandTuples(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql7.PQcmdTuples(Res);
end;

function TZPostgreSQL7PlainDriver.GetConnectDefaults:
  PZPostgreSQLConnectInfoOption;
begin
  Result := PZPostgreSQLConnectInfoOption(ZPlainPostgreSql7.PQconndefaults);
end;

function TZPostgreSQL7PlainDriver.GetDatabase(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql7.PQdb(Handle);
end;

function TZPostgreSQL7PlainDriver.GetErrorMessage(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql7.PQerrorMessage(Handle);
end;

function TZPostgreSQL7PlainDriver.GetFieldCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql7.PQnfields(Res);
end;

function TZPostgreSQL7PlainDriver.GetFieldMode(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.PQfmod(Res, FieldNum);
end;

function TZPostgreSQL7PlainDriver.GetFieldName(Res: PZPostgreSQLResult;
  FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql7.PQfname(Res, FieldNum);
end;

function TZPostgreSQL7PlainDriver.GetFieldNumber(
  Res: PZPostgreSQLResult; FieldName: PChar): Integer;
begin
  Result := ZPlainPostgreSql7.PQfnumber(Res, FieldName);
end;

function TZPostgreSQL7PlainDriver.GetFieldSize(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.PQfsize(Res, FieldNum);
end;

function TZPostgreSQL7PlainDriver.GetFieldType(Res: PZPostgreSQLResult;
  FieldNum: Integer): Oid;
begin
  Result := ZPlainPostgreSql7.PQftype(Res, FieldNum);
end;

function TZPostgreSQL7PlainDriver.GetHost(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql7.PQhost(Handle);
end;

function TZPostgreSQL7PlainDriver.GetIsNull(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.PQgetisnull(Res, TupNum, FieldNum);
end;

function TZPostgreSQL7PlainDriver.GetLength(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.PQgetlength(Res, TupNum, FieldNum);
end;

function TZPostgreSQL7PlainDriver.GetLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.PQgetline(Handle, Buffer, Length);
end;

function TZPostgreSQL7PlainDriver.GetLineAsync(
  Handle: PZPostgreSQLConnect; Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.PQgetlineAsync(Handle, Buffer, Length);
end;

function TZPostgreSQL7PlainDriver.GetOidStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql7.PQoidStatus(Res);
end;

function TZPostgreSQL7PlainDriver.GetOidValue(
  Res: PZPostgreSQLResult): Oid;
begin
  Result := ZPlainPostgreSql7.PQoidValue(Res);
end;

function TZPostgreSQL7PlainDriver.GetOptions(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql7.PQoptions(Handle);
end;

function TZPostgreSQL7PlainDriver.GetPassword(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql7.PQpass(Handle);
end;

function TZPostgreSQL7PlainDriver.GetPort(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql7.PQport(Handle);
end;

function TZPostgreSQL7PlainDriver.GetResult(
  Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql7.PQgetResult(Handle);
end;

function TZPostgreSQL7PlainDriver.GetResultErrorField(Res: PZPostgreSQLResult;  FieldCode: TZPostgreSQLFieldCode): PChar;
begin
 // Not implemented for 7
 result:='';
end;

function TZPostgreSQL7PlainDriver.GetResultErrorMessage(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql7.PQresultErrorMessage(Res);
end;

function TZPostgreSQL7PlainDriver.GetResultStatus(
  Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
begin
  Result := TZPostgreSQLExecStatusType(ZPlainPostgreSql7.PQresultStatus(Res));
end;

function TZPostgreSQL7PlainDriver.GetRowCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql7.PQntuples(Res);
end;

function TZPostgreSQL7PlainDriver.GetSocket(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql7.PQsocket(Handle);
end;

function TZPostgreSQL7PlainDriver.GetStatus(
  Handle: PZPostgreSQLConnect): TZPostgreSQLConnectStatusType;
begin
  Result := TZPostgreSQLConnectStatusType(ZPlainPostgreSql7.PQstatus(Handle));
end;

function TZPostgreSQL7PlainDriver.GetTTY(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql7.PQtty(Handle);
end;

function TZPostgreSQL7PlainDriver.GetUser(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql7.PQuser(Handle);
end;

function TZPostgreSQL7PlainDriver.GetValue(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql7.PQgetvalue(Res, TupNum, FieldNum);
end;

function TZPostgreSQL7PlainDriver.ImportLargeObject(
  Handle: PZPostgreSQLConnect; FileName: PChar): Oid;
begin
  Result := ZPlainPostgreSql7.lo_import(Handle, FileName);
end;

function TZPostgreSQL7PlainDriver.IsBusy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql7.PQisBusy(Handle);
end;

function TZPostgreSQL7PlainDriver.MakeEmptyResult(
  Handle: PZPostgreSQLConnect;
  Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql7.PQmakeEmptyPGresult(Handle,
    ZPlainPostgreSql7.ExecStatusType(Status));
end;

function TZPostgreSQL7PlainDriver.Notifies(
  Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
begin
  Result := PZPostgreSQLNotify(ZPlainPostgreSql7.PQnotifies(Handle));
end;

function TZPostgreSQL7PlainDriver.OpenLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; Mode: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.lo_open(Handle, ObjId, Mode);
end;

function TZPostgreSQL7PlainDriver.PutBytes(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.PQputnbytes(Handle, Buffer, Length);
end;

function TZPostgreSQL7PlainDriver.PutLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar): Integer;
begin
  Result := ZPlainPostgreSql7.PQputline(Handle, Buffer);
end;

function TZPostgreSQL7PlainDriver.ReadLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.lo_read(Handle, Fd, Buffer, Length);
end;

function TZPostgreSQL7PlainDriver.RequestCancel(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql7.PQrequestCancel(Handle);
end;

procedure TZPostgreSQL7PlainDriver.Reset(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql7.PQreset(Handle);
end;

function TZPostgreSQL7PlainDriver.SeekLargeObject(
  Handle: PZPostgreSQLConnect; Fd, Offset, Whence: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.lo_lseek(Handle, Fd, Offset, Whence);
end;

function TZPostgreSQL7PlainDriver.SendQuery(Handle: PZPostgreSQLConnect;
  Query: PChar): Integer;
begin
  Result := ZPlainPostgreSql7.PQsendQuery(Handle, Query);
end;

function TZPostgreSQL7PlainDriver.SetDatabaseLogin(Host, Port, Options,
  TTY, Db, User, Passwd: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql7.PQsetdbLogin(Host, Port, Options, TTY, Db,
    User, Passwd);
end;

procedure TZPostgreSQL7PlainDriver.SetNoticeProcessor(
  Handle: PZPostgreSQLConnect; Proc: TZPostgreSQLNoticeProcessor;
  Arg: Pointer);
begin
  ZPlainPostgreSql7.PQsetNoticeProcessor(Handle, Proc, Arg);
end;

function TZPostgreSQL7PlainDriver.TellLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.lo_tell(Handle, Fd);
end;

procedure TZPostgreSQL7PlainDriver.Trace(Handle: PZPostgreSQLConnect;
  DebugPort: Pointer);
begin
  ZPlainPostgreSql7.PQtrace(Handle, DebugPort);
end;

function TZPostgreSQL7PlainDriver.UnlinkLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid): Integer;
begin
  Result := ZPlainPostgreSql7.lo_unlink(Handle, ObjId);
end;

procedure TZPostgreSQL7PlainDriver.Untrace(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql7.PQuntrace(Handle);
end;

function TZPostgreSQL7PlainDriver.WriteLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql7.lo_write(Handle, Fd, Buffer, Length);
end;


{ TZPostgreSQL8PlainDriver }

constructor TZPostgreSQL8PlainDriver.Create;
begin
end;

function TZPostgreSQL8PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-8';
end;

function TZPostgreSQL8PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 8.x';
end;

procedure TZPostgreSQL8PlainDriver.Initialize;
begin
  ZPlainPostgreSql8.LibraryLoader.LoadIfNeeded;
end;

procedure TZPostgreSQL8PlainDriver.Clear(Res: PZPostgreSQLResult);
begin
  ZPlainPostgreSql8.PQclear(Res);
end;

function TZPostgreSQL8PlainDriver.CloseLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.lo_close(Handle, Fd);
end;

function TZPostgreSQL8PlainDriver.ConnectDatabase(
  ConnInfo: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql8.PQconnectdb(ConnInfo);
end;

function TZPostgreSQL8PlainDriver.ConsumeInput(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8.PQconsumeInput(Handle);
end;

function TZPostgreSQL8PlainDriver.CreateLargeObject(
  Handle: PZPostgreSQLConnect; Mode: Integer): Oid;
begin
  Result := ZPlainPostgreSql8.lo_creat(Handle, Mode);
end;

function TZPostgreSQL8PlainDriver.DecodeBYTEA(value: string): string;
var decoded:pchar;
    len:Longword;
begin
  decoded:=ZPlainPostgreSql8.PQUnescapeBytea(pansichar(value),@len);
  SetLength(result,len);
  if (len > 0) then Move(decoded^,result[1],len);
  ZPlainPostgreSql8.PQFreemem(decoded);
end;

function TZPostgreSQL8PlainDriver.EncodeBYTEA(Value: string;Handle: PZPostgreSQLConnect): string;
var encoded:pchar;
    len:Longword;
    leng:cardinal;
begin
 leng:=Length(Value);
 if assigned(ZPlainPostgreSql8.PQescapeByteaConn) then begin
  encoded:=ZPlainPostgreSql8.PQescapeByteaConn(Handle,pansichar(value),leng,@len);
 end else begin
  encoded:=ZPlainPostgreSql8.PQescapeBytea(pansichar(value),leng,@len);
 end;
 setlength(result,len-1);
 StrCopy(pansichar(result),encoded);
 ZPlainPostgreSql8.PQFreemem(encoded);
 result:=''''+result+'''';
end;

function TZPostgreSQL8PlainDriver.EndCopy( Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8.PQendcopy(Handle);
end;

function TZPostgreSQL8PlainDriver.ExecuteFunction(
  Handle: PZPostgreSQLConnect; fnid: Integer; result_buf,
  result_len: PInteger; result_is_int: Integer; args: PZPostgreSQLArgBlock;
  nargs: Integer): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql8.PQfn(Handle, fnid, result_buf,
    result_len, result_is_int, ZPlainPostgreSql8.PPQArgBlock(args), nargs);
end;

function TZPostgreSQL8PlainDriver.ExecuteQuery(
  Handle: PZPostgreSQLConnect; Query: PChar): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql8.PQexec(Handle, Query);
end;

function TZPostgreSQL8PlainDriver.ExportLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; FileName: PChar): Integer;
begin
  Result := ZPlainPostgreSql8.lo_export(Handle, ObjId, FileName);
end;

procedure TZPostgreSQL8PlainDriver.Finish(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql8.PQfinish(Handle);
end;

procedure TZPostgreSQL8PlainDriver.FreeNotify(Handle: PZPostgreSQLNotify);
begin
  ZPlainPostgreSql8.PQfreeNotify(ZPlainPostgreSql8.PPGnotify(Handle));
end;

function TZPostgreSQL8PlainDriver.GetBackendPID(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8.PQbackendPID(Handle);
end;

function TZPostgreSQL8PlainDriver.GetBinaryTuples(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql8.PQbinaryTuples(Res);
end;

function TZPostgreSQL8PlainDriver.GetCommandStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql8.PQcmdStatus(Res);
end;

function TZPostgreSQL8PlainDriver.GetCommandTuples(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql8.PQcmdTuples(Res);
end;

function TZPostgreSQL8PlainDriver.GetConnectDefaults:
  PZPostgreSQLConnectInfoOption;
begin
  Result := PZPostgreSQLConnectInfoOption(ZPlainPostgreSql8.PQconndefaults);
end;

function TZPostgreSQL8PlainDriver.GetDatabase(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8.PQdb(Handle);
end;

function TZPostgreSQL8PlainDriver.GetErrorMessage(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8.PQerrorMessage(Handle);
end;

function TZPostgreSQL8PlainDriver.GetFieldCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql8.PQnfields(Res);
end;

function TZPostgreSQL8PlainDriver.GetFieldMode(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.PQfmod(Res, FieldNum);
end;

function TZPostgreSQL8PlainDriver.GetFieldName(Res: PZPostgreSQLResult;
  FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql8.PQfname(Res, FieldNum);
end;

function TZPostgreSQL8PlainDriver.GetFieldNumber(
  Res: PZPostgreSQLResult; FieldName: PChar): Integer;
begin
  Result := ZPlainPostgreSql8.PQfnumber(Res, FieldName);
end;

function TZPostgreSQL8PlainDriver.GetFieldSize(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.PQfsize(Res, FieldNum);
end;

function TZPostgreSQL8PlainDriver.GetFieldType(Res: PZPostgreSQLResult;
  FieldNum: Integer): Oid;
begin
  Result := ZPlainPostgreSql8.PQftype(Res, FieldNum);
end;

function TZPostgreSQL8PlainDriver.GetHost(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8.PQhost(Handle);
end;

function TZPostgreSQL8PlainDriver.GetIsNull(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.PQgetisnull(Res, TupNum, FieldNum);
end;

function TZPostgreSQL8PlainDriver.GetLength(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.PQgetlength(Res, TupNum, FieldNum);
end;

function TZPostgreSQL8PlainDriver.GetLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.PQgetline(Handle, Buffer, Length);
end;

function TZPostgreSQL8PlainDriver.GetLineAsync(
  Handle: PZPostgreSQLConnect; Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.PQgetlineAsync(Handle, Buffer, Length);
end;

function TZPostgreSQL8PlainDriver.GetOidStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql8.PQoidStatus(Res);
end;

function TZPostgreSQL8PlainDriver.GetOidValue(
  Res: PZPostgreSQLResult): Oid;
begin
  Result := ZPlainPostgreSql8.PQoidValue(Res);
end;

function TZPostgreSQL8PlainDriver.GetOptions(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8.PQoptions(Handle);
end;

function TZPostgreSQL8PlainDriver.GetPassword(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8.PQpass(Handle);
end;

function TZPostgreSQL8PlainDriver.GetPort(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8.PQport(Handle);
end;

function TZPostgreSQL8PlainDriver.GetResult(
  Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql8.PQgetResult(Handle);
end;

function TZPostgreSQL8PlainDriver.GetResultErrorField(Res: PZPostgreSQLResult;  FieldCode: TZPostgreSQLFieldCode): PChar;
begin
  Result := ZPlainPostgreSql8.PQresultErrorField(Res,ord(FieldCode));
end;

function TZPostgreSQL8PlainDriver.GetResultErrorMessage(Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql8.PQresultErrorMessage(Res);
end;

function TZPostgreSQL8PlainDriver.GetResultStatus(
  Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
begin
  Result := TZPostgreSQLExecStatusType(ZPlainPostgreSql8.PQresultStatus(Res));
end;

function TZPostgreSQL8PlainDriver.GetRowCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql8.PQntuples(Res);
end;

function TZPostgreSQL8PlainDriver.GetSocket(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8.PQsocket(Handle);
end;

function TZPostgreSQL8PlainDriver.GetStatus(
  Handle: PZPostgreSQLConnect): TZPostgreSQLConnectStatusType;
begin
  Result := TZPostgreSQLConnectStatusType(ZPlainPostgreSql8.PQstatus(Handle));
end;

function TZPostgreSQL8PlainDriver.GetTTY(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8.PQtty(Handle);
end;

function TZPostgreSQL8PlainDriver.GetUser(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8.PQuser(Handle);
end;

function TZPostgreSQL8PlainDriver.GetValue(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql8.PQgetvalue(Res, TupNum, FieldNum);
end;

function TZPostgreSQL8PlainDriver.ImportLargeObject(
  Handle: PZPostgreSQLConnect; FileName: PChar): Oid;
begin
  Result := ZPlainPostgreSql8.lo_import(Handle, FileName);
end;

function TZPostgreSQL8PlainDriver.IsBusy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8.PQisBusy(Handle);
end;

function TZPostgreSQL8PlainDriver.MakeEmptyResult(
  Handle: PZPostgreSQLConnect;
  Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql8.PQmakeEmptyPGresult(Handle,
    ZPlainPostgreSql8.ExecStatusType(Status));
end;

function TZPostgreSQL8PlainDriver.Notifies(
  Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
begin
  Result := PZPostgreSQLNotify(ZPlainPostgreSql8.PQnotifies(Handle));
end;

function TZPostgreSQL8PlainDriver.OpenLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; Mode: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.lo_open(Handle, ObjId, Mode);
end;

function TZPostgreSQL8PlainDriver.PutBytes(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.PQputnbytes(Handle, Buffer, Length);
end;

function TZPostgreSQL8PlainDriver.PutLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar): Integer;
begin
  Result := ZPlainPostgreSql8.PQputline(Handle, Buffer);
end;

function TZPostgreSQL8PlainDriver.ReadLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.lo_read(Handle, Fd, Buffer, Length);
end;

function TZPostgreSQL8PlainDriver.RequestCancel(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8.PQrequestCancel(Handle);
end;

procedure TZPostgreSQL8PlainDriver.Reset(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql8.PQreset(Handle);
end;

function TZPostgreSQL8PlainDriver.SeekLargeObject(
  Handle: PZPostgreSQLConnect; Fd, Offset, Whence: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.lo_lseek(Handle, Fd, Offset, Whence);
end;

function TZPostgreSQL8PlainDriver.SendQuery(Handle: PZPostgreSQLConnect;
  Query: PChar): Integer;
begin
  Result := ZPlainPostgreSql8.PQsendQuery(Handle, Query);
end;

function TZPostgreSQL8PlainDriver.SetDatabaseLogin(Host, Port, Options,
  TTY, Db, User, Passwd: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql8.PQsetdbLogin(Host, Port, Options, TTY, Db,
    User, Passwd);
end;

procedure TZPostgreSQL8PlainDriver.SetNoticeProcessor(
  Handle: PZPostgreSQLConnect; Proc: TZPostgreSQLNoticeProcessor;
  Arg: Pointer);
begin
  ZPlainPostgreSql8.PQsetNoticeProcessor(Handle, Proc, Arg);
end;

function TZPostgreSQL8PlainDriver.TellLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.lo_tell(Handle, Fd);
end;

procedure TZPostgreSQL8PlainDriver.Trace(Handle: PZPostgreSQLConnect;
  DebugPort: Pointer);
begin
  ZPlainPostgreSql8.PQtrace(Handle, DebugPort);
end;

function TZPostgreSQL8PlainDriver.UnlinkLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid): Integer;
begin
  Result := ZPlainPostgreSql8.lo_unlink(Handle, ObjId);
end;

procedure TZPostgreSQL8PlainDriver.Untrace(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql8.PQuntrace(Handle);
end;

function TZPostgreSQL8PlainDriver.WriteLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8.lo_write(Handle, Fd, Buffer, Length);
end;

end.
