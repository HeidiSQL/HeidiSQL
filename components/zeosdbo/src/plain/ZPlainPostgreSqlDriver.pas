{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for PostgreSQL           }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

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

{** Implements a driver for PostgreSQL 7.3 }
  TZPostgreSQL73PlainDriver = class(TZAbstractObject, IZPlainDriver,
    IZPostgreSQLPlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

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
  TZPostgreSQL74PlainDriver = class(TZAbstractObject, IZPlainDriver,
    IZPostgreSQLPlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

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

  {** Implements a driver for PostgreSQL 8.x }
  TZPostgreSQL8xPlainDriver = class(TZAbstractObject, IZPlainDriver,
    IZPostgreSQLPlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

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

uses SysUtils, ZPlainPostgreSql73, ZPlainPostgreSql74, ZPlainPostgreSql8x;

{ TZPostgreSQL73PlainDriver }

constructor TZPostgreSQL73PlainDriver.Create;
begin
end;

function TZPostgreSQL73PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-7.3';
end;

function TZPostgreSQL73PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 7.3';
end;

procedure TZPostgreSQL73PlainDriver.Initialize;
begin
  ZPlainPostgreSql73.LibraryLoader.LoadIfNeeded;
end;

procedure TZPostgreSQL73PlainDriver.Clear(Res: PZPostgreSQLResult);
begin
  ZPlainPostgreSql73.PQclear(Res);
end;

function TZPostgreSQL73PlainDriver.CloseLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.lo_close(Handle, Fd);
end;

function TZPostgreSQL73PlainDriver.ConnectDatabase(
  ConnInfo: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql73.PQconnectdb(ConnInfo);
end;

function TZPostgreSQL73PlainDriver.ConsumeInput(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql73.PQconsumeInput(Handle);
end;

function TZPostgreSQL73PlainDriver.CreateLargeObject(
  Handle: PZPostgreSQLConnect; Mode: Integer): Oid;
begin
  Result := ZPlainPostgreSql73.lo_creat(Handle, Mode);
end;

function TZPostgreSQL73PlainDriver.EndCopy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql73.PQendcopy(Handle);
end;

function TZPostgreSQL73PlainDriver.ExecuteFunction(
  Handle: PZPostgreSQLConnect; fnid: Integer; result_buf,
  result_len: PInteger; result_is_int: Integer; args: PZPostgreSQLArgBlock;
  nargs: Integer): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql73.PQfn(Handle, fnid, result_buf,
    result_len, result_is_int, ZPlainPostgreSql73.PPQArgBlock(args), nargs);
end;

function TZPostgreSQL73PlainDriver.ExecuteQuery(
  Handle: PZPostgreSQLConnect; Query: PChar): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql73.PQexec(Handle, Query);
end;

function TZPostgreSQL73PlainDriver.ExportLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; FileName: PChar): Integer;
begin
  Result := ZPlainPostgreSql73.lo_export(Handle, ObjId, FileName);
end;

procedure TZPostgreSQL73PlainDriver.Finish(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql73.PQfinish(Handle);
end;

procedure TZPostgreSQL73PlainDriver.FreeNotify(Handle: PZPostgreSQLNotify);
begin
  ZPlainPostgreSql73.PQfreeNotify(ZPlainPostgreSql73.PPGnotify(Handle));
end;

function TZPostgreSQL73PlainDriver.GetBackendPID(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql73.PQbackendPID(Handle);
end;

function TZPostgreSQL73PlainDriver.GetBinaryTuples(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql73.PQbinaryTuples(Res);
end;

function TZPostgreSQL73PlainDriver.GetCommandStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql73.PQcmdStatus(Res);
end;

function TZPostgreSQL73PlainDriver.GetCommandTuples(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql73.PQcmdTuples(Res);
end;

function TZPostgreSQL73PlainDriver.GetConnectDefaults:
  PZPostgreSQLConnectInfoOption;
begin
  Result := PZPostgreSQLConnectInfoOption(ZPlainPostgreSql73.PQconndefaults);
end;

function TZPostgreSQL73PlainDriver.GetDatabase(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql73.PQdb(Handle);
end;

function TZPostgreSQL73PlainDriver.GetErrorMessage(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql73.PQerrorMessage(Handle);
end;

function TZPostgreSQL73PlainDriver.GetFieldCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql73.PQnfields(Res);
end;

function TZPostgreSQL73PlainDriver.GetFieldMode(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.PQfmod(Res, FieldNum);
end;

function TZPostgreSQL73PlainDriver.GetFieldName(Res: PZPostgreSQLResult;
  FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql73.PQfname(Res, FieldNum);
end;

function TZPostgreSQL73PlainDriver.GetFieldNumber(
  Res: PZPostgreSQLResult; FieldName: PChar): Integer;
begin
  Result := ZPlainPostgreSql73.PQfnumber(Res, FieldName);
end;

function TZPostgreSQL73PlainDriver.GetFieldSize(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.PQfsize(Res, FieldNum);
end;

function TZPostgreSQL73PlainDriver.GetFieldType(Res: PZPostgreSQLResult;
  FieldNum: Integer): Oid;
begin
  Result := ZPlainPostgreSql73.PQftype(Res, FieldNum);
end;

function TZPostgreSQL73PlainDriver.GetHost(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql73.PQhost(Handle);
end;

function TZPostgreSQL73PlainDriver.GetIsNull(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.PQgetisnull(Res, TupNum, FieldNum);
end;

function TZPostgreSQL73PlainDriver.GetLength(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.PQgetlength(Res, TupNum, FieldNum);
end;

function TZPostgreSQL73PlainDriver.GetLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.PQgetline(Handle, Buffer, Length);
end;

function TZPostgreSQL73PlainDriver.GetLineAsync(
  Handle: PZPostgreSQLConnect; Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.PQgetlineAsync(Handle, Buffer, Length);
end;

function TZPostgreSQL73PlainDriver.GetOidStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql73.PQoidStatus(Res);
end;

function TZPostgreSQL73PlainDriver.GetOidValue(
  Res: PZPostgreSQLResult): Oid;
begin
  Result := ZPlainPostgreSql73.PQoidValue(Res);
end;

function TZPostgreSQL73PlainDriver.GetOptions(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql73.PQoptions(Handle);
end;

function TZPostgreSQL73PlainDriver.GetPassword(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql73.PQpass(Handle);
end;

function TZPostgreSQL73PlainDriver.GetPort(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql73.PQport(Handle);
end;

function TZPostgreSQL73PlainDriver.GetResult(
  Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql73.PQgetResult(Handle);
end;

function TZPostgreSQL73PlainDriver.GetResultErrorMessage(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql73.PQresultErrorMessage(Res);
end;

function TZPostgreSQL73PlainDriver.GetResultStatus(
  Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
begin
  Result := TZPostgreSQLExecStatusType(ZPlainPostgreSql73.PQresultStatus(Res));
end;

function TZPostgreSQL73PlainDriver.GetRowCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql73.PQntuples(Res);
end;

function TZPostgreSQL73PlainDriver.GetSocket(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql73.PQsocket(Handle);
end;

function TZPostgreSQL73PlainDriver.GetStatus(
  Handle: PZPostgreSQLConnect): TZPostgreSQLConnectStatusType;
begin
  Result := TZPostgreSQLConnectStatusType(ZPlainPostgreSql73.PQstatus(Handle));
end;

function TZPostgreSQL73PlainDriver.GetTTY(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql73.PQtty(Handle);
end;

function TZPostgreSQL73PlainDriver.GetUser(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql73.PQuser(Handle);
end;

function TZPostgreSQL73PlainDriver.GetValue(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql73.PQgetvalue(Res, TupNum, FieldNum);
end;

function TZPostgreSQL73PlainDriver.ImportLargeObject(
  Handle: PZPostgreSQLConnect; FileName: PChar): Oid;
begin
  Result := ZPlainPostgreSql73.lo_import(Handle, FileName);
end;

function TZPostgreSQL73PlainDriver.IsBusy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql73.PQisBusy(Handle);
end;

function TZPostgreSQL73PlainDriver.MakeEmptyResult(
  Handle: PZPostgreSQLConnect;
  Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql73.PQmakeEmptyPGresult(Handle,
    ZPlainPostgreSql73.ExecStatusType(Status));
end;

function TZPostgreSQL73PlainDriver.Notifies(
  Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
begin
  Result := PZPostgreSQLNotify(ZPlainPostgreSql73.PQnotifies(Handle));
end;

function TZPostgreSQL73PlainDriver.OpenLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; Mode: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.lo_open(Handle, ObjId, Mode);
end;

function TZPostgreSQL73PlainDriver.PutBytes(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.PQputnbytes(Handle, Buffer, Length);
end;

function TZPostgreSQL73PlainDriver.PutLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar): Integer;
begin
  Result := ZPlainPostgreSql73.PQputline(Handle, Buffer);
end;

function TZPostgreSQL73PlainDriver.ReadLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.lo_read(Handle, Fd, Buffer, Length);
end;

function TZPostgreSQL73PlainDriver.RequestCancel(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql73.PQrequestCancel(Handle);
end;

procedure TZPostgreSQL73PlainDriver.Reset(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql73.PQreset(Handle);
end;

function TZPostgreSQL73PlainDriver.SeekLargeObject(
  Handle: PZPostgreSQLConnect; Fd, Offset, Whence: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.lo_lseek(Handle, Fd, Offset, Whence);
end;

function TZPostgreSQL73PlainDriver.SendQuery(Handle: PZPostgreSQLConnect;
  Query: PChar): Integer;
begin
  Result := ZPlainPostgreSql73.PQsendQuery(Handle, Query);
end;

function TZPostgreSQL73PlainDriver.SetDatabaseLogin(Host, Port, Options,
  TTY, Db, User, Passwd: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql73.PQsetdbLogin(Host, Port, Options, TTY, Db,
    User, Passwd);
end;

procedure TZPostgreSQL73PlainDriver.SetNoticeProcessor(
  Handle: PZPostgreSQLConnect; Proc: TZPostgreSQLNoticeProcessor;
  Arg: Pointer);
begin
  ZPlainPostgreSql73.PQsetNoticeProcessor(Handle, Proc, Arg);
end;

function TZPostgreSQL73PlainDriver.TellLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.lo_tell(Handle, Fd);
end;

procedure TZPostgreSQL73PlainDriver.Trace(Handle: PZPostgreSQLConnect;
  DebugPort: Pointer);
begin
  ZPlainPostgreSql73.PQtrace(Handle, DebugPort);
end;

function TZPostgreSQL73PlainDriver.UnlinkLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid): Integer;
begin
  Result := ZPlainPostgreSql73.lo_unlink(Handle, ObjId);
end;

procedure TZPostgreSQL73PlainDriver.Untrace(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql73.PQuntrace(Handle);
end;

function TZPostgreSQL73PlainDriver.WriteLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql73.lo_write(Handle, Fd, Buffer, Length);
end;

{ TZPostgreSQL74PlainDriver }

constructor TZPostgreSQL74PlainDriver.Create;
begin
end;

function TZPostgreSQL74PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-7.4';
end;

function TZPostgreSQL74PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 7.4';
end;

procedure TZPostgreSQL74PlainDriver.Initialize;
begin
  ZPlainPostgreSql74.LibraryLoader.LoadIfNeeded;
end;

procedure TZPostgreSQL74PlainDriver.Clear(Res: PZPostgreSQLResult);
begin
  ZPlainPostgreSql74.PQclear(Res);
end;

function TZPostgreSQL74PlainDriver.CloseLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.lo_close(Handle, Fd);
end;

function TZPostgreSQL74PlainDriver.ConnectDatabase(
  ConnInfo: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql74.PQconnectdb(ConnInfo);
end;

function TZPostgreSQL74PlainDriver.ConsumeInput(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql74.PQconsumeInput(Handle);
end;

function TZPostgreSQL74PlainDriver.CreateLargeObject(
  Handle: PZPostgreSQLConnect; Mode: Integer): Oid;
begin
  Result := ZPlainPostgreSql74.lo_creat(Handle, Mode);
end;

function TZPostgreSQL74PlainDriver.EndCopy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql74.PQendcopy(Handle);
end;

function TZPostgreSQL74PlainDriver.ExecuteFunction(
  Handle: PZPostgreSQLConnect; fnid: Integer; result_buf,
  result_len: PInteger; result_is_int: Integer; args: PZPostgreSQLArgBlock;
  nargs: Integer): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql74.PQfn(Handle, fnid, result_buf,
    result_len, result_is_int, ZPlainPostgreSql74.PPQArgBlock(args), nargs);
end;

function TZPostgreSQL74PlainDriver.ExecuteQuery(
  Handle: PZPostgreSQLConnect; Query: PChar): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql74.PQexec(Handle, Query);
end;

function TZPostgreSQL74PlainDriver.ExportLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; FileName: PChar): Integer;
begin
  Result := ZPlainPostgreSql74.lo_export(Handle, ObjId, FileName);
end;

procedure TZPostgreSQL74PlainDriver.Finish(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql74.PQfinish(Handle);
end;

procedure TZPostgreSQL74PlainDriver.FreeNotify(Handle: PZPostgreSQLNotify);
begin
  ZPlainPostgreSql74.PQfreeNotify(ZPlainPostgreSql74.PPGnotify(Handle));
end;

function TZPostgreSQL74PlainDriver.GetBackendPID(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql74.PQbackendPID(Handle);
end;

function TZPostgreSQL74PlainDriver.GetBinaryTuples(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql74.PQbinaryTuples(Res);
end;

function TZPostgreSQL74PlainDriver.GetCommandStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql74.PQcmdStatus(Res);
end;

function TZPostgreSQL74PlainDriver.GetCommandTuples(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql74.PQcmdTuples(Res);
end;

function TZPostgreSQL74PlainDriver.GetConnectDefaults:
  PZPostgreSQLConnectInfoOption;
begin
  Result := PZPostgreSQLConnectInfoOption(ZPlainPostgreSql74.PQconndefaults);
end;

function TZPostgreSQL74PlainDriver.GetDatabase(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql74.PQdb(Handle);
end;

function TZPostgreSQL74PlainDriver.GetErrorMessage(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql74.PQerrorMessage(Handle);
end;

function TZPostgreSQL74PlainDriver.GetFieldCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql74.PQnfields(Res);
end;

function TZPostgreSQL74PlainDriver.GetFieldMode(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.PQfmod(Res, FieldNum);
end;

function TZPostgreSQL74PlainDriver.GetFieldName(Res: PZPostgreSQLResult;
  FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql74.PQfname(Res, FieldNum);
end;

function TZPostgreSQL74PlainDriver.GetFieldNumber(
  Res: PZPostgreSQLResult; FieldName: PChar): Integer;
begin
  Result := ZPlainPostgreSql74.PQfnumber(Res, FieldName);
end;

function TZPostgreSQL74PlainDriver.GetFieldSize(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.PQfsize(Res, FieldNum);
end;

function TZPostgreSQL74PlainDriver.GetFieldType(Res: PZPostgreSQLResult;
  FieldNum: Integer): Oid;
begin
  Result := ZPlainPostgreSql74.PQftype(Res, FieldNum);
end;

function TZPostgreSQL74PlainDriver.GetHost(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql74.PQhost(Handle);
end;

function TZPostgreSQL74PlainDriver.GetIsNull(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.PQgetisnull(Res, TupNum, FieldNum);
end;

function TZPostgreSQL74PlainDriver.GetLength(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.PQgetlength(Res, TupNum, FieldNum);
end;

function TZPostgreSQL74PlainDriver.GetLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.PQgetline(Handle, Buffer, Length);
end;

function TZPostgreSQL74PlainDriver.GetLineAsync(
  Handle: PZPostgreSQLConnect; Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.PQgetlineAsync(Handle, Buffer, Length);
end;

function TZPostgreSQL74PlainDriver.GetOidStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql74.PQoidStatus(Res);
end;

function TZPostgreSQL74PlainDriver.GetOidValue(
  Res: PZPostgreSQLResult): Oid;
begin
  Result := ZPlainPostgreSql74.PQoidValue(Res);
end;

function TZPostgreSQL74PlainDriver.GetOptions(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql74.PQoptions(Handle);
end;

function TZPostgreSQL74PlainDriver.GetPassword(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql74.PQpass(Handle);
end;

function TZPostgreSQL74PlainDriver.GetPort(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql74.PQport(Handle);
end;

function TZPostgreSQL74PlainDriver.GetResult(
  Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql74.PQgetResult(Handle);
end;

function TZPostgreSQL74PlainDriver.GetResultErrorMessage(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql74.PQresultErrorMessage(Res);
end;

function TZPostgreSQL74PlainDriver.GetResultStatus(
  Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
begin
  Result := TZPostgreSQLExecStatusType(ZPlainPostgreSql74.PQresultStatus(Res));
end;

function TZPostgreSQL74PlainDriver.GetRowCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql74.PQntuples(Res);
end;

function TZPostgreSQL74PlainDriver.GetSocket(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql74.PQsocket(Handle);
end;

function TZPostgreSQL74PlainDriver.GetStatus(
  Handle: PZPostgreSQLConnect): TZPostgreSQLConnectStatusType;
begin
  Result := TZPostgreSQLConnectStatusType(ZPlainPostgreSql74.PQstatus(Handle));
end;

function TZPostgreSQL74PlainDriver.GetTTY(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql74.PQtty(Handle);
end;

function TZPostgreSQL74PlainDriver.GetUser(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql74.PQuser(Handle);
end;

function TZPostgreSQL74PlainDriver.GetValue(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql74.PQgetvalue(Res, TupNum, FieldNum);
end;

function TZPostgreSQL74PlainDriver.ImportLargeObject(
  Handle: PZPostgreSQLConnect; FileName: PChar): Oid;
begin
  Result := ZPlainPostgreSql74.lo_import(Handle, FileName);
end;

function TZPostgreSQL74PlainDriver.IsBusy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql74.PQisBusy(Handle);
end;

function TZPostgreSQL74PlainDriver.MakeEmptyResult(
  Handle: PZPostgreSQLConnect;
  Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql74.PQmakeEmptyPGresult(Handle,
    ZPlainPostgreSql74.ExecStatusType(Status));
end;

function TZPostgreSQL74PlainDriver.Notifies(
  Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
begin
  Result := PZPostgreSQLNotify(ZPlainPostgreSql74.PQnotifies(Handle));
end;

function TZPostgreSQL74PlainDriver.OpenLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; Mode: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.lo_open(Handle, ObjId, Mode);
end;

function TZPostgreSQL74PlainDriver.PutBytes(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.PQputnbytes(Handle, Buffer, Length);
end;

function TZPostgreSQL74PlainDriver.PutLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar): Integer;
begin
  Result := ZPlainPostgreSql74.PQputline(Handle, Buffer);
end;

function TZPostgreSQL74PlainDriver.ReadLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.lo_read(Handle, Fd, Buffer, Length);
end;

function TZPostgreSQL74PlainDriver.RequestCancel(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql74.PQrequestCancel(Handle);
end;

procedure TZPostgreSQL74PlainDriver.Reset(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql74.PQreset(Handle);
end;

function TZPostgreSQL74PlainDriver.SeekLargeObject(
  Handle: PZPostgreSQLConnect; Fd, Offset, Whence: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.lo_lseek(Handle, Fd, Offset, Whence);
end;

function TZPostgreSQL74PlainDriver.SendQuery(Handle: PZPostgreSQLConnect;
  Query: PChar): Integer;
begin
  Result := ZPlainPostgreSql74.PQsendQuery(Handle, Query);
end;

function TZPostgreSQL74PlainDriver.SetDatabaseLogin(Host, Port, Options,
  TTY, Db, User, Passwd: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql74.PQsetdbLogin(Host, Port, Options, TTY, Db,
    User, Passwd);
end;

procedure TZPostgreSQL74PlainDriver.SetNoticeProcessor(
  Handle: PZPostgreSQLConnect; Proc: TZPostgreSQLNoticeProcessor;
  Arg: Pointer);
begin
  ZPlainPostgreSql74.PQsetNoticeProcessor(Handle, Proc, Arg);
end;

function TZPostgreSQL74PlainDriver.TellLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.lo_tell(Handle, Fd);
end;

procedure TZPostgreSQL74PlainDriver.Trace(Handle: PZPostgreSQLConnect;
  DebugPort: Pointer);
begin
  ZPlainPostgreSql74.PQtrace(Handle, DebugPort);
end;

function TZPostgreSQL74PlainDriver.UnlinkLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid): Integer;
begin
  Result := ZPlainPostgreSql74.lo_unlink(Handle, ObjId);
end;

procedure TZPostgreSQL74PlainDriver.Untrace(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql74.PQuntrace(Handle);
end;

function TZPostgreSQL74PlainDriver.WriteLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql74.lo_write(Handle, Fd, Buffer, Length);
end;

{ TZPostgreSQL8xPlainDriver }

constructor TZPostgreSQL8xPlainDriver.Create;
begin
end;

function TZPostgreSQL8xPlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-8.x';
end;

function TZPostgreSQL8xPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 8.x';
end;

procedure TZPostgreSQL8xPlainDriver.Initialize;
begin
  ZPlainPostgreSql8x.LibraryLoader.LoadIfNeeded;
end;

procedure TZPostgreSQL8xPlainDriver.Clear(Res: PZPostgreSQLResult);
begin
  ZPlainPostgreSql8x.PQclear(Res);
end;

function TZPostgreSQL8xPlainDriver.CloseLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.lo_close(Handle, Fd);
end;

function TZPostgreSQL8xPlainDriver.ConnectDatabase(
  ConnInfo: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql8x.PQconnectdb(ConnInfo);
end;

function TZPostgreSQL8xPlainDriver.ConsumeInput(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8x.PQconsumeInput(Handle);
end;

function TZPostgreSQL8xPlainDriver.CreateLargeObject(
  Handle: PZPostgreSQLConnect; Mode: Integer): Oid;
begin
  Result := ZPlainPostgreSql8x.lo_creat(Handle, Mode);
end;

function TZPostgreSQL8xPlainDriver.EndCopy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8x.PQendcopy(Handle);
end;

function TZPostgreSQL8xPlainDriver.ExecuteFunction(
  Handle: PZPostgreSQLConnect; fnid: Integer; result_buf,
  result_len: PInteger; result_is_int: Integer; args: PZPostgreSQLArgBlock;
  nargs: Integer): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql8x.PQfn(Handle, fnid, result_buf,
    result_len, result_is_int, ZPlainPostgreSql8x.PPQArgBlock(args), nargs);
end;

function TZPostgreSQL8xPlainDriver.ExecuteQuery(
  Handle: PZPostgreSQLConnect; Query: PChar): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql8x.PQexec(Handle, Query);
end;

function TZPostgreSQL8xPlainDriver.ExportLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; FileName: PChar): Integer;
begin
  Result := ZPlainPostgreSql8x.lo_export(Handle, ObjId, FileName);
end;

procedure TZPostgreSQL8xPlainDriver.Finish(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql8x.PQfinish(Handle);
end;

procedure TZPostgreSQL8xPlainDriver.FreeNotify(Handle: PZPostgreSQLNotify);
begin
  ZPlainPostgreSql8x.PQfreeNotify(ZPlainPostgreSql8x.PPGnotify(Handle));
end;

function TZPostgreSQL8xPlainDriver.GetBackendPID(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8x.PQbackendPID(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetBinaryTuples(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql8x.PQbinaryTuples(Res);
end;

function TZPostgreSQL8xPlainDriver.GetCommandStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql8x.PQcmdStatus(Res);
end;

function TZPostgreSQL8xPlainDriver.GetCommandTuples(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql8x.PQcmdTuples(Res);
end;

function TZPostgreSQL8xPlainDriver.GetConnectDefaults:
  PZPostgreSQLConnectInfoOption;
begin
  Result := PZPostgreSQLConnectInfoOption(ZPlainPostgreSql8x.PQconndefaults);
end;

function TZPostgreSQL8xPlainDriver.GetDatabase(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8x.PQdb(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetErrorMessage(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8x.PQerrorMessage(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetFieldCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql8x.PQnfields(Res);
end;

function TZPostgreSQL8xPlainDriver.GetFieldMode(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.PQfmod(Res, FieldNum);
end;

function TZPostgreSQL8xPlainDriver.GetFieldName(Res: PZPostgreSQLResult;
  FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql8x.PQfname(Res, FieldNum);
end;

function TZPostgreSQL8xPlainDriver.GetFieldNumber(
  Res: PZPostgreSQLResult; FieldName: PChar): Integer;
begin
  Result := ZPlainPostgreSql8x.PQfnumber(Res, FieldName);
end;

function TZPostgreSQL8xPlainDriver.GetFieldSize(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.PQfsize(Res, FieldNum);
end;

function TZPostgreSQL8xPlainDriver.GetFieldType(Res: PZPostgreSQLResult;
  FieldNum: Integer): Oid;
begin
  Result := ZPlainPostgreSql8x.PQftype(Res, FieldNum);
end;

function TZPostgreSQL8xPlainDriver.GetHost(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8x.PQhost(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetIsNull(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.PQgetisnull(Res, TupNum, FieldNum);
end;

function TZPostgreSQL8xPlainDriver.GetLength(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.PQgetlength(Res, TupNum, FieldNum);
end;

function TZPostgreSQL8xPlainDriver.GetLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.PQgetline(Handle, Buffer, Length);
end;

function TZPostgreSQL8xPlainDriver.GetLineAsync(
  Handle: PZPostgreSQLConnect; Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.PQgetlineAsync(Handle, Buffer, Length);
end;

function TZPostgreSQL8xPlainDriver.GetOidStatus(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql8x.PQoidStatus(Res);
end;

function TZPostgreSQL8xPlainDriver.GetOidValue(
  Res: PZPostgreSQLResult): Oid;
begin
  Result := ZPlainPostgreSql8x.PQoidValue(Res);
end;

function TZPostgreSQL8xPlainDriver.GetOptions(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8x.PQoptions(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetPassword(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8x.PQpass(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetPort(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8x.PQport(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetResult(
  Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql8x.PQgetResult(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetResultErrorMessage(
  Res: PZPostgreSQLResult): PChar;
begin
  Result := ZPlainPostgreSql8x.PQresultErrorMessage(Res);
end;

function TZPostgreSQL8xPlainDriver.GetResultStatus(
  Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
begin
  Result := TZPostgreSQLExecStatusType(ZPlainPostgreSql8x.PQresultStatus(Res));
end;

function TZPostgreSQL8xPlainDriver.GetRowCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := ZPlainPostgreSql8x.PQntuples(Res);
end;

function TZPostgreSQL8xPlainDriver.GetSocket(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8x.PQsocket(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetStatus(
  Handle: PZPostgreSQLConnect): TZPostgreSQLConnectStatusType;
begin
  Result := TZPostgreSQLConnectStatusType(ZPlainPostgreSql8x.PQstatus(Handle));
end;

function TZPostgreSQL8xPlainDriver.GetTTY(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8x.PQtty(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetUser(
  Handle: PZPostgreSQLConnect): PChar;
begin
  Result := ZPlainPostgreSql8x.PQuser(Handle);
end;

function TZPostgreSQL8xPlainDriver.GetValue(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): PChar;
begin
  Result := ZPlainPostgreSql8x.PQgetvalue(Res, TupNum, FieldNum);
end;

function TZPostgreSQL8xPlainDriver.ImportLargeObject(
  Handle: PZPostgreSQLConnect; FileName: PChar): Oid;
begin
  Result := ZPlainPostgreSql8x.lo_import(Handle, FileName);
end;

function TZPostgreSQL8xPlainDriver.IsBusy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8x.PQisBusy(Handle);
end;

function TZPostgreSQL8xPlainDriver.MakeEmptyResult(
  Handle: PZPostgreSQLConnect;
  Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;
begin
  Result := ZPlainPostgreSql8x.PQmakeEmptyPGresult(Handle,
    ZPlainPostgreSql8x.ExecStatusType(Status));
end;

function TZPostgreSQL8xPlainDriver.Notifies(
  Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
begin
  Result := PZPostgreSQLNotify(ZPlainPostgreSql8x.PQnotifies(Handle));
end;

function TZPostgreSQL8xPlainDriver.OpenLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; Mode: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.lo_open(Handle, ObjId, Mode);
end;

function TZPostgreSQL8xPlainDriver.PutBytes(Handle: PZPostgreSQLConnect;
  Buffer: PChar; Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.PQputnbytes(Handle, Buffer, Length);
end;

function TZPostgreSQL8xPlainDriver.PutLine(Handle: PZPostgreSQLConnect;
  Buffer: PChar): Integer;
begin
  Result := ZPlainPostgreSql8x.PQputline(Handle, Buffer);
end;

function TZPostgreSQL8xPlainDriver.ReadLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.lo_read(Handle, Fd, Buffer, Length);
end;

function TZPostgreSQL8xPlainDriver.RequestCancel(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := ZPlainPostgreSql8x.PQrequestCancel(Handle);
end;

procedure TZPostgreSQL8xPlainDriver.Reset(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql8x.PQreset(Handle);
end;

function TZPostgreSQL8xPlainDriver.SeekLargeObject(
  Handle: PZPostgreSQLConnect; Fd, Offset, Whence: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.lo_lseek(Handle, Fd, Offset, Whence);
end;

function TZPostgreSQL8xPlainDriver.SendQuery(Handle: PZPostgreSQLConnect;
  Query: PChar): Integer;
begin
  Result := ZPlainPostgreSql8x.PQsendQuery(Handle, Query);
end;

function TZPostgreSQL8xPlainDriver.SetDatabaseLogin(Host, Port, Options,
  TTY, Db, User, Passwd: PChar): PZPostgreSQLConnect;
begin
  Result := ZPlainPostgreSql8x.PQsetdbLogin(Host, Port, Options, TTY, Db,
    User, Passwd);
end;

procedure TZPostgreSQL8xPlainDriver.SetNoticeProcessor(
  Handle: PZPostgreSQLConnect; Proc: TZPostgreSQLNoticeProcessor;
  Arg: Pointer);
begin
  ZPlainPostgreSql8x.PQsetNoticeProcessor(Handle, Proc, Arg);
end;

function TZPostgreSQL8xPlainDriver.TellLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.lo_tell(Handle, Fd);
end;

procedure TZPostgreSQL8xPlainDriver.Trace(Handle: PZPostgreSQLConnect;
  DebugPort: Pointer);
begin
  ZPlainPostgreSql8x.PQtrace(Handle, DebugPort);
end;

function TZPostgreSQL8xPlainDriver.UnlinkLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid): Integer;
begin
  Result := ZPlainPostgreSql8x.lo_unlink(Handle, ObjId);
end;

procedure TZPostgreSQL8xPlainDriver.Untrace(Handle: PZPostgreSQLConnect);
begin
  ZPlainPostgreSql8x.PQuntrace(Handle);
end;

function TZPostgreSQL8xPlainDriver.WriteLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PChar;
  Length: Integer): Integer;
begin
  Result := ZPlainPostgreSql8x.lo_write(Handle, Fd, Buffer, Length);
end;

end.
