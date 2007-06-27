{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Plain interface to libpq.dll               }
{                     Version 7.4                         }
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

unit ZPlainPostgreSql7;

interface

{$I ZPlain.inc}

{$J+}

uses ZCompatibility, ZPlainLoader;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS1_DLL_LOCATION   = 'libpq74.dll';
{$IFNDEF STRICT_DLL_LOADING}
  WINDOWS2_DLL_LOCATION   = 'libpq.dll';
{$ENDIF}
  LINUX_DLL_LOCATION   = 'libpq.so';

{ Type Lengths }
  NAMEDATALEN  = 32;
{ OIDNAMELEN should be set to NAMEDATALEN + sizeof(Oid) }
  OIDNAMELEN   = 36;

  INV_WRITE    = $00020000;
  INV_READ     = $00040000;

  BLOB_SEEK_SET     = 0;
  BLOB_SEEK_CUR     = 1;
  BLOB_SEEK_END     = 2;


{ ****************** Plain API Types definition ***************** }

type
  Oid = Integer;

{ Application-visible enum types }
  ConnStatusType = (
    CONNECTION_OK,
    CONNECTION_BAD
  );

  ExecStatusType = (
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

{ String descriptions of the ExecStatusTypes }
  pgresStatus = array[$00..$ff] of PChar;

{ PGconn encapsulates a connection to the backend.
  The contents of this struct are not supposed to be known to applications.
}
  PGconn = Pointer;
  PPGconn = Pointer;

{ PGresult encapsulates the result of a query (or more precisely, of a single
  SQL command --- a query string given to PQsendQuery can contain multiple
  commands and thus return multiple PGresult objects).
  The contents of this struct are not supposed to be known to applications.
}
  PGresult = Pointer;
  PPGresult = Pointer;

{ PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  PGnotify = packed record
    relname: array [0..NAMEDATALEN-1] of Char; { name of relation containing data }
    be_pid:  Integer;			      { process id of backend }
  end;

  PPGnotify = ^PGnotify;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  PQnoticeProcessor = procedure(arg: Pointer; message: PChar); cdecl;

{ Print options for PQprint() }

{
  We can't use the conventional "bool", because we are designed to be
  included in a user's program, and user may already have that type
  defined.  Pqbool, on the other hand, is unlikely to be used.
}

  PPChar = array[00..$ff] of PChar;

  PQprintOpt = packed record
    header:    Byte;	   { print output field headings and row count }
    align:     Byte;	   { fill align the fields }
    standard:  Byte;	   { old brain dead format }
    html3:     Byte;	   { output html tables }
    expanded:  Byte;	   { expand tables }
    pager:     Byte;	   { use pager for output if needed }
    fieldSep:  PChar;	   { field separator }
    tableOpt:  PChar;      { insert to HTML <table ...> }
    caption:   PChar;	   { HTML <caption> }
    fieldName: PPChar; 	   { null terminated array of repalcement field names }
  end;

  PPQprintOpt = ^PQprintOpt;

{ ----------------
  Structure for the conninfo parameter definitions returned by PQconndefaults
  ----------------
}
  PQconninfoOption = packed record
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

  PPQConninfoOption = ^PQconninfoOption;

{ ----------------
  PQArgBlock -- structure for PQfn() arguments
  ----------------
}
  PQArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger);	{ can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PPQArgBlock = ^PQArgBlock;


{ ************** Plain API Function types definition ************* }

{ ===	in fe-connect.c === }
  TPQconnectdb     = function(ConnInfo: PChar): PPGconn; cdecl;
  TPQsetdbLogin    = function(Host, Port, Options, Tty, Db, User, Passwd: PChar): PPGconn; cdecl;
  TPQconndefaults  = function: PPQconninfoOption; cdecl;
  TPQfinish        = procedure(Handle: PPGconn); cdecl;
  TPQreset         = procedure(Handle: PPGconn); cdecl;
  TPQrequestCancel = function(Handle: PPGconn): Integer; cdecl;
  TPQdb            = function(Handle: PPGconn): PChar; cdecl;
  TPQuser          = function(Handle: PPGconn): PChar; cdecl;
  TPQpass          = function(Handle: PPGconn): PChar; cdecl;
  TPQhost          = function(Handle: PPGconn): PChar; cdecl;
  TPQport          = function(Handle: PPGconn): PChar; cdecl;
  TPQtty           = function(Handle: PPGconn): PChar; cdecl;
  TPQoptions       = function(Handle: PPGconn): PChar; cdecl;
  TPQstatus        = function(Handle: PPGconn): ConnStatusType; cdecl;
  TPQerrorMessage  = function(Handle: PPGconn): PChar; cdecl;
  TPQsocket        = function(Handle: PPGconn): Integer; cdecl;
  TPQbackendPID    = function(Handle: PPGconn): Integer; cdecl;
  TPQtrace         = procedure(Handle: PPGconn; DebugPort: Pointer); cdecl;
  TPQuntrace       = procedure(Handle: PPGconn); cdecl;
  TPQsetNoticeProcessor = procedure(Handle: PPGconn; Proc: PQnoticeProcessor; Arg: Pointer); cdecl;

{ === in fe-exec.c === }
  TPQexec          = function(Handle: PPGconn; Query: PChar): PPGresult; cdecl;
  TPQnotifies      = function(Handle: PPGconn): PPGnotify; cdecl;
  TPQfreeNotify    = procedure(Handle: PPGnotify);cdecl;
  TPQsendQuery     = function(Handle: PPGconn; Query: PChar): Integer; cdecl;
  TPQgetResult     = function(Handle: PPGconn): PPGresult; cdecl;
  TPQisBusy        = function(Handle: PPGconn): Integer; cdecl;
  TPQconsumeInput  = function(Handle: PPGconn): Integer; cdecl;
  TPQgetline       = function(Handle: PPGconn; Str: PChar; length: Integer): Integer; cdecl;
  TPQputline       = function(Handle: PPGconn; Str: PChar): Integer; cdecl;
  TPQgetlineAsync  = function(Handle: PPGconn; Buffer: PChar; BufSize: Integer): Integer; cdecl;
  TPQputnbytes     = function(Handle: PPGconn; Buffer: PChar; NBytes: Integer): Integer; cdecl;
  TPQendcopy       = function(Handle: PPGconn): Integer; cdecl;
  TPQfn            = function(Handle: PPGconn; fnid: Integer; result_buf, result_len: PInteger; result_is_int: Integer; args: PPQArgBlock; nargs: Integer): PPGresult; cdecl;
  TPQresultStatus  = function(Result: PPGresult): ExecStatusType; cdecl;
  TPQresultErrorMessage = function(Result: PPGresult): PChar; cdecl;
  TPQntuples       = function(Result: PPGresult): Integer; cdecl;
  TPQnfields       = function(Result: PPGresult): Integer; cdecl;
  TPQbinaryTuples  = function(Result: PPGresult): Integer; cdecl;
  TPQfname         = function(Result: PPGresult; field_num: Integer): PChar; cdecl;
  TPQfnumber       = function(Result: PPGresult; field_name: PChar): Integer; cdecl;
  TPQftype         = function(Result: PPGresult; field_num: Integer): Oid; cdecl;
  TPQfsize         = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQfmod          = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQcmdStatus     = function(Result: PPGresult): PChar; cdecl;
  TPQoidValue      = function(Result: PPGresult): Oid; cdecl;
  TPQoidStatus     = function(Result: PPGresult): PChar; cdecl;
  TPQcmdTuples     = function(Result: PPGresult): PChar; cdecl;
  TPQgetvalue      = function(Result: PPGresult; tup_num, field_num: Integer): PChar; cdecl;
  TPQgetlength     = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQgetisnull     = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQclear         = procedure(Result: PPGresult); cdecl;
  TPQmakeEmptyPGresult  = function(Handle: PPGconn; status: ExecStatusType): PPGresult; cdecl;

{ === in fe-lobj.c === }
  Tlo_open         = function(Handle: PPGconn; lobjId: Oid; mode: Integer): Integer; cdecl;
  Tlo_close        = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_read         = function(Handle: PPGconn; fd: Integer; buf: PChar; len: Integer): Integer; cdecl;
  Tlo_write        = function(Handle: PPGconn; fd: Integer; buf: PChar; len: Integer): Integer; cdecl;
  Tlo_lseek        = function(Handle: PPGconn; fd, offset, whence: Integer): Integer; cdecl;
  Tlo_creat        = function(Handle: PPGconn; mode: Integer): Oid; cdecl;
  Tlo_tell         = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_unlink       = function(Handle: PPGconn; lobjId: Oid): Integer; cdecl;
  Tlo_import       = function(Handle: PPGconn; filename: PChar): Oid; cdecl;
  Tlo_export       = function(Handle: PPGconn; lobjId: Oid; filename: PChar): Integer; cdecl;


{ ************* Plain API Function variables definition ************ }

var
{ ===	in fe-connect.c === }
  PQconnectdb:     TPQconnectdb;
  PQsetdbLogin:    TPQsetdbLogin;
  PQconndefaults:  TPQconndefaults;
  PQfinish:        TPQfinish;
  PQreset:         TPQreset;
  PQrequestCancel: TPQrequestCancel;
  PQdb:            TPQdb;
  PQuser:          TPQuser;
  PQpass:          TPQpass;
  PQhost:          TPQhost;
  PQport:          TPQport;
  PQtty:           TPQtty;
  PQoptions:       TPQoptions;
  PQstatus:        TPQstatus;
  PQerrorMessage:  TPQerrorMessage;
  PQsocket:        TPQsocket;
  PQbackendPID:    TPQbackendPID;
  PQtrace:         TPQtrace;
  PQuntrace:       TPQuntrace;
  PQsetNoticeProcessor: TPQsetNoticeProcessor;

{ === in fe-exec.c === }
  PQexec:          TPQexec;
  PQnotifies:      TPQnotifies;
  PQfreeNotify:    TPQfreeNotify;
  PQsendQuery:     TPQsendQuery;
  PQgetResult:     TPQgetResult;
  PQisBusy:        TPQisBusy;
  PQconsumeInput:  TPQconsumeInput;
  PQgetline:       TPQgetline;
  PQputline:       TPQputline;
  PQgetlineAsync:  TPQgetlineAsync;
  PQputnbytes:     TPQputnbytes;
  PQendcopy:       TPQendcopy;
  PQfn:            TPQfn;
  PQresultStatus:  TPQresultStatus;
  PQresultErrorMessage: TPQresultErrorMessage;
  PQntuples:       TPQntuples;
  PQnfields:       TPQnfields;
  PQbinaryTuples:  TPQbinaryTuples;
  PQfname:         TPQfname;
  PQfnumber:       TPQfnumber;
  PQftype:         TPQftype;
  PQfsize:         TPQfsize;
  PQfmod:          TPQfmod;
  PQcmdStatus:     TPQcmdStatus;
  PQoidValue:      TPQoidValue;
  PQoidStatus:     TPQoidStatus;
  PQcmdTuples:     TPQcmdTuples;
  PQgetvalue:      TPQgetvalue;
  PQgetlength:     TPQgetlength;
  PQgetisnull:     TPQgetisnull;
  PQclear:         TPQclear;
  PQmakeEmptyPGresult:  TPQmakeEmptyPGresult;

{ === in fe-lobj.c === }
  lo_open:         Tlo_open;
  lo_close:        Tlo_close;
  lo_read:         Tlo_read;
  lo_write:        Tlo_write;
  lo_lseek:        Tlo_lseek;
  lo_creat:        Tlo_creat;
  lo_tell:         Tlo_tell;
  lo_unlink:       Tlo_unlink;
  lo_import:       Tlo_import;
  lo_export:       Tlo_export;

var
  LibraryLoader: TZNativeLibraryLoader;

implementation

type
  {** Implements a loader for PostgreSQL native library. }
  TZPostgreSQLNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
  end;

{ TZPostgreSQLNativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZPostgreSQLNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

{ ===	in fe-connect.c === }
  @PQconnectdb    := GetAddress('PQconnectdb');
  @PQsetdbLogin   := GetAddress('PQsetdbLogin');
  @PQconndefaults := GetAddress('PQconndefaults');
  @PQfinish       := GetAddress('PQfinish');
  @PQreset        := GetAddress('PQreset');
  @PQrequestCancel := GetAddress('PQrequestCancel');
  @PQdb           := GetAddress('PQdb');
  @PQuser         := GetAddress('PQuser');
  @PQpass         := GetAddress('PQpass');
  @PQhost         := GetAddress('PQhost');
  @PQport         := GetAddress('PQport');
  @PQtty          := GetAddress('PQtty');
  @PQoptions      := GetAddress('PQoptions');
  @PQstatus       := GetAddress('PQstatus');
  @PQerrorMessage := GetAddress('PQerrorMessage');
  @PQsocket       := GetAddress('PQsocket');
  @PQbackendPID   := GetAddress('PQbackendPID');
  @PQtrace        := GetAddress('PQtrace');
  @PQuntrace      := GetAddress('PQuntrace');
  @PQsetNoticeProcessor := GetAddress('PQsetNoticeProcessor');

{ === in fe-exec.c === }
  @PQexec         := GetAddress('PQexec');
  @PQnotifies     := GetAddress('PQnotifies');
  @PQfreeNotify   := GetAddress('PQfreeNotify');
  @PQsendQuery    := GetAddress('PQsendQuery');
  @PQgetResult    := GetAddress('PQgetResult');
  @PQisBusy       := GetAddress('PQisBusy');
  @PQconsumeInput := GetAddress('PQconsumeInput');
  @PQgetline      := GetAddress('PQgetline');
  @PQputline      := GetAddress('PQputline');
  @PQgetlineAsync := GetAddress('PQgetlineAsync');
  @PQputnbytes    := GetAddress('PQputnbytes');
  @PQendcopy      := GetAddress('PQendcopy');
  @PQfn           := GetAddress('PQfn');
  @PQresultStatus := GetAddress('PQresultStatus');
  @PQresultErrorMessage := GetAddress('PQresultErrorMessage');
  @PQntuples      := GetAddress('PQntuples');
  @PQnfields      := GetAddress('PQnfields');
  @PQbinaryTuples := GetAddress('PQbinaryTuples');
  @PQfname        := GetAddress('PQfname');
  @PQfnumber      := GetAddress('PQfnumber');
  @PQftype        := GetAddress('PQftype');
  @PQfsize        := GetAddress('PQfsize');
  @PQfmod         := GetAddress('PQfmod');
  @PQcmdStatus    := GetAddress('PQcmdStatus');
  @PQoidValue     := GetAddress('PQoidValue');
  @PQoidStatus    := GetAddress('PQoidStatus');
  @PQcmdTuples    := GetAddress('PQcmdTuples');
  @PQgetvalue     := GetAddress('PQgetvalue');
  @PQgetlength    := GetAddress('PQgetlength');
  @PQgetisnull    := GetAddress('PQgetisnull');
  @PQclear        := GetAddress('PQclear');
  @PQmakeEmptyPGresult := GetAddress('PQmakeEmptyPGresult');

{ === in fe-lobj.c === }
  @lo_open        := GetAddress('lo_open');
  @lo_close       := GetAddress('lo_close');
  @lo_read        := GetAddress('lo_read');
  @lo_write       := GetAddress('lo_write');
  @lo_lseek       := GetAddress('lo_lseek');
  @lo_creat       := GetAddress('lo_creat');
  @lo_tell        := GetAddress('lo_tell');
  @lo_unlink      := GetAddress('lo_unlink');
  @lo_import      := GetAddress('lo_import');
  @lo_export      := GetAddress('lo_export');
end;

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZPostgreSQLNativeLibraryLoader.Create(
    [WINDOWS1_DLL_LOCATION
{$IFNDEF STRICT_DLL_LOADING}
    , WINDOWS2_DLL_LOCATION
{$ENDIF}
    ]);
{$ELSE}
  LibraryLoader := TZPostgreSQLNativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION]);
{$ENDIF}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
end.
