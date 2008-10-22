{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{    Delphi plain interface to SybaseASE using dblib      }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZPlainDbLibSybaseAse125;

interface

{$I ZPlain.inc}

uses Classes, ZCompatibility, ZPlainLoader, ZPlainDbLibDriver;

const
  WINDOWS_DLL_LOCATION = 'libsybdb.dll';
  LINUX_DLL_LOCATION = 'libsybdb.so';

{ Macros for dbsetlname() }
  DBSETHOST             = 1;
  DBSETUSER             = 2;
  DBSETPWD              = 3;
  DBSETHID              = 4;
  DBSETAPP              = 5;
  DBSETBCP              = 6;
  DBSETLANG             = 7;
  DBSETNOSHORT          = 8;
  DBSETHIER             = 9;
  DBSETCHARSET          = 10;
  DBSETPACKET           = 11;
  DBSETENCRYPT          = 12;
  DBSETLABELED          = 13;

{ Macros for setting the PLOGINREC }
function DBSETLHOST(Login: PLOGINREC; ClientHost: PChar): RETCODE;
function DBSETLUSER(Login: PLOGINREC; UserName: PChar): RETCODE;
function DBSETLPWD(Login: PLOGINREC; Passwd: PChar): RETCODE;
function DBSETLAPP(Login: PLOGINREC; AppName: PChar): RETCODE;
function DBSETLNATLANG(Login: PLOGINREC; Lang: PChar): RETCODE;
function DBSETLCHARSET(Login: PLOGINREC; Charset: PChar): RETCODE;

{ Function macros }
function dbrbuf(Proc: PDBPROCESS): DBINT;


{************** Plain API Function types definition *************}

type
  DBERRHANDLE_PROC = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PChar): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  DBMSGHANDLE_PROC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PChar; Line: DBUSMALLINT):
    Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tdb12hour = function(Proc: PDBPROCESS; Language: PChar): DBBOOL; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tdberrhandle = function(Handler: DBERRHANDLE_PROC): DBERRHANDLE_PROC; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbmsghandle = function(Handler: DBMSGHANDLE_PROC): DBMSGHANDLE_PROC; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Two-phase commit functions }
  Tabort_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbuild_xact_string = procedure(XActName, Service: PChar; CommId: DBINT;
    Result: PChar); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tclose_commit = procedure(Proc: PDBPROCESS); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tcommit_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Topen_commit = function(Login: PLOGINREC; ServerName: PChar): PDBPROCESS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tremove_xact = function(Proc: PDBPROCESS; CommId: DBINT; SiteCount: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tscan_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tstart_xact = function(Proc: PDBPROCESS; AppName, XActName: PChar;
    SiteCount: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tstat_xact = function(Proc: PDBPROCESS; CommId: DBINT): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{ BCP functions }
  Tbcp_batch = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_bind = function(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
    VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_colfmt = function(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
    FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
    TableColumn: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_collen = function(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_colptr = function(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_columns = function(Proc: PDBPROCESS; FileColCount: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_control = function(Proc: PDBPROCESS; Field: Integer; Value: DBINT):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_done = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_exec = function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_init = function(Proc: PDBPROCESS; TableName, hFile, ErrFile: PChar;
    Direction: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_moretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_readfmt = function(Proc: PDBPROCESS; FileName: PChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_sendrow = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tbcp_writefmt = function(Proc: PDBPROCESS; FileName: PChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{ Standard DB-Library functions }
  Tdbadata = function(Proc: PDBPROCESS; ComputeId, Column: Integer): PByte; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbadlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbaltbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbaltcolid = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbaltlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbaltop = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbalttype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbaltutype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbanullbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    Indicator: PDBINT): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbbind = function(Proc: PDBPROCESS; Column, VarType, VarLen: Integer;
    VarAddr: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbbylist = function(Proc: PDBPROCESS; ComputeId: Integer; Size: PInteger):
    PByte; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcancel = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcanquery = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbchange = function(Proc: PDBPROCESS): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbclose = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbclrbuf = procedure(Proc: PDBPROCESS; N: DBINT); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbclropt = function(Proc: PDBPROCESS; Option: Integer; Param: PChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcmd = function(Proc: PDBPROCESS; Cmd: PChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcmdrow = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcollen = function(Proc: PDBPROCESS; Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcolname = function(Proc: PDBPROCESS; Column: Integer): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcolsource = function(Proc: PDBPROCESS; Column: Integer): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
//  Tdbcoltypeinfo = function(Proc: PDBPROCESS; Column: Integer): PDBTYPEINFO; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcoltype = function(Proc: PDBPROCESS; Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcolutype = function(Proc: PDBPROCESS; Column: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbconvert = function(Proc: PDBPROCESS; SrcType: Integer; Src: PByte;
    SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcount = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcurcmd = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcurrow = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tdbcursor = function(hCursor: PDBCURSOR; OpType, Row: Integer; Table,
    Values: PChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen: DBINT;
    POutLen: PDBINT; VarAddr: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcursorclose = function(DbHandle: PDBHANDLE): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: Integer; ColName: PChar;
    ColType: PInteger; ColLen: PDBINT; UserType: PInteger): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcursorfetch = function(hCursor: PDBCURSOR; FetchType, RowNum: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcursorinfo = function(hCursor: PDBCURSOR; nCols: PInteger; nRows: PDBINT):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbcursoropen = function(Proc: PDBPROCESS; Sql: PChar; ScrollOpt,
    ConCurOpt: Integer; nRows: Cardinal; PStatus: PDBINT): PDBCURSOR; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbdata = function(Proc: PDBPROCESS; Column: Integer): PByte; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbdatecrack = function(Proc: PDBPROCESS; DateInfo: PDBDATEREC;
    DateType: PDBDATETIME): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbdatlen = function(Proc: PDBPROCESS; Column: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbdead = function(Proc: PDBPROCESS): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbexit = procedure; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbfcmd = function(Proc: PDBPROCESS; CmdString: PChar; var Params): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbfirstrow = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbfreebuf = procedure(Proc: PDBPROCESS); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbloginfree = procedure(Login: PLOGINREC); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbfreequal = procedure(Ptr: PChar); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbgetchar = function(Proc: PDBPROCESS; N: Integer): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbgetmaxprocs = function: SmallInt; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbgetoff = function(Proc: PDBPROCESS; OffType: DBUSMALLINT;
    StartFrom: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbgetpacket = function(Proc: PDBPROCESS): Cardinal; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbgetrow = function(Proc: PDBPROCESS; Row: DBINT): STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbgetuserdata = function(Proc: PDBPROCESS): Pointer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbhasretstat = function(Proc: PDBPROCESS): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbinit = function: RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbisavail = function(Proc: PDBPROCESS): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbisopt = function(Proc: PDBPROCESS; Option: Integer; Param: PChar): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdblastrow = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdblogin = function: PLOGINREC; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbmorecmds = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbmoretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbname = function(Proc: PDBPROCESS): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbnextrow = function(Proc: PDBPROCESS): STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbnullbind = function(Proc: PDBPROCESS; Column: Integer; Indicator: PDBINT):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbnumalts = function(Proc: PDBPROCESS; ComputeId: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbnumcols = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbnumcompute = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbnumorders = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbnumrets = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbopen = function(Login: PLOGINREC; Host: PChar): PDBPROCESS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbordercol = function(Proc: PDBPROCESS; Order: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbprhead = procedure(Proc: PDBPROCESS); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbprrow = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbprtype = function(Token: Integer): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbqual = function(Proc: PDBPROCESS; TabNum: Integer; TabName: PChar): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbreadtext = function(Proc: PDBPROCESS; Buf: Pointer; BufSize: DBINT): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbresults = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbretdata = function(Proc: PDBPROCESS; RetNum: Integer): PByte; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbretlen = function(Proc: PDBPROCESS; RetNum: Integer): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbretname = function(Proc: PDBPROCESS; RetNum: Integer): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbretstatus = function(Proc: PDBPROCESS): DBINT; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbrettype = function(Proc: PDBPROCESS; RetNum: Integer): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbrows = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF}; //!!!
  Tdbrowtype = function(Proc: PDBPROCESS): STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbrpcinit = function(Proc: PDBPROCESS; ProcName: PChar; Options: DBSMALLINT):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF}; //!!!
  Tdbrpcparam = function(Proc: PDBPROCESS; ParamName: PChar; Status: Byte;
    Typ: Integer; MaxLen, DataLen: DBINT; Value: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbrpcsend = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tdbrpwclr = procedure(Login: PLOGINREC); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsetavail = procedure(Proc: PDBPROCESS); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsetmaxprocs = function(MaxProcs: SmallInt): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsetlname = function(Login: PLOGINREC; Value: PChar; Item: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsetlogintime = function(Seconds: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tdbsetnull = function(Proc: PDBPROCESS; BindType, BindLen: Integer;
    BindVal: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsetopt = function(Proc: PDBPROCESS; Option: Integer; CharParam: PChar; IntParam: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsettime = function(Seconds: Integer): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsetuserdata = procedure(Proc: PDBPROCESS; Ptr: Pointer); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsqlexec = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsqlok = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbsqlsend = function(Proc: PDBPROCESS): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbstrcpy = function(Proc: PDBPROCESS; Start, NumBytes: Integer; Dest: PChar):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbstrlen = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtabbrowse = function(Proc: PDBPROCESS; TabNum: Integer): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtabcount = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtabname = function(Proc: PDBPROCESS; Table: Integer): PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtabsource = function(Proc: PDBPROCESS; Column: Integer; TabNum: PInteger):
    PChar; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtsnewlen = function(Proc: PDBPROCESS): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtsnewval = function(Proc: PDBPROCESS): PDBBINARY; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtsput = function(Proc: PDBPROCESS; NewTs: PDBBINARY; NewTsName,
    TabNum: Integer; TableName: PChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtxptr = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtxtimestamp = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtxtsnewval = function(Proc: PDBPROCESS): PDBBINARY; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbtxtsput = function(Proc: PDBPROCESS; NewTxts: PDBBINARY; Column: Integer):
    RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbuse = function(Proc: PDBPROCESS; DbName: PChar): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbvarylen = function(Proc: PDBPROCESS; Column: Integer): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbwillconvert = function(SrcType, DestType: Integer): LongBool; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tdbwritetext = function(Proc: PDBPROCESS; ObjName: PChar; TextPtr: PDBBINARY;
    TextPtrLen: DBTINYINT; Timestamp: PDBBINARY; Log: LongBool; Size: DBINT;
    Text: PByte): RETCODE; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{************* Plain API Function variables definition ************}

var
  db12hour              : Tdb12hour;
  
  dberrhandle           : Tdberrhandle;
  dbmsghandle           : Tdbmsghandle;

  { Two-phase commit functions }
  abort_xact            : Tabort_xact;
  build_xact_string     : Tbuild_xact_string;
  close_commit          : Tclose_commit;
  commit_xact           : Tcommit_xact;
  open_commit           : Topen_commit;
  remove_xact           : Tremove_xact;
  scan_xact             : Tscan_xact;
  start_xact            : Tstart_xact;
  stat_xact             : Tstat_xact;

{ BCP functions }
  bcp_batch             : Tbcp_batch;
  bcp_bind              : Tbcp_bind;
  bcp_colfmt            : Tbcp_colfmt;
  bcp_collen            : Tbcp_collen;
  bcp_colptr            : Tbcp_colptr;
  bcp_columns           : Tbcp_columns;
  bcp_control           : Tbcp_control;
  bcp_done              : Tbcp_done;
  bcp_exec              : Tbcp_exec;
  bcp_init              : Tbcp_init;
  bcp_moretext          : Tbcp_moretext;
  bcp_readfmt           : Tbcp_readfmt;
  bcp_sendrow           : Tbcp_sendrow;
  bcp_writefmt          : Tbcp_writefmt;

{ Standard DB-Library functions }
  dbadata               : Tdbadata;
  dbadlen               : Tdbadlen;
  dbaltbind             : Tdbaltbind;
  dbaltcolid            : Tdbaltcolid;
  dbaltlen              : Tdbaltlen;
  dbaltop               : Tdbaltop;
  dbalttype             : Tdbalttype;
  dbaltutype            : Tdbaltutype;
  dbanullbind           : Tdbanullbind;
  dbbind                : Tdbbind;
  dbbylist              : Tdbbylist;
  dbcancel              : Tdbcancel;
  dbcanquery            : Tdbcanquery;
  dbchange              : Tdbchange;
  dbclose               : Tdbclose;
  dbclrbuf              : Tdbclrbuf;
  dbclropt              : Tdbclropt;
  dbcmd                 : Tdbcmd;
  dbcmdrow              : Tdbcmdrow;
  dbcolbrowse           : Tdbcolbrowse;
  dbcollen              : Tdbcollen;
  dbcolname             : Tdbcolname;
  dbcolsource           : Tdbcolsource;
//  dbcoltypeinfo         : Tdbcoltypeinfo;
  dbcoltype             : Tdbcoltype;
  dbcolutype            : Tdbcolutype;
  dbconvert             : Tdbconvert;
  dbcount               : Tdbcount;
  dbcurcmd              : Tdbcurcmd;
  dbcurrow              : Tdbcurrow;

  dbcursor              : Tdbcursor;
  dbcursorbind          : Tdbcursorbind;
  dbcursorclose         : Tdbcursorclose;
  dbcursorcolinfo       : Tdbcursorcolinfo;
  dbcursorfetch         : Tdbcursorfetch;
  dbcursorinfo          : Tdbcursorinfo;
  dbcursoropen          : Tdbcursoropen;
  dbdata                : Tdbdata;
  dbdatecrack           : Tdbdatecrack;
  dbdatlen              : Tdbdatlen;
  dbdead                : Tdbdead;
  dbexit                : Tdbexit;
  dbfcmd                : Tdbfcmd;
  dbfirstrow            : Tdbfirstrow;
  dbfreebuf             : Tdbfreebuf;
  dbloginfree           : Tdbloginfree;
  dbfreequal            : Tdbfreequal;
  dbgetchar             : Tdbgetchar;
  dbgetmaxprocs         : Tdbgetmaxprocs;
  dbgetoff              : Tdbgetoff;
  dbgetpacket           : Tdbgetpacket;
  dbgetrow              : Tdbgetrow;
  dbgetuserdata         : Tdbgetuserdata;
  dbhasretstat          : Tdbhasretstat;
  dbinit                : Tdbinit;
  dbisavail             : Tdbisavail;
  dbisopt               : Tdbisopt;
  dblastrow             : Tdblastrow;
  dblogin               : Tdblogin;
  dbmorecmds            : Tdbmorecmds;
  dbmoretext            : Tdbmoretext;
  dbname                : Tdbname;
  dbnextrow             : Tdbnextrow;
  dbnullbind            : Tdbnullbind;
  dbnumalts             : Tdbnumalts;
  dbnumcols             : Tdbnumcols;
  dbnumcompute          : Tdbnumcompute;
  dbnumorders           : Tdbnumorders;
  dbnumrets             : Tdbnumrets;
  dbopen                : Tdbopen;
  dbordercol            : Tdbordercol;
  dbprhead              : Tdbprhead;
  dbprrow               : Tdbprrow;
  dbprtype              : Tdbprtype;
  dbqual                : Tdbqual;
  dbreadtext            : Tdbreadtext;
  dbresults             : Tdbresults;
  dbretdata             : Tdbretdata;
  dbretlen              : Tdbretlen;
  dbretname             : Tdbretname;
  dbretstatus           : Tdbretstatus;
  dbrettype             : Tdbrettype;
  dbrows                : Tdbrows;
  dbrowtype             : Tdbrowtype;
  dbrpcinit             : Tdbrpcinit;
  dbrpcparam            : Tdbrpcparam;
  dbrpcsend             : Tdbrpcsend;

  dbrpwclr              : Tdbrpwclr;
  dbsetavail            : Tdbsetavail;
  dbsetmaxprocs         : Tdbsetmaxprocs;
  dbsetlname            : Tdbsetlname;
  dbsetlogintime        : Tdbsetlogintime;

  dbsetnull             : Tdbsetnull;
  dbsetopt              : Tdbsetopt;
  dbsettime             : Tdbsettime;
  dbsetuserdata         : Tdbsetuserdata;
  dbsqlexec             : Tdbsqlexec;
  dbsqlok               : Tdbsqlok;
  dbsqlsend             : Tdbsqlsend;
  dbstrcpy              : Tdbstrcpy;
  dbstrlen              : Tdbstrlen;
  dbtabbrowse           : Tdbtabbrowse;
  dbtabcount            : Tdbtabcount;
  dbtabname             : Tdbtabname;
  dbtabsource           : Tdbtabsource;
  dbtsnewlen            : Tdbtsnewlen;
  dbtsnewval            : Tdbtsnewval;
  dbtsput               : Tdbtsput;
  dbtxptr               : Tdbtxptr;
  dbtxtimestamp         : Tdbtxtimestamp;
  dbtxtsnewval          : Tdbtxtsnewval;
  dbtxtsput             : Tdbtxtsput;
  dbuse                 : Tdbuse;
  dbvarylen             : Tdbvarylen;
  dbwillconvert         : Tdbwillconvert;
  dbwritetext           : Tdbwritetext;

type
  {** Implements a loader for Sybase native library. }
  TZSybaseNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
    procedure FreeNativeLibrary; override;
  end;

var
  LibraryLoader: TZNativeLibraryLoader;
  SybaseErrors: TList;
  SybaseMessages: TList;

implementation

{ Handle sql server errors }
var
  OldErrorHandle: DBERRHANDLE_PROC = nil;
  OldMessageHandle: DBMSGHANDLE_PROC = nil;

{ Handle sql server error messages }
function ErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PChar): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
var
  SybaseError: PDBLibError;
begin
  New(SybaseError);
  SybaseError.dbProc := Proc;
  SybaseError.Severity := Severity;
  SybaseError.DbErr := DbErr;
  SybaseError.OsErr := OsErr;
  SybaseError.DbErrStr := DbErrStr;
  SybaseError.OsErrStr := OsErrStr;
  SybaseErrors.Add(SybaseError);

  Result := INT_CANCEL;
end;

{ Handle sql server messages }
function MessageHandle(Proc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
  MsgText, SrvName, ProcName: PChar; Line: DBUSMALLINT): Integer; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
var
  SybaseMessage: PDBLibMessage;
begin
  New(SybaseMessage);
  SybaseMessage.dbProc := Proc;
  SybaseMessage.MsgNo := MsgNo;
  SybaseMessage.MsgState := MsgState;
  SybaseMessage.Severity := Severity;
  SybaseMessage.MsgText := MsgText;
  SybaseMessage.SrvName := SrvName;
  SybaseMessage.ProcName := ProcName;
  SybaseMessage.Line := Line;
  SybaseMessages.Add(SybaseMessage);

  Result := 0;
end;

function DBSETLHOST(Login: PLOGINREC; ClientHost: PChar): RETCODE;
begin
  Result := dbsetlname(Login, ClientHost, DBSETHOST);
end;

function DBSETLUSER(Login: PLOGINREC; UserName: PChar): RETCODE;
begin
  Result := dbsetlname(Login, UserName, DBSETUSER);
end;

function DBSETLPWD(Login: PLOGINREC; Passwd: PChar): RETCODE;
begin
  Result := dbsetlname(Login, Passwd, DBSETPWD);
end;

function DBSETLAPP(Login: PLOGINREC; AppName: PChar): RETCODE;
begin
  Result := dbsetlname(Login, AppName, DBSETAPP);
end;

function DBSETLNATLANG(Login: PLOGINREC; Lang: PChar): RETCODE;
begin
  Result := dbsetlname(Login, Lang, DBSETLANG);
end;

function DBSETLCHARSET(Login: PLOGINREC; Charset: PChar): RETCODE;
begin
  Result := dbsetlname(Login, Charset, DBSETCHARSET);
end;

function dbrbuf(Proc: PDBPROCESS): DBINT;
begin
  Result := 0;
end;

{ TZSybaseNativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZSybaseNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;
  if not Result then
    Exit;

  @db12hour              := GetAddress('db12hour');
  @dberrhandle           := GetAddress('dberrhandle');
  @dbmsghandle           := GetAddress('dbmsghandle');
  @abort_xact            := GetAddress('abort_xact');
  @build_xact_string     := GetAddress('build_xact_string');
  @close_commit          := GetAddress('close_commit');
  @commit_xact           := GetAddress('commit_xact');
  @open_commit           := GetAddress('open_commit');
  @remove_xact           := GetAddress('remove_xact');
  @scan_xact             := GetAddress('scan_xact');
  @start_xact            := GetAddress('start_xact');
  @stat_xact             := GetAddress('stat_xact');
  @bcp_batch             := GetAddress('bcp_batch');
  @bcp_bind              := GetAddress('bcp_bind');
  @bcp_colfmt            := GetAddress('bcp_colfmt');
  @bcp_collen            := GetAddress('bcp_collen');
  @bcp_colptr            := GetAddress('bcp_colptr');
  @bcp_columns           := GetAddress('bcp_columns');
  @bcp_control           := GetAddress('bcp_control');
  @bcp_done              := GetAddress('bcp_done');
  @bcp_exec              := GetAddress('bcp_exec');
  @bcp_init              := GetAddress('bcp_init');
  @bcp_moretext          := GetAddress('bcp_moretext');
  @bcp_readfmt           := GetAddress('bcp_readfmt');
  @bcp_sendrow           := GetAddress('bcp_sendrow');
  @bcp_writefmt          := GetAddress('bcp_writefmt');
  @dbadata               := GetAddress('dbadata');
  @dbadlen               := GetAddress('dbadlen');
  @dbaltbind             := GetAddress('dbaltbind');
  @dbaltcolid            := GetAddress('dbaltcolid');
  @dbaltlen              := GetAddress('dbaltlen');
  @dbaltop               := GetAddress('dbaltop');
  @dbalttype             := GetAddress('dbalttype');
  @dbaltutype            := GetAddress('dbaltutype');
  @dbanullbind           := GetAddress('dbanullbind');
  @dbbind                := GetAddress('dbbind');
  @dbbylist              := GetAddress('dbbylist');
  @dbcancel              := GetAddress('dbcancel');
  @dbcanquery            := GetAddress('dbcanquery');
  @dbchange              := GetAddress('dbchange');
  @dbclose               := GetAddress('dbclose');
  @dbclrbuf              := GetAddress('dbclrbuf');
  @dbclropt              := GetAddress('dbclropt');
  @dbcmd                 := GetAddress('dbcmd');
  @dbcmdrow              := GetAddress('dbcmdrow');
  @dbcolbrowse           := GetAddress('dbcolbrowse');
  @dbcollen              := GetAddress('dbcollen');
  @dbcolname             := GetAddress('dbcolname');
  @dbcolsource           := GetAddress('dbcolsource');
//  @dbcoltypeinfo         := GetAddress('dbcoltypeinfo');
  @dbcoltype             := GetAddress('dbcoltype');
  @dbcolutype            := GetAddress('dbcolutype');
  @dbconvert             := GetAddress('dbconvert');
  @dbcount               := GetAddress('dbcount');
  @dbcurcmd              := GetAddress('dbcurcmd');
  @dbcurrow              := GetAddress('dbcurrow');
  @dbcursor              := GetAddress('dbcursor');
  @dbcursorbind          := GetAddress('dbcursorbind');
  @dbcursorclose         := GetAddress('dbcursorclose');
  @dbcursorcolinfo       := GetAddress('dbcursorcolinfo');
  @dbcursorfetch         := GetAddress('dbcursorfetch');
  @dbcursorinfo          := GetAddress('dbcursorinfo');
  @dbcursoropen          := GetAddress('dbcursoropen');
  @dbdata                := GetAddress('dbdata');
  @dbdatecrack           := GetAddress('dbdatecrack');
  @dbdatlen              := GetAddress('dbdatlen');
  @dbdead                := GetAddress('dbdead');
  @dbexit                := GetAddress('dbexit');
  @dbfcmd                := GetAddress('dbfcmd');
  @dbfirstrow            := GetAddress('dbfirstrow');
  @dbfreebuf             := GetAddress('dbfreebuf');
  @dbloginfree           := GetAddress('dbloginfree');
  @dbfreequal            := GetAddress('dbfreequal');
  @dbgetchar             := GetAddress('dbgetchar');
  @dbgetmaxprocs         := GetAddress('dbgetmaxprocs');
  @dbgetoff              := GetAddress('dbgetoff');
  @dbgetpacket           := GetAddress('dbgetpacket');
  @dbgetrow              := GetAddress('dbgetrow');
  @dbgetuserdata         := GetAddress('dbgetuserdata');
  @dbhasretstat          := GetAddress('dbhasretstat');
  @dbinit                := GetAddress('dbinit');
  @dbisavail             := GetAddress('dbisavail');
  @dbisopt               := GetAddress('dbisopt');
  @dblastrow             := GetAddress('dblastrow');
  @dblogin               := GetAddress('dblogin');
  @dbmorecmds            := GetAddress('dbmorecmds');
  @dbmoretext            := GetAddress('dbmoretext');
  @dbname                := GetAddress('dbname');
  @dbnextrow             := GetAddress('dbnextrow');
  @dbnullbind            := GetAddress('dbnullbind');
  @dbnumalts             := GetAddress('dbnumalts');
  @dbnumcols             := GetAddress('dbnumcols');
  @dbnumcompute          := GetAddress('dbnumcompute');
  @dbnumorders           := GetAddress('dbnumorders');
  @dbnumrets             := GetAddress('dbnumrets');
  @dbopen                := GetAddress('dbopen');
  @dbordercol            := GetAddress('dbordercol');
  @dbprhead              := GetAddress('dbprhead');
  @dbprrow               := GetAddress('dbprrow');
  @dbprtype              := GetAddress('dbprtype');
  @dbqual                := GetAddress('dbqual');
  @dbreadtext            := GetAddress('dbreadtext');
  @dbresults             := GetAddress('dbresults');
  @dbretdata             := GetAddress('dbretdata');
  @dbretlen              := GetAddress('dbretlen');
  @dbretname             := GetAddress('dbretname');
  @dbretstatus           := GetAddress('dbretstatus');
  @dbrettype             := GetAddress('dbrettype');
  @dbrows                := GetAddress('dbrows');
  @dbrowtype             := GetAddress('dbrowtype');
  @dbrpcinit             := GetAddress('dbrpcinit');
  @dbrpcparam            := GetAddress('dbrpcparam');
  @dbrpcsend             := GetAddress('dbrpcsend');
  @dbrpwclr              := GetAddress('dbrpwclr');
  @dbsetavail            := GetAddress('dbsetavail');
  @dbsetmaxprocs         := GetAddress('dbsetmaxprocs');
  @dbsetlname            := GetAddress('dbsetlname');
  @dbsetlogintime        := GetAddress('dbsetlogintime');
  @dbsetnull             := GetAddress('dbsetnull');
  @dbsetopt              := GetAddress('dbsetopt');
  @dbsettime             := GetAddress('dbsettime');
  @dbsetuserdata         := GetAddress('dbsetuserdata');
  @dbsqlexec             := GetAddress('dbsqlexec');
  @dbsqlok               := GetAddress('dbsqlok');
  @dbsqlsend             := GetAddress('dbsqlsend');
  @dbstrcpy              := GetAddress('dbstrcpy');
  @dbstrlen              := GetAddress('dbstrlen');
  @dbtabbrowse           := GetAddress('dbtabbrowse');
  @dbtabcount            := GetAddress('dbtabcount');
  @dbtabname             := GetAddress('dbtabname');
  @dbtabsource           := GetAddress('dbtabsource');
  @dbtsnewlen            := GetAddress('dbtsnewlen');
  @dbtsnewval            := GetAddress('dbtsnewval');
  @dbtsput               := GetAddress('dbtsput');
  @dbtxptr               := GetAddress('dbtxptr');
  @dbtxtimestamp         := GetAddress('dbtxtimestamp');
  @dbtxtsnewval          := GetAddress('dbtxtsnewval');
  @dbtxtsput             := GetAddress('dbtxtsput');
  @dbuse                 := GetAddress('dbuse');
  @dbvarylen             := GetAddress('dbvarylen');
  @dbwillconvert         := GetAddress('dbwillconvert');
  @dbwritetext           := GetAddress('dbwritetext');

  dbInit;

  OldErrorHandle := dberrhandle(ErrorHandle);
  OldMessageHandle := dbmsghandle(MessageHandle);
end;

procedure TZSybaseNativeLibraryLoader.FreeNativeLibrary;
begin
  dberrhandle(OldErrorHandle);
  dbmsghandle(OldMessageHandle);
  dbExit;

  inherited;
end;

initialization
  SybaseErrors := TList.Create;
  SybaseMessages := TList.Create;

{$IFNDEF UNIX}
  LibraryLoader := TZSybaseNativeLibraryLoader.Create(
    [WINDOWS_DLL_LOCATION]);
{$ELSE}
  LibraryLoader := TZSybaseNativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION]);
{$ENDIF}

finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;

//Free any record in the list if any
  while SybaseMessages.Count > 0 do
  begin
    Dispose(SybaseMessages.Items[0]);
    SybaseMessages.Delete(0);
  end;
  if SybaseMessages <> nil then
  begin
    SybaseMessages.Free;
    SybaseMessages := nil;
  end;

//Free any record in the list if any
  while SybaseErrors.Count > 0 do
  begin
    Dispose(SybaseErrors.Items[0]);
    SybaseErrors.Delete(0);
  end;
  if SybaseErrors <> nil then
  begin
    SybaseErrors.Free;
    SybaseErrors := nil;
  end;
end.



