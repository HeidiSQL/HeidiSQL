{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Delphi plain interface to MSSql using dblib        }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Janos Fegyverneki                 }
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

unit ZPlainDbLibMsSql7;

interface

{$I ZPlain.inc}

uses Classes, ZCompatibility, ZPlainLoader, ZPlainDBLibDriver;

const
  WINDOWS_DLL_LOCATION = 'ntwdblib.dll';

{ Macros for dbsetlname() }
  DBSETHOST             = 1;
  DBSETUSER             = 2;
  DBSETPWD              = 3;
  DBSETAPP              = 4;
  DBSETID               = 5;
  DBSETLANG             = 6;

  DBSETSECURE           = 7;
  DBVER42               = 8;
  DBVER60               = 9;
  DBSET_LOGIN_TIME      = 10;
  DBSETFALLBACK         = 12;
  
{ Macros for setting the PLOGINREC }
function DBSETLHOST(Login: PLOGINREC; ClientHost: PChar): RETCODE;
function DBSETLUSER(Login: PLOGINREC; UserName: PChar): RETCODE;
function DBSETLPWD(Login: PLOGINREC; Passwd: PChar): RETCODE;
function DBSETLAPP(Login: PLOGINREC; AppName: PChar): RETCODE;
function DBSETLNATLANG(Login: PLOGINREC; Lang: PChar): RETCODE;
function DBSETLSECURE(Login: PLOGINREC): RETCODE;
function DBSETLVERSION(Login: PLOGINREC; Version: Byte): RETCODE;
function DBSETLTIME(Login: PLOGINREC; Seconds: LongWord): RETCODE;
function DBSETLFALLBACK(Login: PLOGINREC; Fallback: PChar): RETCODE;

{ Function macros }
function dbrbuf(Proc: PDBPROCESS): DBINT;


{************** Plain API Function types definition *************}
type
  DBERRHANDLE_PROC = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PChar): Integer; cdecl;
  DBMSGHANDLE_PROC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PChar; Line: DBUSMALLINT):
    Integer; cdecl;

  Tdberrhandle = function(Handler: DBERRHANDLE_PROC): DBERRHANDLE_PROC; cdecl;
  Tdbmsghandle = function(Handler: DBMSGHANDLE_PROC): DBMSGHANDLE_PROC; cdecl;

  Tdbprocerrhandle = function(DbHandle: PDBHANDLE; Handler: DBERRHANDLE_PROC):
    DBERRHANDLE_PROC; cdecl;
  Tdbprocmsghandle = function(DbHandle: PDBHANDLE; Handler: DBMSGHANDLE_PROC):
    DBMSGHANDLE_PROC; cdecl;

  { Two-phase commit functions }
  Tabort_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Tbuild_xact_string = procedure(XActName, Service: PChar; CommId: DBINT;
    Result: PChar); cdecl;
  Tclose_commit = procedure(Proc: PDBPROCESS); cdecl;
  Tcommit_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Topen_commit = function(Login: PLOGINREC; ServerName: PChar): PDBPROCESS; cdecl;
  Tremove_xact = function(Proc: PDBPROCESS; CommId: DBINT; SiteCount: Integer):
    RETCODE; cdecl;
  Tscan_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Tstart_xact = function(Proc: PDBPROCESS; AppName, XActName: PChar;
    SiteCount: Integer): DBINT; cdecl;
  Tstat_xact = function(Proc: PDBPROCESS; CommId: DBINT): Integer; cdecl;

{ BCP functions }
  Tbcp_batch = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tbcp_bind = function(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
    VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_colfmt = function(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
    FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
    TableColumn: Integer): RETCODE; cdecl;
  Tbcp_collen = function(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_colptr = function(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_columns = function(Proc: PDBPROCESS; FileColCount: Integer): RETCODE; cdecl;
  Tbcp_control = function(Proc: PDBPROCESS; Field: Integer; Value: DBINT):
    RETCODE; cdecl;
  Tbcp_done = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tbcp_exec = function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; cdecl;
  Tbcp_init = function(Proc: PDBPROCESS; TableName, hFile, ErrFile: PChar;
    Direction: Integer): RETCODE; cdecl;
  Tbcp_moretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte):
    RETCODE; cdecl;
  Tbcp_readfmt = function(Proc: PDBPROCESS; FileName: PChar): RETCODE; cdecl;
  Tbcp_sendrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tbcp_setl = function(Login: PLOGINREC; Enable: LongBool): RETCODE; cdecl;
  Tbcp_writefmt = function(Proc: PDBPROCESS; FileName: PChar): RETCODE; cdecl;

{ Standard DB-Library functions }
  Tdbadata = function(Proc: PDBPROCESS; ComputeId, Column: Integer): PByte; cdecl;
  Tdbadlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; cdecl;
  Tdbaltcolid = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltop = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbalttype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltutype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbanullbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    Indicator: PDBINT): RETCODE; cdecl;
  Tdbbind = function(Proc: PDBPROCESS; Column, VarType, VarLen: Integer;
    VarAddr: PByte): RETCODE; cdecl;
  Tdbbylist = function(Proc: PDBPROCESS; ComputeId: Integer; Size: PInteger):
    PByte; cdecl;
  Tdbcancel = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbcanquery = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbchange = function(Proc: PDBPROCESS): PChar; cdecl;
  Tdbclose = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbclrbuf = procedure(Proc: PDBPROCESS; N: DBINT); cdecl;
  Tdbclropt = function(Proc: PDBPROCESS; Option: Integer; Param: PChar): RETCODE; cdecl;
  Tdbcmd = function(Proc: PDBPROCESS; Cmd: PChar): RETCODE; cdecl;
  Tdbcmdrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): LongBool; cdecl;
  Tdbcolinfo = function(Handle: PDBHANDLE; Typ, Column, ComputeId: Integer;
    DbColumn: PDBCOL): RETCODE; cdecl;
  Tdbcollen = function(Proc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbcolname = function(Proc: PDBPROCESS; Column: Integer): PChar; cdecl;
  Tdbcolsource = function(Proc: PDBPROCESS; Column: Integer): PChar; cdecl;
  Tdbcoltype = function(Proc: PDBPROCESS; Column: Integer): Integer; cdecl;
  Tdbcolutype = function(Proc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbconvert = function(Proc: PDBPROCESS; SrcType: Integer; Src: PByte;
    SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; cdecl;
  Tdbcount = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbcurcmd = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbcurrow = function(Proc: PDBPROCESS): DBINT; cdecl;

  Tdbcursor = function(hCursor: PDBCURSOR; OpType, Row: Integer; Table,
    Values: PChar): RETCODE; cdecl;
  Tdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen: DBINT;
    POutLen: PDBINT; VarAddr: PByte): RETCODE; cdecl;
  Tdbcursorclose = function(DbHandle: PDBHANDLE): RETCODE; cdecl;
  Tdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: Integer; ColName: PChar;
    ColType: PInteger; ColLen: PDBINT; UserType: PInteger): RETCODE; cdecl;
  Tdbcursorfetch = function(hCursor: PDBCURSOR; FetchType, RowNum: Integer):
    RETCODE; cdecl;
  Tdbcursorfetchex = function(hCursor: PDBCURSOR; FetchType: Integer; RowNum,
    nFetchRows, Reserved: DBINT): RETCODE; cdecl;
  Tdbcursorinfo = function(hCursor: PDBCURSOR; nCols: PInteger; nRows: PDBINT):
    RETCODE; cdecl;
  Tdbcursorinfoex = function(hCursor: PDBCURSOR; DbCursorInfo: PDBCURSORINFO):
    RETCODE; cdecl;
  Tdbcursoropen = function(Proc: PDBPROCESS; Sql: PChar; ScrollOpt,
    ConCurOpt: Integer; nRows: Cardinal; PStatus: PDBINT): PDBCURSOR; cdecl;
  Tdbdata = function(Proc: PDBPROCESS; Column: Integer): PByte; cdecl;
  Tdbdataready = function(Proc: PDBPROCESS): LongBool; cdecl;
  Tdbdatecrack = function(Proc: PDBPROCESS; DateInfo: PDBDATEREC;
    DateType: PDBDATETIME): RETCODE; cdecl;
  Tdbdatlen = function(Proc: PDBPROCESS; Column: Integer): Integer; cdecl;
  Tdbdead = function(Proc: PDBPROCESS): LongBool; cdecl;
  Tdbexit = procedure; cdecl;
  TdbWinexit = procedure; cdecl;
  Tdbenlisttrans = function(Proc: PDBPROCESS; Transaction: Pointer): RETCODE; cdecl;
  Tdbenlistxatrans = function(Proc: PDBPROCESS; EnlistTran: LongBool): RETCODE; cdecl;
  Tdbfcmd = function(Proc: PDBPROCESS; CmdString: PChar; var Params): RETCODE; cdecl;
  Tdbfirstrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbfreebuf = procedure(Proc: PDBPROCESS); cdecl;
  Tdbfreelogin = procedure(Login: PLOGINREC); cdecl;
  Tdbfreequal = procedure(Ptr: PChar); cdecl;
  Tdbgetchar = function(Proc: PDBPROCESS; N: Integer): PChar; cdecl;
  Tdbgetmaxprocs = function: SmallInt; cdecl;
  Tdbgetoff = function(Proc: PDBPROCESS; OffType: DBUSMALLINT;
    StartFrom: Integer): Integer; cdecl;
  Tdbgetpacket = function(Proc: PDBPROCESS): Cardinal; cdecl;
  Tdbgetrow = function(Proc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
  Tdbgettime = function: Integer; cdecl;
  Tdbgetuserdata = function(Proc: PDBPROCESS): Pointer; cdecl;
  Tdbhasretstat = function(Proc: PDBPROCESS): LongBool; cdecl;
  Tdbinit = function: PChar; cdecl;
  Tdbisavail = function(Proc: PDBPROCESS): LongBool; cdecl;
  Tdbiscount = function(Proc: PDBPROCESS): LongBool; cdecl;
  Tdbisopt = function(Proc: PDBPROCESS; Option: Integer; Param: PChar): LongBool; cdecl;
  Tdblastrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdblogin = function: PLOGINREC; cdecl;
  Tdbmorecmds = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbmoretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE; cdecl;
  Tdbname = function(Proc: PDBPROCESS): PChar; cdecl;
  Tdbnextrow = function(Proc: PDBPROCESS): STATUS; cdecl;
  Tdbnullbind = function(Proc: PDBPROCESS; Column: Integer; Indicator: PDBINT):
    RETCODE; cdecl;
  Tdbnumalts = function(Proc: PDBPROCESS; ComputeId: Integer): Integer; cdecl;
  Tdbnumcols = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumcompute = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumorders = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumrets = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbopen = function(Login: PLOGINREC; Host: PChar): PDBPROCESS; cdecl;
  Tdbordercol = function(Proc: PDBPROCESS; Order: Integer): Integer; cdecl;
  Tdbprocinfo = function(Proc: PDBPROCESS; DbProcInfo: PDBPROCINFO): RETCODE; cdecl;
  Tdbprhead = procedure(Proc: PDBPROCESS); cdecl;
  Tdbprrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbprtype = function(Token: Integer): PChar; cdecl;
  Tdbqual = function(Proc: PDBPROCESS; TabNum: Integer; TabName: PChar): PChar; cdecl;
  Tdbreadtext = function(Proc: PDBPROCESS; Buf: Pointer; BufSize: DBINT): DBINT; cdecl;
  Tdbresults = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbretdata = function(Proc: PDBPROCESS; RetNum: Integer): PByte; cdecl;
  Tdbretlen = function(Proc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
  Tdbretname = function(Proc: PDBPROCESS; RetNum: Integer): PChar; cdecl;
  Tdbretstatus = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbrettype = function(Proc: PDBPROCESS; RetNum: Integer): Integer; cdecl;
  Tdbrows = function(Proc: PDBPROCESS): RETCODE; cdecl; //!!!
  Tdbrowtype = function(Proc: PDBPROCESS): STATUS; cdecl;
  Tdbrpcinit = function(Proc: PDBPROCESS; ProcName: PChar; Options: DBSMALLINT):
    RETCODE; cdecl; //!!!
  Tdbrpcparam = function(Proc: PDBPROCESS; ParamName: PChar; Status: Byte;
    Typ: Integer; MaxLen, DataLen: DBINT; Value: PByte): RETCODE; cdecl;
  Tdbrpcsend = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbrpcexec = function(Proc: PDBPROCESS): RETCODE; cdecl;

  Tdbrpwclr = procedure(Login: PLOGINREC); cdecl;
  Tdbserverenum = function(SearchMode: Word; ServNameBuf: PChar;
    ServNameBufSize: Word; NumEntries: PWord): Integer; cdecl;
  Tdbsetavail = procedure(Proc: PDBPROCESS); cdecl;
  Tdbsetmaxprocs = function(MaxProcs: SmallInt): RETCODE; cdecl;
  Tdbsetlname = function(Login: PLOGINREC; Value: PChar; Item: Integer): RETCODE; cdecl;
  Tdbsetlogintime = function(Seconds: Integer): RETCODE; cdecl;

  Tdbsetlpacket = function(Login: PLOGINREC; PacketSize: Word): RETCODE; cdecl;
  Tdbsetnull = function(Proc: PDBPROCESS; BindType, BindLen: Integer;
    BindVal: PByte): RETCODE; cdecl;
  Tdbsetopt = function(Proc: PDBPROCESS; Option: Integer; Param: PChar):
    RETCODE; cdecl;
  Tdbsettime = function(Seconds: Integer): RETCODE; cdecl;
  Tdbsetuserdata = procedure(Proc: PDBPROCESS; Ptr: Pointer); cdecl;
  Tdbsqlexec = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlok = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlsend = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbstrcpy = function(Proc: PDBPROCESS; Start, NumBytes: Integer; Dest: PChar):
    RETCODE; cdecl;
  Tdbstrlen = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtabbrowse = function(Proc: PDBPROCESS; TabNum: Integer): LongBool; cdecl;
  Tdbtabcount = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtabname = function(Proc: PDBPROCESS; Table: Integer): PChar; cdecl;
  Tdbtabsource = function(Proc: PDBPROCESS; Column: Integer; TabNum: PInteger):
    PChar; cdecl;
  Tdbtsnewlen = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtsnewval = function(Proc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtsput = function(Proc: PDBPROCESS; NewTs: PDBBINARY; NewTsName,
    TabNum: Integer; TableName: PChar): RETCODE; cdecl;
  Tdbtxptr = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtimestamp = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtsnewval = function(Proc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtxtsput = function(Proc: PDBPROCESS; NewTxts: PDBBINARY; Column: Integer):
    RETCODE; cdecl;
  Tdbuse = function(Proc: PDBPROCESS; DbName: PChar): RETCODE; cdecl;
  Tdbvarylen = function(Proc: PDBPROCESS; Column: Integer): LongBool; cdecl;
  Tdbwillconvert = function(SrcType, DestType: Integer): LongBool; cdecl;
  Tdbwritetext = function(Proc: PDBPROCESS; ObjName: PChar; TextPtr: PDBBINARY;
    TextPtrLen: DBTINYINT; Timestamp: PDBBINARY; Log: LongBool; Size: DBINT;
    Text: PByte): RETCODE; cdecl;
  Tdbupdatetext = function(Proc: PDBPROCESS; DestObject: PChar; DestTextPtr,
    DestTimestamp: PDBBINARY; UpdateType: Integer; InsertOffset,
    DeleteLength: DBINT; SrcObject: PChar; SrcSize: DBINT; SrcText: PDBBINARY):
    RETCODE; cdecl;

{************* Plain API Function variables definition ************}

var
  dberrhandle           : Tdberrhandle;
  dbmsghandle           : Tdbmsghandle;

  dbprocerrhandle       : Tdbprocerrhandle;
  dbprocmsghandle       : Tdbprocmsghandle;

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
  bcp_setl              : Tbcp_setl;
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
  dbcolinfo             : Tdbcolinfo;
  dbcollen              : Tdbcollen;
  dbcolname             : Tdbcolname;
  dbcolsource           : Tdbcolsource;
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
  dbcursorfetchex       : Tdbcursorfetchex;
  dbcursorinfo          : Tdbcursorinfo;
  dbcursorinfoex        : Tdbcursorinfoex;
  dbcursoropen          : Tdbcursoropen;
  dbdata                : Tdbdata;
  dbdataready           : Tdbdataready;
  dbdatecrack           : Tdbdatecrack;
  dbdatlen              : Tdbdatlen;
  dbdead                : Tdbdead;
  dbexit                : Tdbexit;
  dbWinexit                : TdbWinexit;
  dbenlisttrans         : Tdbenlisttrans;
  dbenlistxatrans       : Tdbenlistxatrans;
  dbfcmd                :Tdbfcmd;
  dbfirstrow            : Tdbfirstrow;
  dbfreebuf             : Tdbfreebuf;
  dbfreelogin           : Tdbfreelogin;
  dbfreequal            : Tdbfreequal;
  dbgetchar             : Tdbgetchar;
  dbgetmaxprocs         : Tdbgetmaxprocs;
  dbgetoff              : Tdbgetoff;
  dbgetpacket           : Tdbgetpacket;
  dbgetrow              : Tdbgetrow;
  dbgettime             : Tdbgettime;
  dbgetuserdata         : Tdbgetuserdata;
  dbhasretstat          : Tdbhasretstat;
  dbinit                : Tdbinit;
  dbisavail             : Tdbisavail;
  dbiscount             : Tdbiscount;
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
  dbprocinfo            : Tdbprocinfo;
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
  dbrpcexec             : Tdbrpcexec;

  dbrpwclr              : Tdbrpwclr;
  dbserverenum          : Tdbserverenum;
  dbsetavail            : Tdbsetavail;
  dbsetmaxprocs         : Tdbsetmaxprocs;
  dbsetlname            : Tdbsetlname;
  dbsetlogintime        : Tdbsetlogintime;

  dbsetlpacket          : Tdbsetlpacket;
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
  dbupdatetext          : Tdbupdatetext;

var
  LibraryLoader: TZNativeLibraryLoader;
  MSSqlErrors: TList;
  MSSqlMessages: TList;

implementation

{ Handle sql server errors }
var
  OldErrorHandle: DBERRHANDLE_PROC = nil;
  OldMessageHandle: DBMSGHANDLE_PROC = nil;

{ Handle sql server error messages }
function ErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PChar): Integer; cdecl;
var
  MSSqlError: PDBLibError;
begin
  New(MSSqlError);
  MSSqlError.dbProc := Proc;
  MSSqlError.Severity := Severity;
  MSSqlError.DbErr := DbErr;
  MSSqlError.OsErr := OsErr;
  MSSqlError.DbErrStr := DbErrStr;
  MSSqlError.OsErrStr := OsErrStr;
  MSSqlErrors.Add(MSSqlError);

  Result := INT_CANCEL;
end;

{ Handle sql server messages }
function MessageHandle(Proc: PDBPROCESS; MsgNo: DBINT; MsgState, Severity: Integer;
  MsgText, SrvName, ProcName: PChar; Line: DBUSMALLINT): Integer; cdecl;
var
  MSSqlMessage: PDBLibMessage;
begin
  New(MSSqlMessage);
  MSSqlMessage.dbProc := Proc;
  MSSqlMessage.MsgNo := MsgNo;
  MSSqlMessage.MsgState := MsgState;
  MSSqlMessage.Severity := Severity;
  MSSqlMessage.MsgText := MsgText;
  MSSqlMessage.SrvName := SrvName;
  MSSqlMessage.ProcName := ProcName;
  MSSqlMessage.Line := Line;
  MSSqlMessages.Add(MSSqlMessage);

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

function DBSETLSECURE(Login: PLOGINREC): RETCODE;
begin
  Result := dbsetlname(Login, nil, DBSETSECURE);
end;

function DBSETLVERSION(Login: PLOGINREC; Version: Byte): RETCODE;
begin
  Result := dbsetlname(Login, nil, Version);
end;

function DBSETLTIME(Login: PLOGINREC; Seconds: LongWord): RETCODE;
begin
  Result := dbsetlname(Login, PChar(Cardinal(Seconds)), DBSET_LOGIN_TIME);
end;

function DBSETLFALLBACK(Login: PLOGINREC; Fallback: PChar): RETCODE;
begin
  Result := dbsetlname(Login, Fallback, DBSETFALLBACK);
end;

function dbrbuf(Proc: PDBPROCESS): DBINT;
begin
  Result := DBINT(dbdataready(Proc));
end;

type
  {** Implements a loader for MSSql native library. }
  TZMSSqlNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
    procedure FreeNativeLibrary; override;
  end;

{ TZMSSqlNativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZMSSqlNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;
  if not Result then
    Exit;

  @dberrhandle           := GetAddress('dberrhandle');
  @dbmsghandle           := GetAddress('dbmsghandle');
  @dbprocerrhandle       := GetAddress('dbprocerrhandle');
  @dbprocmsghandle       := GetAddress('dbprocmsghandle');
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
  @bcp_setl              := GetAddress('bcp_setl');
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
  @dbcolinfo             := GetAddress('dbcolinfo');
  @dbcollen              := GetAddress('dbcollen');
  @dbcolname             := GetAddress('dbcolname');
  @dbcolsource           := GetAddress('dbcolsource');
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
  @dbcursorfetchex       := GetAddress('dbcursorfetchex');
  @dbcursorinfo          := GetAddress('dbcursorinfo');
  @dbcursorinfoex        := GetAddress('dbcursorinfoex');
  @dbcursoropen          := GetAddress('dbcursoropen');
  @dbdata                := GetAddress('dbdata');
  @dbdataready           := GetAddress('dbdataready');
  @dbdatecrack           := GetAddress('dbdatecrack');
  @dbdatlen              := GetAddress('dbdatlen');
  @dbdead                := GetAddress('dbdead');
  @dbexit                := GetAddress('dbexit');
  @dbWinexit             := GetAddress('dbwinexit');
  @dbenlisttrans         := GetAddress('dbenlisttrans');
  @dbenlistxatrans       := GetAddress('dbenlistxatrans');
  @dbfcmd                := GetAddress('dbfcmd');
  @dbfirstrow            := GetAddress('dbfirstrow');
  @dbfreebuf             := GetAddress('dbfreebuf');
  @dbfreelogin           := GetAddress('dbfreelogin');
  @dbfreequal            := GetAddress('dbfreequal');
  @dbgetchar             := GetAddress('dbgetchar');
  @dbgetmaxprocs         := GetAddress('dbgetmaxprocs');
  @dbgetoff              := GetAddress('dbgetoff');
  @dbgetpacket           := GetAddress('dbgetpacket');
  @dbgetrow              := GetAddress('dbgetrow');
  @dbgettime             := GetAddress('dbgettime');
  @dbgetuserdata         := GetAddress('dbgetuserdata');
  @dbhasretstat          := GetAddress('dbhasretstat');
  @dbinit                := GetAddress('dbinit');
  @dbisavail             := GetAddress('dbisavail');
  @dbiscount             := GetAddress('dbiscount');
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
  @dbprocinfo            := GetAddress('dbprocinfo');
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
  @dbrpcexec             := GetAddress('dbrpcexec');
  @dbrpwclr              := GetAddress('dbrpwclr');
  @dbserverenum          := GetAddress('dbserverenum');
  @dbsetavail            := GetAddress('dbsetavail');
  @dbsetmaxprocs         := GetAddress('dbsetmaxprocs');
  @dbsetlname            := GetAddress('dbsetlname');
  @dbsetlogintime        := GetAddress('dbsetlogintime');
  @dbsetlpacket          := GetAddress('dbsetlpacket');
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
  @dbupdatetext          := GetAddress('dbupdatetext');

  dbInit;

  OldErrorHandle := dberrhandle(ErrorHandle);
  OldMessageHandle := dbmsghandle(MessageHandle);
end;

procedure TZMSSqlNativeLibraryLoader.FreeNativeLibrary;
begin
  dberrhandle(OldErrorHandle);
  dbmsghandle(OldMessageHandle);
  dbWinExit;
  dbExit;

  inherited;
end;

initialization
  MSSqlErrors := TList.Create;
  MSSqlMessages := TList.Create;
  LibraryLoader := TZMSSqlNativeLibraryLoader.Create([WINDOWS_DLL_LOCATION]);
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;

//Free any record in the list if any
  while MSSqlMessages.Count > 0 do
  begin
    Dispose(MSSqlMessages.Items[0]);
    MSSqlMessages.Delete(0);
  end;
  if MSSqlMessages <> nil then
  begin
    MSSqlMessages.Free;
    MSSqlMessages := nil;
  end;

//Free any record in the list if any
  while MSSqlErrors.Count > 0 do
  begin
    Dispose(MSSqlErrors.Items[0]);
    MSSqlErrors.Delete(0);
  end;
  if MSSqlErrors <> nil then
  begin
    MSSqlErrors.Free;
    MSSqlErrors := nil;
  end;
end.



