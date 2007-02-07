{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Delphi plain driver interface to DBLibrary         }
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

unit ZPlainDbLibDriver;

interface

{$I ZPlain.inc}

uses Classes, ZClasses, ZCompatibility, ZPlainDriver;

{***************** Plain API Constants definition ****************}

const
{ General  #define }
  TIMEOUT_IGNORE        = Cardinal(-1);
  TIMEOUT_INFINITE      = 0;
  TIMEOUT_MAXIMUM       = 1200; { 20 minutes maximum timeout value }

{ Used for ServerType in dbgetprocinfo }
  SERVTYPE_UNKNOWN      = 0;
  SERVTYPE_MICROSOFT    = 1;

{ Used by dbcolinfo }
{enum CI_TYPES }
  CI_REGULAR            = 1;
  CI_ALTERNATE          = 2;
  CI_CURSOR             = 3;

{ Bulk Copy Definitions (bcp) }
  DB_IN	                = 1;  { Transfer from client to server }
  DB_OUT	        = 2;  { Transfer from server to client }

  BCPMAXERRS            = 1;  { bcp_control parameter }
  BCPFIRST              = 2;  { bcp_control parameter }
  BCPLAST               = 3;  { bcp_control parameter }
  BCPBATCH              = 4;  { bcp_control parameter }
  BCPKEEPNULLS          = 5;  { bcp_control parameter }
  BCPABORT              = 6;  { bcp_control parameter }

  TINYBIND              = 1;
  SMALLBIND             = 2;
  INTBIND               = 3;
  CHARBIND              = 4;
  BINARYBIND            = 5;
  BITBIND               = 6;
  DATETIMEBIND          = 7;
  MONEYBIND             = 8;
  FLT8BIND              = 9;
  STRINGBIND            = 10;
  NTBSTRINGBIND         = 11;
  VARYCHARBIND          = 12;
  VARYBINBIND           = 13;
  FLT4BIND              = 14;
  SMALLMONEYBIND        = 15;
  SMALLDATETIBIND       = 16;
  DECIMALBIND           = 17;
  NUMERICBIND           = 18;
  SRCDECIMALBIND        = 19;
  SRCNUMERICBIND        = 20;
  MAXBIND               = SRCNUMERICBIND;

  DBSAVE                = 1;
  DBNOSAVE              = 0;

  DBNOERR               = -1;
  DBFAIL                = 0;
  DBSUCCEED             = 1;
  DBFINDONE             = $04;  { Definately done }
  DBMORE                = $10;  { Maybe more commands waiting }
  DBMORE_ROWS           = $20;  { This command returned rows }

  MAXNAME               = 31;
  DBTXTSLEN             = 8;     { Timestamp length }
  DBTXPLEN              = 16;    { Text pointer length }

{ Error code returns }
  INT_EXIT              = 0;
  INT_CONTINUE          = 1;
  INT_CANCEL            = 2;

{ dboptions }
  DBBUFFER              = 0;
  DBOFFSET              = 1;
  DBROWCOUNT            = 2;
  DBSTAT                = 3;
  DBTEXTLIMIT           = 4;
  DBTEXTSIZE            = 5;
  DBARITHABORT          = 6;
  DBARITHIGNORE         = 7;
  DBNOAUTOFREE          = 8;
  DBNOCOUNT             = 9;
  DBNOEXEC              = 10;
  DBPARSEONLY           = 11;
  DBSHOWPLAN            = 12;
  DBSTORPROCID		= 13;
  DBANSITOOEM		= 14;
  DBOEMTOANSI	        = 15;
  DBCLIENTCURSORS       = 16;
  DBSET_TIME            = 17;
  DBQUOTEDIDENT         = 18;

{ Data Type Tokens }
  SQLVOID               = $1f;
  SQLTEXT               = $23;
  SQLVARBINARY          = $25;
  SQLINTN               = $26;
  SQLVARCHAR            = $27;
  SQLBINARY             = $2d;
  SQLIMAGE              = $22;
  SQLCHAR               = $2f;
  SQLINT1               = $30;
  SQLBIT                = $32;
  SQLINT2               = $34;
  SQLINT4               = $38;
  SQLMONEY              = $3c;
  SQLDATETIME           = $3d;
  SQLFLT8               = $3e;
  SQLFLTN               = $6d;
  SQLMONEYN             = $6e;
  SQLDATETIMN           = $6f;
  SQLFLT4               = $3b;
  SQLMONEY4             = $7a;
  SQLDATETIM4           = $3a;
  SQLDECIMAL            = $6a;
  SQLNUMERIC            = $6c;

{ Data stream tokens }
  SQLCOLFMT             = $a1;
  OLD_SQLCOLFMT         = $2a;
  SQLPROCID             = $7c;
  SQLCOLNAME            = $a0;
  SQLTABNAME            = $a4;
  SQLCOLINFO            = $a5;
  SQLALTNAME            = $a7;
  SQLALTFMT             = $a8;
  SQLERROR              = $aa;
  SQLINFO               = $ab;
  SQLRETURNVALUE        = $ac;
  SQLRETURNSTATUS       = $79;
  SQLRETURN             = $db;
  SQLCONTROL            = $ae;
  SQLALTCONTROL         = $af;
  SQLROW                = $d1;
  SQLALTROW             = $d3;
  SQLDONE               = $fd;
  SQLDONEPROC           = $fe;
  SQLDONEINPROC         = $ff;
  SQLOFFSET             = $78;
  SQLORDER              = $a9;
  SQLLOGINACK           = $ad; { NOTICE: change to real value }

{ Ag op tokens }
  SQLAOPCNT		= $4b;
  SQLAOPSUM             = $4d;
  SQLAOPAVG             = $4f;
  SQLAOPMIN             = $51;
  SQLAOPMAX             = $52;
  SQLAOPANY             = $53;
  SQLAOPNOOP            = $56;

{ Error numbers (dberrs) DB-Library error codes }
  SQLEMEM               = 10000;
  SQLENULL              = 10001;
  SQLENLOG              = 10002;
  SQLEPWD               = 10003;
  SQLECONN              = 10004;
  SQLEDDNE              = 10005;
  SQLENULLO             = 10006;
  SQLESMSG              = 10007;
  SQLEBTOK              = 10008;
  SQLENSPE              = 10009;
  SQLEREAD              = 10010;
  SQLECNOR              = 10011;
  SQLETSIT              = 10012;
  SQLEPARM              = 10013;
  SQLEAUTN              = 10014;
  SQLECOFL              = 10015;
  SQLERDCN              = 10016;
  SQLEICN               = 10017;
  SQLECLOS              = 10018;
  SQLENTXT              = 10019;
  SQLEDNTI              = 10020;
  SQLETMTD              = 10021;
  SQLEASEC              = 10022;
  SQLENTLL              = 10023;
  SQLETIME              = 10024;
  SQLEWRIT              = 10025;
  SQLEMODE              = 10026;
  SQLEOOB               = 10027;
  SQLEITIM              = 10028;
  SQLEDBPS              = 10029;
  SQLEIOPT              = 10030;
  SQLEASNL              = 10031;
  SQLEASUL              = 10032;
  SQLENPRM              = 10033;
  SQLEDBOP              = 10034;
  SQLENSIP              = 10035;
  SQLECNULL             = 10036;
  SQLESEOF              = 10037;
  SQLERPND              = 10038;
  SQLECSYN              = 10039;
  SQLENONET             = 10040;
  SQLEBTYP              = 10041;
  SQLEABNC              = 10042;
  SQLEABMT              = 10043;
  SQLEABNP              = 10044;
  SQLEBNCR              = 10045;
  SQLEAAMT              = 10046;
  SQLENXID              = 10047;
  SQLEIFNB              = 10048;
  SQLEKBCO              = 10049;
  SQLEBBCI              = 10050;
  SQLEKBCI              = 10051;
  SQLEBCWE              = 10052;
  SQLEBCNN              = 10053;
  SQLEBCOR              = 10054;
  SQLEBCPI              = 10055;
  SQLEBCPN              = 10056;
  SQLEBCPB              = 10057;
  SQLEVDPT              = 10058;
  SQLEBIVI              = 10059;
  SQLEBCBC              = 10060;
  SQLEBCFO              = 10061;
  SQLEBCVH              = 10062;
  SQLEBCUO              = 10063;
  SQLEBUOE              = 10064;
  SQLEBWEF              = 10065;
  SQLEBTMT              = 10066;
  SQLEBEOF              = 10067;
  SQLEBCSI              = 10068;
  SQLEPNUL              = 10069;
  SQLEBSKERR            = 10070;
  SQLEBDIO              = 10071;
  SQLEBCNT              = 10072;
  SQLEMDBP              = 10073;
  SQLINIT               = 10074;
  SQLCRSINV             = 10075;
  SQLCRSCMD             = 10076;
  SQLCRSNOIND           = 10077;
  SQLCRSDIS             = 10078;
  SQLCRSAGR             = 10079;
  SQLCRSORD             = 10080;
  SQLCRSMEM             = 10081;
  SQLCRSBSKEY           = 10082;
  SQLCRSNORES           = 10083;
  SQLCRSVIEW            = 10084;
  SQLCRSBUFR            = 10085;
  SQLCRSFROWN           = 10086;
  SQLCRSBROL            = 10087;
  SQLCRSFRAND           = 10088;
  SQLCRSFLAST           = 10089;
  SQLCRSRO              = 10090;
  SQLCRSTAB             = 10091;
  SQLCRSUPDTAB          = 10092;
  SQLCRSUPDNB           = 10093;
  SQLCRSVIIND           = 10094;
  SQLCRSNOUPD           = 10095;
  SQLCRSOS2             = 10096;
  SQLEBCSA              = 10097;
  SQLEBCRO              = 10098;
  SQLEBCNE              = 10099;
  SQLEBCSK              = 10100;
  SQLEUVBF              = 10101;
  SQLEBIHC              = 10102;
  SQLEBWFF              = 10103;
  SQLNUMVAL             = 10104;
  SQLEOLDVR             = 10105;
  SQLEBCPS	        = 10106;
  SQLEDTC 	        = 10107;
  SQLENOTIMPL	        = 10108;
  SQLENONFLOAT	        = 10109;
  SQLECONNFB            = 10110;

{ The severity levels are defined here }
  EXINFO                = 1;  { Informational, non-error }
  EXUSER                = 2;  { User error }
  EXNONFATAL            = 3;  { Non-fatal error }
  EXCONVERSION          = 4;  { Error in DB-LIBRARY data conversion }
  EXSERVER              = 5;  { The Server has returned an error flag }
  EXTIME                = 6;  { We have exceeded our timeout period while }
                           { waiting for a response from the Server - the }
                           { DBPROCESS is still alive }
  EXPROGRAM             = 7;  { Coding error in user program }
  EXRESOURCE            = 8;  { Running out of resources - the DBPROCESS may be dead }
  EXCOMM                = 9;  { Failure in communication with Server - the DBPROCESS is dead }
  EXFATAL               = 10; { Fatal error - the DBPROCESS is dead }
  EXCONSISTENCY         = 11; { Internal software error  - notify MS Technical Supprt }

{ Offset identifiers }
  OFF_SELECT            = $16d;
  OFF_FROM              = $14f;
  OFF_ORDER             = $165;
  OFF_COMPUTE           = $139;
  OFF_TABLE             = $173;
  OFF_PROCEDURE         = $16a;
  OFF_STATEMENT         = $1cb;
  OFF_PARAM             = $1c4;
  OFF_EXEC              = $12c;

{ Decimal constants }
  MAXNUMERICLEN = 16;
  MAXNUMERICDIG = 38;

  DEFAULTPRECISION = 18;
  DEFAULTSCALE     = 0;

{ Print lengths for certain fixed length data types }
  PRINT4                = 11;
  PRINT2                = 6;
  PRINT1                = 3;
  PRFLT8                = 20;
  PRMONEY               = 26;
  PRBIT                 = 3;
  PRDATETIME            = 27;
  PRDECIMAL             = (MAXNUMERICDIG + 2);
  PRNUMERIC             = (MAXNUMERICDIG + 2);

  SUCCEED               = 1;
  FAIL                  = 0;
  SUCCEED_ABORT         = 2;

  DBUNKNOWN             = 2;

  MORE_ROWS             = -1;
  NO_MORE_ROWS          = -2;
  REG_ROW               = MORE_ROWS;
  BUF_FULL              = -3;

{ Status code for dbresults(). Possible return values are }
{ SUCCEED, FAIL, and NO_MORE_RESULTS. }
  NO_MORE_RESULTS       = 2;
  NO_MORE_RPC_RESULTS   = 3;

{ Standard exit and error values }
  STDEXIT               = 0;
  ERREXIT               = -1;

{ dbrpcinit flags }
  DBRPCRECOMPILE        = $0001;
  DBRPCRESET            = $0004;
  DBRPCCURSOR           = $0008;

{ dbrpcparam flags }
  DBRPCRETURN           = $1;
  DBRPCDEFAULT          = $2;

{ Cursor related constants }

{ Following flags are used in the concuropt parameter in the dbcursoropen function }
  CUR_READONLY          = 1; { Read only cursor, no data modifications }
  CUR_LOCKCC            = 2; { Intent to update, all fetched data locked when }
                       { dbcursorfetch is called inside a transaction block }
  CUR_OPTCC             = 3; { Optimistic concurrency control, data modifications }
                       { succeed only if the row hasn't been updated since }
                       { the last fetch. }
  CUR_OPTCCVAL          = 4; { Optimistic concurrency control based on selected column values }

{ Following flags are used in the scrollopt parameter in dbcursoropen }
  CUR_FORWARD           = 0;   { Forward only scrolling }
  CUR_KEYSET            = -1;  { Keyset driven scrolling }
  CUR_DYNAMIC           = 1;   { Fully dynamic }
  CUR_INSENSITIVE       = -2;  { Server-side cursors only }

{ Following flags define the fetchtype in the dbcursorfetch function }
  FETCH_FIRST           = 1;  { Fetch first n rows }
  FETCH_NEXT            = 2;  { Fetch next n rows }
  FETCH_PREV            = 3;  { Fetch previous n rows }
  FETCH_RANDOM          = 4;  { Fetch n rows beginning with given row # }
  FETCH_RELATIVE        = 5;  { Fetch relative to previous fetch row # }
  FETCH_LAST            = 6;  { Fetch the last n rows }

{ Following flags define the per row status as filled by dbcursorfetch and/or dbcursorfetchex }
  FTC_EMPTY             = $00;  { No row available }
  FTC_SUCCEED           = $01;  { Fetch succeeded, (failed if not set) }
  FTC_MISSING           = $02;  { The row is missing }
  FTC_ENDOFKEYSET       = $04;  { End of the keyset reached }
  FTC_ENDOFRESULTS      = $08;  { End of results set reached }

{ Following flags define the operator types for the dbcursor function }
  CRS_UPDATE            = 1;  { Update operation }
  CRS_DELETE            = 2;  { Delete operation }
  CRS_INSERT            = 3;  { Insert operation }
  CRS_REFRESH           = 4;  { Refetch given row }
  CRS_LOCKCC            = 5;  { Lock given row }

{ Following value can be passed to the dbcursorbind function for NOBIND type }
  NOBIND                = -2; { Return length and pointer to data }

{ Following are values used by DBCURSORINFO's Type parameter }
  CU_CLIENT             = $00000001;
  CU_SERVER             = $00000002;
  CU_KEYSET             = $00000004;
  CU_MIXED              = $00000008;
  CU_DYNAMIC            = $00000010;
  CU_FORWARD            = $00000020;
  CU_INSENSITIVE        = $00000040;
  CU_READONLY           = $00000080;
  CU_LOCKCC             = $00000100;
  CU_OPTCC              = $00000200;
  CU_OPTCCVAL           = $00000400;

{ Following are values used by DBCURSORINFO's Status parameter }
  CU_FILLING            = $00000001;
  CU_FILLED             = $00000002;

{ Following are values used by dbupdatetext's type parameter }
  UT_TEXTPTR            = $0001;
  UT_TEXT               = $0002;
  UT_MORETEXT           = $0004;
  UT_DELETEONLY         = $0008;
  UT_LOG                = $0010;

{ The following values are passed to dbserverenum for searching criteria. }
  NET_SEARCH            = $0001;
  LOC_SEARCH            = $0002;

{ These constants are the possible return values from dbserverenum. }
  ENUM_SUCCESS          = $0000;
  MORE_DATA             = $0001;
  NET_NOT_AVAIL         = $0002;
  OUT_OF_MEMORY         = $0004;
  NOT_SUPPORTED         = $0008;
  ENUM_INVALID_PARAM    = $0010;

{ Netlib Error problem codes.  ConnectionError() should return one of }
{ these as the dblib-mapped problem code, so the corresponding string }
{ is sent to the dblib app's error handler as dberrstr.  Return NE_E_NOMAP }
{ for a generic DB-Library error string (as in prior versions of dblib). }

  NE_E_NOMAP            = 0;   { No string; uses dblib default. }
  NE_E_NOMEMORY         = 1;   { Insufficient memory. }
  NE_E_NOACCESS         = 2;   { Access denied. }
  NE_E_CONNBUSY         = 3;   { Connection is busy. }
  NE_E_CONNBROKEN       = 4;   { Connection broken. }
  NE_E_TOOMANYCONN      = 5;   { Connection limit exceeded. }
  NE_E_SERVERNOTFOUND   = 6;   { Specified SQL server not found. }
  NE_E_NETNOTSTARTED    = 7;   { The network has not been started. }
  NE_E_NORESOURCE       = 8;   { Insufficient network resources. }
  NE_E_NETBUSY          = 9;   { Network is busy. }
  NE_E_NONETACCESS      = 10;  { Network access denied. }
  NE_E_GENERAL          = 11;  { General network error.  Check your documentation. }
  NE_E_CONNMODE         = 12;  { Incorrect connection mode. }
  NE_E_NAMENOTFOUND     = 13;  { Name not found in directory service. }
  NE_E_INVALIDCONN      = 14;  { Invalid connection. }
  NE_E_NETDATAERR       = 15;  { Error reading or writing network data. }
  NE_E_TOOMANYFILES     = 16;  { Too many open file handles. }
  NE_E_CANTCONNECT	= 17;  { SQL Server does not exist or access denied. }

  NE_MAX_NETERROR       = 17;


{ DB-Library datatype definitions }
const
  DBMAXCHAR             = 256; { Max length of DBVARBINARY and DBVARCHAR, etc. }

const
{ Pack the following structures on a word boundary }
  MAXCOLNAMELEN = 30;
  MAXTABLENAME  = 30;

const
  MAXSERVERNAME = 30;
  MAXNETLIBNAME = 255;
  MAXNETLIBCONNSTR = 255;

const
  INVALID_UROWNUM       = Cardinal(-1);


  {****************** Plain API Types definition *****************}
type
{ DBPROCESS, LOGINREC and DBCURSOR }
  PDBPROCESS            = Pointer;
  PLOGINREC             = Pointer;
  PDBCURSOR             = Pointer;
  PDBHANDLE             = Pointer;

type
  RETCODE               = Integer;
  STATUS                = Integer;

{ DB-Library datatypes }
  DBCHAR                = Char;
  DBBINARY              = Byte;
  DBTINYINT             = Byte;
  DBSMALLINT            = SmallInt;
  DBUSMALLINT           = Word;
  DBINT                 = LongInt;
  DBFLT8                = Double;
  DBBIT                 = Byte;
  DBBOOL                = Byte;
  DBFLT4                = Single;
  DBMONEY4              = LongInt;

  DBREAL                = DBFLT4;
  DBUBOOL               = Cardinal;

  DBDATETIM4 = packed record
    numdays:    Word;        { No of days since Jan-1-1900 }
    nummins:    Word;        { No. of minutes since midnight }
  end;
  PDBDATETIM4 = ^DBDATETIM4;

  DBVARYCHAR = packed record
    Len:        DBSMALLINT;
    Str:        array[0..DBMAXCHAR-1] of DBCHAR;
  end;

  DBVARYBIN = packed record
    Len:        DBSMALLINT;
    Bytes:	array[0..DBMAXCHAR-1] of Byte;
  end;

  DBMONEY = packed record
    mnyhigh:    DBINT;
    mnylow:     Cardinal;
  end;

  DBDATETIME = packed record
    dtdays:	DBINT;          // Days since Jan 1, 1900
    dttime:	Cardinal;       // 300ths of a second since midnight, 25920000 unit is 1 day
  end;
  PDBDATETIME = ^DBDATETIME;

{ DBDATEREC structure used by dbdatecrack }
  DBDATEREC = packed record
    year:       Integer;      { 1753 - 9999 }
    quarter:    Integer;      { 1 - 4 }
    month:      Integer;      { 1 - 12 }
    dayofyear:  Integer;      { 1 - 366 }
    day:        Integer;      { 1 - 31 }
    week:       Integer;      { 1 - 54 (for leap years) }
    weekday:    Integer;      { 1 - 7  (Mon - Sun) }
    hour:       Integer;      { 0 - 23 }
    minute:     Integer;      { 0 - 59 }
    second:     Integer;      { 0 - 59 }
    millisecond: Integer;     { 0 - 999 }
  end;
  PDBDATEREC = ^DBDATEREC;

type
  DBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Sign:       Byte; { 1 = Positive, 0 = Negative }
    Val:        array[0..MAXNUMERICLEN-1] of Byte;
  end;

  DBDECIMAL = DBNUMERIC;



type
{ TODO -ofjanos -cAPI :
Strange but I had to insert X1 and X2 into the structure to make it work.
I have not find any reason for this yet. }
  DBCOL = packed record
    SizeOfStruct: DBINT;
    Name:       array[0..MAXCOLNAMELEN] of Char;
    ActualName: array[0..MAXCOLNAMELEN] of Char;
    TableName:  array[0..MAXTABLENAME] of Char;
    X1:         Byte;
    Typ:        SmallInt;
    UserType:   DBINT;
    MaxLength:  DBINT;
    Precision:  Byte;
    Scale:      Byte;
    VarLength:  LongBool;{ TRUE, FALSE }
    Null:       Byte;    { TRUE, FALSE or DBUNKNOWN }
    CaseSensitive: Byte; { TRUE, FALSE or DBUNKNOWN }
    Updatable:  Byte;    { TRUE, FALSE or DBUNKNOWN }
    Identity:   LongBool;{ TRUE, FALSE }
    X2:         Byte;
  end;
  PDBCOL = ^DBCOL;


type
  DBPROC_INFO = packed record
    SizeOfStruct:       DBINT;
    ServerType:         Byte;
    ServerMajor:        Word;
    ServerMinor:        Word;
    ServerRevision:     Word;
    ServerName:         array[0..MAXSERVERNAME] of Char;
    NetLibName:         array[0..MAXNETLIBNAME] of Char;
    NetLibConnStr:      array[0..MAXNETLIBCONNSTR] of Char;
  end;
  PDBPROCINFO = ^DBPROC_INFO;

  DBCURSOR_INFO = packed record
    SizeOfStruct:       DBINT;    { Use sizeof(DBCURSORINFO) }
    TotCols:            Cardinal; { Total Columns in cursor }
    TotRows:            Cardinal; { Total Rows in cursor }
    CurRow:             Cardinal; { Current actual row in server }
    TotRowsFetched:     Cardinal; { Total rows actually fetched }
    CurType:            Cardinal; { See CU_... }
    Status:             Cardinal; { See CU_... }
  end;
  PDBCURSORINFO = ^DBCURSOR_INFO;

type
{ Pointer Datatypes }
  PDBINT        = ^DBINT;
  PDBBINARY     = ^DBBINARY;

type
  PDBLibError = ^TDBLibError;
  TDBLibError = record
    dbProc: PDBPROCESS;
    Severity: Integer;
    DbErr: Integer;
    OsErr: Integer;
    DbErrStr: string;
    OsErrStr: string;
  end;

  PDBLibMessage = ^TDBLibMessage;
  TDBLibMessage = record
    dbProc: PDBPROCESS;
    MsgNo: DBINT;
    MsgState: Integer;
    Severity: Integer;
    MsgText: string;
    SrvName: string;
    ProcName: string;
    Line: DBUSMALLINT;
  end;

type
  {** Represents a generic interface to DBLIB native API. }
  IZDBLibPlainDriver = interface (IZPlainDriver)
    ['{7731C3B4-0608-4B6B-B089-240AC43A3463}']

    procedure CheckError;
    
    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: Integer): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PChar; Item: Integer): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PChar): RETCODE;
    function dbSetLSecure(Login: PLOGINREC): RETCODE;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
    function dbOpen(Login: PLOGINREC; Host: PChar): PDBPROCESS;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(dbProc: PDBPROCESS; Cmd: PChar): RETCODE;
    function dbSqlExec(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: Integer;
      Char_Param: PChar = nil; Int_Param: Integer = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): Integer;
    function dbColName(dbProc: PDBPROCESS; Column: Integer): PChar;
    function dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
    function dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
    function dbCount(dbProc: PDBPROCESS): Integer;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PChar; Status: Byte;
      Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): Integer;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
    function dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;

  end;

  {** Implements a dblib driver for Sybase ASE 12.5 }
  TZDBLibSybaseASE125PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZDBLibPlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    procedure CheckError;

    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: Integer): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PChar; Item: Integer): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PChar): RETCODE;
    function dbSetLSecure(Login: PLOGINREC): RETCODE;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
    function dbOpen(Login: PLOGINREC; Host: PChar): PDBPROCESS;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(dbProc: PDBPROCESS; Cmd: PChar): RETCODE;
    function dbSqlExec(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PChar = nil; Int_Param: Integer = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): Integer;
    function dbColName(dbProc: PDBPROCESS; Column: Integer): PChar;
    function dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
    function dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
    function dbCount(dbProc: PDBPROCESS): Integer;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PChar; Status: Byte;
      Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): Integer;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
    function dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
  end;

  {** Implements a dblib driver for MSSql7 }
  TZDBLibMSSQL7PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZDBLibPlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    procedure CheckError;

    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: Integer): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PChar; Item: Integer): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PChar): RETCODE;
    function dbSetLSecure(Login: PLOGINREC): RETCODE;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
    function dbOpen(Login: PLOGINREC; Host: PChar): PDBPROCESS;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(dbProc: PDBPROCESS; Cmd: PChar): RETCODE;
    function dbSqlExec(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PChar = nil; Int_Param: Integer = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): Integer;
    function dbColName(dbProc: PDBPROCESS; Column: Integer): PChar;
    function dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
    function dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
    function dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
    function dbCount(dbProc: PDBPROCESS): Integer;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PChar; Status: Byte;
      Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): Integer;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
    function dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
  end;


implementation

uses SysUtils, ZPlainDbLibSybaseAse125, ZPlainDbLibMsSql7;

{ TZDBLibSybaseASE125PlainDriver }

constructor TZDBLibSybaseASE125PlainDriver.Create;
begin
end;

function TZDBLibSybaseASE125PlainDriver.GetProtocol: string;
begin
  Result := 'sybase';
end;

function TZDBLibSybaseASE125PlainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for Sybase ASE 12.5';
end;

procedure TZDBLibSybaseASE125PlainDriver.Initialize;
begin
  ZPlainDBLibSybaseASE125.LibraryLoader.LoadIfNeeded;
end;


procedure TZDBLibSybaseASE125PlainDriver.CheckError;
var
  I: Integer;
  S: string;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;
begin
  { TODO -ofjanos -cGeneral : Error handling should be based on connection object.
  At the moment it is global. }
  if (SybaseErrors.Count = 0) and (SybaseMessages.Count = 0) then
    Exit;
  S := '';
  for I := 0 to SybaseErrors.Count - 1 do
    S := S + PDBLibError(SybaseErrors.Items[I]).DbErrStr + ' '
      + PDBLibError(SybaseErrors.Items[I]).OsErrStr + ' '#13;
  for I := 0 to SybaseMessages.Count - 1 do
    if PDBLibMessage(SybaseMessages.Items[I]).Severity > EXINFO then
      S := S + PDBLibMessage(SybaseMessages.Items[I]).MsgText + ' '#13;
  while SybaseErrors.Count > 0 do
  begin
    lErrorEntry := SybaseErrors.Items[0];
    Dispose(lErrorEntry);
    SybaseErrors.Delete(0);
  end;
  SybaseErrors.Clear;
  while SybaseMessages.Count > 0 do
  begin
    lMesageEntry := SybaseMessages.Items[0];
    Dispose(lMesageEntry);
    SybaseMessages.Delete(0);
  end;
  SybaseMessages.Clear;
  if S <> '' then
    raise Exception.Create(S);
end;


function TZDBLibSybaseASE125PlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := ZPlainDBLibSybaseASE125.dbDead(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbLogin: PLOGINREC;
begin
  Result := ZPlainDBLibSybaseASE125.dbLogin;
end;

procedure TZDBLibSybaseASE125PlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  ZPlainDBLibSybaseASE125.dbLoginFree(Login);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLoginTime(Seconds: Integer): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetlogintime(Seconds);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetlname(Login: PLOGINREC; Value: PChar; Item: Integer): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetlname(Login, Value, Item);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLHost(Login: PLOGINREC; HostName: PChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.DBSETLHOST(Login, HostName);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetluser(Login: PLOGINREC; UserName: PChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetluser(Login, UserName);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetlpwd(Login: PLOGINREC; Password: PChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetlpwd(Login, Password);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLApp(Login: PLOGINREC; AppName: PChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.DBSETLAPP(Login, AppName);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLNatLang(Login: PLOGINREC; NatLangName: PChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.DBSETLNATLANG(Login, NatLangName);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.DBSETLCHARSET(Login, CharsetName);
end;

function TZDBLibSybaseASE125PlainDriver.dbsetlsecure(Login: PLOGINREC): RETCODE;
begin
  Result := 0;
end;

function TZDBLibSybaseASE125PlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetmaxprocs(MaxProcs);
end;

function TZDBLibSybaseASE125PlainDriver.dbOpen(Login: PLOGINREC; Host: PChar): PDBPROCESS;
begin
  Result := ZPlainDBLibSybaseASE125.dbOpen(Login, Host);
end;

function TZDBLibSybaseASE125PlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbcancel(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCmd(dbProc: PDBPROCESS; Cmd: PChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbcmd(dbProc, Cmd);
end;

function TZDBLibSybaseASE125PlainDriver.dbSqlExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbSqlExec(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbResults(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbCanQuery(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbMoreCmds(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbUse(dbProc: PDBPROCESS; dbName: PChar): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbUse(dbProc, dbName);
end;

function TZDBLibSybaseASE125PlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PChar = nil; Int_Param: Integer = -1): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbSetOpt(dbProc, Option, Char_Param, Int_Param);
end;

function TZDBLibSybaseASE125PlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbClose(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbName(dbProc: PDBPROCESS): PChar;
begin
  Result := ZPlainDBLibSybaseASE125.dbName(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbCmdRow(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbNumCols(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbNumCols(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbColName(dbProc: PDBPROCESS; Column: Integer): PChar;
begin
  Result := ZPlainDBLibSybaseASE125.dbColName(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbColType(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
begin
  Result := ZPlainDBLibSybaseASE125.dbColLen(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
begin
  Result := ZPlainDBLibSybaseASE125.dbData(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbDatLen(dbProc, Column);
end;

function TZDBLibSybaseASE125PlainDriver.dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
  SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbConvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen);
end;

function TZDBLibSybaseASE125PlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  Result := ZPlainDBLibSybaseASE125.dbNextRow(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
begin
  Result := ZPlainDBLibSybaseASE125.dbGetRow(dbProc, Row);
end;

function TZDBLibSybaseASE125PlainDriver.dbCount(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbCount(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PChar; Options: SmallInt): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbRpcInit(dbProc, RpcName, Options);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PChar; Status: Byte;
  Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbRpcSend(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbRpcSend(dbProc);
  if Result = SUCCEED then
    Result := ZPlainDBLibSybaseASE125.dbSqlOk(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetStatus(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetStatus(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := ZPlainDBLibSybaseASE125.dbHasRetStat(dbProc);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PChar;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetName(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetData(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetLen(dbProc, RetNum);
end;

function TZDBLibSybaseASE125PlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := ZPlainDBLibSybaseASE125.dbRetType(dbProc, RetNum);
end;


{TZDBLibMSSQL7PlainDriver}

constructor TZDBLibMSSQL7PlainDriver.Create;
begin
end;

function TZDBLibMSSQL7PlainDriver.GetProtocol: string;
begin
  Result := 'mssql';
end;

function TZDBLibMSSQL7PlainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for MS SQL 7+';
end;

procedure TZDBLibMSSQL7PlainDriver.Initialize;
begin
  ZPlainDBLibMSSql7.LibraryLoader.LoadIfNeeded;
end;


procedure TZDBLibMSSQL7PlainDriver.CheckError;
var
  I: Integer;
  S: string;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;
begin
  { TODO -ofjanos -cGeneral : Error handling should be based on connection object.
  At the moment it is global. }
  if (MSSqlErrors.Count = 0) and (MSSqlMessages.Count = 0) then
    Exit;
  S := '';
  for I := 0 to MSSqlErrors.Count - 1 do
    S := S + PDBLibError(MSSqlErrors.Items[I]).DbErrStr + ' '
      + PDBLibError(MSSqlErrors.Items[I]).OsErrStr + ' '#13;
  for I := 0 to MSSqlMessages.Count - 1 do
    if PDBLibMessage(MSSqlMessages.Items[I]).Severity > EXINFO then
      S := S + PDBLibMessage(MSSqlMessages.Items[I]).MsgText + ' '#13;
  while MSSqlErrors.Count > 0 do
  begin
    lErrorEntry := MSSqlErrors.Items[0];
    Dispose(lErrorEntry);
    MSSqlErrors.Delete(0);
  end;
  MSSqlErrors.Clear;
  while MSSqlMessages.Count > 0 do
  begin
    lMesageEntry := MSSqlMessages.Items[0];
    Dispose(lMesageEntry);
    MSSqlMessages.Delete(0);
  end;
  MSSqlMessages.Clear;
  if S <> '' then
    raise Exception.Create(S);
end;


function TZDBLibMSSQL7PlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
  Result := ZPlainDBLibMSSql7.dbDead(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbLogin: PLOGINREC;
begin
  Result := ZPlainDBLibMSSql7.dbLogin;
end;

procedure TZDBLibMSSQL7PlainDriver.dbLoginFree(Login: PLOGINREC);
begin
  ZPlainDBLibMSSql7.dbFreeLogin(Login);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLoginTime(Seconds: Integer): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbSetLoginTime(Seconds);
end;

function TZDBLibMSSQL7PlainDriver.dbsetlname(Login: PLOGINREC; Value: PChar; Item: Integer): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetlname(Login, Value, Item);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLHost(Login: PLOGINREC; HostName: PChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.DBSETLHOST(Login, HostName);
end;

function TZDBLibMSSQL7PlainDriver.dbsetluser(Login: PLOGINREC; UserName: PChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetluser(Login, UserName);
end;

function TZDBLibMSSQL7PlainDriver.dbsetlpwd(Login: PLOGINREC; Password: PChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetlpwd(Login, Password);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLApp(Login: PLOGINREC; AppName: PChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.DBSETLAPP(Login, AppName);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLNatLang(Login: PLOGINREC; NatLangName: PChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.DBSETLNATLANG(Login, NatLangName);
end;

function TZDBLibMSSQL7PlainDriver.dbSetLCharSet(Login: PLOGINREC; CharsetName: PChar): RETCODE;
begin
  Result := 0;
end;

function TZDBLibMSSQL7PlainDriver.dbsetlsecure(Login: PLOGINREC): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbsetlsecure(Login);
end;

function TZDBLibMSSQL7PlainDriver.dbsetmaxprocs(
  MaxProcs: SmallInt): RETCODE;
begin
  Result := ZPlainDBLibSybaseASE125.dbsetmaxprocs(MaxProcs);
end;

function TZDBLibMSSQL7PlainDriver.dbOpen(Login: PLOGINREC; Host: PChar): PDBPROCESS;
begin
  Result := ZPlainDBLibMSSql7.dbOpen(Login, Host);
end;


function TZDBLibMSSQL7PlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbcancel(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbCmd(dbProc: PDBPROCESS; Cmd: PChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbcmd(dbProc, Cmd);
end;

function TZDBLibMSSQL7PlainDriver.dbSqlExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbSqlExec(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbResults(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbCanQuery(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbMoreCmds(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbUse(dbProc: PDBPROCESS; dbName: PChar): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbUse(dbProc, dbName);
end;

function TZDBLibMSSQL7PlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: Integer; Char_Param: PChar = nil; Int_Param: Integer = -1): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbSetOpt(dbProc, Option, Char_Param);
end;

function TZDBLibMSSQL7PlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbClose(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbName(dbProc: PDBPROCESS): PChar;
begin
  Result := ZPlainDBLibMSSql7.dbName(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbCmdRow(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbNumCols(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbNumCols(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbColName(dbProc: PDBPROCESS; Column: Integer): PChar;
begin
  Result := ZPlainDBLibMSSql7.dbColName(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbColType(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbColType(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbColLen(dbProc: PDBPROCESS; Column: Integer): DBInt;
begin
  Result := ZPlainDBLibMSSql7.dbColLen(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbData(dbProc: PDBPROCESS; Column: Integer): PByte;
begin
  Result := ZPlainDBLibMSSql7.dbData(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbDatLen(dbProc: PDBPROCESS; Column: Integer): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbDatLen(dbProc, Column);
end;

function TZDBLibMSSQL7PlainDriver.dbConvert(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
  SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbConvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen);
end;

function TZDBLibMSSQL7PlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
  Result := ZPlainDBLibMSSql7.dbNextRow(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbGetRow(dbProc: PDBPROCESS; Row: Integer): STATUS;
begin
  Result := ZPlainDBLibMSSql7.dbGetRow(dbProc, Row);
end;

function TZDBLibMSSQL7PlainDriver.dbCount(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbCount(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbRpcInit(dbProc: PDBPROCESS; RpcName: PChar; Options: SmallInt): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbRpcInit(dbProc, RpcName, Options);
end;

function TZDBLibMSSQL7PlainDriver.dbRpcParam(dbProc: PDBPROCESS; ParamName: PChar; Status: Byte;
  Type_: Integer; MaxLen: Integer; DataLen: Integer; Value: Pointer): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbRpcParam(dbProc, ParamName, Status, Type_, MaxLen, DataLen, Value);
end;

function TZDBLibMSSQL7PlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbRpcSend(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
  Result := ZPlainDBLibMSSql7.dbRpcExec(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbRetStatus(dbProc: PDBPROCESS): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbRetStatus(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
  Result := ZPlainDBLibMSSql7.dbHasRetStat(dbProc);
end;

function TZDBLibMSSQL7PlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: Integer): PChar;
begin
  Result := ZPlainDBLibMSSql7.dbRetName(dbProc, RetNum);
end;

function TZDBLibMSSQL7PlainDriver.dbRetData(dbProc: PDBPROCESS; RetNum: Integer): Pointer;
begin
  Result := ZPlainDBLibMSSql7.dbRetData(dbProc, RetNum);
end;

function TZDBLibMSSQL7PlainDriver.dbRetLen(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbRetLen(dbProc, RetNum);
end;

function TZDBLibMSSQL7PlainDriver.dbRetType(dbProc: PDBPROCESS; RetNum: Integer): Integer;
begin
  Result := ZPlainDBLibMSSql7.dbRetType(dbProc, RetNum);
end;


initialization
end.
