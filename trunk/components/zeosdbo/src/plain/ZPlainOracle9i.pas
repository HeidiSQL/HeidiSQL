{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi interface to Oracle Call Interface        }
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

unit ZPlainOracle9i;

interface

{$I ZPlain.inc}

{$J+}

uses
{$IFNDEF UNIX}
//  Windows,
{$ENDIF}
  ZPlainLoader;

{***************** Plain API types definition ****************}

const
  WINDOWS_DLL_LOCATION = 'oci.dll';
//  WINDOWS_DLL_LOCATION = 'ora803.dll';
  LINUX_DLL_LOCATION = 'libclntsh.so';
//  LINUX_DLL_LOCATION = 'libwtc8.so';

type
  { Generic Oracle Types }
  sword   = Integer;
  eword   = Integer;
  uword   = LongInt;
  sb4     = Integer;
  ub4     = LongInt;
  sb2     = SmallInt;
  ub2     = Word;
  sb1     = ShortInt;
  ub1     = Byte;
  dvoid   = Pointer;
  text    = PChar;
  size_T  = Integer;

  pub1 = ^ub1;
  psb1 = ^sb1;
  pub2 = ^ub2;
  psb2 = ^sb2;
  pub4 = ^ub4;
  psb4 = ^sb4;

  { Handle Types }
  POCIHandle = Pointer;
  PPOCIHandle = ^Pointer;
  POCIEnv = POCIHandle;
  POCIServer = POCIHandle;
  POCIError = POCIHandle;
  POCISvcCtx = POCIHandle;
  POCIStmt = POCIHandle;
  POCIDefine = POCIHandle;
  POCISession = POCIHandle;
  POCIBind = POCIHandle;
  POCIDescribe = POCIHandle;
  POCITrans = POCIHandle;

  { Descriptor Types }
  POCIDescriptor = Pointer;
  PPOCIDescriptor = ^POCIDescriptor;
  POCISnapshot = POCIDescriptor;
  POCILobLocator = POCIDescriptor;
  POCIParam = POCIDescriptor;
  POCIRowid = POCIDescriptor;
  POCIComplexObjectComp = POCIDescriptor;
  POCIAQEnqOptions = POCIDescriptor;
  POCIAQDeqOptions = POCIDescriptor;
  POCIAQMsgProperties = POCIDescriptor;
  POCIAQAgent = POCIDescriptor;
  POCIDate = POCIDescriptor;
  POCIDateTime = POCIDescriptor;
  POCINumber = POCIDescriptor;
  POCIString = POCIDescriptor;

  OCIDuration = ub2;

const
  OCI_DURATION_INVALID = $FFFF;      { Invalid duration }
  OCI_DURATION_BEGIN = 10;           { beginning sequence of duration }
  OCI_DURATION_NULL = (OCI_DURATION_BEGIN-1); { null duration }
  OCI_DURATION_DEFAULT = (OCI_DURATION_BEGIN-2); { default }
  OCI_DURATION_USER_CALLBACK = (OCI_DURATION_BEGIN-3);
  OCI_DURATION_NEXT = (OCI_DURATION_BEGIN-4); { next special duration }
  OCI_DURATION_SESSION = (OCI_DURATION_BEGIN); { the end of user session }
  OCI_DURATION_TRANS = (OCI_DURATION_BEGIN+1); { the end of user transaction }
  OCI_DURATION_STATEMENT = (OCI_DURATION_BEGIN+3);
{ This is to be used only during callouts.  It is similar to that
of OCI_DURATION_CALL, but lasts only for the duration of a callout.
Its heap is from PGA }
  OCI_DURATION_CALLOUT = (OCI_DURATION_BEGIN+4);
  OCI_DURATION_LAST = OCI_DURATION_CALLOUT; { last of predefined durations }

  OCI_TEMP_BLOB     = 1; { LOB type - BLOB }
  OCI_TEMP_CLOB     = 2; { LOB type - CLOB }

const
  MAXTXNAMELEN    = 64;
  XIDDATASIZE     = 128; { size in bytes }
  MAXGTRIDSIZE    = 64;  { maximum size in bytes of gtrid }
  MAXBQUALSIZE    = 64;  { maximum size in bytes of bqual }
  NULLXID_ID      = -1;

  { Transaction branch identification: XID and NULLXID: }
type
  PXID = ^TXID;
  TXID = record
    formatID: sb4;     { format identifier }
    gtrid_length: sb4; { value from 1 through 64 }
    bqual_length: sb4; { value from 1 through 64 }
    data: array [0 .. XIDDATASIZE - 1] of ub1;
  end;

const
  MAXUB4  = High(ub4);
  MAXSB4  = High(sb4);

{***************** Plain API constants definition ****************}

  { OCI Handle Types }
  OCI_HTYPE_FIRST               = 1;
  OCI_HTYPE_ENV                 = 1;
  OCI_HTYPE_ERROR               = 2;
  OCI_HTYPE_SVCCTX              = 3;
  OCI_HTYPE_STMT                = 4;
  OCI_HTYPE_BIND                = 5;
  OCI_HTYPE_DEFINE              = 6;
  OCI_HTYPE_DESCRIBE            = 7;
  OCI_HTYPE_SERVER              = 8;
  OCI_HTYPE_SESSION             = 9;
  OCI_HTYPE_TRANS               = 10;
  OCI_HTYPE_COMPLEXOBJECT       = 11;
  OCI_HTYPE_SECURITY            = 12;
  OCI_HTYPE_SUBSCRIPTION        = 13;
  OCI_HTYPE_DIRPATH_CTX         = 14;
  OCI_HTYPE_DIRPATH_COLUMN_ARRAY = 15;
  OCI_HTYPE_DIRPATH_STREAM      = 16;
  OCI_HTYPE_PROC                = 17;
  OCI_HTYPE_LAST                = 17;

  { OCI Descriptor Types }
  OCI_DTYPE_FIRST               = 50;
  OCI_DTYPE_LOB                 = 50;
  OCI_DTYPE_SNAP                = 51;
  OCI_DTYPE_RSET                = 52;
  OCI_DTYPE_PARAM               = 53;
  OCI_DTYPE_ROWID               = 54;
  OCI_DTYPE_COMPLEXOBJECTCOMP   = 55;
  OCI_DTYPE_FILE                = 56;
  OCI_DTYPE_AQENQ_OPTIONS       = 57;
  OCI_DTYPE_AQDEQ_OPTIONS       = 58;
  OCI_DTYPE_AQMSG_PROPERTIES    = 59;
  OCI_DTYPE_AQAGENT             = 60;
  OCI_DTYPE_LOCATOR             = 61;
  OCI_DTYPE_DATETIME            = 62;
  OCI_DTYPE_INTERVAL            = 63;
  OCI_DTYPE_AQNFY_DESCRIPTOR    = 64;
  OCI_DTYPE_LAST                = 64;
  OCI_DTYPE_DATE                = 65;  { Date }
  OCI_DTYPE_TIME                = 66;  { Time }
  OCI_DTYPE_TIME_TZ             = 67;  { Time with timezone }
  OCI_DTYPE_TIMESTAMP           = 68;  { Timestamp }
  OCI_DTYPE_TIMESTAMP_TZ        = 69;  { Timestamp with timezone }
  OCI_DTYPE_TIMESTAMP_LTZ       = 70;  { Timestamp with local tz }

  { OCI Attributes Types }
  OCI_ATTR_FNCODE               = 1;   // the OCI function code
  OCI_ATTR_OBJECT               = 2;   // is the environment initialized in object mode
  OCI_ATTR_NONBLOCKING_MODE     = 3;   // non blocking mode
  OCI_ATTR_SQLCODE              = 4;   // the SQL verb
  OCI_ATTR_ENV                  = 5;   // the environment handle
  OCI_ATTR_SERVER               = 6;   // the server handle
  OCI_ATTR_SESSION              = 7;   // the user session handle
  OCI_ATTR_TRANS                = 8;   // the transaction handle
  OCI_ATTR_ROW_COUNT            = 9;   // the rows processed so far
  OCI_ATTR_SQLFNCODE            = 10;  // the SQL verb of the statement
  OCI_ATTR_PREFETCH_ROWS        = 11;  // sets the number of rows to prefetch
  OCI_ATTR_NESTED_PREFETCH_ROWS = 12;  // the prefetch rows of nested table
  OCI_ATTR_PREFETCH_MEMORY      = 13;  // memory limit for rows fetched
  OCI_ATTR_NESTED_PREFETCH_MEMORY = 14;// memory limit for nested rows
  OCI_ATTR_CHAR_COUNT           = 15;  // this specifies the bind and define size in characters
  OCI_ATTR_PDSCL                = 16;  // packed decimal scale
  OCI_ATTR_FSPRECISION          = OCI_ATTR_PDSCL; // fs prec for datetime data types
  OCI_ATTR_PDPRC                = 17;  // packed decimal format
  OCI_ATTR_LFPRECISION          = OCI_ATTR_PDPRC; // fs prec for datetime data types
  OCI_ATTR_PARAM_COUNT          = 18;  // number of column in the select list
  OCI_ATTR_ROWID                = 19;  // the rowid
  OCI_ATTR_CHARSET              = 20;  // the character set value
  OCI_ATTR_NCHAR                = 21;  // NCHAR type
  OCI_ATTR_USERNAME             = 22;  // username attribute
  OCI_ATTR_PASSWORD             = 23;  // password attribute
  OCI_ATTR_STMT_TYPE            = 24;  // statement type
  OCI_ATTR_INTERNAL_NAME        = 25;  // user friendly global name
  OCI_ATTR_EXTERNAL_NAME        = 26;  // the internal name for global txn
  OCI_ATTR_XID                  = 27;  // XOPEN defined global transaction id
  OCI_ATTR_TRANS_LOCK           = 28;  //
  OCI_ATTR_TRANS_NAME           = 29;  // string to identify a global transaction
  OCI_ATTR_HEAPALLOC            = 30;  // memory allocated on the heap
  OCI_ATTR_CHARSET_ID           = 31;  // Character Set ID
  OCI_ATTR_CHARSET_FORM         = 32;  // Character Set Form
  OCI_ATTR_MAXDATA_SIZE         = 33;  // Maximumsize of data on the server
  OCI_ATTR_CACHE_OPT_SIZE       = 34;  // object cache optimal size
  OCI_ATTR_CACHE_MAX_SIZE       = 35;  // object cache maximum size percentage
  OCI_ATTR_PINOPTION            = 36;  // object cache default pin option
  OCI_ATTR_ALLOC_DURATION       = 37;  // object cache default allocation duration
  OCI_ATTR_PIN_DURATION         = 38;  // object cache default pin duration
  OCI_ATTR_FDO                  = 39;  // Format Descriptor object attribute
  OCI_ATTR_POSTPROCESSING_CALLBACK = 40;  // Callback to process outbind data
  OCI_ATTR_POSTPROCESSING_CONTEXT = 41; // Callback context to process outbind data
  OCI_ATTR_ROWS_RETURNED        = 42;  // Number of rows returned in current iter - for Bind handles
  OCI_ATTR_FOCBK                = 43;  // Failover Callback attribute
  OCI_ATTR_IN_V8_MODE           = 44;  // is the server/service context in V8 mode
  OCI_ATTR_LOBEMPTY             = 45;  // empty lob ?
  OCI_ATTR_SESSLANG             = 46;  // session language handle

  OCI_ATTR_VISIBILITY           = 47;  // visibility
  OCI_ATTR_RELATIVE_MSGID       = 48;  // relative message id
  OCI_ATTR_SEQUENCE_DEVIATION   = 49;  // sequence deviation

  OCI_ATTR_CONSUMER_NAME        = 50;  // consumer name
  OCI_ATTR_DEQ_MODE             = 51;  // dequeue mode
  OCI_ATTR_NAVIGATION           = 52;  // navigation
  OCI_ATTR_WAIT                 = 53;  // wait
  OCI_ATTR_DEQ_MSGID            = 54;  // dequeue message id

  OCI_ATTR_PRIORITY             = 55;  // priority
  OCI_ATTR_DELAY                = 56;  // delay
  OCI_ATTR_EXPIRATION           = 57;  // expiration
  OCI_ATTR_CORRELATION          = 58;  // correlation id
  OCI_ATTR_ATTEMPTS             = 59;  // # of attempts
  OCI_ATTR_RECIPIENT_LIST       = 60;  // recipient list
  OCI_ATTR_EXCEPTION_QUEUE      = 61;  // exception queue name
  OCI_ATTR_ENQ_TIME             = 62;  // enqueue time (only OCIAttrGet)
  OCI_ATTR_MSG_STATE            = 63;  // message state (only OCIAttrGet)
                                       // NOTE: 64-66 used below
  OCI_ATTR_AGENT_NAME           = 64;  // agent name
  OCI_ATTR_AGENT_ADDRESS        = 65;  // agent address
  OCI_ATTR_AGENT_PROTOCOL       = 66;  // agent protocol

  OCI_ATTR_SENDER_ID            = 68;  // sender id
  OCI_ATTR_ORIGINAL_MSGID       = 69;  // original message id

  OCI_ATTR_QUEUE_NAME           = 70;  // queue name
  OCI_ATTR_NFY_MSGID            = 71;  // message id
  OCI_ATTR_MSG_PROP             = 72;  // message properties

  OCI_ATTR_NUM_DML_ERRORS       = 73;  // num of errs in array DML
  OCI_ATTR_DML_ROW_OFFSET       = 74;  // row offset in the array

  OCI_ATTR_DATEFORMAT           = 75;  // default date format string
  OCI_ATTR_BUF_ADDR             = 76;  // buffer address
  OCI_ATTR_BUF_SIZE             = 77;  // buffer size
  OCI_ATTR_DIRPATH_MODE         = 78;  // mode of direct path operation
  OCI_ATTR_DIRPATH_NOLOG        = 79;  // nologging option
  OCI_ATTR_DIRPATH_PARALLEL     = 80;  // parallel (temp seg) option
  OCI_ATTR_NUM_ROWS             = 81;  // number of rows in column array
                                       // NOTE that OCI_ATTR_NUM_COLS is a column
                                       // array attribute too.

  OCI_ATTR_COL_COUNT            = 82;  // columns of column array processed so far.
  OCI_ATTR_STREAM_OFFSET        = 83;  // str off of last row processed
  OCI_ATTR_SHARED_HEAPALLOC     = 84;  // Shared Heap Allocation Size

  OCI_ATTR_SERVER_GROUP         = 85;  // server group name

  OCI_ATTR_MIGSESSION           = 86;  // migratable session attribute

  OCI_ATTR_NOCACHE              = 87;  // Temporary LOBs

  OCI_ATTR_MEMPOOL_SIZE         = 88;  // Pool Size
  OCI_ATTR_MEMPOOL_INSTNAME     = 89;  // Instance name
  OCI_ATTR_MEMPOOL_APPNAME      = 90;  // Application name
  OCI_ATTR_MEMPOOL_HOMENAME     = 91;  // Home Directory name
  OCI_ATTR_MEMPOOL_MODEL        = 92;  // Pool Model (proc,thrd,both)
  OCI_ATTR_MODES                = 93;  // Modes

  OCI_ATTR_SUBSCR_NAME          = 94;  // name of subscription
  OCI_ATTR_SUBSCR_CALLBACK      = 95;  // associated callback
  OCI_ATTR_SUBSCR_CTX           = 96;  // associated callback context
  OCI_ATTR_SUBSCR_PAYLOAD       = 97;  // associated payload
  OCI_ATTR_SUBSCR_NAMESPACE     = 98;  // associated namespace

  OCI_ATTR_PROXY_CREDENTIALS    = 99;  // Proxy user credentials
  OCI_ATTR_INITIAL_CLIENT_ROLES = 100; // Initial client role list

  OCI_ATTR_UNK                  = 101; // unknown attribute
  OCI_ATTR_NUM_COLS             = 102; // number of columns
  OCI_ATTR_LIST_COLUMNS         = 103; // parameter of the column list
  OCI_ATTR_RDBA                 = 104; // DBA of the segment header
  OCI_ATTR_CLUSTERED            = 105; // whether the table is clustered
  OCI_ATTR_PARTITIONED          = 106; // whether the table is partitioned
  OCI_ATTR_INDEX_ONLY           = 107; // whether the table is index only
  OCI_ATTR_LIST_ARGUMENTS       = 108; // parameter of the argument list
  OCI_ATTR_LIST_SUBPROGRAMS     = 109; // parameter of the subprogram list
  OCI_ATTR_REF_TDO              = 110; // REF to the type descriptor
  OCI_ATTR_LINK                 = 111; // the database link name
  OCI_ATTR_MIN                  = 112; // minimum value
  OCI_ATTR_MAX                  = 113; // maximum value
  OCI_ATTR_INCR                 = 114; // increment value
  OCI_ATTR_CACHE                = 115; // number of sequence numbers cached
  OCI_ATTR_ORDER                = 116; // whether the sequence is ordered
  OCI_ATTR_HW_MARK              = 117; // high-water mark
  OCI_ATTR_TYPE_SCHEMA          = 118; // type's schema name
  OCI_ATTR_TIMESTAMP            = 119; // timestamp of the object
  OCI_ATTR_NUM_ATTRS            = 120; // number of sttributes
  OCI_ATTR_NUM_PARAMS           = 121; // number of parameters
  OCI_ATTR_OBJID                = 122; // object id for a table or view
  OCI_ATTR_PTYPE                = 123; // type of info described by
  OCI_ATTR_PARAM                = 124; // parameter descriptor
  OCI_ATTR_OVERLOAD_ID          = 125; // overload ID for funcs and procs
  OCI_ATTR_TABLESPACE           = 126; // table name space
  OCI_ATTR_TDO                  = 127; // TDO of a type
  OCI_ATTR_LTYPE                = 128; // list type
  OCI_ATTR_PARSE_ERROR_OFFSET   = 129; // Parse Error offset
  OCI_ATTR_IS_TEMPORARY         = 130; // whether table is temporary
  OCI_ATTR_IS_TYPED             = 131; // whether table is typed
  OCI_ATTR_DURATION             = 132; // duration of temporary table
  OCI_ATTR_IS_INVOKER_RIGHTS    = 133; // is invoker rights
  OCI_ATTR_OBJ_NAME             = 134; // top level schema obj name
  OCI_ATTR_OBJ_SCHEMA           = 135; // schema name
  OCI_ATTR_OBJ_ID               = 136; // top level schema object id

  { OCI Error Return Values }
  OCI_SUCCESS             = 0;
  OCI_SUCCESS_WITH_INFO   = 1;
  OCI_NO_DATA             = 100;
  OCI_ERROR               = -1;
  OCI_INVALID_HANDLE      = -2;
  OCI_NEED_DATA           = 99;
  OCI_STILL_EXECUTING     = -3123;
  OCI_CONTINUE            = -24200;

  { Generic Default Value for Modes, .... }
  OCI_DEFAULT     = $0;

  { OCI Init Mode }
  OCI_THREADED    = $1;
  OCI_OBJECT      = $2;
  OCI_EVENTS      = $4;
  OCI_SHARED      = $10;
  OCI_NO_UCB      = $40;
  OCI_NO_MUTEX    = $80;

  { OCI Credentials }
  OCI_CRED_RDBMS  = 1;
  OCI_CRED_EXT    = 2;
  OCI_CRED_PROXY  = 3;

  { OCI Authentication Mode }
  OCI_MIGRATE     = $0001;             // migratable auth context
  OCI_SYSDBA      = $0002;             // for SYSDBA authorization
  OCI_SYSOPER     = $0004;             // for SYSOPER authorization
  OCI_PRELIM_AUTH = $0008;             // for preliminary authorization

  { OCIPasswordChange }
  OCI_AUTH        = $08;               // Change the password but do not login

  { OCI Data Types }
  SQLT_CHR = 1  ;
  SQLT_NUM = 2  ;
  SQLT_INT = 3  ;
  SQLT_FLT = 4  ;
  SQLT_STR = 5  ;
  SQLT_VNU = 6  ;
  SQLT_PDN = 7  ;
  SQLT_LNG = 8  ;
  SQLT_VCS = 9  ;
  SQLT_NON = 10 ;
  SQLT_RID = 11 ;
  SQLT_DAT = 12 ;
  SQLT_VBI = 15 ;
  SQLT_BIN = 23 ;
  SQLT_LBI = 24 ;
  _SQLT_PLI = 29;
  SQLT_UIN = 68 ;
  SQLT_SLS = 91 ;
  SQLT_LVC = 94 ;
  SQLT_LVB = 95 ;
  SQLT_AFC = 96 ;
  SQLT_AVC = 97 ;
  SQLT_CUR = 102;
  SQLT_RDD = 104;
  SQLT_LAB = 105;
  SQLT_OSL = 106;
  SQLT_NTY = 108;
  SQLT_REF = 110;
  SQLT_CLOB = 112;
  SQLT_BLOB = 113;
  SQLT_BFILEE = 114;
  SQLT_CFILEE = 115;
  SQLT_RSET = 116;
  SQLT_NCO = 122;
  SQLT_VST = 155;
  SQLT_ODT = 156;

  _SQLT_REC = 250;
  _SQLT_TAB = 251;
  _SQLT_BOL = 252;

  { OCI Statement Types }
  OCI_STMT_SELECT  = 1;   // select statement
  OCI_STMT_UPDATE  = 2;   // update statement
  OCI_STMT_DELETE  = 3;   // delete statement
  OCI_STMT_INSERT  = 4;   // Insert Statement
  OCI_STMT_CREATE  = 5;   // create statement
  OCI_STMT_DROP    = 6;   // drop statement
  OCI_STMT_ALTER   = 7;   // alter statement
  OCI_STMT_BEGIN   = 8;   // begin ... (pl/sql statement)
  OCI_STMT_DECLARE = 9;   // declare .. (pl/sql statement)

  { OCI Statement language }
  OCI_NTV_SYNTAX  = 1;    // Use what so ever is the native lang of server
  OCI_V7_SYNTAX   = 2;    // V7 language
  OCI_V8_SYNTAX   = 3;    // V8 language

  { OCI Statement Execute mode }
  OCI_BATCH_MODE        = $01;    // batch the oci statement for execution
  OCI_EXACT_FETCH       = $02;    // fetch the exact rows specified
  OCI_SCROLLABLE_CURSOR = $08;    // cursor scrollable
  OCI_DESCRIBE_ONLY     = $10;    // only describe the statement
  OCI_COMMIT_ON_SUCCESS = $20;    // commit, if successful execution
  OCI_NON_BLOCKING      = $40;    // non-blocking
  OCI_BATCH_ERRORS      = $80;    // batch errors in array dmls
  OCI_PARSE_ONLY        = $100;   // only parse the statement

  OCI_DATA_AT_EXEC    = $02;      // data at execute time
  OCI_DYNAMIC_FETCH   = $02;      // fetch dynamically
  OCI_PIECEWISE       = $04;      // piecewise DMLs or fetch

  { OCI Transaction modes }
  OCI_TRANS_NEW          = $00000001; // starts a new transaction branch
  OCI_TRANS_JOIN         = $00000002; // join an existing transaction
  OCI_TRANS_RESUME       = $00000004; // resume this transaction
  OCI_TRANS_STARTMASK    = $000000ff;

  OCI_TRANS_READONLY     = $00000100; // starts a readonly transaction
  OCI_TRANS_READWRITE    = $00000200; // starts a read-write transaction
  OCI_TRANS_SERIALIZABLE = $00000400; // starts a serializable transaction
  OCI_TRANS_ISOLMASK     = $0000ff00;

  OCI_TRANS_LOOSE        = $00010000; // a loosely coupled branch
  OCI_TRANS_TIGHT        = $00020000; // a tightly coupled branch
  OCI_TRANS_TYPEMASK     = $000f0000;

  OCI_TRANS_NOMIGRATE    = $00100000; // non migratable transaction
  OCI_TRANS_TWOPHASE     = $01000000; // use two phase commit

  { OCI pece wise fetch }
  OCI_ONE_PIECE       = 0; // one piece
  OCI_FIRST_PIECE     = 1; // the first piece
  OCI_NEXT_PIECE      = 2; // the next of many pieces
  OCI_LAST_PIECE      = 3; // the last piece

  { OCI fetch modes }
  OCI_FETCH_NEXT      = $02;  // next row
  OCI_FETCH_FIRST     = $04;  // first row of the result set
  OCI_FETCH_LAST      = $08;  // the last row of the result set
  OCI_FETCH_PRIOR     = $10;  // the previous row relative to current
  OCI_FETCH_ABSOLUTE  = $20;  // absolute offset from first
  OCI_FETCH_RELATIVE  = $40;  // offset relative to current

  {****************** Describe Handle Parameter Attributes *****************}

  { Attributes common to Columns and Stored Procs }
  OCI_ATTR_DATA_SIZE      = 1;    // maximum size of the data
  OCI_ATTR_DATA_TYPE      = 2;    // the SQL type of the column/argument
  OCI_ATTR_DISP_SIZE      = 3;    // the display size
  OCI_ATTR_NAME           = 4;    // the name of the column/argument
  OCI_ATTR_PRECISION      = 5;    // precision if number type
  OCI_ATTR_SCALE          = 6;    // scale if number type
  OCI_ATTR_IS_NULL        = 7;    // is it null ?
  OCI_ATTR_TYPE_NAME      = 8;    // name of the named data type or a package name for package private types
  OCI_ATTR_SCHEMA_NAME    = 9;    // the schema name
  OCI_ATTR_SUB_NAME       = 10;   // type name if package private type
  OCI_ATTR_POSITION       = 11;   // relative position of col/arg in the list of cols/args

  { complex object retrieval parameter attributes }
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE         = 50;
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL   = 51;
  OCI_ATTR_COMPLEXOBJECT_LEVEL            = 52;
  OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE   = 53;

  { Only Columns }
  OCI_ATTR_DISP_NAME                 = 100;  // the display name

  { Only Stored Procs }
  OCI_ATTR_OVERLOAD                  = 210;  // is this position overloaded
  OCI_ATTR_LEVEL                     = 211;  // level for structured types
  OCI_ATTR_HAS_DEFAULT               = 212;  // has a default value
  OCI_ATTR_IOMODE                    = 213;  // in, out inout
  OCI_ATTR_RADIX                     = 214;  // returns a radix
  OCI_ATTR_NUM_ARGS                  = 215;  // total number of arguments

  { only named type attributes }
  OCI_ATTR_TYPECODE                  = 216;   // object or collection
  OCI_ATTR_COLLECTION_TYPECODE       = 217;   // varray or nested table
  OCI_ATTR_VERSION                   = 218;   // user assigned version
  OCI_ATTR_IS_INCOMPLETE_TYPE        = 219;   // is this an incomplete type
  OCI_ATTR_IS_SYSTEM_TYPE            = 220;   // a system type
  OCI_ATTR_IS_PREDEFINED_TYPE        = 221;   // a predefined type
  OCI_ATTR_IS_TRANSIENT_TYPE         = 222;   // a transient type
  OCI_ATTR_IS_SYSTEM_GENERATED_TYPE  = 223;   // system generated type
  OCI_ATTR_HAS_NESTED_TABLE          = 224;   // contains nested table attr
  OCI_ATTR_HAS_LOB                   = 225;   // has a lob attribute
  OCI_ATTR_HAS_FILE                  = 226;   // has a file attribute
  OCI_ATTR_COLLECTION_ELEMENT        = 227;   // has a collection attribute
  OCI_ATTR_NUM_TYPE_ATTRS            = 228;   // number of attribute types
  OCI_ATTR_LIST_TYPE_ATTRS           = 229;   // list of type attributes
  OCI_ATTR_NUM_TYPE_METHODS          = 230;   // number of type methods
  OCI_ATTR_LIST_TYPE_METHODS         = 231;   // list of type methods
  OCI_ATTR_MAP_METHOD                = 232;   // map method of type
  OCI_ATTR_ORDER_METHOD              = 233;   // order method of type

  { only collection element }
  OCI_ATTR_NUM_ELEMS                 = 234;   // number of elements

  { only type methods }
  OCI_ATTR_ENCAPSULATION             = 235;   // encapsulation level
  OCI_ATTR_IS_SELFISH                = 236;   // method selfish
  OCI_ATTR_IS_VIRTUAL                = 237;   // virtual
  OCI_ATTR_IS_INLINE                 = 238;   // inline
  OCI_ATTR_IS_CONSTANT               = 239;   // constant
  OCI_ATTR_HAS_RESULT                = 240;   // has result
  OCI_ATTR_IS_CONSTRUCTOR            = 241;   // constructor
  OCI_ATTR_IS_DESTRUCTOR             = 242;   // destructor
  OCI_ATTR_IS_OPERATOR               = 243;   // operator
  OCI_ATTR_IS_MAP                    = 244;   // a map method
  OCI_ATTR_IS_ORDER                  = 245;   // order method
  OCI_ATTR_IS_RNDS                   = 246;   // read no data state method
  OCI_ATTR_IS_RNPS                   = 247;   // read no process state
  OCI_ATTR_IS_WNDS                   = 248;   // write no data state method
  OCI_ATTR_IS_WNPS                   = 249;   // write no process state

  OCI_ATTR_DESC_PUBLIC               = 250;   // public object

  { Object Cache Enhancements : attributes for User Constructed Instances }
  OCI_ATTR_CACHE_CLIENT_CONTEXT      = 251;
  OCI_ATTR_UCI_CONSTRUCT             = 252;
  OCI_ATTR_UCI_DESTRUCT              = 253;
  OCI_ATTR_UCI_COPY                  = 254;
  OCI_ATTR_UCI_PICKLE                = 255;
  OCI_ATTR_UCI_UNPICKLE              = 256;
  OCI_ATTR_UCI_REFRESH               = 257;

  { for type inheritance }
  OCI_ATTR_IS_SUBTYPE                = 258;
  OCI_ATTR_SUPERTYPE_SCHEMA_NAME     = 259;
  OCI_ATTR_SUPERTYPE_NAME            = 260;

  { for schemas }
  OCI_ATTR_LIST_OBJECTS              = 261;   // list of objects in schema

  { for database }
  OCI_ATTR_NCHARSET_ID               = 262;   // char set id
  OCI_ATTR_LIST_SCHEMAS              = 263;   // list of schemas
  OCI_ATTR_MAX_PROC_LEN              = 264;   // max procedure length
  OCI_ATTR_MAX_COLUMN_LEN            = 265;   // max column name length
  OCI_ATTR_CURSOR_COMMIT_BEHAVIOR    = 266;   // cursor commit behavior
  OCI_ATTR_MAX_CATALOG_NAMELEN       = 267;   // catalog namelength
  OCI_ATTR_CATALOG_LOCATION          = 268;   // catalog location
  OCI_ATTR_SAVEPOINT_SUPPORT         = 269;   // savepoint support
  OCI_ATTR_NOWAIT_SUPPORT            = 270;   // nowait support
  OCI_ATTR_AUTOCOMMIT_DDL            = 271;   // autocommit DDL
  OCI_ATTR_LOCKING_MODE              = 272;   // locking mode

  OCI_ATTR_CACHE_ARRAYFLUSH          = $40;
  OCI_ATTR_OBJECT_NEWNOTNULL         = $10;
  OCI_ATTR_OBJECT_DETECTCHANGE       = $20;

  { Piece Information }
  OCI_PARAM_IN                       = $01;  // in parameter
  OCI_PARAM_OUT                      = $02;  // out parameter

  { LOB Buffering Flush Flags }
  OCI_LOB_BUFFER_FREE     = 1;
  OCI_LOB_BUFFER_NOFREE   = 2;

  { FILE open modes }
  OCI_FILE_READONLY   = 1;    // readonly mode open for FILE types
  { LOB open modes }
  OCI_LOB_READONLY    = 1;    // readonly mode open for ILOB types
  OCI_LOB_READWRITE   = 2;    // read write mode open for ILOBs

  { CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information }
  SQLCS_IMPLICIT = 1;     // for CHAR, VARCHAR2, CLOB w/o a specified set
  SQLCS_NCHAR    = 2;     // for NCHAR, NCHAR VARYING, NCLOB
  SQLCS_EXPLICIT = 3;     // for CHAR, etc, with "CHARACTER SET ..." syntax
  SQLCS_FLEXIBLE = 4;     // for PL/SQL "flexible" parameters
  SQLCS_LIT_NULL = 5;     // for typecheck of NULL and empty_clob() lits

  {************************ OCIDesribeAny *************************}

  { Describe mode }
  OCI_OTYPE_NAME = 1;
  OCI_OTYPE_REF = 2;
  OCI_OTYPE_PTR = 3;

  { Object type }
  OCI_PTYPE_UNK           = 0;    // unknown
  OCI_PTYPE_TABLE         = 1;    // table
  OCI_PTYPE_VIEW          = 2;    // view
  OCI_PTYPE_PROC          = 3;    // procedure
  OCI_PTYPE_FUNC          = 4;    // function
  OCI_PTYPE_PKG           = 5;    // package
  OCI_PTYPE_TYPE          = 6;    // user-defined type
  OCI_PTYPE_SYN           = 7;    // synonym
  OCI_PTYPE_SEQ           = 8;    // sequence
  OCI_PTYPE_COL           = 9;    // column
  OCI_PTYPE_ARG           = 10;   // argument
  OCI_PTYPE_LIST          = 11;   // list
  OCI_PTYPE_TYPE_ATTR     = 12;   // user-defined type's attribute
  OCI_PTYPE_TYPE_COLL     = 13;   // collection type's element
  OCI_PTYPE_TYPE_METHOD   = 14;   // user-defined type's method
  OCI_PTYPE_TYPE_ARG      = 15;   // user-defined type method's argument
  OCI_PTYPE_TYPE_RESULT   = 16;   // user-defined type method's result

  { Proc/Func param type }
  OCI_TYPEPARAM_IN    = 0;
  OCI_TYPEPARAM_OUT   = 1;
  OCI_TYPEPARAM_INOUT = 2;

  { Number formats }
  OCI_NUMBER_UNSIGNED = 0;
  OCI_NUMBER_SIGNED   = 2;

type
  PPointer = ^Pointer;

  TOCIInitialize = function(mode: ub4; ctxp: Pointer; malocfp: Pointer;
    ralocfp: Pointer; mfreefp: Pointer): sword; cdecl;

  TOCIEnvInit = function(var envhpp: POCIEnv; mode: ub4; xtramemsz: size_T;
    usrmempp: PPointer): sword; cdecl;

  TOCIEnvCreate = function(var envhpp: POCIEnv; mode: ub4; ctxp: Pointer;
    malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer; xtramemsz: size_T;
    usrmempp: PPointer): sword; cdecl;

  TOCIHandleAlloc = function(parenth: POCIHandle; var hndlpp: POCIHandle;
    atype: ub4; xtramem_sz: size_T; usrmempp: PPointer): sword; cdecl;

  TOCIServerAttach = function(srvhp: POCIServer; errhp: POCIError; dblink: text;
    dblink_len: sb4; mode: ub4): sword; cdecl;

  TOCIAttrSet = function(trgthndlp: POCIHandle; trghndltyp: ub4;
    attributep: Pointer; size: ub4; attrtype: ub4; errhp: POCIError):sword; cdecl;

  TOCISessionBegin = function(svchp: POCISvcCtx; errhp: POCIError;
    usrhp: POCISession; credt: ub4; mode: ub4):sword; cdecl;

  TOCISessionEnd = function(svchp: POCISvcCtx; errhp: POCIError;
    usrhp: POCISession; mode: ub4): sword; cdecl;

  TOCIServerDetach = function(srvhp: POCIServer; errhp: POCIError;
    mode: ub4): sword; cdecl;

  TOCIHandleFree = function(hndlp: Pointer; atype: ub4): sword; cdecl;

  TOCIErrorGet = function(hndlp: Pointer; recordno: ub4; sqlstate: text;
    var errcodep: sb4; bufp: text; bufsiz: ub4; atype: ub4): sword; cdecl;

  TOCIStmtPrepare = function(stmtp: POCIStmt; errhp: POCIError; stmt: text;
    stmt_len: ub4; language:ub4; mode: ub4):sword; cdecl;

  TOCIStmtExecute = function(svchp: POCISvcCtx; stmtp: POCIStmt;
    errhp: POCIError; iters: ub4; rowoff: ub4; snap_in: POCISnapshot;
    snap_out: POCISnapshot; mode: ub4): sword; cdecl;

  TOCIParamGet = function(hndlp: Pointer; htype: ub4; errhp: POCIError;
    var parmdpp: Pointer; pos: ub4): sword; cdecl;

  TOCIAttrGet = function(trgthndlp: POCIHandle; trghndltyp: ub4;
    attributep: Pointer; sizep: Pointer; attrtype: ub4;
    errhp: POCIError):sword; cdecl;

  TOCIStmtFetch = function(stmtp: POCIStmt; errhp: POCIError; nrows: ub4;
    orientation: ub2; mode: ub4): sword; cdecl;

  TOCIDefineByPos = function(stmtp: POCIStmt; var defnpp: POCIDefine;
    errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
    indp: Pointer; rlenp: Pointer; rcodep: Pointer; mode: ub4): sword; cdecl;

  TOCIDefineArrayOfStruct = function(defnpp: POCIDefine; errhp: POCIError;
    pvskip: ub4; indskip: ub4; rlskip: ub4; rcskip: ub4): sword; cdecl;

  TOCIBindByPos = function(stmtp: POCIStmt; var bindpp: POCIBind;
    errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
    indp: Pointer; alenp: Pointer; rcodep: Pointer; maxarr_len: ub4;
    curelep: Pointer; mode: ub4): sword; cdecl;

  TOCIBindByName = function(stmtp: POCIStmt; var bindpp: POCIBind;
    errhp: POCIError; placeholder: text; placeh_len: sb4; valuep: Pointer;
    value_sz: sb4; dty: ub2; indp: Pointer; alenp: Pointer; rcodep: Pointer;
    maxarr_len: ub4; curelep: Pointer; mode: ub4): sword; cdecl;

  TOCIBindDynamic = function(bindp: POCIBind; errhp: POCIError; ictxp: Pointer;
    icbfp: Pointer; octxp: Pointer; ocbfp: Pointer): sword; cdecl;

  TOCITransStart = function(svchp: POCISvcCtx; errhp: POCIError; timeout: word;
    flags: ub4): sword; cdecl;

  TOCITransRollback = function(svchp:POCISvcCtx; errhp:POCIError;
    flags: ub4): sword; cdecl;

  TOCITransCommit = function(svchp: POCISvcCtx; errhp: POCIError;
    flags: ub4) :sword; cdecl;

  TOCITransDetach = function(svchp: POCISvcCtx; errhp: POCIError;
    flags: ub4) :sword; cdecl;

  TOCITransPrepare = function(svchp: POCISvcCtx; errhp: POCIError;
    flags: ub4) :sword; cdecl;

  TOCITransForget = function(svchp: POCISvcCtx; errhp: POCIError;
    flags: ub4) :sword; cdecl;

  TOCIDescribeAny = function(svchp: POCISvcCtx; errhp: POCIError;
    objptr: Pointer; objnm_len: ub4; objptr_typ: ub1; info_level: ub1;
    objtyp: ub1; dschp: POCIDescribe): sword; cdecl;

  TOCIBreak = function(svchp: POCISvcCtx; errhp:POCIError): sword; cdecl;

  TOCIReset = function(svchp: POCISvcCtx; errhp:POCIError): sword; cdecl;

  TOCIDescriptorAlloc = function(parenth: POCIEnv; var descpp: POCIDescriptor;
    htype: ub4; xtramem_sz: integer; usrmempp: Pointer): sword; cdecl;

  TOCIDescriptorFree = function(descp: Pointer; htype: ub4): sword; cdecl;

  TOCIDateTimeAssign = function(hndl: POCIEnv; err: POCIError;
    const from: POCIDateTime;_to: POCIDateTime): sword; cdecl;

  TOCIDateTimeCheck = function(hndl: POCIEnv; err: POCIError;
    const date: POCIDateTime; var valid: ub4): sword; cdecl;

  TOCIDateTimeCompare = function(hndl: POCIEnv; err: POCIError;
    const date1: POCIDateTime; const date2: POCIDateTime;
    var result: sword): sword; cdecl;

  TOCIDateTimeConvert = function(hndl: POCIEnv; err: POCIError;
    indate: POCIDateTime; outdate: POCIDateTime): sword; cdecl;

  TOCIDateTimeFromText= function(hndl: POCIEnv; err: POCIError;
    const date_str: text; d_str_length: size_t; const fmt: text;
    fmt_length: ub1; const lang_name: text; lang_length: size_t;
    date: POCIDateTime): sword; cdecl;

  TOCIDateTimeGetDate = function(hndl: POCIEnv; err: POCIError;
    const date: POCIDateTime; var year: sb2; var month: ub1;
    var day: ub1): sword; cdecl;

  TOCIDateTimeGetTime = function(hndl: POCIEnv; err: POCIError;
    datetime: POCIDateTime; var hour: ub1; var minute: ub1; var sec: ub1;
    var fsec: ub4): sword; cdecl;

  TOCIDateTimeGetTimeZoneOffset = function(hndl: POCIEnv; err: POCIError;
    const datetime: POCIDateTime; var hour: sb1; var minute: sb1): sword; cdecl;

  TOCIDateTimeSysTimeStamp = function(hndl: POCIEnv; err: POCIError;
    sys_date: POCIDateTime): sword; cdecl;

  TOCIDateTimeConstruct = function(hndl: POCIEnv; err: POCIError;
    datetime: POCIDateTime; year: sb2; month: ub1; day: ub1; hour: ub1;
    min: ub1; sec: ub1; fsec: ub4; timezone: text;
    timezone_length: size_t): sword; cdecl;

  TOCIDateTimeToText = function(hndl: POCIEnv; err: POCIError;
    const date: POCIDateTime; const fmt: text; fmt_length: ub1;
    fsprec: ub1; const lang_name: text; lang_length: size_t;
    var buf_size: ub4; buf: text): sword; cdecl;

  TOCIDateTimeGetTimeZoneName = function(hndl: POCIEnv; err: POCIError;
    datetime: POCIDateTime; var buf: ub1; var buflen: ub4): sword; cdecl;

  TOCILobAppend = function(svchp: POCISvcCtx; errhp: POCIError; dst_locp,
    src_locp: POCILobLocator): sword; cdecl;

  TOCILobAssign = function(svchp: POCISvcCtx; errhp: POCIError;
    src_locp: POCILobLocator; var dst_locpp: POCILobLocator): sword; cdecl;

  TOCILobClose = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator): sword; cdecl;

  TOCILobCopy = function(svchp: POCISvcCtx; errhp: POCIError;
    dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
    dst_offset: ub4; src_offset: ub4): sword; cdecl;

  TOCILobCreateTemporary = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; csid: ub2; csfrm: ub1; lobtype: ub1;
    cache: LongBool; duration: OCIDuration): sword; cdecl;

  TOCILobEnableBuffering = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator): sword; cdecl;

  TOCILobDisableBuffering = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator): sword; cdecl;

  TOCILobErase = function(svchp: POCISvcCtx; errhp: POCIError; locp: POCILobLocator;
    var amount: ub4; offset: ub4): sword; cdecl;

  TOCILobFileExists = function(svchp: POCISvcCtx; errhp: POCIError;
    filep: POCILobLocator; var flag: Boolean): sword; cdecl;

  TOCILobFileGetName = function(envhp: POCIEnv; errhp: POCIError;
    filep: POCILobLocator; dir_alias: text; var d_length: ub2; filename: text;
    var f_length: ub2): sword; cdecl;

  TOCILobFileSetName = function(envhp: POCIEnv; errhp: POCIError;
    var filep: POCILobLocator; dir_alias: text; d_length: ub2; filename: text;
    f_length: ub2): sword; cdecl;

  TOCILobFlushBuffer = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; flag: ub4): sword; cdecl;

  TOCILobFreeTemporary = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator): sword; cdecl;

  TOCILobGetLength = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; var lenp: ub4): sword; cdecl;

  TOCILobIsOpen = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; var flag: LongBool): sword; cdecl;

  TOCILobIsTemporary = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; var is_temporary: LongBool): sword; cdecl;

  TOCILobLoadFromFile = function(svchp: POCISvcCtx; errhp: POCIError;
    dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
    dst_offset: ub4; src_offset: ub4): sword; cdecl;

  TOCILobLocatorIsInit = function(envhp: POCIEnv; errhp: POCIError;
   locp: POCILobLocator; var is_initialized: LongBool): sword; cdecl;

  TOCILobOpen = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; mode: ub1): sword; cdecl;

  TOCILobRead = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
    ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;

  TOCILobTrim = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; newlen: ub4): sword; cdecl;

  TOCILobWrite = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
    piece: ub1; ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;

  TOCIStmtGetPieceInfo = function(stmtp: POCIStmt; errhp: POCIError;
    var hndlpp: Pointer; var typep: ub4; var in_outp: ub1; var iterp: ub4;
    var idxp: ub4; var piecep: ub1): sword; cdecl;

  TOCIStmtSetPieceInfo = function(handle: Pointer; typep: ub4; errhp: POCIError;
    buf: Pointer; var alenp: ub4; piece: ub1; indp: Pointer;
    var rcodep: ub2): sword; cdecl;

  TOCIPasswordChange = function(svchp: POCISvcCtx; errhp: POCIError;
    user_name: text; usernm_len: ub4; opasswd: text; opasswd_len: ub4;
    npasswd: text; npasswd_len: sb4; mode: ub4): sword; cdecl;

  TOCIServerVersion = function(hndlp: POCIHandle; errhp: POCIError; bufp: text;
    bufsz: ub4; hndltype: ub1): sword; cdecl;

  TOCIResultSetToStmt = function(rsetdp: POCIHandle; errhp: POCIError): sword; cdecl;

var
  OCIPasswordChange:      TOCIPasswordChange;
  OCIInitialize:          TOCIInitialize;
  OCIEnvInit:             TOCIEnvInit;
  OCIEnvCreate:           TOCIEnvCreate;
  OCIHandleAlloc:         TOCIHandleAlloc;
  OCIServerAttach:        TOCIServerAttach;
  OCIAttrSet:             TOCIAttrSet;
  OCISessionBegin:        TOCISessionBegin;
  OCISessionEnd:          TOCISessionEnd;
  OCIServerDetach:        TOCIServerDetach;
  OCIHandleFree:          TOCIHandleFree;
  OCIErrorGet:            TOCIErrorGet;
  OCIStmtPrepare:         TOCIStmtPrepare;
  OCIStmtExecute:         TOCIStmtExecute;
  OCIParamGet:            TOCIParamGet;
  OCIAttrGet:             TOCIAttrGet;
  OCIStmtFetch:           TOCIStmtFetch;
  OCIDefineByPos:         TOCIDefineByPos;
  OCIDefineArrayOfStruct: TOCIDefineArrayOfStruct;
  OCIBindByPos:           TOCIBindByPos;
  OCIBindByName:          TOCIBindByName;
  OCITransStart:          TOCITransStart;
  OCITransCommit:         TOCITransCommit;
  OCITransRollback:       TOCITransRollback;
  OCITransDetach:         TOCITransDetach;
  OCITransPrepare:        TOCITransPrepare;
  OCITransForget:         TOCITransForget;
  OCIDescribeAny:         TOCIDescribeAny;
  OCIBreak:               TOCIBreak;
  OCIReset:               TOCIReset;
  OCIDescriptorAlloc:     TOCIDescriptorAlloc;
  OCIDescriptorFree:      TOCIDescriptorFree;
  OCIStmtGetPieceInfo:    TOCIStmtGetPieceInfo;
  OCIStmtSetPieceInfo:    TOCIStmtSetPieceInfo;
  OCIServerVersion:       TOCIServerVersion;
  OCIBindDynamic:         TOCIBindDynamic;
  OCIDateTimeAssign:      TOCIDateTimeAssign;
  OCIDateTimeCheck:       TOCIDateTimeCheck;
  OCIDateTimeCompare:     TOCIDateTimeCompare;
  OCIDateTimeConvert:     TOCIDateTimeConvert;
  OCIDateTimeFromText:    TOCIDateTimeFromText;
  OCIDateTimeGetDate:     TOCIDateTimeGetDate;
  OCIDateTimeGetTime:     TOCIDateTimeGetTime;
  OCIDateTimeGetTimeZoneOffset: TOCIDateTimeGetTimeZoneOffset;
  OCIDateTimeSysTimeStamp: TOCIDateTimeSysTimeStamp;
  OCIDateTimeConstruct:   TOCIDateTimeConstruct;
  OCIDateTimeToText:      TOCIDateTimeToText;
  OCIDateTimeGetTimeZoneName: TOCIDateTimeGetTimeZoneName;
  OCILobAppend:           TOCILobAppend;
  OCILobAssign:           TOCILobAssign;
  OCILobClose:            TOCILobClose;
  OCILobCopy:             TOCILobCopy;
  OCILobCreateTemporary:  TOCILobCreateTemporary;
  OCILobEnableBuffering:  TOCILobEnableBuffering;
  OCILobDisableBuffering: TOCILobDisableBuffering;
  OCILobErase:            TOCILobErase;
  OCILobFileExists:       TOCILobFileExists;
  OCILobFileGetName:      TOCILobFileGetName;
  OCILobFileSetName:      TOCILobFileSetName;
  OCILobFlushBuffer:      TOCILobFlushBuffer;
  OCILobFreeTemporary:    TOCILobFreeTemporary;
  OCILobGetLength:        TOCILobGetLength;
  OCILobIsOpen:           TOCILobIsOpen;
  OCILobIsTemporary:      TOCILobIsTemporary;
  OCILobLoadFromFile:     TOCILobLoadFromFile;
  OCILobLocatorIsInit:    TOCILobLocatorIsInit;
  OCILobOpen:             TOCILobOpen;
  OCILobRead:             TOCILobRead;
  OCILobTrim:             TOCILobTrim;
  OCILobWrite:            TOCILobWrite;
  OCIResultSetToStmt:     TOCIResultSetToStmt;

var
  LibraryLoader: TZNativeLibraryLoader;

implementation

type
  {** Implements a loader for Oracle native library. }
  TZOracleNativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
  end;

{ TZOracleNativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZOracleNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

  @OCIEnvCreate       := GetAddress('OCIEnvCreate');
  @OCIInitialize      := GetAddress('OCIInitialize');
  @OCIEnvInit         := GetAddress('OCIEnvInit');

  @OCIHandleAlloc     := GetAddress('OCIHandleAlloc');
  @OCIHandleFree      := GetAddress('OCIHandleFree');
  @OCIAttrSet         := GetAddress('OCIAttrSet');
  @OCIAttrGet         := GetAddress('OCIAttrGet');
  @OCIDescriptorAlloc := GetAddress('OCIDescriptorAlloc');
  @OCIDescriptorFree  := GetAddress('OCIDescriptorFree');
  @OCIErrorGet        := GetAddress('OCIErrorGet');

  @OCIServerAttach    := GetAddress('OCIServerAttach');
  @OCIServerDetach    := GetAddress('OCIServerDetach');
  @OCIServerVersion   := GetAddress('OCIServerVersion');
  @OCIBreak           := GetAddress('OCIBreak');

  { For Oracle >= 8.1 }
  @OCIReset           := GetAddress('OCIReset');

  @OCISessionBegin    := GetAddress('OCISessionBegin');
  @OCISessionEnd      := GetAddress('OCISessionEnd');
  @OCIPasswordChange  := GetAddress('OCIPasswordChange');

  @OCITransStart      := GetAddress('OCITransStart');
  @OCITransCommit     := GetAddress('OCITransCommit');
  @OCITransRollback   := GetAddress('OCITransRollback');
  @OCITransDetach     := GetAddress('OCITransDetach');
  @OCITransPrepare    := GetAddress('OCITransPrepare');
  @OCITransForget     := GetAddress('OCITransForget');

  @OCIStmtPrepare     := GetAddress('OCIStmtPrepare');
  @OCIStmtExecute     := GetAddress('OCIStmtExecute');
  @OCIStmtFetch       := GetAddress('OCIStmtFetch');
  @OCIStmtGetPieceInfo := GetAddress('OCIStmtGetPieceInfo');
  @OCIStmtSetPieceInfo := GetAddress('OCIStmtSetPieceInfo');
  @OCIParamGet        := GetAddress('OCIParamGet');
  @OCIResultSetToStmt := GetAddress('OCIResultSetToStmt');

  @OCIDefineByPos     := GetAddress('OCIDefineByPos');
  @OCIDefineArrayOfStruct := GetAddress('OCIDefineArrayOfStruct');

  @OCIBindByPos       := GetAddress('OCIBindByPos');
  @OCIBindByName      := GetAddress('OCIBindByName');
  @OCIBindDynamic     := GetAddress('OCIBindDynamic');

  @OCILobAppend       := GetAddress('OCILobAppend');
  @OCILobAssign       := GetAddress('OCILobAssign');
  @OCILobCopy         := GetAddress('OCILobCopy');
  @OCILobEnableBuffering := GetAddress('OCILobEnableBuffering');
  @OCILobDisableBuffering := GetAddress('OCILobDisableBuffering');
  @OCILobErase        := GetAddress('OCILobErase');
  @OCILobFileExists   := GetAddress('OCILobFileExists');
  @OCILobFileGetName  := GetAddress('OCILobFileGetName');
  @OCILobFileSetName  := GetAddress('OCILobFileSetName');
  @OCILobFlushBuffer  := GetAddress('OCILobFlushBuffer');
  @OCILobGetLength    := GetAddress('OCILobGetLength');
  @OCILobLoadFromFile := GetAddress('OCILobLoadFromFile');
  @OCILobLocatorIsInit := GetAddress('OCILobLocatorIsInit');
  @OCILobRead         := GetAddress('OCILobRead');
  @OCILobTrim         := GetAddress('OCILobTrim');
  @OCILobWrite        := GetAddress('OCILobWrite');

  { For Oracle >= 8.1 }
  @OCILobCreateTemporary := GetAddress('OCILobCreateTemporary');
  @OCILobFreeTemporary := GetAddress('OCILobFreeTemporary');
  @OCILobClose        := GetAddress('OCILobClose');
  @OCILobIsOpen       := GetAddress('OCILobIsOpen');
  @OCILobIsTemporary  := GetAddress('OCILobIsTemporary');
  @OCILobOpen         := GetAddress('OCILobOpen');

  @OCIDateTimeAssign  := GetAddress('OCIDateTimeAssign');
  @OCIDateTimeCheck   := GetAddress('OCIDateTimeCheck');
  @OCIDateTimeCompare := GetAddress('OCIDateTimeCompare');
  @OCIDateTimeConvert := GetAddress('OCIDateTimeConvert');
  @OCIDateTimeFromText := GetAddress('OCIDateTimeFromText');
  @OCIDateTimeGetDate := GetAddress('OCIDateTimeGetDate');
  @OCIDateTimeGetTime := GetAddress('OCIDateTimeGetTime');
  @OCIDateTimeGetTimeZoneOffset := GetAddress('OCIDateTimeGetTimeZoneOffset');
  @OCIDateTimeSysTimeStamp := GetAddress('OCIDateTimeSysTimeStamp');
  @OCIDateTimeConstruct := GetAddress('OCIDateTimeConstruct');
  @OCIDateTimeToText  := GetAddress('OCIDateTimeToText');
  @OCIDateTimeGetTimeZoneName := GetAddress('OCIDateTimeGetTimeZoneName');

  { For Oracle < 8.1 }
  //@OCILobClose        := GetAddress('OCILobFileClose');
  //@OCILobIsOpen       := GetAddress('OCILobFileIsOpen');
  //@OCILobOpen         := GetAddress('OCILobFileOpen');

  @OCIDescribeAny     := GetAddress('OCIDescribeAny');
end;

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZOracleNativeLibraryLoader.Create(
    [WINDOWS_DLL_LOCATION]);
{$ELSE}
  LibraryLoader := TZOracleNativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION]);
{$ENDIF}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
end.

