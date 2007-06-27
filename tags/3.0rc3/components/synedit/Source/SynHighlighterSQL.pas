{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSQL.pas, released 2000-04-21.
The Original Code is based on the wmSQLSyn.pas and wmSybaseSyn.pas files from
the mwEdit component suite by Martin Waldenburg and other developers, the
Initial Author of these files is Willo van der Merwe. Initial Author of
SynHighlighterSQL.pas is Michael Hieke.
Portions created by Willo van der Merwe are Copyright 1999 Willo van der Merwe.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterSQL.pas,v 1.25 2002/04/10 07:21:22 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(SQL highlighter for SynEdit with support for different dialects.)
@author(Michael Hieke)
@created(2000-04-21)
@lastmod(2000-11-16)
The SynHighlighterSQL implements a highlighter for SQL for the SynEdit projects.
Different SQL dialects can be selected via the Dialect property.
}
unit SynHighlighterSQL;

{$I SynEdit.inc}

interface

uses
  SysUtils,
{$IFDEF SYN_CLX}
  Types,
  kTextDrawer,
  QGraphics,
  QControls,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Registry,
  Controls,
{$ENDIF}
  Classes,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries;

type
  TtkTokenKind = (tkComment, tkDatatype, tkDefaultPackage, tkException,         // DJLP 2000-08-11
    tkFunction, tkIdentifier, tkKey, tkNull, tkNumber, tkSpace, tkPLSQL,        // DJLP 2000-08-11
    tkSQLPlus, tkString, tkSymbol, tkTableName, tkUnknown, tkVariable);         // DJLP 2000-08-11

  TRangeState = (rsUnknown, rsComment, rsString);

  TProcTableProc = procedure of object;

  TSQLDialect = (sqlStandard, sqlInterbase6, sqlMSSQL7, sqlMySQL, sqlOracle,
    sqlSybase, sqlIngres, sqlMSSQL2K);                                           // JJV 2000-11-16

type
  PIdentifierTable = ^TIdentifierTable;
  TIdentifierTable = array[Char] of ByteBool;

  PHashTable = ^THashTable;
  THashTable = array[Char] of Integer;

type
  TSynSQLSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fKeywords: TSynHashEntryList;
    fTableNames: TStrings;
    fDialect: TSQLDialect;
    fCommentAttri: TSynHighlighterAttributes;
    fDataTypeAttri: TSynHighlighterAttributes;
    fDefaultPackageAttri: TSynHighlighterAttributes;                            // DJLP 2000-08-11
    fExceptionAttri: TSynHighlighterAttributes;
    fFunctionAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fPLSQLAttri: TSynHighlighterAttributes;                                     // DJLP 2000-08-11
    fSpaceAttri: TSynHighlighterAttributes;
    fSQLPlusAttri: TSynHighlighterAttributes;                                   // DJLP 2000-09-05
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTableNameAttri: TSynHighlighterAttributes;
    fVariableAttri: TSynHighlighterAttributes;
    fIdentifiersPtr: PIdentifierTable;
    fmHashTablePtr: PHashTable;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure AndSymbolProc;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure SymbolAssignProc;
    procedure VariableProc;
    procedure UnknownProc;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure AnsiCProc;
    procedure DoAddKeyword(AKeyword: string; AKind: integer);
    procedure SetDialect(Value: TSQLDialect);
    procedure SetTableNames(const Value: TStrings);
    procedure TableNamesChanged(Sender: TObject);
    procedure InitializeKeywordLists;
    procedure PutTableNamesInKeywordList;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource : String; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    function IsKeyword(const AKeyword: string): boolean; override;              // DJLP 2000-08-09
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property DataTypeAttri: TSynHighlighterAttributes read fDataTypeAttri
      write fDataTypeAttri;
    property DefaultPackageAttri: TSynHighlighterAttributes                     // DJLP 2000-08-11
      read fDefaultPackageAttri write fDefaultPackageAttri;
    property ExceptionAttri: TSynHighlighterAttributes read fExceptionAttri
      write fExceptionAttri;
    property FunctionAttri: TSynHighlighterAttributes read fFunctionAttri
      write fFunctionAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property PLSQLAttri: TSynHighlighterAttributes read fPLSQLAttri             // DJLP 2000-08-11
      write fPLSQLAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property SQLPlusAttri: TSynHighlighterAttributes read fSQLPlusAttri         // DJLP 2000-09-05
      write fSQLPlusAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property TableNameAttri: TSynHighlighterAttributes read fTableNameAttri
      write fTableNameAttri;
    property TableNames: TStrings read fTableNames write SetTableNames;
    property VariableAttri: TSynHighlighterAttributes read fVariableAttri
      write fVariableAttri;
    property SQLDialect: TSQLDialect read fDialect write SetDialect;
  end;

implementation

uses
  SynEditStrConst;

var
  Identifiers: TIdentifierTable;
  mHashTable: THashTable;

  IdentifiersMSSQL7: TIdentifierTable;
  mHashTableMSSQL7: THashTable;

const
//---"Standard" (old set of keywords--------------------------------------------
  StandardKW: string =
    'active,after,all,alter,and,any,as,asc,ascending,at,auto,' +
    'base_name,before,begin,between,by,cache,cast,check,column,commit,' +
    'committed,computed,conditional,constraint,containing,count,create,' +
    'current,cursor,database,debug,declare,default,delete,desc,descending,' +
    'distinct,do,domain,drop,else,end,entry_point,escape,exception,execute,' +
    'exists,exit,external,extract,filter,for,foreign,from,full,function,' +
    'generator,grant,group,having,if,in,inactive,index,inner,insert,into,is,' +
    'isolation,join,key,left,level,like,merge,names,no,not,null,of,on,only,' +
    'or,order,outer,parameter,password,plan,position,primary,privileges,' +
    'procedure,protected,read,retain,returns,revoke,right,rollback,schema,' +
    'select,set,shadow,shared,snapshot,some,suspend,table,then,to,' +
    'transaction,trigger,uncommitted,union,unique,update,user,using,view,' +
    'wait,when,where,while,with,work';

//---Sybase keywords------------------------------------------------------------
  SybaseKW: string =
    'absolute,action,add,after,alias,all,allocate,alter,and,any,are,' +
    'arith_overflow,as,asc,assertion,async,at,authorization,avg,before,begin,' +
    'between,bit,bit_length,boolean,both,breadth,break,browse,bulk,by,call,' +
    'cascade,cascaded,case,cast,catalog,char,char_convert,char_length,' +
    'character,character_length,check,checkpoint,close,clustered,coalesce,' +
    'collate,collation,column,commit,completion,compute,confirm,' +
    'connect,connection,constraint,constraints,continue,controlrow,convert,' +
    'corresponding,count,create,cross,current,current_date,current_time,' +
    'current_timestamp,current_user,cursor,cycle,data,database,date,day,dbcc,' +
    'deallocate,dec,decimal,declare,default,deferrable,deferred,delete,depth,' +
    'desc,describe,descriptor,diagnostics,dictionary,dis,disconnect,distinct,' +
    'domain,double,drop,dummy,dump,each,else,elseif,en,end,endtran,equals,' +
    'errlvl,errordata,errorexit,escape,except,exception,exclusive,exec,' +
    'execute,exists,exit,exp_row_size,external,extract,false,fetch,' +
    'fillfactor,first,float,for,foreign,found,from,full,general,get,global,' +
    'go,goto,grant,group,having,holdlock,hour,identity,identity_gap,' +
    'identity_insert,identity_start,if,ignore,immediate,in,index,indicator,' +
    'initially,inner,input,insensitive,insert,install,int,integer,intersect,' +
    'interval,into,is,isolation,jar,join,key,kill,language,last,leading,' +
    'leave,left,less,level,like,limit,lineno,load,local,lock,loop,lower,' +
    'match,max,max_rows_per_page,min,minute,mirror,mirrorexit,modify,module,' +
    'month,names,national,natural,nchar,new,next,no,noholdlock,nonclustered,' +
    'none,not,null,nullif,numeric,numeric_truncation,object,' +
    'octet_length,of,off,offsets,oid,old,on,once,online,only,open,operation,' +
    'operators,option,or,order,others,outer,output,over,overlaps,pad,' +
    'parameters,partial,partition,pendant,perm,permanent,plan,position,' +
    'precision,preorder,prepare,preserve,primary,print,prior,private,' +
    'privileges,proc,procedure,processexit,protected,proxy_table,public,' +
    'quiesce,raiserror,read,readpast,readtext,real,reconfigure,recursive,' +
    'ref,reference,referencing,relative,remove,reorg,replace,replication,' +
    'reservepagegap,resignal,restrict,return,returns,revoke,right,role,' +
    'rollback,routine,row,rowcount,rows,rule,save,savepoint,schema,scroll,' +
    'search,second,section,select,sensitive,sequence,session_user,set,' +
    'setuser,shared,shutdown,signal,similar,size,smallint,some,space,sql,' +
    'sqlcode,sqlerror,sqlexception,sqlstate,statistics,stripe,structure,' +
    'substring,sum,syb_identity,syb_restree,system_user,table,temp,temporary,' +
    'test,textsize,then,there,time,timestamp,timezone_hour,timezone_minute,' +
    'to,trailing,tran,transaction,translate,translation,trigger,trim,true,' +
    'truncate,tsequal,type,under,union,unique,unknown,unpartition,update,' +
    'upper,usage,use,user,user_option,using,value,values,varchar,variable,' +
    'varying,view,virtual,visible,wait,waitfor,when,whenever,where,while,' +
    'with,without,work,write,writetext,year,zone';

//---Oracle---------------------------------------------------------------------

{begin}                                                                         // DJLP 2000-09-05
  // Oracle SQL keywords                                                        // JJV 2001-05-23
  OracleKW: string =
    'ACCESS,ACCOUNT,ACTIVATE,ADD,ADMIN,ADVISE,ALL,ALLOCATE,ALTER,ANALYZE,' +
    'ANCILLARY,AND,ANY,ARCHIVELOG,AS,ASC,ASSOCIATE,ATTRIBUTES,AUDIT,' +
    'AUTHENTICATED,AUTHID,AUTOALLOCATE,AUTOEXTEND,AUTOMATIC,BACKUP,BECOME,' +
    'BEFORE,BEHALF,BETWEEN,BINDING,BLOCK,BUFFER_POOL,BY,CACHE,CANCEL,CASCADE,' +
    'CAST,CATEGORY,CHAINED,CHANGE,CHARACTER,CHECK,CHECKPOINT,CHILD,CHUNK,' +
    'CLASS,CLONE,CLUSTER,COALESCE,COBOL,COLUMN,COLUMNS,COMMENT,COMPATIBILITY,' +
    'COMPILE,COMPLETE,COMPOSITE_LIMIT,COMPRESS,COMPUTE,CONNECT,CONNECT_TIME,' +
    'CONSTRAINT,CONSTRAINTS,CONTENTS,CONTEXT,CONTINUE,CONTROL,CONTROLFILE,' +
    'COST,CPU_PER_CALL,CPU_PER_SESSION,CREATE,CURRENT,CURRENT_USER,CYCLE,' +
    'DANGLING,DATAFILE,DEALLOCATE,DEBUG,DEFAULT,DEFERRED,DEFINER,DELETE,' +
    'DEMAND,DETERMINES,DICTIONARY,DIMENSION,DIRECTORY,DISABLE,DISASSOCIATE,' +
    'DISMOUNT,DISTINCT,DISTRIBUTED,DML,DROP,ELSE,ENABLE,ESCAPE,ESTIMATE,' +
    'EVENTS,EXCEPT,EXCEPTIONS,EXCHANGE,EXCLUDING,EXCLUSIVE,EXISTS,' +
    'EXPIRE,EXPLAIN,EXTENT,EXTERNALLY,FAILED_LOGIN_ATTEMPTS,FAST,FILE,FLUSH,' +
    'FOR,FORCE,FOREIGN,FORTRAN,FREELIST,FREELISTS,FROM,FUNCTIONS,GENERATED,' +
    'GLOBAL,GLOBAL_NAME,GLOBALLY,GO,GRANT,GROUP,GROUPS,HASH,HAVING,HEAP,' +
    'HIERARCHY,IDENTIFIED,IDLE_TIME,IMMEDIATE,IN,INCLUDING,INCREMENT,INDEX,' +
    'INDEXTYPE,INDEXTYPES,INFILE,INITIAL,INITIALLY,INITRANS,INSERT,INSTANCE,' +
    'INT,INTERSECT,INTO,INVALIDATE,IS,JAVA,JOIN,KEEP,KEY,KILL,LAYERLISTS,' +
    'LEVEL,LIBRARY,LIKE,LIMIT,LINK,LIST,LOB,LOCAL,LOCATOR,LOCK,LOGFILE,' +
    'LOGGING,LOGICAL_READS_PER_CALL,LOGICAL_READS_PER_SESSION,MANAGE,MANAGED,' +
    'MANUAL,MAP,MASTER,MATERIALIZED,MAXDATAFILES,MAXEXTENTS,MAXINSTANCES,' +
    'MAXLOGFILES,MAXLOGHISTORY,MAXLOGMEMBERS,MAXSIZE,MAXTRANS,MAXVALUE,' +
    'MEMBER,MINEXTENTS,MINIMIZE,MINIMUM,MINUS,MINVALUE,MODE,MODIFY,MODULE,' +
    'MOUNT,MOVE,MOVEMENT,MULTISET,NAMED,NATIONAL,NESTED,NEVER,NEXT,NO,' +
    'NOARCHIVELOG,NOAUDIT,NOCACHE,NOCOMPRESS,NOCYCLE,NOFORCE,NOLOGGING,' +
    'NOMAXVALUE,NOMINIMIZE,NOMINVALUE,NONE,NOORDER,NORELY,NORESETLOGS,' +
    'NOREVERSE,NORMAL,NOSORT,NOT,NOTHING,NOVALIDATE,NOWAIT,NULL,OF,OFFLINE,' +
    'OID,ON,ONLINE,ONLY,OPERATOR,OPTIMAL,OPTION,OR,ORDER,OUTLINE,OVERFLOW,' +
    'OWN,PACKAGES,PARALLEL,PARAMETERS,PARTITIONS,PASSWORD,' +
    'PASSWORD_GRACE_TIME,PASSWORD_LIFE_TIME,PASSWORD_LOCK_TIME,' +
    'PASSWORD_REUSE_MAX,PASSWORD_REUSE_TIME,PASSWORD_VERIFY_FUNCTION,' +
    'PCTFREE,PCTINCREASE,PCTTHRESHOLD,PCTUSED,PCTVERSION,PERCENT,PERMANENT,' +
    'PLAN,PLI,POST_TRANSACTION,PREBUILD,PRECISION,PRIMARY,PRIOR,PRIVATE_SGA,' +
    'PRIVILEGES,PROFILE,PUBLIC,QUERY,QUOTA,READ,REBUILD,RECORDS_PER_BLOCK,' +
    'RECOVER,RECOVERABLE,RECOVERY,RECYCLE,REDUCED,REFERENCES,REFRESH,RELY,' +
    'RENAME,RESET,RESETLOGS,RESIZE,RESOLVE,RESOLVER,RESOURCE,RESTRICT,' +
    'RESTRICTED,RESUME,REUSE,REVOKE,REWRITE,ROLE,ROLES,ROW,ROWLABEL,' +
    'ROWNUM,ROWS,SAMPLE,SCN,SCOPE,SECTION,SEGMENT,SELECT,SELECTIVITY,' +
    'SEQUENCE,SESSION,SESSIONS_PER_USER,SHARE,SHARED,SHARED_POOL,SHRINK,SIZE,' +
    'SNAPSHOT,SOME,SORT,SOURCE,SPECIFICATION,SPECIFIED,SPLIT,STANDBY,START,' +
    'START_DATE,STATISTICS,STOP,STORAGE,STRUCTURE,SUBPARTITION,SUBPARTITIONS,' +
    'SUCCESSFUL,SUSPEND,SWITCH,SYNONYM,SYSTEM,TABLE,TABLESPACE,TEMPFILE,' +
    'TEMPORARY,THE,THEN,THREAD,THROUGH,TIME,TIMEOUT,TO,TRACING,TRANSACTION,' +
    'TRIGGER,TRUNCATE,TYPES,UNARCHIVED,UNDER,UNIFORM,UNION,UNIQUE,UNLIMITED,' +
    'UNLOCK,UNRECOVERABLE,UNTIL,UNUSABLE,UNUSED,UPDATE,VALIDATE,VALIDATION,' +
    'VALUES,VARGRAPHIC,VARRAY,VIEW,WHERE,WITH,WITHOUT';

  // PLSQL keywords
  OraclePLSQLKW: string =
    'ABORT,ACCEPT,AFTER,ARRAY,ARRAYLEN,ASSERT,ASSIGN,AT,AUTHORIZATION,' +
    'AUTONOMOUS_TRANSACTION,BASE_TABLE,BEGIN,BODY,BULK,BULK_ROWCOUNT,CALL,' +
    'CALLING,CASE,CHAR_BASE,CHARSETFORM,CHARSETID,CLOSE,CLUSTERS,COLAUTH,' +
    'COLLECT,COMMIT,CONSTANT,CRASH,CURRVAL,CURSOR,DATA_BASE,DATABASE,DBA,' +
    'DEBUGOFF,DEBUGON,DECLARE,DEFINITION,DELAY,DELTA,DETERMINISTIC,DIGITS,' +
    'DISPOSE,DO,EACH,ELSIF,END,ENTRY,EXCEPTION,EXCEPTION_INIT,EXIT,EXTERNAL,' +
    'FALSE,FETCH,FIXED,FORALL,FORM,FOUND,FUNCTION,GENERIC,GOTO,IF,INDEXES,' +
    'INDICATOR,INSTEAD,INTERFACE,ISOPEN,LANGUAGE,LIMITED,LOOP,MAXLEN,NAME,NEW,' +
    'NEXTVAL,NOTFOUND,NUMBER_BASE,OLD,OPEN,OUT,PACKAGE,PARALLEL_ENABLE,' +
    'PARTITION,PASCAL,PRAGMA,PRIVATE,PROCEDURE,RAISE,RANGE,RECORD,REF,' +
    'REFERENCING,RELEASE,REMR,RESTRICT_REFERENCES,RETURN,REVERSE,ROLLBACK,' +
    'ROWCOUNT,ROWTYPE,SAVEPOINT,SCHEMA,SELF,SEPARATE,SERIALLY_REUSABLE,SPACE,' +
    'SQL,SQLERROR,STATEMENT,STRUCT,SUBTYPE,TABAUTH,TABLES,TASK,TDO,TERMINATE,' +
    'TRUE,TYPE,USE,VARYING,VIEWS,WHEN,WHILE,WORK,WRITE,XOR';

  // Oracle data types
  OracleTypes: string =
    'BFILE,BINARY_INTEGER,BLOB,BOOLEAN,CHAR,CLOB,DATE,DEC,DECIMAL,DOUBLE,' +
    'FLOAT,INTEGER,LONG,MLSLABEL,NATURAL,NATURALN,NCHAR,NCLOB,NUMBER,NUMERIC,' +
    'NVARCHAR2,PLS_INTEGER,POSITIVE,POSITIVEN,RAW,REAL,ROWID,SMALLINT,UROWID,' +
    'VARCHAR,VARCHAR2';

  // Oracle built in exceptions
  OracleExceptions: string =
    'ACCESS_INTO_NULL,COLLECTION_IS_NULL,CURSOR_ALREADY_OPEN,' +
    'DUP_VAL_ON_INDEX,INVALID_CURSOR,INVALID_NUMBER,LOGIN_DENIED,' +
    'NO_DATA_FOUND,NOT_LOGGED_ON,OTHERS,PROGRAM_ERROR,ROWTYPE_MISMATCH,' +
    'STORAGE_ERROR,SUBSCRIPT_BEYOND_COUNT,SUBSCRIPT_OUTSIDE_LIMIT,' +
    'SYS_INVALID_ROWID,TIMEOUT_ON_RESOURCE,TOO_MANY_ROWS,VALUE_ERROR,' +
    'ZERO_DIVIDE';

  // Oracle built in functions
  OracleFunctions: string =
    'ABS,ACOS,ADD_MONTHS,AGGREGATE,ANALYTIC,ASCII,ASIN,ATAN,ATAN2,AVERAGE,' +
    'AVG,BFILENAME,CEIL,CHARTOROWID,CHR,CONCAT,CONVERSION,CONVERT,CORR,COS,' +
    'COSH,COUNT,COVAR_POP,COVAR_SAMP,CUBE,CUME_DIST,DECODE,DENSE_RANK,DEREF,' +
    'DUMP,EMPTY_BLOB,EMPTY_CLOB,EXP,EXTEND,FIRST,FIRST_VALUE,FLOOR,GLB,' +
    'GREATEST,GREATEST_LB,GROUPING,HEXTORAW,INITCAP,INSTR,INSTRB,LAG,LAST,' +
    'LAST_DAY,LAST_VALUE,LEAD,LEAST,LEAST_LB,LENGTH,LENGTHB,LINEAR,LN,LOG,' +
    'LOWER,LPAD,LTRIM,LUB,MAKE_REF,MAX,MIN,MISCELLANEOUS,MOD,MONTHS_BETWEEN,' +
    'NEW_TIME,NEXT_DAY,NLS_CHARSET_DECL_LEN,NLS_CHARSET_ID,NLS_CHARSET_NAME,' +
    'NLS_INITCAP,NLS_LOWER,NLS_SORT,NLS_UPPER,NLSSORT,NTILE,NUMTODSINTERVAL,' +
    'NUMTOYMINTERVAL,NVL,NVL2,OBJECT,OVER,PERCENT_RANK,POWER,RANK,' +
    'RATION_TO_REPORT,RAWTOHEX,REFERENCE,REFTOHEX,REGR_AVGX,REGR_AVGY,' +
    'REGR_COUNT,REGR_INTERCEPT,REGR_R2,REGR_SLOPE,REGR_SXX,REGR_SXY,' +
    'REGR_SYY,REGRESSION,REPLACE,RETURNING,ROLLUP,ROUND,ROW_NUMBER,' +
    'ROWIDTOCHAR,RPAD,RTRIM,SIGN,SIN,SINH,SOUNDEX,SQLCODE,SQLERRM,SQRT,' +
    'STDDEV,STDDEV_POP,STDDEV_SAMP,STDDEVP,STDDEVS,SUBSTR,SUBSTRB,SUM,' +
    'SYS_CONTEXT,SYS_GUID,SYSDATE,TAN,TANH,TO_CHAR,TO_DATE,TO_LABEL,TO_LOB,' +
    'TO_MULTI_BYTE,TO_NUMBER,TO_SINGLE_BYTE,TRANSLATE,TRIM,TRUNC,UID,UPPER,' +
    'USER,USERENV,USING,VALUE,VAR_POP,VAR_SAMP,VARIANCE,VARP,VARS,VSIZE';

  OracleDefaultPackages: string =
    'DBMS_ALERT,DBMS_APPLICATION_INFO,DBMS_AQ,DBMS_AQ_EXP_HISTORY_TABLES,' +
    'DBMS_AQ_EXP_INDEX_TABLES,DBMS_AQ_EXP_QUEUE_TABLES,DBMS_AQ_EXP_QUEUES,' +
    'DBMS_AQ_EXP_SUBSCRIBER_TABLES,DBMS_AQ_EXP_TIMEMGR_TABLES,' +
    'DBMS_AQ_EXP_ZECURITY,DBMS_AQ_IMP_INTERNAL,DBMS_AQ_IMP_ZECURITY,' +
    'DBMS_AQ_IMPORT_INTERNAL,DBMS_AQ_SYS_EXP_ACTIONS,' +
    'DBMS_AQ_SYS_EXP_INTERNAL,DBMS_AQ_SYS_IMP_INTERNAL,' +
    'DBMS_AQADM,DBMS_AQADM_SYS,DBMS_AQADM_SYSCALLS,DBMS_AQIN,DBMS_AQJMS,' +
    'DBMS_BACKUP_RESTORE,DBMS_DDL,DBMS_DEBUG,DBMS_DEFER_IMPORT_INTERNAL,' +
    'DBMS_DEFER_SYS,DBMS_DESCRIBE,DBMS_DISTRIBUTED_TRUST_ADMIN,' +
    'DBMS_EXPORT_EXTENSION,DBMS_IJOB,DBMS_INTERNAL_TRIGGER,DBMS_IREFRESH,' +
    'DBMS_ISNAPSHOT,DBMS_JAVA_TEST,DBMS_JOB,DBMS_LOB,DBMS_LOCK,DBMS_LOGMNR,' +
    'DBMS_LOGMNR_D,DBMS_OUTPUT,DBMS_PCLXUTIL,DBMS_PICKLER,DBMS_PIPE,' +
    'DBMS_PITR,DBMS_PLUGTS,DBMS_PROFILER,DBMS_PRVTAQIM,DBMS_PRVTAQIP,' +
    'DBMS_PRVTAQIS,DBMS_PRVTRMIE,DBMS_PSP,DBMS_PSWMG_IMPORT,DBMS_RCVMAN,' +
    'DBMS_REFRESH,DBMS_REFRESH_EXP_LWM,DBMS_REFRESH_EXP_SITES,DBMS_REPAIR,' +
    'DBMS_REPCAT_AUTH,DBMS_RESOURCE_MANAGER,DBMS_RESOURCE_MANAGER_PRIVS,' +
    'DBMS_RLS,DBMS_RMGR_GROUP_EXPORT,DBMS_RMGR_PACT_EXPORT,' +
    'DBMS_RMGR_PLAN_EXPORT,DBMS_RMIN,DBMS_ROWID,DBMS_RULE,DBMS_RULE_ADM,' +
    'DBMS_RULE_EXIMP,DBMS_SESSION,DBMS_SNAP_INTERNAL,DBMS_SNAP_REPAPI,' +
    'DBMS_SNAPSHOT,DBMS_SNAPSHOT_UTL,DBMS_SPACE,DBMS_SPACE_ADMIN,DBMS_SQL,' +
    'DBMS_STANDARD,DBMS_STATS,DBMS_SUMADV,DBMS_SUMMARY,DBMS_SUMREF_CHILD,' +
    'DBMS_SUMREF_PARENT,DBMS_SUMREF_UTIL,DBMS_SUMREF_UTIL2,DBMS_SUMVDM,' +
    'DBMS_SYS_ERROR,DBMS_SYS_SQL,DBMS_SYSTEM,DBMS_TRACE,DBMS_TRANSACTION,' +
    'DBMS_TTS,DBMS_UTILITY,DBMS_ZHELP,DBMS_ZHELP_IR,DBMSZEXP_SYSPKGGRNT,' +
    'DIANA,DIUTIL,ODCICONST,OUTLN_PKG,PBREAK,PBRPH,PBSDE,PBUTL,PIDL,PLITBLM,' +
    'STANDARD,SYS_STUB_FOR_PURITY_ANALYSIS,UTL_COLL,UTL_FILE,UTL_HTTP,' +
    'UTL_INADDR,UTL_RAW,UTL_REF,UTL_SMTP,UTL_TCP';

  OracleSQLPlusCommands: string =
   'ARCHIVE,ATTRIBUTE,BREAK,BRE,BTITLE,BTI,CL,CLEAR,COL,COMP,' +
   'CONN,COPY,DEFINE,DEF,DESCRIBE,DESCRIB,DESCRI,DESCR,DESC,DISCONNECT,' +
   'DISCONNEC,DISCONNE,DISCONN,DISCON,DISCO,DISC,EXEC,EXECUTE,FAILURE,' +
   'HELP,HO,HOST,OFF,OSERROR,PAU,PAUSE,PASSW,PPRINT,PRI,PROMPT,' +
   'REPFOOTER,REPF,REPHEADER,REPH,RUN,SET,SHOW,SHO,SHUTDOWN,SPOOL,STA,' +
   'STARTUP,SUCCESS,TIMING,TIMI,TTITLE,TTI,UNDEFINE,UNDEF,VARIABLE,VAR,' +
   'WHENEVER';

  OracleCommentKW: string =
   'REM,REMA,REMAR,REMARK';                                                     // JJV 2001-05-23
{end}                                                                           // DJLP 2000-09-05
//---MS-SQL 7-------------------------------------------------------------------

  // keywords
  MSSQL7KW: string =
    'ABSOLUTE,ADD,ALL,ALTER,ANY,AS,ASC,AUTHORIZATION,AVG,BACKUP,BEGIN,' +
    'BETWEEN,BREAK,BROWSE,BULK,BY,CASCADE,CHECK,CHECKPOINT,CLOSE,CLUSTERED,' +
    'COLUMN,COMMIT,COMMITTED,COMPUTE,CONFIRM,CONSTRAINT,CONTAINS,' +
    'CONTAINSTABLE,CONTINUE,CONTROLROW,COUNT,CREATE,CROSS,CURRENT,' +
    'CURRENT_DATE,CURRENT_TIME,CURSOR,DATABASE,DBCC,DEALLOCATE,DECLARE,' +
    'DEFAULT,DELETE,DENY,DESC,DISK,DISTINCT,DISTRIBUTED,DOUBLE,DROP,DUMMY,' +
    'DUMP,ELSE,END,ERRLVL,ERROREXIT,ESCAPE,EXCEPT,EXEC,EXECUTE,EXISTS,EXIT,' +
    'FETCH,FILE,FILLFACTOR,FIRST,FLOPPY,FOR,FOREIGN,FREETEXT,FREETEXTTABLE,' +
    'FROM,FULL,GLOBAL,GOTO,GRANT,GROUP,HAVING,HOLDLOCK,IDENTITY,IDENTITYCOL,' +
    'IDENTITY_INSERT,IF,IN,INDEX,INNER,INSERT,INTERSECT,INTO,IS,ISOLATION,' +
    'JOIN,KEY,KILL,LAST,LEFT,LEVEL,LIKE,LINENO,LOAD,MAX,MIN,MIRROREXIT,' +
    'NATIONAL,NEXT,NOCHECK,NONCLUSTERED,NOT,NULL,OF,OFF,OFFSETS,ON,ONCE,' +
    'ONLY,OPEN,OPENDATASOURCE,OPENQUERY,OPENROWSET,OPTION,OR,ORDER,OUTER,' +
    'OVER,PERCENT,PERM,PERMANENT,PIPE,PLAN,PRECISION,PREPARE,PRIMARY,PRINT,' +
    'PRIOR,PRIVILEGES,PROC,PROCEDURE,PROCESSEXIT,PUBLIC,RAISERROR,READ,' +
    'READTEXT,RECONFIGURE,REFERENCES,RELATIVE,REPEATABLE,REPLICATION,RESTORE,' +
    'RESTRICT,RETURN,REVOKE,RIGHT,ROLLBACK,ROWCOUNT,ROWGUIDCOL,RULE,SAVE,' +
    'SCHEMA,SELECT,SERIALIZABLE,SET,SETUSER,SHUTDOWN,SOME,STATISTICS,SUM,' +
    'TABLE,TAPE,TEMP,TEMPORARY,TEXTSIZE,THEN,TO,TOP,TRAN,TRANSACTION,TRIGGER,' +
    'TRUNCATE,TSEQUAL,UNCOMMITTED,UNION,UNIQUE,UPDATE,UPDATETEXT,USE,USER,' +
    'VALUES,VARYING,VIEW,WAITFOR,WHEN,WHERE,WHILE,WITH,WORK,WRITETEXT';

  // functions
  MSSQL7Functions: string =
    '@@CONNECTIONS,@@CPU_BUSY,@@CURSOR_ROWS,@@DATEFIRST,@@DBTS,@@ERROR,' +
    '@@FETCH_STATUS,@@IDENTITY,@@IDLE,@@IO_BUSY,@@LANGID,@@LANGUAGE,' +
    '@@LOCK_TIMEOUT,@@MAX_CONNECTIONS,@@MAX_PRECISION,@@NESTLEVEL,@@OPTIONS,' +
    '@@PACKET_ERRORS,@@PACK_RECEIVED,@@PACK_SENT,@@PROCID,@@REMSERVER,' +
    '@@ROWCOUNT,@@SERVERNAME,@@SERVICENAME,@@SPID,@@TEXTSIZE,@@TIMETICKS,' +
    '@@TOTAL_ERRORS,@@TOTAL_READ,@@TOTAL_WRITE,@@TRANCOUNT,@@VERSION,ABS,' +
    'ACOS,AND,APP_NAME,ASCII,ASIN,ATAN,ATN2,CASE,CAST,CEILING,CHARINDEX,' +
    'COALESCE,COLUMNPROPERTY,COL_LENGTH,COL_NAME,CONVERT,COS,COT,' +
    'CURRENT_TIMESTAMP,CURRENT_USER,CURSOR_STATUS,DATABASEPROPERTY,' +
    'DATALENGTH,DATEADD,DATEDIFF,DATENAME,DATEPART,DAY,DB_ID,DB_NAME,' +
    'DEGREES,DIFFERENCE,EXP,FILEGROUPPROPERTY,FILEGROUP_ID,FILEGROUP_NAME,' +
    'FILEPROPERTY,FILE_ID,FILE_NAME,FLOOR,FORMATMESSAGE,' +
    'FULLTEXTCATALOGPROPERTY,FULLTEXTSERVICEPROPERTY,GETANSINULL,GETDATE,' +
    'HOST_ID,HOST_NAME,IDENT_INCR,IDENT_SEED,INDEXPROPERTY,INDEX_COL,' +
    'ISDATE,ISNULL,ISNUMERIC,IS_MEMBER,IS_SRVROLEMEMBER,LEN,LOG,LOG10,LOWER,' +
    'LTRIM,MONTH,NEWID,NULLIF,OBJECTPROPERTY,OBJECT_ID,OBJECT_NAME,PARSENAME,' +
    'PATINDEX,PERMISSIONS,PI,POWER,QUOTENAME,RADIANS,RAND,REPLACE,REPLICATE,' +
    'REVERSE,ROUND,RTRIM,SESSION_USER,SIGN,SIN,SOUNDEX,SPACE,SQRT,SQUARE,' +
    'STATS_DATE,STR,STUFF,SUBSTRING,SUSER_ID,SUSER_NAME,SUSER_SID,' +
    'SUSER_SNAME,SYSTEM_USER,TAN,TEXTPTR,TEXTVALID,TYPEPROPERTY,UNICODE,' +
    'UPPER,USER_ID,USER_NAME,YEAR';

  // types
  MSSQL7Types: string =
    'BINARY,BIT,CHAR,DATETIME,DECIMAL,FLOAT,IMAGE,INT,MONEY,NCHAR,NTEXT,' +
    'NUMERIC,NVARCHAR,REAL,SMALLDATETIME,SMALLINT,SMALLMONEY,SYSNAME,TEXT,' +
    'TIMESTAMP,TINYINT,UNIQUEIDENTIFIER,VARBINARY,VARCHAR';

//---MS-SQL2K-------------------------------------------------------------------

  // keywords
  MSSQL2000KW =
    'ADD,ALL,ALTER,AND,ANY,AS,ASC,AUTHORIZATION,BACKUP,' +
    'BEGIN,BETWEEN,BREAK,BROWSE,BULK,BY,CASCADE,CASE,' +
    'CHECK,CHECKPOINT,CLOSE,CLUSTERED,COLLATE,' +
    'COLUMN,COMMIT,COMPUTE,CONSTRAINT,CONTAINS,CONTAINSTABLE,' +
    'CONTINUE,CREATE,CROSS,CURRENT,CURSOR,DATABASE,' +
    'DBCC,DEALLOCATE,DECLARE,DEFAULT,DELETE,DENY,DESC,DISK,' +
    'DISTINCT,DISTRIBUTED,DOUBLE,DROP,DUMMY,DUMP,ELSE,END,' +
    'ERRLVL,ESCAPE,EXCEPT,EXEC,EXECUTE,EXISTS,EXIT,FETCH,FILE,' +
    'FILLFACTOR,FOR,FOREIGN,FORMSOF,FREETEXT,FREETEXTTABLE,FROM,FULL,' +
    'FUNCTION,GOTO,GRANT,GROUP,HAVING,HOLDLOCK,IDENTITY,' +
    'IDENTITYCOL,IDENTITY_INSERT,IF,IN,INFLECTIONAL,INDEX,INNER,INSERT,'+
    'INTERSECT,INTO,IS,ISABOUT,JOIN,KEY,KILL,LEFT,LIKE,LINENO,LOAD,'+
    'NATIONAL,NOCHECK,NONCLUSTERED,NOT,NULL,NULLIF,OF,OFF,'+
    'OFFSETS,ON,OPEN,OPENDATASOURCE,OPENQUERY,OPENROWSET,OPENXML,'+
    'OPTION,OR,ORDER,OUTER,OVER,PERCENT,PLAN,PRECISION,' +
    'PRIMARY,PRINT,PROC,PROCEDURE,PUBLIC,RAISERROR,READ,' +
    'READTEXT,RECONFIGURE,REFERENCES,REPLICATION,RESTORE,' +
    'RESTRICT,RETURN,REVOKE,RIGHT,ROLLBACK,ROWCOUNT,ROWGUIDCOL,' +
    'RULE,SAVE,SCHEMA,SELECT,SESSION_USER,SET,SETUSER,SHUTDOWN,' +
    'SOME,STATISTICS,TABLE,TEXTSIZE,THEN,TO,TOP,TRAN,TRANSACTION,' +
    'TRIGGER,TRUNCATE,TSEQUAL,UNION,UNIQUE,UPDATE,UPDATETEXT,' +
    'USE,USER,VALUES,VARYING,VIEW,WAITFOR,WEIGHT,WHEN,WHERE,WHILE,' +
    'WITH,WRITETEXT';

  // functions
  MSSQL2000Functions =
    '@@CONNECTIONS,@@CPU_BUSY,@@CURSOR_ROWS,@@DATEFIRST,@@DBTS,@@ERROR,' +
    '@@FETCH_STATUS,@@IDENTITY,@@IDLE,@@IO_BUSY,@@LANGID,@@LANGUAGE,' +
    '@@LOCK_TIMEOUT,@@MAX_CONNECTIONS,@@MAX_PRECISION,@@NESTLEVEL,@@OPTIONS,' +
    '@@PACKET_ERRORS,@@PACK_RECEIVED,@@PACK_SENT,@@PROCID,@@REMSERVER,' +
    '@@ROWCOUNT,@@SERVERNAME,@@SERVICENAME,@@SPID,@@TEXTSIZE,@@TIMETICKS,' +
    '@@TOTAL_ERRORS,@@TOTAL_READ,@@TOTAL_WRITE,@@TRANCOUNT,@@VERSION,' +
    'ABS,ACOS,APP_NAME,ASCII,ASIN,ATAN,ATN2,AVG,BINARY_CHECKSUM,CAST,' +
    'CEILING,CHARINDEX,CHECKSUM,CHECKSUM_AGG,COALESCE,COLLATIONPROPERTY,' +
    'COLUMNPROPERTY,COL_LENGTH,COL_NAME,CONVERT,COS,COT,COUNT,' +
    'COUNT_BIG,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,' +
    'CURRENT_USER,CURSOR_STATUS,DATABASEPROPERTY,DATABASEPROPERTYEX,' +
    'DATALENGTH,DATEADD,DATEDIFF,DATENAME,DATEPART,DAY,DB_ID,DB_NAME,DEGREES,' +
    'DIFFERENCE,EXP,FILEGROUPPROPERTY,FILEGROUP_ID,FILEGROUP_NAME,FILEPROPERTY,' +
    'FILE_ID,FILE_NAME,FLOOR,fn_helpcollations,fn_listextendedproperty,' +
    'fn_servershareddrives,fn_trace_geteventinfo,fn_trace_getfilterinfo,' +
    'fn_trace_getinfo,fn_trace_gettable,fn_virtualfilestats,' +
    'FORMATMESSAGE,FULLTEXTCATALOGPROPERTY,FULLTEXTSERVICEPROPERTY,' +
    'GETANSINULL,GETDATE,GETUTCDATE,GROUPING,HAS_DBACCESS,HOST_ID,HOST_NAME,' +
    'IDENT_CURRENT,IDENT_INCR,IDENT_SEED,INDEXKEY_PROPERTY,INDEXPROPERTY,' +
    'INDEX_COL,ISDATE,ISNULL,ISNUMERIC,IS_MEMBER,IS_SRVROLEMEMBER,LEN,LOG,' +
    'LOG10,LOWER,LTRIM,MAX,MIN,MONTH,NEWID,OBJECTPROPERTY,OBJECT_ID,' +
    'OBJECT_NAME,PARSENAME,PATINDEX,' +
    'PERMISSIONS,PI,POWER,QUOTENAME,RADIANS,RAND,REPLACE,REPLICATE,REVERSE,' +
    'ROUND,ROWCOUNT_BIG,RTRIM,SCOPE_IDENTITY,SERVERPROPERTY,SESSIONPROPERTY,' +
    'SIGN,SIN,SOUNDEX,SPACE,SQL_VARIANT_PROPERTY,SQRT,SQUARE,' +
    'STATS_DATE,STDEV,STDEVP,STR,STUFF,SUBSTRING,SUM,SUSER_SID,SUSER_SNAME,' +
    'SYSTEM_USER,TAN,TEXTPTR,TEXTVALID,TYPEPROPERTY,UNICODE,UPPER,' +
    'USER_ID,USER_NAME,VAR,VARP,YEAR';

  // types
  MSSQL2000Types =
    'bigint,binary,bit,char,character,datetime,' +
    'dec,decimal,float,image,int,' +
    'integer,money,nchar,ntext,nvarchar,real,' +
    'rowversion,smalldatetime,smallint,smallmoney,' +
    'sql_variant,sysname,text,timestamp,tinyint,uniqueidentifier,' +
    'varbinary,varchar';

//---Interbase 6----------------------------------------------------------------

  // functions
  Interbase6Functions = 'AVG,CAST,COUNT,GEN_ID,MAX,MIN,SUM,UPPER';

  // keywords
  Interbase6KW: string = 'ACTIVE,ADD,AFTER,ALL,ALTER,AND,ANY,AS,ASC,' +
    'ASCENDING,AT,AUTO,AUTODDL,BASED,BASENAME,BASE_NAME,BEFORE,BEGIN,BETWEEN,' +
    'BLOBEDIT,BUFFER,BY,CACHE,CHARACTER_LENGTH,CHAR_LENGTH,CHECK,' +
    'CHECK_POINT_LEN,CHECK_POINT_LENGTH,COLLATE,COLLATION,COLUMN,COMMIT,' +
    'COMMITED,COMPILETIME,COMPUTED,CLOSE,CONDITIONAL,CONNECT,CONSTRAINT,' +
    'CONTAINING,CONTINUE,CREATE,CURRENT,CURRENT_DATE,CURRENT_TIME,' +
    'CURRENT_TIMESTAMP,CURSOR,DATABASE,DAY,DB_KEY,DEBUG,DEC,DECLARE,DEFAULT,' +
    'DELETE,DESC,DESCENDING,DESCRIBE,DESCRIPTOR,DISCONNECT,DISTINCT,DO,' +
    'DOMAIN,DROP,ECHO,EDIT,ELSE,END,ENTRY_POINT,ESCAPE,EVENT,EXCEPTION,' +
    'EXECUTE,EXISTS,EXIT,EXTERN,EXTERNAL,EXTRACT,FETCH,FILE,FILTER,FOR,' +
    'FOREIGN,FOUND,FROM,FULL,FUNCTION,GDSCODE,GENERATOR,GLOBAL,GOTO,GRANT,' +
    'GROUP,GROUP_COMMIT_WAIT,GROUP_COMMIT_WAIT_TIME,HAVING,HELP,HOUR,IF,' +
    'IMMEDIATE,IN,INACTIVE,INDEX,INDICATOR,INIT,INNER,INPUT,INPUT_TYPE,' +
    'INSERT,INT,INTO,IS,ISOLATION,ISQL,JOIN,KEY,LC_MESSAGES,LC_TYPE,LEFT,' +
    'LENGTH,LEV,LEVEL,LIKE,LOGFILE,LOG_BUFFER_SIZE,LOG_BUF_SIZE,LONG,MANUAL,' +
    'MAXIMUM,MAXIMUM_SEGMENT,MAX_SEGMENT,MERGE,MESSAGE,MINIMUM,MINUTE,' +
    'MODULE_NAME,MONTH,NAMES,NATIONAL,NATURAL,NCHAR,NO,NOAUTO,NOT,NULL,' +
    'NUM_LOG_BUFFS,NUM_LOG_BUFFERS,OCTET_LENGTH,OF,ON,ONLY,OPEN,OPTION,OR,' +
    'ORDER,OUTER,OUTPUT,OUTPUT_TYPE,OVERFLOW,PAGE,PAGELENGTH,PAGES,PAGE_SIZE,' +
    'PARAMETER,PASSWORD,PLAN,POSITION,POST_EVENT,PRECISION,PREPARE,PROCEDURE,' +
    'PROTECTED,PRIMARY,PRIVILEGES,PUBLIC,QUIT,RAW_PARTITIONS,READ,REAL,' +
    'RECORD_VERSION,REFERENCES,RELEASE,RESERV,RESERVING,RETAIN,RETURN,' +
    'RETURNING_VALUES,RETURNS,REVOKE,RIGHT,ROLLBACK,RUNTIME,SCHEMA,SECOND,' +
    'SEGMENT,SELECT,SET,SHADOW,SHARED,SHELL,SHOW,SINGULAR,SIZE,SNAPSHOT,SOME,' +
    'SORT,SQL,SQLCODE,SQLERROR,SQLWARNING,STABILITY,STARTING,STARTS,' +
    'STATEMENT,STATIC,STATISTICS,SUB_TYPE,SUSPEND,TABLE,TERMINATOR,THEN,TO,' +
    'TRANSACTION,TRANSLATE,TRANSLATION,TRIGGER,TRIM,TYPE,UNCOMMITTED,UNION,' +
    'UNIQUE,UPDATE,USER,USING,VALUE,VALUES,VARIABLE,VARYING,VERSION,VIEW,' +
    'WAIT,WEEKDAY,WHEN,WHENEVER,WHERE,WHILE,WITH,WORK,WRITE,YEAR,YEARDAY';

  // types
  Interbase6Types = 'BLOB,CHAR,CHARACTER,DATE,DECIMAL,DOUBLE,FLOAT,INTEGER,' +
    'NUMERIC,SMALLINT,TIME,TIMESTAMP,VARCHAR';

//---MySQL----------------------------------------------------------------------

  // keywords
  MySqlKW: string = 'ACTION,AFTER,AGGREGATE,ALL,ALTER,AS,ASC,'+
    'AUTO_INCREMENT,AVG_ROW_LENGTH,BETWEEN,BINARY,BIT,BOOL,BOTH,BY,'+
    'CASCADE,CHANGE,CHARACTER,CHECK,CHECKSUM,COLUMN,COLUMNS,COMMENT,'+
    'CONSTRAINT,CREATE,CROSS,DATA,DATABASES,DEC,DEFAULT,DELAYED,'+
    'DELAY_KEY_WRITE,DELETE,DESC,DESCRIBE,DISTINCT,DISTINCTROW,DROP,'+
    'ELSE,ENCLOSED,END,ESCAPE,ESCAPED,EXISTS,EXPLAIN,FIELDS,FIRST,'+
    'FLOAT4,FLOAT8,FLUSH,FOR,FOREIGN,FROM,FULL,FUNCTION,GLOBAL,GRANT,'+
    'GRANTS,GROUP,HAVING,HEAP,HIGH_PRIORITY,HOSTS,IDENTIFIED,IGNORE,'+
    'INFILE,INNER,INT1,INT2,INT3,INT4,INT8,INTO,IS,ISAM,JOIN,KEY,'+
    'KEYS,KILL,LEADING,LIKE,LIMIT,LINES,LOAD,LOCAL,LOGS,LONG,'+
    'LOW_PRIORITY,MATCH,MAX_ROWS,MIDDLEINT,MIN_ROWS,MODIFY,MYISAM,'+
    'NATURAL,NO,NOT,OPTIMIZE,OPTION,OPTIONALLY,ORDER,OUTER,OUTFILE,'+
    'PACK_KEYS,PARTIAL,PRIMARY,PRIVILEGES,PROCEDURE,PROCESS,PROCESSLIST,'+
    'READ,REFERENCES,REGEXP,RELOAD,RENAME,RESTRICT,RETURNS,REVOKE,RLIKE,'+
    'ROW,ROWS,SELECT,SHOW,SHUTDOWN,SONAME,SQL_BIG_RESULT,SQL_BIG_SELECTS,'+
    'SQL_BIG_TABLES,SQL_LOG_OFF,SQL_LOG_UPDATE,SQL_LOW_PRIORITY_UPDATES,'+
    'SQL_SELECT_LIMIT,SQL_SMALL_RESULT,SQL_WARNINGS,STARTING,STATUS,'+
    'STRAIGHT_JOIN,TABLE,TABLES,TEMPORARY,TERMINATED,THEN,TO,TRAILING,'+
    'TYPE,UNIQUE,UNLOCK,UNSIGNED,UPDATE,USAGE,USE,USING,VALUES,'+
    'VARBINARY,VARCHAR,VARIABLES,VARYING,WHERE,WITH,WRITE,ZEROFILL';

  // types
  MySqlTypes: string = 'TINYINT,SMALLINT,MEDIUMINT,INT,INTEGER,BIGINT,FLOAT,' +
    'DOUBLE,REAL,DECIMAL,NUMERIC,DATE,DATETIME,TIMESTAMP,TIME,YEAR,CHAR,' +
    'NATIONAL,TINYBLOB,TINYTEXT,TEXT,MEDIUMBLOB,MEDIUMTEXT,LONGBLOB,' +
    'LONGTEXT,ENUM,SET';

  // functions
  MySqlFunctions: string = 'ABS,ACOS,ACSII,ADDDATE,ASIN,ATAN,ATAN2,AVG,' +
    'BENCHMARK,BIN,BIT_AND,BIT_COUNT,BIT_OR,CASE,CEILING,' +
    'CHARACTOR_LENGTH,CHAR_LENGTH,COALESCE,CONCAT,CONV,COS,COT,COUNT,' +
    'CURDATE,CURRENT_DATE,CURRENT_TIME,CURRENT_TIMESTAMP,CURTIME,DATABASE,' +
    'DATE_ADD,DATE_FORMAT,DATE_SUB,DAY,DAYNAME,DAYOFMONTH,DAYOFWEEK,' +
    'DAYOFYEAR,DAY_HOUR,DAY_MINUTE,DAY_SECOND,DECODE,DEGREES,ELT,ENCODE,' +
    'ENCRYPT,EXP,EXPORT_SET,FIELD,FIND_IN_SET,FLOOR,FORMAT,FROM_DAYS,' +
    'FROM_UNIXTIME,GET_LOCK,GREATEST,HEX,HOUR,HOUR_MINUTE,HOUR_SECOND,IF,' +
    'IFNULL,IN,INSERT,INSTR,INTERVAL,ISNULL,LAST_INSERT_ID,LCASE,LEAST,LEFT,' +
    'LENGTH,LOAD_FILE,LOCATE,LOG,LOG10,LOWER,LPAD,LTRIM,MAKE_SET,MAX,MD5,MID,' +
    'MIN,MINUTE,MINUTE_SECOND,MOD,MONTH,MONTHNAME,NOW,OCT,OCTET_LENGTH,ORD,' +
    'PASSWORD,PERIOD_ADD,PERIOD_DIFF,PI,POSITION,POW,POWER,QUARTER,RADIANS,' +
    'RAND,RELEASE_LOCK,REPEAT,REPLACE,REVERSE,RIGHT,ROUND,RPAD,RTRIM,SECOND,' +
    'SEC_TO_TIME,SESSION_USER,SIGN,SIN,SOUNDEX,SPACE,SQRT,STD,STDDEV,STRCMP,' +
    'SUBDATE,SUBSTRING,SUBSTRING_INDEX,SUM,SYSDATE,SYSTEM_USER,TAN,' +
    'TIME_FORMAT,TIME_TO_SEC,TO_DAYS,TRIM,TRUNCATE,UCASE,UNIX_TIMESTAMP,' +
    'UPPER,USER,VERSION,WEEK,WEEKDAY,WHEN,YEARWEEK,YEAR_MONTH';

{begin}                                                                         // JJV 2000-11-16
//---Ingres---------------------------------------------------------------------

  // keywords
  IngresKW: string =
    'ABORT,ACTIVATE,ADD,ADDFORM,AFTER,AGGREGATE,ALL,ALTER,AND,APPEND,ARRAY,' +
    'AS,ASC,AT,AUDIT_LOG,AUTHORIZATION,AUTOCOMMIT,AVGU,BEFORE,BEGIN,BETWEEN,' +
    'BREAKDISPLAY,BY,BYREF,CACHE,CALL,CALLFRAME,CALLPROC,CASCADE,CHECK,CLEAR,' +
    'CLEARROW,CLOSE,COLUMN,COMMAND,COMMENT,COMMIT,CONNECT,CONSTRAINT,' +
    'CONTINUE,COPY,COUNTU,CPUFACTOR,CREATE,CURRENT,CURRENT_USER,CURSOR,DATA,' +
    'DATAHANDLER,DATE_FORMAT,DBEVENT,DDL_CONCURRENCY,DEADLOCK,DECLARE,' +
    'DEFAULT,DEFERRED,DEFINE,DELETE,DELETEROW,DESC,DESCRIBE,DESCRIPTOR,' +
    'DESTROY,DIRECT,DISABLE,DISCONNECT,DISPLAY,DISTINCT,DISTRIBUTE,DO,DOWN,' +
    'DROP,ELSE,ELSEIF,ENABLE,END,ENDDATA,ENDDISPLAY,ENDFORMS,ENDIF,ENDLOOP,' +
    'ENDRETRIEVE,ENDSELECT,ENDWHILE,ERROR,ESCAPE,EXCLUDE,EXCLUDING,EXEC,' +
    'EXECUTE,EXISTS,EXIT,FETCH,FIELD,FINALIZE,FOR,FOREIGN,FORMDATA,FORMINIT,' +
    'FORMS,FROM,FULL,GET,GETFORM,GETOPER,GETROW,GLOBAL,GOTO,GRANT,GRANTED,' +
    'HAVING,HELP,HELP_FORMS,HELP_FRS,HELPFILE,IDENTIFIED,IF,IIMESSAGE,' +
    'IIPRINTF,IIPROMPT,IISTATEMENT,IMMEDIATE,IMPORT,IN,INCLUDE,INDEX,' +
    'INDICATOR,INGRES,INITIALIZE,INITTABLE,INNER,INQUIRE_EQUEL,INQUIRE_FORMS,' +
    'INQUIRE_FRS,INQUIRE_INGRES,INSERT,INSERTROW,INSTALLATION,INTEGRITY,INTO,' +
    'IO_TRACE,IS,J_FREESZ1,J_FREESZ2,J_FREESZ3,J_FREESZ4,J_SORTBUFSZ,' +
    'JCPUFACTOR,JOIN,JOINOP,JOURNALING,KEY,LEVEL,LIKE,LINK,LOADTABLE,LOCAL,' +
    'LOCATION,LOCK_TRACE,LOG_TRACE,LOGDBEVENTS,LOGGING,MAXCOST,MAXCPU,' +
    'MAXPAGE,MENUITEM,MESSAGE,MODE,MODIFY,MODULE,MONEY_FORMAT,MONEY_PREC,' +
    'MOVE,NATURAL,NEXT,NODEADLOCK,NOECHO,NOIO_TRACE,NOJIONOP,NOJOURNALING,' +
    'NOLOCK_TRACE,NOLOG_TRACE,NOLOGDBEVENTS,NOLOGGING,NOMAXCOST,NOMAXCPU,' +
    'NOMAXIO,NOMAXPAGE,NOMAXQUERY,NOMAXROW,NOOPTIMIZEONLY,NOPRINTDBEVENTS,' +
    'NOPRINTQRY,NOPRINTRULES,NOQEP,NORULES,NOSQL,NOSTATISTICS,NOT,NOTRACE,' +
    'NULL,OF,ON,ONLY,OPEN,OPTIMIZEONLY,OPTION,OR,ORDER,OUT,PARAM,PERMIT,' +
    'PREPARE,PRESERVE,PRIMARY,PRINT,PRINTDBEVENTS,PRINTQRY,PRINTSCREEN,' +
    'PRIVILEGES,PROCEDURE,PROMPT,PUBLIC,PUT,PUTFORM,PUTOPER,PUTROW,QBUFSIZE,' +
    'QEP,QRY,QUALIFICATION,QUERY_SIZE,RAISE,RANGE,READONLY,REDISPLAY,' +
    'REFERENCES,REFERENCING,REGISTER,RELOCATE,REMOVE,RENAME,REPEAT,REPEATED,' +
    'REPLACE,REPLICATE,RESTRICT,RESULT_STRUCTURE,RESUME,RET_INTO,RETRIEVE,' +
    'RETURN,RETURNING,REVOKE,ROLLBACK,ROWS,RULE,RUN,SAVE,SAVEPOINT,SCHEMA,' +
    'SCREEN,SCROLL,SCROLLDOWN,SCROLLUP,SECTION,SECURITY_ALARM,SECURITY_AUDIT,' +
    'SELECT,SESSION,SET,SET_4GL,SET_EQUAL,SET_FORMS,SET_FRS,SET_INGRES,' +
    'SET_SQL,SHORT_REMARK,SLEEP,SOME,SORT,SORTBUFSIZE,SQL,STATISTICS,STOP,' +
    'SUBMENU,SUMU,SYNONYM,SYSTEM,TABLE,TABLEDATA,TEWMPORARY,THEN,TO,TRACE,' +
    'TRANSACTION,TYPE,UNION,UNIQUE,UNLOADTABLE,UNTIL,UP,UPDATE,USER,USING,' +
    'VALIDATE,VALIDROW,VALUES,VIEW,WHEN,WHENEVER,WHERE,WHILE,WITH,WORK';

  // types
  IngresTypes: string =
    'BYTE,C,CHAR,CHARACTER,DATE,DECIMAL,FLOAT,FLOAT4,FLOAT8,INTEGER,INTEGER1,' +
    'INTEGER2,INTEGER4,LONG,MONEY,OBJECT_KEY,SECURITY_LABEL,SHORT,SMALLINT,' +
    'TABLE_KEY,TEXT,VARCHAR,VARYING';

  // functions
  IngresFunctions: string =
    '_BINTIM,_CPU_MS,_DATE,_DIO_CNT,_ET_SEC,_PFAULT_CNT,_TIME,_VERSION,ABS,' +
    'ANY,ATAN,AUTOCOMMIT_STATE,AVG,BIOCNT,CHAREXTRACT,COLLATION,CONCAT,' +
    'CONNECT_TIME_LIMIT,COS,COUNT,CREATE_PROCEDURE,CREATE_TABLE,DATABASE,' +
    'DATE_GMT,DATE_PART,DATE_TRUNC,DB_ADMIN,DB_DELIMITED_CASE,DB_NAME_CASE,' +
    'DBA,DBMS_BIO,DBMS_CPU,DBMS_DIO,DBMSINFO,DOW,EXP,FLATTEN_AGGREGATE,' +
    'FLATTEN_NONE,FLATTEN_OPTIMIZE,FLATTEN_SINGLETON,GROUP,HEX,' +
    'IDLE_TIME_LIMIT,IFNULL,INITIAL_USER,INQUIRE_SQL,INT1,INT2,INT4,INTERVAL,' +
    'LANGUAGE,LEFT,LENGTH,LOCATE,LOCKMODE,LOG,LONG_BYTE,LONG_VARCHAR,' +
    'LOWERCASE,MAX,MAXCONNECT,MAXIDLE,MAXIO,MAXQUERY,MAXROW,MIN,MOD,NOTRIM,' +
    'ON_ERROR_STATE,PAD,QUERY_IO_LIMIT,QUERY_LANGUAGE,QUERY_ROW_LIMIT,RIGHT,' +
    'ROLE,SECURITY_AUDIT_LOG,SECURITY_AUDIT_STATE,SECURITY_PRIV,' +
    'SELECT_SYSCAT,SERVER_CLASS,SESSION_ID,SESSION_PRIORITY,' +
    'SESSION_PRIORITY_LIMIT,SESSION_PRIV,SESSION_SECLABEL,SESSION_USER,SHIFT,' +
    'SIN,SIZE,SQRT,SQUEEZE,SUM,SYSTEM_USER,TABLE_STATISTICS,TERMINAL,' +
    'TRANSACTION_STATE,TRIM,UPDATE_ROWCNT,UPDATE_SYSCAT,UPPERCASE,USERNAME,' +
    'VARBYTE';
{end}                                                                           // JJV 2000-11-16

procedure MakeIdentTable;
var
  c: char;
begin
  FillChar(Identifiers, SizeOf(Identifiers), 0);
  for c := 'a' to 'z' do
    Identifiers[c] := TRUE;
  for c := 'A' to 'Z' do
    Identifiers[c] := TRUE;
  for c := '0' to '9' do
    Identifiers[c] := TRUE;
  Identifiers['_'] := TRUE;
  Identifiers['#'] := TRUE;                                                     // DJLP 2000-09-05
  Identifiers['$'] := TRUE;                                                     // DJLP 2000-09-05

  FillChar(mHashTable, SizeOf(mHashTable), 0);
  mHashTable['_'] := 1;
  for c := 'a' to 'z' do
    mHashTable[c] := 2 + Ord(c) - Ord('a');
  for c := 'A' to 'Z' do
    mHashTable[c] := 2 + Ord(c) - Ord('A');

  Move(Identifiers, IdentifiersMSSQL7, SizeOf(Identifiers));
  Move(mHashTable, mHashTableMSSQL7, SizeOf(mHashTable));
  IdentifiersMSSQL7['@'] := TRUE;
  mHashTableMSSQL7['@'] := mHashTableMSSQL7['Z'] + 1;
end;

function TSynSQLSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while fIdentifiersPtr[ToHash^] do begin
{$IFOPT Q-}
    Result := 2 * Result + fmHashTablePtr[ToHash^];
{$ELSE}
    Result := (2 * Result + fmHashTablePtr[ToHash^]) and $FFFFFF;
{$ENDIF}
    inc(ToHash);
  end;
  Result := Result and $FF; // 255
  fStringLen := ToHash - fToIdent;
end;

function TSynSQLSyn.KeyComp(const aKey: string): Boolean;
var
  i: integer;
  pKey1, pKey2: PChar;
begin
  pKey1 := fToIdent;
  // Note: fStringLen is always > 0 !
  pKey2 := pointer(aKey);
  for i := 1 to fStringLen do
  begin
    if mHashTable[pKey1^] <> mHashTable[pKey2^] then
    begin
      Result := FALSE;
      exit;
    end;
    Inc(pKey1);
    Inc(pKey2);
  end;
  Result := TRUE;
end;

function TSynSQLSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[KeyHash(MayBe)];
  while Assigned(Entry) do begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if KeyComp(Entry.Keyword) then begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

procedure TSynSQLSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
       #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      #39: fProcTable[I] := AsciiCharProc;
      '=': fProcTable[I] := EqualProc;
      '>': fProcTable[I] := GreaterProc;
      '<': fProcTable[I] := LowerProc;
      '-': fProcTable[I] := MinusProc;
      '|': fProcTable[I] := OrSymbolProc;
      '+': fProcTable[I] := PlusProc;
      '/': fProcTable[I] := SlashProc;
      '&': fProcTable[I] := AndSymbolProc;
      #34: fProcTable[I] := StringProc;
      ':', '@':
        fProcTable[I] := VariableProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[I] := IdentProc;
      '0'..'9':
        fProcTable[I] := NumberProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := SpaceProc;
      '^', '%', '*', '!':
        fProcTable[I] := SymbolAssignProc;
      '{', '}', '.', ',', ';', '?', '(', ')', '[', ']', '~':
        fProcTable[I] := SymbolProc;
      else
        fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynSQLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fKeywords := TSynHashEntryList.Create;
  fTableNames := TStringList.Create;
  TStringList(fTableNames).OnChange := TableNamesChanged;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  AddAttribute(fCommentAttri);
  fDataTypeAttri := TSynHighlighterAttributes.Create(SYNS_AttrDataType);
  fDataTypeAttri.Style := [fsBold];
  AddAttribute(fDataTypeAttri);
{begin}                                                                         // DJLP 2000-08-11
  fDefaultPackageAttri :=
    TSynHighlighterAttributes.Create(SYNS_AttrDefaultPackage);
  fDefaultPackageAttri.Style := [fsBold];
  AddAttribute(fDefaultPackageAttri);
{end}                                                                           // DJLP 2000-08-11
  fExceptionAttri := TSynHighlighterAttributes.Create(SYNS_AttrException);
  fExceptionAttri.Style := [fsItalic];
  AddAttribute(fExceptionAttri);
  fFunctionAttri := TSynHighlighterAttributes.Create(SYNS_AttrFunction);
  fFunctionAttri.Style := [fsBold];
  AddAttribute(fFunctionAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  AddAttribute(fNumberAttri);
{begin}                                                                         // DJLP 2000-08-11
  fPLSQLAttri := TSynHighlighterAttributes.Create(SYNS_AttrPLSQL);
  fPLSQLAttri.Style := [fsBold];
  AddAttribute(fPLSQLAttri);
{end}                                                                           // DJLP 2000-08-11
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
{begin}                                                                         // DJLP 2000-09-05
  fSQLPlusAttri:=TSynHighlighterAttributes.Create(SYNS_AttrSQLPlus);
  fSQLPlusAttri.Style := [fsBold];
  AddAttribute(fSQLPlusAttri);
{end}                                                                           // DJLP 2000-09-05
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_Attrstring);
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fTableNameAttri := TSynHighlighterAttributes.Create(SYNS_AttrTableName);
  AddAttribute(fTableNameAttri);
  fVariableAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable);
  AddAttribute(fVariableAttri);
  SetAttributesOnChange(DefHighlightChange);
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterSQL;
  fRange := rsUnknown;
  fDialect := sqlStandard;
  SQLDialect := sqlSybase;
end;

destructor TSynSQLSyn.Destroy;
begin
  fKeywords.Free;
  fTableNames.Free;
  inherited Destroy;
end;

procedure TSynSQLSyn.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if (Source is TSynSQLSyn) then
    SQLDialect := TSynSQLSyn(Source).SQLDialect;
end;

procedure TSynSQLSyn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynSQLSyn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '&'] then Inc(Run);
end;

procedure TSynSQLSyn.AsciiCharProc;
begin
  // Oracle SQL allows strings to go over multiple lines
  if fLine[Run] = #0 then
    NullProc
  else begin
    fTokenID := tkString;
    // else it's end of multiline string
    if SQLDialect <> sqlMySql then begin
      if (Run > 0) or (fRange <> rsString) or (fLine[Run] <> #39) then begin
        fRange := rsString;
        repeat
          Inc(Run);
        until fLine[Run] in [#0, #10, #13, #39];
      end;
      if fLine[Run] = #39 then begin
        Inc(Run);
        fRange := rsUnknown;
      end;
    end
    else begin
      if (Run > 0) or (fRange <> rsString) or ((fLine[Run] <> #39) and (fLine[Run-1] <> '\')) then begin
        fRange := rsString;
        repeat
          if (fLine[Run] <> '\') and (fLine[Run+1] = #39) then begin
            Inc(Run);
            break;
          end;
          Inc(Run);
        until fLine[Run] in [#0, #10, #13];
      end;
      if (fLine[Run] = #39) and not(fLine[Run-1] = '\') then begin
        Inc(Run);
        fRange := rsUnknown;
      end;
    end;
  end;
end;

procedure TSynSQLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynSQLSyn.EqualProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSynSQLSyn.GreaterProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '>'] then Inc(Run);
end;

procedure TSynSQLSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
{begin}                                                                         // DJLP 2000-08-11
  if fTokenID = tkComment then begin
    while not (fLine[Run] in [#0, #10, #13]) do
      Inc(Run);
  end else
{end}                                                                           // DJLP 2000-08-11
    while fIdentifiersPtr[fLine[Run]] do inc(Run);
end;

procedure TSynSQLSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSQLSyn.LowerProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  case fLine[Run] of
    '=': Inc(Run);
    '<': begin
           Inc(Run);
           if fLine[Run] = '=' then Inc(Run);
         end;
  end;
end;

procedure TSynSQLSyn.MinusProc;
begin
  Inc(Run);
  if fLine[Run] = '-' then begin
    fTokenID := tkComment;
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end else
    fTokenID := tkSymbol;
end;

procedure TSynSQLSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSQLSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', '-'] do begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynSQLSyn.OrSymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '|'] then Inc(Run);
end;

procedure TSynSQLSyn.PlusProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] in ['=', '+'] then Inc(Run);
end;

procedure TSynSQLSyn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '*':
      begin
        fRange := rsComment;
        fTokenID := tkComment;
        repeat
          Inc(Run);
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
            fRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
        until fLine[Run] in [#0, #10, #13];
      end;
    '=':
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynSQLSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynSQLSyn.StringProc;
begin
  fTokenID := tkString;
  Inc(Run);
  while not (fLine[Run] in [#0, #10, #13]) do begin
    case fLine[Run] of
      '\': if fLine[Run + 1] = #34 then
             Inc(Run);
      #34: if fLine[Run + 1] <> #34 then
           begin
             Inc(Run);
             break;
           end;
    end;
    Inc(Run);
  end;
end;

procedure TSynSQLSyn.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSQLSyn.SymbolAssignProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
  if fLine[Run] = '=' then Inc(Run);
end;

procedure TSynSQLSyn.VariableProc;
var
  i: integer;
begin
  // MS SQL 7 uses @@ to indicate system functions/variables
  if(SQLDialect = sqlMSSQL7) and (fLine[Run] = '@') and (fLine[Run + 1] = '@')
  then
    IdentProc
{begin}                                                                         //JDR 2000-25-2000
  else if (SQLDialect in [sqlMySql, sqlOracle]) and (fLine[Run] = '@') then
    SymbolProc
{end}                                                                           //JDR 2000-25-2000
  // Oracle uses the ':' character to indicate bind variables
{begin}                                                                         //JJV 2000-11-16
  // Ingres II also uses the ':' character to indicate variables
  else
    if not (SQLDialect in [sqlOracle, sqlIngres]) and (fLine[Run] = ':') then
{end}                                                                           //JJV 2000-11-16
    SymbolProc
  else begin
    fTokenID := tkVariable;
    i := Run;
    repeat
      Inc(i);
    until not (fIdentifiersPtr[fLine[i]]);
    Run := i;
  end;
end;

procedure TSynSQLSyn.UnknownProc;
begin
  if (SQLDialect = sqlMySql) and (fLine[Run] = '#') and (Run = 0) then          //DDH Changes from Tonci Grgin for MYSQL 
  begin
    fTokenID := tkComment;
    fRange := rsComment;
  end else begin
  {$IFDEF SYN_MBCSSUPPORT}
    if FLine[Run] in LeadBytes then
      Inc(Run,2)
    else
  {$ENDIF}
    inc(Run);
    fTokenID := tkUnknown;
  end;
end;

procedure TSynSQLSyn.AnsiCProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    else begin
      fTokenID := tkComment;
      if (SQLDialect = sqlMySql) and (fLine[Run] = '#') then begin              //DDH Changes from Tonci Grgin for MYSQL
        repeat
          Inc(Run);
        until fLine[Run] in [#0, #10, #13];
        fRange := rsUnknown;
      end
      else begin

        repeat
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then begin
            fRange := rsUnknown;
            Inc(Run, 2);
            break;
          end;
          Inc(Run);
        until fLine[Run] in [#0, #10, #13];
      end;
    end;
  end;
end;

{begin}                                                                         // DJLP 2000-08-09
function TSynSQLSyn.IsKeyword(const AKeyword: string): boolean;
var
  tk: TtkTokenKind;
begin
  tk := IdentKind(PChar(AKeyword));
  Result := tk in [tkDatatype, tkException, tkFunction, tkKey, tkPLSQL,
    tkDefaultPackage];
end;
{end}                                                                           // DJLP 2000-08-09

procedure TSynSQLSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsComment:
      AnsiCProc;
    rsString:
      AsciiCharProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynSQLSyn.GetDefaultAttribute(Index: integer):
  TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSQLSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynSQLSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynSQLSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  Setstring(Result, (FLine + fTokenPos), Len);
end;

function TSynSQLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSQLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkDatatype: Result := fDataTypeAttri;
    tkDefaultPackage: Result := fDefaultPackageAttri;                           // DJLP 2000-08-11
    tkException: Result := fExceptionAttri;
    tkFunction: Result := fFunctionAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkPLSQL: Result := fPLSQLAttri;                                             // DJLP 2000-08-11   
    tkSpace: Result := fSpaceAttri;
    tkSQLPlus: Result := fSQLPlusAttri;                                         // DJLP 2000-08-11
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkTableName: Result := fTableNameAttri;
    tkVariable: Result := fVariableAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynSQLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSQLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynSQLSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSQLSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynSQLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
  if (fDialect = sqlMSSQL7) or (fDialect = sqlMSSQL2K) then
    Include(Result, '@')
{begin}                                                                         // DJLP 2000-08-11
  else if fDialect = sqlOracle then begin
    Include(Result, '#');
    Include(Result, '$');
  end;
{end}                                                                           // DJLP 2000-08-11
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynSQLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSQL;
end;

procedure TSynSQLSyn.DoAddKeyword(AKeyword: string; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := KeyHash(PChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

procedure TSynSQLSyn.SetTableNames(const Value: TStrings);
begin
  fTableNames.Assign(Value);
end;

procedure TSynSQLSyn.TableNamesChanged(Sender: TObject);
begin
  InitializeKeywordLists;
end;

procedure TSynSQLSyn.PutTableNamesInKeywordList;
var
  i: Integer;
  Entry: TSynHashEntry;
begin
  for i := 0 to (fTableNames.Count - 1) do
  begin
    Entry := fKeywords[KeyHash(PChar(fTableNames[i]))];
    while Assigned(Entry) do
    begin
      if (UpperCase(Entry.Keyword) = Uppercase(fTableNames[i])) then
        Break;
      Entry := Entry.Next;
    end;
    if not Assigned(Entry) then
      DoAddKeyword(fTableNames[i], Ord(tkTableName));
  end;
end;

procedure TSynSQLSyn.InitializeKeywordLists;
begin
  fKeywords.Clear;
  if (fDialect in [sqlMSSQL7, sqlMSSQL2K]) then
  begin
    fIdentifiersPtr := @IdentifiersMSSQL7;
    fmHashTablePtr := @mHashTableMSSQL7;
  end else begin
    fIdentifiersPtr := @Identifiers;
    fmHashTablePtr := @mHashTable;
  end;

  case fDialect of
    sqlIngres:
      begin
        EnumerateKeywords(Ord(tkDatatype), IngresTypes, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkKey), IngresKW, IdentChars, DoAddKeyword);
        EnumerateKeywords(Ord(tkFunction), IngresFunctions, IdentChars,
          DoAddKeyword);
      end;
    sqlInterbase6:
      begin
        EnumerateKeywords(Ord(tkDatatype), Interbase6Types, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkFunction), Interbase6Functions, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkKey), Interbase6KW, IdentChars, DoAddKeyword);
      end;
    sqlMSSQL7:
      begin
        EnumerateKeywords(Ord(tkKey), MSSQL7KW, IdentChars, DoAddKeyword);
        EnumerateKeywords(Ord(tkDatatype), MSSQL7Types, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkFunction), MSSQL7Functions, IdentChars,
          DoAddKeyword);
      end;
    sqlMSSQL2K:
      begin
        EnumerateKeywords(ord(tkKey), MSSQL2000KW, IdentChars, DoAddKeyword);
        EnumerateKeywords(ord(tkDataType), MSSQL2000Types, IdentChars, DoAddKeyword);
        EnumerateKeywords(ord(tkFunction), MSSQL2000Functions, IdentChars, DoAddKeyword);
      end;
    sqlMySql:
      begin
        EnumerateKeywords(Ord(tkKey), MySqlKW, IdentChars, DoAddKeyword);
        EnumerateKeywords(Ord(tkDatatype), MySqlTypes, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkFunction), MySqlFunctions, IdentChars,
          DoAddKeyword);
      end;
    sqlOracle:
      begin
        EnumerateKeywords(Ord(tkKey), OracleKW, IdentChars, DoAddKeyword);
        EnumerateKeywords(Ord(tkDatatype), OracleTypes, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkException), OracleExceptions, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkFunction), OracleFunctions, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkComment), OracleCommentKW, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkDefaultPackage), OracleDefaultPackages,
          IdentChars, DoAddKeyword);
        EnumerateKeywords(Ord(tkPLSQL), OraclePLSQLKW, IdentChars,
          DoAddKeyword);
        EnumerateKeywords(Ord(tkSQLPlus), OracleSQLPlusCommands, IdentChars,
          DoAddKeyword);
      end;
    sqlStandard:
      EnumerateKeywords(Ord(tkKey), StandardKW, IdentChars, DoAddKeyword);
    sqlSybase:
      EnumerateKeywords(Ord(tkKey), SybaseKW, IdentChars, DoAddKeyword);
  end;
  PutTableNamesInKeywordList;
  DefHighlightChange(Self);
end;

procedure TSynSQLSyn.SetDialect(Value: TSQLDialect);
begin
  if (Value <> fDialect) then
  begin
    fDialect := Value;
    InitializeKeywordLists;
  end;
end;

function TSynSQLSyn.GetSampleSource: String;
begin
  Result:= '';
  case fDialect of
    sqlStandard   : ;
    sqlInterbase6 : Result := '/* Interbase sample source */'#13#10 +
                              'SET TERM !! ;'#13#10 +
                              #13#10 +
                              'CREATE PROCEDURE HelloWorld(P_MSG VARCHAR(80)) AS'#13#10 +
                              'BEGIN'#13#10 +
                              '  EXECUTE PROCEDURE WRITELN(:P_MSG);'#13#10 +
                              'END !!'#13#10 +
                              #13#10 +
                              'SET TERM ; !!';
    sqlMySQL      : ;
    sqlOracle     : Result := 'PROMPT Oracle sample source'#13#10 +
                              'declare'#13#10 +
                              '  x varchar2(2000);'#13#10 +
                              'begin   -- Show some text here'#13#10 +
                              '  select to_char(count(*)) into x'#13#10 +
                              '  from tab;'#13#10 +
                              #13#10 +
                              '  dbms_output.put_line(''Hello World: '' || x);'#13#10 +
                              'exception'#13#10 +
                              '  when others then'#13#10 +
                              '    null;'#13#10 +
                              'end;';
    sqlSybase     : ;
    sqlIngres     : ;
    sqlMSSQL7     : ;
    sqlMSSQL2K    : ;
  end;
end;


initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSQLSyn);
{$ENDIF}
end.
