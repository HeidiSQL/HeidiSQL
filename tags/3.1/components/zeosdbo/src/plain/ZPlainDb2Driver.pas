{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for Interbase            }
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

unit ZPlainDb2Driver;

interface

{$I ZPlain.inc}

uses ZClasses, ZCompatibility,
{$IFNDEF UNIX}
  Windows,
{$ENDIF}
  ZPlainDriver;

const
{ generally useful constants }
  SQL_MAX_MESSAGE_LENGTH   = 1024; { message buffer size             }
  SQL_MAX_ID_LENGTH        = 128;  { maximum identifier name size,
                                          e.g. cursor names               }

{ date/time length constants }
  SQL_DATE_LEN             = 10;
  SQL_TIME_LEN             = 8;  { add P+1 if precision is nonzero }
  SQL_TIMESTAMP_LEN        = 19;  { add P+1 if precision is nonzero }

{ handle type identifiers }
  SQL_HANDLE_ENV           = 1;
  SQL_HANDLE_DBC           = 2;
  SQL_HANDLE_STMT          = 3;
  SQL_HANDLE_DESC          = 4;

{ RETCODE values             }
  SQL_SUCCESS              = 0;
  SQL_SUCCESS_WITH_INFO    = 1;
  SQL_NEED_DATA            = 99;
  SQL_NO_DATA              = 100;
  SQL_STILL_EXECUTING      = 2;
  SQL_ERROR                = -1;
  SQL_INVALID_HANDLE       = -2;

{ test for SQL_SUCCESS or SQL_SUCCESS_WITH_INFO }
// SQL_SUCCEEDED(rc) (((rc)&(~1))==0)

{ SQLFreeStmt option values  }
  SQL_CLOSE                = 0;
  SQL_DROP                 = 1;
  SQL_UNBIND               = 2;
  SQL_RESET_PARAMS         = 3;

{ SQLTransact option values  }
  SQL_COMMIT               = 0;
  SQL_ROLLBACK             = 1;

{ Standard SQL data types }
  SQL_UNKNOWN_TYPE         = 0;
  SQL_CHAR                 = 1;
  SQL_NUMERIC              = 2;
  SQL_DECIMAL              = 3;
  SQL_INTEGER              = 4;
  SQL_SMALLINT             = 5;
  SQL_FLOAT                = 6;
  SQL_REAL                 = 7;
  SQL_DOUBLE               = 8;
  SQL_DATETIME             = 9;
  SQL_VARCHAR              = 12;
  SQL_LONGVARCHAR          = (-1);
  SQL_WCHAR                = (-8);
  SQL_WVARCHAR             = (-9);
  SQL_WLONGVARCHAR         = (-10);
  SQL_BIGINT               = (-5);
  SQL_BINARY               = (-2);
{ One-parameter shortcuts for date/time data types }
  SQL_TYPE_DATE            = 91;
  SQL_TYPE_TIME            = 92;
  SQL_TYPE_TIMESTAMP       = 93;

{ Statement attribute values for cursor sensitivity }
  SQL_UNSPECIFIED          = 0;
  SQL_INSENSITIVE          = 1;
  SQL_SENSITIVE            = 2;

{ Default conversion code for SQLBindCol(), SQLBindParam() and SQLGetData() }
  SQL_DEFAULT              = 99;

{ SQLGetData() code indicating that the application row descriptor
  specifies the data type }
  SQL_ARD_TYPE             = (-99);

{ SQL date/time type subcodes }
  SQL_CODE_DATE            = 1;
  SQL_CODE_TIME            = 2;
  SQL_CODE_TIMESTAMP       = 3;

{ SQL extended data types }
  SQL_GRAPHIC              = -95;
  SQL_VARGRAPHIC           = -96;
  SQL_LONGVARGRAPHIC       = -97;
  SQL_BLOB                 = -98;
  SQL_CLOB                 = -99;
  SQL_DBCLOB               = -350;
  SQL_DATALINK             = -400;
  SQL_USER_DEFINED_TYPE    = -450;

{ C data type to SQL data type mapping }
  SQL_C_DBCHAR             = SQL_DBCLOB;
  SQL_C_DECIMAL_IBM        = SQL_DECIMAL;
//  SQL_C_DATALINK           = SQL_C_CHAR;
  SQL_C_PTR                = 2463;
  SQL_C_DECIMAL_OLEDB      = 2514;

{ locator type identifier }
  SQL_BLOB_LOCATOR         = 31;
  SQL_CLOB_LOCATOR         = 41;
  SQL_DBCLOB_LOCATOR       = -351;

{ C Data Type for the LOB locator types }
  SQL_C_BLOB_LOCATOR       = SQL_BLOB_LOCATOR;
  SQL_C_CLOB_LOCATOR       = SQL_CLOB_LOCATOR;
  SQL_C_DBCLOB_LOCATOR     = SQL_DBCLOB_LOCATOR;

{ NULL status defines; these are used in SQLColAttributes, SQLDescribeCol,
  to describe the nullability of a column in a table. }

  SQL_NO_NULLS             = 0;
  SQL_NULLABLE             = 1;
  SQL_NULLABLE_UNKNOWN     = 2;

{ values of UNNAMED field in descriptor used in SQLColAttribute }
  SQL_NAMED                 = 0;
  SQL_UNNAMED               = 1;

{ values of ALLOC_TYPE field in descriptor }
  SQL_DESC_ALLOC_AUTO       = 1;
  SQL_DESC_ALLOC_USER       = 2;

{ values of USER_DEFINED_TYPE_CODE }
  SQL_TYPE_BASE            = 0;
  SQL_TYPE_DISTINCT        = 1;
  SQL_TYPE_STRUCTURED      = 2;
  SQL_TYPE_REFERENCE       = 3;

{ Special length values  }
  SQL_NULL_DATA            = -1;
  SQL_DATA_AT_EXEC         = -2;
  SQL_NTS                  = -3;      { NTS = Null Terminated String    }
  SQL_NTSL                 = -3;      { NTS = Null Terminated String    }

{ SQLColAttributes defines }
  SQL_COLUMN_SCHEMA_NAME    = 16;
  SQL_COLUMN_CATALOG_NAME   = 17;
  SQL_COLUMN_DISTINCT_TYPE  = 1250;
  SQL_DESC_DISTINCT_TYPE    = SQL_COLUMN_DISTINCT_TYPE;
  SQL_COLUMN_REFERENCE_TYPE = 1251;
  SQL_DESC_REFERENCE_TYPE   = SQL_COLUMN_REFERENCE_TYPE;
  SQL_DESC_STRUCTURED_TYPE  = 1252;
  SQL_DESC_USER_TYPE        = 1253;
  SQL_DESC_BASE_TYPE        = 1254;
  SQL_DESC_KEY_TYPE         = 1255;
  SQL_DESC_KEY_MEMBER       = 1266;

{ identifiers of fields in the SQL descriptor }
  SQL_DESC_COUNT                  = 1001;
  SQL_DESC_TYPE                   = 1002;
  SQL_DESC_LENGTH                 = 1003;
  SQL_DESC_OCTET_LENGTH_PTR       = 1004;
  SQL_DESC_PRECISION              = 1005;
  SQL_DESC_SCALE                  = 1006;
  SQL_DESC_DATETIME_INTERVAL_CODE = 1007;
  SQL_DESC_NULLABLE               = 1008;
  SQL_DESC_INDICATOR_PTR          = 1009;
  SQL_DESC_DATA_PTR               = 1010;
  SQL_DESC_NAME                   = 1011;
  SQL_DESC_UNNAMED                = 1012;
  SQL_DESC_OCTET_LENGTH           = 1013;
  SQL_DESC_ALLOC_TYPE             = 1099;
  SQL_DESC_USER_DEFINED_TYPE_CODE = 1098;

{ SQLColAttribute defines for SQL_DESC_KEY_TYPE. }
  SQL_KEYTYPE_NONE                = 0;
  SQL_KEYTYPE_PRIMARYKEY          = 1;
  SQL_KEYTYPE_UNIQUEINDEX         = 2;
  
{ SQLColAttribute defines for SQL_COLUMN_UPDATABLE condition }
  SQL_UPDT_READONLY          = 0;
  SQL_UPDT_WRITE             = 1;
  SQL_UPDT_READWRITE_UNKNOWN = 2;

{ SQLColAttribute defines for SQL_COLUMN_SEARCHABLE condition. }
  SQL_PRED_NONE              = 0;
  SQL_PRED_CHAR              = 1;
  SQL_PRED_BASIC             = 2;

{ NULL handle defines    }
  SQL_NULL_HENV              = 0;
  SQL_NULL_HDBC              = 0;
  SQL_NULL_HSTMT             = 0;
  SQL_NULL_HDESC             = 0;
  SQL_NULL_HANDLE            = 0;

{ identifiers of fields in the diagnostics area }
  SQL_DIAG_RETURNCODE         = 1;
  SQL_DIAG_NUMBER             = 2;
  SQL_DIAG_ROW_COUNT          = 3;
  SQL_DIAG_SQLSTATE           = 4;
  SQL_DIAG_NATIVE             = 5;
  SQL_DIAG_MESSAGE_TEXT       = 6;
  SQL_DIAG_DYNAMIC_FUNCTION   = 7;
  SQL_DIAG_CLASS_ORIGIN       = 8;
  SQL_DIAG_SUBCLASS_ORIGIN    = 9;
  SQL_DIAG_CONNECTION_NAME    = 10;
  SQL_DIAG_SERVER_NAME        = 11;
  SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12;

{ dynamic function codes }
  SQL_DIAG_ALTER_TABLE           = 4;
  SQL_DIAG_CALL                  = 7;
  SQL_DIAG_CREATE_INDEX          = (-1);
  SQL_DIAG_CREATE_TABLE          = 77;
  SQL_DIAG_CREATE_VIEW           = 84;
  SQL_DIAG_DELETE_WHERE          = 19;
  SQL_DIAG_DROP_INDEX            = (-2);
  SQL_DIAG_DROP_TABLE            = 32;
  SQL_DIAG_DROP_VIEW             = 36;
  SQL_DIAG_DYNAMIC_DELETE_CURSOR = 38;
  SQL_DIAG_DYNAMIC_UPDATE_CURSOR = 81;
  SQL_DIAG_GRANT                 = 48;
  SQL_DIAG_INSERT                = 50;
  SQL_DIAG_REVOKE                = 59;
  SQL_DIAG_SELECT_CURSOR         = 85;
  SQL_DIAG_UNKNOWN_STATEMENT     = 0;
  SQL_DIAG_UPDATE_WHERE          = 82;

{ IBM specific SQLGetDiagField values. }

  SQL_DIAG_DEFERRED_PREPARE_ERROR = 1279;

{ SQL_DIAG_ROW_NUMBER values }
  SQL_ROW_NO_ROW_NUMBER          = (-1);
  SQL_ROW_NUMBER_UNKNOWN         = (-2);

{ SQL_DIAG_COLUMN_NUMBER values }
  SQL_COLUMN_NO_COLUMN_NUMBER    = (-1);
  SQL_COLUMN_NUMBER_UNKNOWN      = (-2);

{ NEW PARAMETERS }

{ Options for SQLGetConnectOption/SQLSetConnectOption extensions }
  SQL_WCHARTYPE                  = 1252;
  SQL_LONGDATA_COMPAT            = 1253;
  SQL_CURRENT_SCHEMA             = 1254;
  SQL_DB2EXPLAIN                 = 1258;
  SQL_DB2ESTIMATE                = 1259;
  SQL_PARAMOPT_ATOMIC            = 1260;
  SQL_STMTTXN_ISOLATION          = 1261;
  SQL_MAXCONN                    = 1262;
  SQL_ATTR_CLISCHEMA             = 1280;
  SQL_ATTR_INFO_USERID           = 1281;
  SQL_ATTR_INFO_WRKSTNNAME       = 1282;
  SQL_ATTR_INFO_APPLNAME         = 1283;
  SQL_ATTR_INFO_ACCTSTR          = 1284;
  SQL_ATTR_AUTOCOMMIT_NOCOMMIT   = 2462;
  SQL_ATTR_QUERY_PATROLLER       = 2466;
  SQL_ATTR_CHAINING_BEGIN        = 2464;
  SQL_ATTR_CHAINING_END          = 2465;

  SQL_ATTR_WCHARTYPE            = SQL_WCHARTYPE;
  SQL_ATTR_LONGDATA_COMPAT      = SQL_LONGDATA_COMPAT;
  SQL_ATTR_CURRENT_SCHEMA       = SQL_CURRENT_SCHEMA;
  SQL_ATTR_DB2EXPLAIN           = SQL_DB2EXPLAIN;
  SQL_ATTR_DB2ESTIMATE          = SQL_DB2ESTIMATE;
  SQL_ATTR_PARAMOPT_ATOMIC      = SQL_PARAMOPT_ATOMIC;
  SQL_ATTR_STMTTXN_ISOLATION    = SQL_STMTTXN_ISOLATION;
  SQL_ATTR_MAXCONN              = SQL_MAXCONN;

{ Options for SQLSetConnectOption, SQLSetEnvAttr }
  SQL_CONNECTTYPE                = 1255;
  SQL_SYNC_POINT                 = 1256;
  SQL_MINMEMORY_USAGE            = 1263;
  SQL_CONN_CONTEXT               = 1269;
  SQL_ATTR_INHERIT_NULL_CONNECT  = 1270;
  SQL_ATTR_FORCE_CONVERSION_ON_CLIENT = 1275;

  SQL_ATTR_CONNECTTYPE           = SQL_CONNECTTYPE;
  SQL_ATTR_SYNC_POINT            = SQL_SYNC_POINT;
  SQL_ATTR_MINMEMORY_USAGE       = SQL_MINMEMORY_USAGE;
  SQL_ATTR_CONN_CONTEXT          = SQL_CONN_CONTEXT;

{ connection attributes }
  SQL_ACCESS_MODE                = 101;
  SQL_AUTOCOMMIT                 = 102;
  SQL_LOGIN_TIMEOUT              = 103;
  SQL_OPT_TRACE                  = 104;
  SQL_OPT_TRACEFILE              = 105;
  SQL_TRANSLATE_DLL              = 106;
  SQL_TRANSLATE_OPTION           = 107;
  SQL_TXN_ISOLATION              = 108;
  SQL_CURRENT_QUALIFIER          = 109;
  SQL_ODBC_CURSORS               = 110;
  SQL_QUIET_MODE                 = 111;
  SQL_PACKET_SIZE                = 112;

{ connection attributes with new names }
  SQL_ATTR_ACCESS_MODE           = SQL_ACCESS_MODE;
  SQL_ATTR_AUTOCOMMIT            = SQL_AUTOCOMMIT;
  SQL_ATTR_CONNECTION_TIMEOUT    = 113;
  SQL_ATTR_CURRENT_CATALOG       = SQL_CURRENT_QUALIFIER;
  SQL_ATTR_DISCONNECT_BEHAVIOR   = 114;
  SQL_ATTR_ENLIST_IN_DTC         = 1207;
  SQL_ATTR_ENLIST_IN_XA          = 1208;
  SQL_ATTR_LOGIN_TIMEOUT         = SQL_LOGIN_TIMEOUT;
  SQL_ATTR_ODBC_CURSORS          = SQL_ODBC_CURSORS;
  SQL_ATTR_PACKET_SIZE           = SQL_PACKET_SIZE;
  SQL_ATTR_QUIET_MODE            = SQL_QUIET_MODE;
  SQL_ATTR_TRACE                 = SQL_OPT_TRACE;
  SQL_ATTR_TRACEFILE             = SQL_OPT_TRACEFILE;
  SQL_ATTR_TRANSLATE_LIB         = SQL_TRANSLATE_DLL;
  SQL_ATTR_TRANSLATE_OPTION      = SQL_TRANSLATE_OPTION;
  SQL_ATTR_TXN_ISOLATION         = SQL_TXN_ISOLATION;

{ SQL_AUTOCOMMIT options }
  SQL_AUTOCOMMIT_OFF             = 0;
  SQL_AUTOCOMMIT_ON              = 1;
  SQL_AUTOCOMMIT_DEFAULT         = SQL_AUTOCOMMIT_ON;

{ SQL_TXN_ISOLATION_OPTION masks }
  SQL_TXN_READ_UNCOMMITTED            = {$IFNDEF FPC}#00000001{$ELSE}1{$ENDIF};
  SQL_TRANSACTION_READ_UNCOMMITTED    = SQL_TXN_READ_UNCOMMITTED;
  SQL_TXN_READ_COMMITTED              = {$IFNDEF FPC}#00000002{$ELSE}2{$ENDIF};
  SQL_TRANSACTION_READ_COMMITTED      = SQL_TXN_READ_COMMITTED;
  SQL_TXN_REPEATABLE_READ             = {$IFNDEF FPC}#00000004{$ELSE}4{$ENDIF};
  SQL_TRANSACTION_REPEATABLE_READ     = SQL_TXN_REPEATABLE_READ;
  SQL_TXN_SERIALIZABLE                = {$IFNDEF FPC}#00000008{$ELSE}8{$ENDIF};
  SQL_TRANSACTION_SERIALIZABLE        = SQL_TXN_SERIALIZABLE;
  SQL_TXN_NOCOMMIT                    = {$IFNDEF FPC}#00000020{$ELSE}20{$ENDIF};
  SQL_TRANSACTION_NOCOMMIT            = SQL_TXN_NOCOMMIT;

{ Defines for SQLBindParameter and
                           SQLProcedureColumns (returned in the result set) }
  SQL_PARAM_TYPE_UNKNOWN              = 0;
  SQL_PARAM_INPUT                     = 1;
  SQL_PARAM_INPUT_OUTPUT              = 2;
  SQL_RESULT_COL                      = 3;
  SQL_PARAM_OUTPUT                    = 4;
  SQL_RETURN_VALUE                    = 5;

  SQL_MAX_NUMERIC_LEN                 = 16;
type
  { SQL portable types for C  }
//  SQLCHAR = Byte;
  PSQLCHAR = PChar;
//  SQLVARCHAR = Byte;
//  SQLSCHAR = Byte;
  SQLINTEGER = Integer;
  PSQLINTEGER = ^SQLINTEGER;
  SQLSMALLINT = SmallInt;
  PSQLSMALLINT = ^SQLSMALLINT;
  SQLDOUBLE = Double;
  SQLFLOAT = Double;
  SQLREAL = Single;

  SQLRETURN = SQLSMALLINT;

  SQLUINTEGER = Cardinal;
  PSQLUINTEGER = ^SQLUINTEGER;
  SQLUSMALLINT = SQLSMALLINT;
  PSQLUSMALLINT = ^SQLUSMALLINT;
  SQLPOINTER = Pointer;
  SQLHANDLE = SQLINTEGER;
  PSQLHANDLE = ^SQLHANDLE;
  SQLHENV = SQLINTEGER;
  PSQLHENV = ^SQLHENV;
  SQLHDBC = SQLINTEGER;
  PSQLHDBC = ^SQLHDBC;
  SQLHSTMT = SQLINTEGER;
  PSQLHSTMT = ^SQLHSTMT;
  SQLHDESC = SQLHANDLE;

  SQLLEN = SQLUINTEGER;
  SQLULEN = SQLUINTEGER;
  SQLSETPOSIROW = SQLUSMALLINT;
{$IFNDEF UNIX}
  SQLHWND = HWnd;
{$ELSE}
  SQLHWND = SQLPOINTER;
{$ENDIF}

  SQLBIGINT = packed record
    dwLowWord: SQLUINTEGER;
    dwHighWord: SQLINTEGER
  end;
  PSQLBIGINT = ^SQLBIGINT;

  SQLUBIGINT = packed record
    dwLowWord: SQLUINTEGER;
    dwHighWord: SQLUINTEGER
  end;
  PSQLUBIGINT = ^SQLUBIGINT;

  SQL_DATE_STRUCT = packed record
    year: SQLSMALLINT;
    month: SQLUSMALLINT;
    day: SQLUSMALLINT;
  end;
  PSQL_DATE_STRUCT = ^SQL_DATE_STRUCT;

  SQL_TIME_STRUCT = packed record
    hour: SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
  end;
  PSQL_TIME_STRUCT = ^SQL_TIME_STRUCT;

  SQL_TIMESTAMP_STRUCT = packed record
    year: SQLSMALLINT;
    month: SQLUSMALLINT;
    day: SQLUSMALLINT;
    hour: SQLUSMALLINT;
    minute: SQLUSMALLINT;
    second: SQLUSMALLINT;
    fraction: SQLUINTEGER;     { fraction of a second }
  end;
  PSQL_TIMESTAMP_STRUCT = ^SQL_TIMESTAMP_STRUCT;

  SQL_YEAR_MONTH_STRUCT = packed record
    year: SQLUINTEGER;
    month: SQLUINTEGER;
  end;
  PSQL_YEAR_MONTH_STRUCT= ^SQL_YEAR_MONTH_STRUCT;

  SQL_DAY_SECOND_STRUCT = packed record
    day: SQLUINTEGER;
    hour: SQLUINTEGER;
    minute: SQLUINTEGER;
    second: SQLUINTEGER;
    fraction: SQLUINTEGER;
  end;
  PSQL_DAY_SECOND_STRUCT = ^SQL_DAY_SECOND_STRUCT;

  SQLINTERVAL = (SQL_IS_YEAR, SQL_IS_MONTH, SQL_IS_DAY, SQL_IS_HOUR,
    SQL_IS_MINUTE, SQL_IS_SECOND, SQL_IS_YEAR_TO_MONTH, SQL_IS_DAY_TO_HOUR,
    SQL_IS_DAY_TO_MINUTE, SQL_IS_DAY_TO_SECOND, SQL_IS_HOUR_TO_MINUTE,
    SQL_IS_HOUR_TO_SECOND, SQL_IS_MINUTE_TO_SECOND);

  SQL_INTERVAL_STRUCT = record
    interval_type: SQLINTERVAL;
    interval_sign: SQLSMALLINT;
    year_month: SQL_YEAR_MONTH_STRUCT;
    day_second: SQL_DAY_SECOND_STRUCT;
  end;
  PSQL_INTERVAL_STRUCT = ^SQL_INTERVAL_STRUCT;

  SQL_NUMERIC_STRUCT = packed record
    precision: Byte;
    scale: Byte;
    sign: Byte; { 1 if positive, 0 if negative }
    val: array[0..SQL_MAX_NUMERIC_LEN-1] of Char;
  end;
  PSQL_NUMERIC_STRUCT = SQL_NUMERIC_STRUCT;

{************** Plain API Function types definition *************}

 {** Represents a generic interface to DB2 native API. }
  IZDb2PlainDriver = interface (IZPlainDriver)
    ['{57AA5614-800C-4543-B5E4-66FE0E12006A}']
    function SQLAllocConnect(henv: SQLHENV; phdbc: PSQLHDBC): SQLRETURN; stdcall;
    function SQLAllocEnv(phenv: PSQLHENV): SQLRETURN; stdcall;
    function SQLAllocStmt(hdbc: SQLHDBC; phstmt: PSQLHSTMT): SQLRETURN; stdcall;
    function SQLAllocHandle(fHandleType: SQLSMALLINT; hInput: SQLHANDLE;
      phOutput: PSQLHANDLE): SQLRETURN; stdcall;
    function SQLBindCol(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
      fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
      pcbValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLCancel(hstmt: SQLHSTMT): SQLRETURN; stdcall;
    function SQLColAttribute(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
      fDescType: SQLUSMALLINT; rgbDesc: SQLPOINTER; cbDescMax: SQLSMALLINT;
      pcbDesc: PSQLSMALLINT; pfDesc: SQLPOINTER): SQLRETURN; stdcall;
    function SQLConnect(hdbc: SQLHDBC; szDSN: PSQLCHAR; cbDSN: SQLSMALLINT;
      szUID: PSQLCHAR; cbUID: SQLSMALLINT; szAuthStr: PSQLCHAR;
      cbAuthStr: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLDescribeCol(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
      szColName: PSQLCHAR; cbColNameMax: SQLSMALLINT; pcbColName: PSQLSMALLINT;
      pfSqlType: PSQLSMALLINT; pcbColDef: PSQLUINTEGER; pibScale: PSQLSMALLINT;
      pfNullable: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLDisconnect(hdbc: SQLHDBC): SQLRETURN; stdcall;
    function SQLError(henv: SQLHENV; hdbc: SQLHDBC; hstmt: SQLHSTMT;
      szSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER; szErrorMsg: PSQLCHAR;
      cbErrorMsgMax: SQLSMALLINT; pcbErrorMsg: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLExecDirect(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
      cbSqlStr: SQLINTEGER): SQLRETURN; stdcall;
    function SQLExecute(hstmt: SQLHSTMT): SQLRETURN; stdcall;
    function SQLFetch(hstmt: SQLHSTMT): SQLRETURN; stdcall;
    function SQLFreeConnect(hdbc: SQLHDBC): SQLRETURN; stdcall;
    function SQLFreeEnv(henv: SQLHENV): SQLRETURN; stdcall;
    function SQLFreeStmt(hstmt: SQLHSTMT; fOption: SQLUSMALLINT): SQLRETURN; stdcall;
    function SQLCloseCursor(hStmt: SQLHSTMT): SQLRETURN; stdcall;
    function SQLGetCursorName(hstmt: SQLHSTMT; szCursor: PSQLCHAR;
      cbCursorMax: SQLSMALLINT; pcbCursor: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLGetData(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
      fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
      pcbValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLNumResultCols(hstmt: SQLHSTMT; pccol: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLPrepare(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
      cbSqlStr: SQLINTEGER): SQLRETURN; stdcall;
    function SQLRowCount(hstmt: SQLHSTMT; pcrow: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLSetCursorName(hstmt: SQLHSTMT; szCursor: PSQLCHAR;
      cbCursor: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLSetParam(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
      fCType: SQLSMALLINT; fSqlType: SQLSMALLINT; cbParamDef: SQLUINTEGER;
      ibScale: SQLSMALLINT; rgbValue: SQLPOINTER; pcbValue: PSQLINTEGER):
      SQLRETURN; stdcall;
    function SQLTransact(henv: SQLHENV; hdbc: SQLHDBC; fType: SQLUSMALLINT):
      SQLRETURN; stdcall;
    function SQLEndTran(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
      fType: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLFreeHandle(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE):
      SQLRETURN; stdcall;
    function SQLGetDiagRec(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
      iRecNumber: SQLSMALLINT; pszSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER;
      pszErrorMsg: PSQLCHAR; cbErrorMsgMax: SQLSMALLINT;
      pcbErrorMsg: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLGetDiagField(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
      iRecNumber: SQLSMALLINT; fDiagIdentifier: SQLSMALLINT; pDiagInfo: SQLPOINTER;
      cbDiagInfoMax: SQLSMALLINT; pcbDiagInfo: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLCopyDesc(hDescSource: SQLHDESC; hDescTarget: SQLHDESC):
      SQLRETURN; stdcall;
    function SQLGetDescField(DescriptorHandle: SQLHDESC;
      RecNumber: SQLSMALLINT; FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER;
      BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLGetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT;
      _Type: PSQLSMALLINT; SubType: PSQLSMALLINT; Length: PSQLINTEGER;
      Precision: PSQLSMALLINT; Scale: PSQLSMALLINT;  Nullable: PSQLSMALLINT):
      SQLRETURN; stdcall;
    function SQLSetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER):
      SQLRETURN; stdcall;
    function SQLSetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLINTEGER;
      Precision: SQLSMALLINT; Scale: SQLSMALLINT; Data: SQLPOINTER;
      StringLength: PSQLINTEGER; Indicator: PSQLINTEGER): SQLRETURN; stdcall;
    { new functions }
    function SQLSetConnectAttr(hdbc: SQLHDBC; fOption: SQLINTEGER;
      pvParam: SQLPOINTER; fStrLen: SQLINTEGER): SQLRETURN; stdcall;
    function SQLSetConnectOption(hdbc: SQLHDBC; fOption: SQLUSMALLINT;
      vParam: SQLUINTEGER): SQLRETURN; stdcall;
    function SQLSetStmtAttr(hstmt: SQLHSTMT; fOption: SQLINTEGER;
      pvParam: SQLPOINTER; fStrLen: SQLINTEGER): SQLRETURN; stdcall;
    function SQLSetStmtOption(hstmt: SQLHSTMT; fOption: SQLUSMALLINT;
      vParam: SQLUINTEGER): SQLRETURN; stdcall;
    function SQLGetSubString(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
      SourceLocator: SQLINTEGER; FromPosition: SQLUINTEGER;
      ForLength: SQLUINTEGER; TargetCType: SQLSMALLINT; rgbValue: SQLPOINTER;
      cbValueMax: SQLINTEGER; StringLength: PSQLINTEGER;
      IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLGetLength(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
      Locator: SQLINTEGER; StringLength: PSQLINTEGER;
      IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLGetPosition(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
      SourceLocator: SQLINTEGER; SearchLocator: SQLINTEGER; SearchLiteral: PSQLCHAR;
      SearchLiteralLength: SQLINTEGER; FromPosition: SQLUINTEGER;
      LocatedAt: PSQLUINTEGER; IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLBindParameter(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
      fParamType: SQLSMALLINT; fCType: SQLSMALLINT; fSqlType: SQLSMALLINT;
      cbColDef: SQLUINTEGER; ibScale: SQLSMALLINT; rgbValue: SQLPOINTER;
      cbValueMax: SQLINTEGER; pcbValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLParamData(hstmt: SQLHSTMT; prgbValue: SQLPOINTER): SQLRETURN; stdcall;
    function SQLPutData(hstmt: SQLHSTMT; rgbValue: SQLPOINTER;
      cbValue: SQLINTEGER): SQLRETURN; stdcall;
    { new1 functions }
    function SQLColumns(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
      cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR; cbSchemaName: SQLSMALLINT;
      szTableName: PSQLCHAR; cbTableName: SQLSMALLINT; szColumnName: PSQLCHAR;
      cbColumnName: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLDataSources(henv: SQLHENV; fDirection: SQLUSMALLINT; szDSN: PSQLCHAR;
      cbDSNMax: SQLSMALLINT; pcbDSN: PSQLSMALLINT; szDescription: PSQLCHAR;
      cbDescriptionMax: SQLSMALLINT; pcbDescription: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLFetchScroll(StatementHandle: SQLHSTMT;
      FetchOrientation: SQLSMALLINT; FetchOffset: SQLLEN): SQLRETURN; stdcall;
    function SQLGetFunctions(hdbc: SQLHDBC; fFunction: SQLUSMALLINT;
      pfExists: PSQLUSMALLINT): SQLRETURN; stdcall;
    function SQLGetInfo(hdbc: SQLHDBC; fInfoType: SQLUSMALLINT;
      rgbInfoValue: SQLPOINTER; cbInfoValueMax: SQLSMALLINT;
      pcbInfoValue: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLGetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: SQLINTEGER): SQLRETURN; stdcall;
    function SQLGetStmtOption(hstmt: SQLHSTMT; fOption: SQLUSMALLINT;
      pvParam: SQLPOINTER): SQLRETURN; stdcall;
    function SQLGetTypeInfo(hstmt: SQLHSTMT; fSqlType: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLSpecialColumns(hstmt: SQLHSTMT; fColType: SQLUSMALLINT;
      szCatalogName: PSQLCHAR; cbCatalogName: SQLUSMALLINT;
      szSchemaName: PSQLCHAR; cbSchemaName: SQLUSMALLINT; szTableName: PSQLCHAR;
      cbTableName: SQLUSMALLINT; fScope: SQLUSMALLINT;
      fNullable: SQLUSMALLINT): SQLRETURN; stdcall;
    function SQLStatistics(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
      cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR;
      cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR; cbTableName: SQLSMALLINT;
      fUnique: SQLUSMALLINT; fAccuracy: SQLUSMALLINT): SQLRETURN; stdcall;
    function SQLTables(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
      cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR;
      cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR; cbTableName: SQLSMALLINT;
      szTableType: SQLSMALLINT; cbTableType: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLNextResult(hstmtSource: SQLHSTMT;
      hstmtTarget: SQLHSTMT): SQLRETURN; stdcall;
  end;

  {** Represents class to DB2 native API. }
  TZDb2PlainDriver = class(TZAbstractObject, IZPlainDriver, IZDb2PlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;
    
    function SQLAllocConnect(henv: SQLHENV; phdbc: PSQLHDBC): SQLRETURN; stdcall;
    function SQLAllocEnv(phenv: PSQLHENV): SQLRETURN; stdcall;
    function SQLAllocStmt(hdbc: SQLHDBC; phstmt: PSQLHSTMT): SQLRETURN; stdcall;
    function SQLAllocHandle(fHandleType: SQLSMALLINT; hInput: SQLHANDLE;
      phOutput: PSQLHANDLE): SQLRETURN; stdcall;
    function SQLBindCol(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
      fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
      pcbValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLCancel(hstmt: SQLHSTMT): SQLRETURN; stdcall;
    function SQLColAttribute(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
      fDescType: SQLUSMALLINT; rgbDesc: SQLPOINTER; cbDescMax: SQLSMALLINT;
      pcbDesc: PSQLSMALLINT; pfDesc: SQLPOINTER): SQLRETURN; stdcall;
    function SQLConnect(hdbc: SQLHDBC; szDSN: PSQLCHAR; cbDSN: SQLSMALLINT;
      szUID: PSQLCHAR; cbUID: SQLSMALLINT; szAuthStr: PSQLCHAR;
      cbAuthStr: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLDescribeCol(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
      szColName: PSQLCHAR; cbColNameMax: SQLSMALLINT; pcbColName: PSQLSMALLINT;
      pfSqlType: PSQLSMALLINT; pcbColDef: PSQLUINTEGER; pibScale: PSQLSMALLINT;
      pfNullable: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLDisconnect(hdbc: SQLHDBC): SQLRETURN; stdcall;
    function SQLError(henv: SQLHENV; hdbc: SQLHDBC; hstmt: SQLHSTMT;
      szSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER; szErrorMsg: PSQLCHAR;
      cbErrorMsgMax: SQLSMALLINT; pcbErrorMsg: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLExecDirect(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
      cbSqlStr: SQLINTEGER): SQLRETURN; stdcall;
    function SQLExecute(hstmt: SQLHSTMT): SQLRETURN; stdcall;
    function SQLFetch(hstmt: SQLHSTMT): SQLRETURN; stdcall;
    function SQLFreeConnect(hdbc: SQLHDBC): SQLRETURN; stdcall;
    function SQLFreeEnv(henv: SQLHENV): SQLRETURN; stdcall;
    function SQLFreeStmt(hstmt: SQLHSTMT; fOption: SQLUSMALLINT): SQLRETURN; stdcall;
    function SQLCloseCursor(hStmt: SQLHSTMT): SQLRETURN; stdcall;
    function SQLGetCursorName(hstmt: SQLHSTMT; szCursor: PSQLCHAR;
      cbCursorMax: SQLSMALLINT; pcbCursor: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLGetData(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
      fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
      pcbValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLNumResultCols(hstmt: SQLHSTMT; pccol: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLPrepare(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
      cbSqlStr: SQLINTEGER): SQLRETURN; stdcall;
    function SQLRowCount(hstmt: SQLHSTMT; pcrow: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLSetCursorName(hstmt: SQLHSTMT; szCursor: PSQLCHAR;
      cbCursor: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLSetParam(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
      fCType: SQLSMALLINT; fSqlType: SQLSMALLINT; cbParamDef: SQLUINTEGER;
      ibScale: SQLSMALLINT; rgbValue: SQLPOINTER; pcbValue: PSQLINTEGER):
      SQLRETURN; stdcall;
    function SQLTransact(henv: SQLHENV; hdbc: SQLHDBC; fType: SQLUSMALLINT):
      SQLRETURN; stdcall;
    function SQLEndTran(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
      fType: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLFreeHandle(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE):
      SQLRETURN; stdcall;
    function SQLGetDiagRec(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
      iRecNumber: SQLSMALLINT; pszSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER;
      pszErrorMsg: PSQLCHAR; cbErrorMsgMax: SQLSMALLINT;
      pcbErrorMsg: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLGetDiagField(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
      iRecNumber: SQLSMALLINT; fDiagIdentifier: SQLSMALLINT; pDiagInfo: SQLPOINTER;
      cbDiagInfoMax: SQLSMALLINT; pcbDiagInfo: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLCopyDesc(hDescSource: SQLHDESC; hDescTarget: SQLHDESC):
      SQLRETURN; stdcall;
    function SQLGetDescField(DescriptorHandle: SQLHDESC;
      RecNumber: SQLSMALLINT; FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER;
      BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLGetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT;
      _Type: PSQLSMALLINT; SubType: PSQLSMALLINT; Length: PSQLINTEGER;
      Precision: PSQLSMALLINT; Scale: PSQLSMALLINT;  Nullable: PSQLSMALLINT):
      SQLRETURN; stdcall;
    function SQLSetDescField(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER):
      SQLRETURN; stdcall;
    function SQLSetDescRec(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
      _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLINTEGER;
      Precision: SQLSMALLINT; Scale: SQLSMALLINT; Data: SQLPOINTER;
      StringLength: PSQLINTEGER; Indicator: PSQLINTEGER): SQLRETURN; stdcall;
    { new functions }
    function SQLSetConnectAttr(hdbc: SQLHDBC; fOption: SQLINTEGER;
      pvParam: SQLPOINTER; fStrLen: SQLINTEGER): SQLRETURN; stdcall;
    function SQLSetConnectOption(hdbc: SQLHDBC; fOption: SQLUSMALLINT;
      vParam: SQLUINTEGER): SQLRETURN; stdcall;
    function SQLSetStmtAttr(hstmt: SQLHSTMT; fOption: SQLINTEGER;
      pvParam: SQLPOINTER; fStrLen: SQLINTEGER): SQLRETURN; stdcall;
    function SQLSetStmtOption(hstmt: SQLHSTMT; fOption: SQLUSMALLINT;
      vParam: SQLUINTEGER): SQLRETURN; stdcall;
    function SQLGetSubString(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
      SourceLocator: SQLINTEGER; FromPosition: SQLUINTEGER;
      ForLength: SQLUINTEGER; TargetCType: SQLSMALLINT; rgbValue: SQLPOINTER;
      cbValueMax: SQLINTEGER; StringLength: PSQLINTEGER;
      IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLGetLength(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
      Locator: SQLINTEGER; StringLength: PSQLINTEGER;
      IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLGetPosition(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
      SourceLocator: SQLINTEGER; SearchLocator: SQLINTEGER; SearchLiteral: PSQLCHAR;
      SearchLiteralLength: SQLINTEGER; FromPosition: SQLUINTEGER;
      LocatedAt: PSQLUINTEGER; IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLBindParameter(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
      fParamType: SQLSMALLINT; fCType: SQLSMALLINT; fSqlType: SQLSMALLINT;
      cbColDef: SQLUINTEGER; ibScale: SQLSMALLINT; rgbValue: SQLPOINTER;
      cbValueMax: SQLINTEGER; pcbValue: PSQLINTEGER): SQLRETURN; stdcall;
    function SQLParamData(hstmt: SQLHSTMT; prgbValue: SQLPOINTER): SQLRETURN; stdcall;
    function SQLPutData(hstmt: SQLHSTMT; rgbValue: SQLPOINTER;
      cbValue: SQLINTEGER): SQLRETURN; stdcall;
    { new1 functions }
    function SQLColumns(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
      cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR; cbSchemaName: SQLSMALLINT;
      szTableName: PSQLCHAR; cbTableName: SQLSMALLINT; szColumnName: PSQLCHAR;
      cbColumnName: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLDataSources(henv: SQLHENV; fDirection: SQLUSMALLINT; szDSN: PSQLCHAR;
      cbDSNMax: SQLSMALLINT; pcbDSN: PSQLSMALLINT; szDescription: PSQLCHAR;
      cbDescriptionMax: SQLSMALLINT; pcbDescription: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLFetchScroll(StatementHandle: SQLHSTMT;
      FetchOrientation: SQLSMALLINT; FetchOffset: SQLLEN): SQLRETURN; stdcall;
    function SQLGetFunctions(hdbc: SQLHDBC; fFunction: SQLUSMALLINT;
      pfExists: PSQLUSMALLINT): SQLRETURN; stdcall;
    function SQLGetInfo(hdbc: SQLHDBC; fInfoType: SQLUSMALLINT;
      rgbInfoValue: SQLPOINTER; cbInfoValueMax: SQLSMALLINT;
      pcbInfoValue: PSQLSMALLINT): SQLRETURN; stdcall;
    function SQLGetStmtAttr(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
      Value: SQLPOINTER; BufferLength: SQLINTEGER;
      StringLength: SQLINTEGER): SQLRETURN; stdcall;
    function SQLGetStmtOption(hstmt: SQLHSTMT; fOption: SQLUSMALLINT;
      pvParam: SQLPOINTER): SQLRETURN; stdcall;
    function SQLGetTypeInfo(hstmt: SQLHSTMT; fSqlType: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLSpecialColumns(hstmt: SQLHSTMT; fColType: SQLUSMALLINT;
      szCatalogName: PSQLCHAR; cbCatalogName: SQLUSMALLINT;
      szSchemaName: PSQLCHAR; cbSchemaName: SQLUSMALLINT; szTableName: PSQLCHAR;
      cbTableName: SQLUSMALLINT; fScope: SQLUSMALLINT;
      fNullable: SQLUSMALLINT): SQLRETURN; stdcall;
    function SQLStatistics(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
      cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR;
      cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR; cbTableName: SQLSMALLINT;
      fUnique: SQLUSMALLINT; fAccuracy: SQLUSMALLINT): SQLRETURN; stdcall;
    function SQLTables(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
      cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR;
      cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR; cbTableName: SQLSMALLINT;
      szTableType: SQLSMALLINT; cbTableType: SQLSMALLINT): SQLRETURN; stdcall;
    function SQLNextResult(hstmtSource: SQLHSTMT;
      hstmtTarget: SQLHSTMT): SQLRETURN; stdcall;
  end;

implementation

uses SysUtils, ZPlainDb2;

{ TZDb2PlainDriver }

constructor TZDb2PlainDriver.Create;
begin

end;

function TZDb2PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for IBM DB2';
end;

function TZDb2PlainDriver.GetProtocol: string;
begin
  Result := 'db2';
end;

procedure TZDb2PlainDriver.Initialize;
begin
  ZPlainDb2.LibraryLoader.LoadIfNeeded;
end;

function TZDb2PlainDriver.SQLAllocConnect(henv: SQLHENV;
  phdbc: PSQLHDBC): SQLRETURN;
begin
  Result := ZPlainDb2.SQLAllocConnect(henv, phdbc);
end;

function TZDb2PlainDriver.SQLAllocEnv(phenv: PSQLHENV): SQLRETURN;
begin
  Result := ZPlainDb2.SQLAllocEnv(phenv);
end;

function TZDb2PlainDriver.SQLAllocHandle(fHandleType: SQLSMALLINT;
  hInput: SQLHANDLE; phOutput: PSQLHANDLE): SQLRETURN;
begin
  Result := ZPlainDb2.SQLAllocHandle(fHandleType, hInput, phOutput);
end;

function TZDb2PlainDriver.SQLAllocStmt(hdbc: SQLHDBC;
  phstmt: PSQLHSTMT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLAllocStmt(hdbc, phstmt);
end;

function TZDb2PlainDriver.SQLBindCol(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
  fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
  pcbValue: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLBindCol(hstmt, icol, fCType, rgbValue, cbValueMax,
    pcbValue);
end;

function TZDb2PlainDriver.SQLBindParameter(hstmt: SQLHSTMT;
  ipar: SQLUSMALLINT; fParamType, fCType, fSqlType: SQLSMALLINT;
  cbColDef: SQLUINTEGER; ibScale: SQLSMALLINT; rgbValue: SQLPOINTER;
  cbValueMax: SQLINTEGER; pcbValue: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLBindParameter(hstmt, ipar, fParamType, fCType, fSqlType,
  cbColDef, ibScale, rgbValue, cbValueMax, pcbValue);
end;

function TZDb2PlainDriver.SQLCancel(hstmt: SQLHSTMT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLCancel(hstmt);
end;

function TZDb2PlainDriver.SQLCloseCursor(hStmt: SQLHSTMT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLCloseCursor(hStmt);
end;

function TZDb2PlainDriver.SQLColAttribute(hstmt: SQLHSTMT; icol,
  fDescType: SQLUSMALLINT; rgbDesc: SQLPOINTER; cbDescMax: SQLSMALLINT;
  pcbDesc: PSQLSMALLINT; pfDesc: SQLPOINTER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLColAttribute(hstmt, icol, fDescType, rgbDesc, cbDescMax,
    pcbDesc, pfDesc);
end;

function TZDb2PlainDriver.SQLColumns(hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR; cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR; cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR;
  cbTableName: SQLSMALLINT; szColumnName: PSQLCHAR;
  cbColumnName: SQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLColumns(hstmt, szCatalogName, cbCatalogName,
    szSchemaName, cbSchemaName, szTableName, cbTableName, szColumnName,
    cbColumnName);
end;

function TZDb2PlainDriver.SQLConnect(hdbc: SQLHDBC; szDSN: PSQLCHAR;
  cbDSN: SQLSMALLINT; szUID: PSQLCHAR; cbUID: SQLSMALLINT;
  szAuthStr: PSQLCHAR; cbAuthStr: SQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLConnect(hdbc, szDSN, cbDSN, szUID, cbUID,
    szAuthStr, cbAuthStr);
end;

function TZDb2PlainDriver.SQLCopyDesc(hDescSource,
  hDescTarget: SQLHDESC): SQLRETURN;
begin
  Result := ZPlainDb2.SQLCopyDesc(hDescSource, hDescTarget);
end;

function TZDb2PlainDriver.SQLDataSources(henv: SQLHENV;
  fDirection: SQLUSMALLINT; szDSN: PSQLCHAR; cbDSNMax: SQLSMALLINT;
  pcbDSN: PSQLSMALLINT; szDescription: PSQLCHAR; cbDescriptionMax: SQLSMALLINT;
  pcbDescription: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLDataSources(henv, fDirection, szDSN, cbDSNMax, pcbDSN,
    szDescription, cbDescriptionMax, pcbDescription);
end;

function TZDb2PlainDriver.SQLDescribeCol(hstmt: SQLHSTMT;
  icol: SQLUSMALLINT; szColName: PSQLCHAR; cbColNameMax: SQLSMALLINT;
  pcbColName, pfSqlType: PSQLSMALLINT; pcbColDef: PSQLUINTEGER; pibScale,
  pfNullable: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLDescribeCol(hstmt, icol, szColName, cbColNameMax,
    pcbColName, pfSqlType, pcbColDef, pibScale, pfNullable);
end;

function TZDb2PlainDriver.SQLDisconnect(hdbc: SQLHDBC): SQLRETURN;
begin
  Result := ZPlainDb2.SQLDisconnect(hdbc);
end;

function TZDb2PlainDriver.SQLEndTran(fHandleType: SQLSMALLINT;
  hHandle: SQLHANDLE; fType: SQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLEndTran(fHandleType, hHandle, fType);
end;

function TZDb2PlainDriver.SQLError(henv: SQLHENV; hdbc: SQLHDBC;
  hstmt: SQLHSTMT; szSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER;
  szErrorMsg: PSQLCHAR; cbErrorMsgMax: SQLSMALLINT;
  pcbErrorMsg: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLError(henv, hdbc, hstmt, szSqlState, pfNativeError,
    szErrorMsg, cbErrorMsgMax, pcbErrorMsg);
end;

function TZDb2PlainDriver.SQLExecDirect(hstmt: SQLHSTMT;
  szSqlStr: PSQLCHAR; cbSqlStr: SQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLExecDirect(hstmt, szSqlStr, cbSqlStr);
end;

function TZDb2PlainDriver.SQLExecute(hstmt: SQLHSTMT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLExecute(hstmt);
end;

function TZDb2PlainDriver.SQLFetch(hstmt: SQLHSTMT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLFetch(hstmt);
end;

function TZDb2PlainDriver.SQLFetchScroll(StatementHandle: SQLHSTMT;
  FetchOrientation: SQLSMALLINT; FetchOffset: SQLLEN): SQLRETURN;
begin
  Result := ZPlainDb2.SQLFetchScroll(StatementHandle, FetchOrientation,
    FetchOffset);
end;

function TZDb2PlainDriver.SQLFreeConnect(hdbc: SQLHDBC): SQLRETURN;
begin
  Result := ZPlainDb2.SQLFreeConnect(hdbc);
end;

function TZDb2PlainDriver.SQLFreeEnv(henv: SQLHENV): SQLRETURN;
begin
  Result := ZPlainDb2.SQLFreeEnv(henv);
end;

function TZDb2PlainDriver.SQLFreeHandle(fHandleType: SQLSMALLINT;
  hHandle: SQLHANDLE): SQLRETURN;
begin
  Result := ZPlainDb2.SQLFreeHandle(fHandleType, hHandle);
end;

function TZDb2PlainDriver.SQLFreeStmt(hstmt: SQLHSTMT;
  fOption: SQLUSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLFreeStmt(hstmt, fOption);
end;

function TZDb2PlainDriver.SQLGetCursorName(hstmt: SQLHSTMT;
  szCursor: PSQLCHAR; cbCursorMax: SQLSMALLINT;
  pcbCursor: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetCursorName(hstmt, szCursor, cbCursorMax,
    pcbCursor);
end;

function TZDb2PlainDriver.SQLGetData(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
  fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
  pcbValue: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetData(hstmt, icol, fCType,  rgbValue, cbValueMax,
    pcbValue);
end;

function TZDb2PlainDriver.SQLGetDescField(DescriptorHandle: SQLHDESC;
  RecNumber, FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER;
  BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetDescField(DescriptorHandle, RecNumber,
    FieldIdentifier, Value, BufferLength, StringLength)
end;

function TZDb2PlainDriver.SQLGetDescRec(DescriptorHandle: SQLHDESC;
  RecNumber: SQLSMALLINT; Name: PSQLCHAR; BufferLength: SQLSMALLINT;
  StringLength, _Type, SubType: PSQLSMALLINT; Length: PSQLINTEGER;
  Precision, Scale, Nullable: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetDescRec(DescriptorHandle, RecNumber, Name,
    BufferLength, StringLength, _Type, SubType, Length, Precision,
    Scale, Nullable);
end;

function TZDb2PlainDriver.SQLGetDiagField(fHandleType: SQLSMALLINT;
  hHandle: SQLHANDLE; iRecNumber, fDiagIdentifier: SQLSMALLINT;
  pDiagInfo: SQLPOINTER; cbDiagInfoMax: SQLSMALLINT;
  pcbDiagInfo: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetDiagField(fHandleType, hHandle, iRecNumber,
    fDiagIdentifier, pDiagInfo, cbDiagInfoMax, pcbDiagInfo);
end;

function TZDb2PlainDriver.SQLGetDiagRec(fHandleType: SQLSMALLINT;
  hHandle: SQLHANDLE; iRecNumber: SQLSMALLINT; pszSqlState: PSQLCHAR;
  pfNativeError: PSQLINTEGER; pszErrorMsg: PSQLCHAR;
  cbErrorMsgMax: SQLSMALLINT; pcbErrorMsg: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetDiagRec(fHandleType, hHandle, iRecNumber,
    pszSqlState, pfNativeError,  pszErrorMsg, cbErrorMsgMax, pcbErrorMsg);
end;

function TZDb2PlainDriver.SQLGetFunctions(hdbc: SQLHDBC;
  fFunction: SQLUSMALLINT; pfExists: PSQLUSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetFunctions(hdbc, fFunction, pfExists);
end;

function TZDb2PlainDriver.SQLGetInfo(hdbc: SQLHDBC;
  fInfoType: SQLUSMALLINT; rgbInfoValue: SQLPOINTER;
  cbInfoValueMax: SQLSMALLINT; pcbInfoValue: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetInfo(hdbc, fInfoType, rgbInfoValue,
    cbInfoValueMax, pcbInfoValue)
end;

function TZDb2PlainDriver.SQLGetLength(hstmt: SQLHSTMT;
  LocatorCType: SQLSMALLINT; Locator: SQLINTEGER; StringLength,
  IndicatorValue: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetLength(hstmt, LocatorCType, Locator,
    StringLength, IndicatorValue);
end;

function TZDb2PlainDriver.SQLGetPosition(hstmt: SQLHSTMT;
  LocatorCType: SQLSMALLINT; SourceLocator, SearchLocator: SQLINTEGER;
  SearchLiteral: PSQLCHAR; SearchLiteralLength: SQLINTEGER;
  FromPosition: SQLUINTEGER; LocatedAt: PSQLUINTEGER;
  IndicatorValue: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetPosition(hstmt, LocatorCType, SourceLocator,
    SearchLocator,SearchLiteral, SearchLiteralLength, FromPosition,
    LocatedAt, IndicatorValue);
end;

function TZDb2PlainDriver.SQLGetStmtAttr(StatementHandle: SQLHSTMT;
  Attribute: SQLINTEGER; Value: SQLPOINTER; BufferLength,
  StringLength: SQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetStmtAttr(StatementHandle, Attribute, Value,
    BufferLength, StringLength);
end;

function TZDb2PlainDriver.SQLGetStmtOption(hstmt: SQLHSTMT;
  fOption: SQLUSMALLINT; pvParam: SQLPOINTER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetStmtOption(hstmt, fOption, pvParam);
end;

function TZDb2PlainDriver.SQLGetSubString(hstmt: SQLHSTMT;
  LocatorCType: SQLSMALLINT; SourceLocator: SQLINTEGER; FromPosition,
  ForLength: SQLUINTEGER; TargetCType: SQLSMALLINT; rgbValue: SQLPOINTER;
  cbValueMax: SQLINTEGER; StringLength,
  IndicatorValue: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetSubString(hstmt, LocatorCType, SourceLocator,
    FromPosition, ForLength, TargetCType, rgbValue, cbValueMax,
    StringLength, IndicatorValue);
end;

function TZDb2PlainDriver.SQLGetTypeInfo(hstmt: SQLHSTMT;
  fSqlType: SQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLGetTypeInfo(hstmt, fSqlType);
end;

function TZDb2PlainDriver.SQLNextResult(hstmtSource,
  hstmtTarget: SQLHSTMT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLNextResult(hstmtSource, hstmtTarget);
end;

function TZDb2PlainDriver.SQLNumResultCols(hstmt: SQLHSTMT;
  pccol: PSQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLNumResultCols(hstmt, pccol);
end;

function TZDb2PlainDriver.SQLParamData(hstmt: SQLHSTMT;
  prgbValue: SQLPOINTER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLParamData(hstmt, prgbValue);
end;

function TZDb2PlainDriver.SQLPrepare(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
  cbSqlStr: SQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLPrepare(hstmt, szSqlStr, cbSqlStr);
end;

function TZDb2PlainDriver.SQLPutData(hstmt: SQLHSTMT; rgbValue: SQLPOINTER;
  cbValue: SQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLPutData(hstmt, rgbValue, cbValue);
end;

function TZDb2PlainDriver.SQLRowCount(hstmt: SQLHSTMT;
  pcrow: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLRowCount(hstmt, pcrow);
end;

function TZDb2PlainDriver.SQLSetConnectAttr(hdbc: SQLHDBC;
  fOption: SQLINTEGER; pvParam: SQLPOINTER;
  fStrLen: SQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSetConnectAttr(hdbc, fOption, pvParam, fStrLen);
end;

function TZDb2PlainDriver.SQLSetConnectOption(hdbc: SQLHDBC;
  fOption: SQLUSMALLINT; vParam: SQLUINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSetConnectOption(hdbc, fOption, vParam);
end;

function TZDb2PlainDriver.SQLSetCursorName(hstmt: SQLHSTMT;
  szCursor: PSQLCHAR; cbCursor: SQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSetCursorName(hstmt, szCursor, cbCursor);
end;

function TZDb2PlainDriver.SQLSetDescField(DescriptorHandle: SQLHDESC;
  RecNumber, FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER;
  BufferLength: SQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSetDescField(DescriptorHandle, RecNumber,
    FieldIdentifier, Value, BufferLength);
end;

function TZDb2PlainDriver.SQLSetDescRec(DescriptorHandle: SQLHDESC;
  RecNumber, _Type, SubType: SQLSMALLINT; Length: SQLINTEGER; Precision,
  Scale: SQLSMALLINT; Data: SQLPOINTER; StringLength,
  Indicator: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSetDescRec(DescriptorHandle,
    RecNumber, _Type, SubType, Length, Precision,
    Scale, Data, StringLength, Indicator)
end;

function TZDb2PlainDriver.SQLSetParam(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
  fCType, fSqlType: SQLSMALLINT; cbParamDef: SQLUINTEGER;
  ibScale: SQLSMALLINT; rgbValue: SQLPOINTER;
  pcbValue: PSQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSetParam(hstmt, ipar, fCType, fSqlType, cbParamDef,
    ibScale, rgbValue, pcbValue);
end;

function TZDb2PlainDriver.SQLSetStmtAttr(hstmt: SQLHSTMT;
  fOption: SQLINTEGER; pvParam: SQLPOINTER;
  fStrLen: SQLINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSetStmtAttr(hstmt, fOption, pvParam, fStrLen);
end;

function TZDb2PlainDriver.SQLSetStmtOption(hstmt: SQLHSTMT;
  fOption: SQLUSMALLINT; vParam: SQLUINTEGER): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSetStmtOption(hstmt, fOption, vParam);
end;

function TZDb2PlainDriver.SQLSpecialColumns(hstmt: SQLHSTMT;
  fColType: SQLUSMALLINT; szCatalogName: PSQLCHAR;
  cbCatalogName: SQLUSMALLINT; szSchemaName: PSQLCHAR;
  cbSchemaName: SQLUSMALLINT; szTableName: PSQLCHAR; cbTableName, fScope,
  fNullable: SQLUSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLSpecialColumns(hstmt, fColType, szCatalogName,
    cbCatalogName, szSchemaName, cbSchemaName, szTableName, cbTableName,
    fScope, fNullable);
end;

function TZDb2PlainDriver.SQLStatistics(hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR; cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR; cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR;
  cbTableName: SQLSMALLINT; fUnique, fAccuracy: SQLUSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLStatistics(hstmt, szCatalogName, cbCatalogName,
    szSchemaName, cbSchemaName, szTableName, cbTableName, fUnique, fAccuracy)
end;

function TZDb2PlainDriver.SQLTables(hstmt: SQLHSTMT;
  szCatalogName: PSQLCHAR; cbCatalogName: SQLSMALLINT;
  szSchemaName: PSQLCHAR; cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR;
  cbTableName, szTableType, cbTableType: SQLSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLTables(hstmt, szCatalogName, cbCatalogName,
    szSchemaName, cbSchemaName, szTableName, cbTableName, szTableType,
    cbTableType);
end;

function TZDb2PlainDriver.SQLTransact(henv: SQLHENV; hdbc: SQLHDBC;
  fType: SQLUSMALLINT): SQLRETURN;
begin
  Result := ZPlainDb2.SQLTransact(henv, hdbc, fType)
end;

end.

