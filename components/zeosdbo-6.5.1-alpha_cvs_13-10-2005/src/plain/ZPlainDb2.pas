{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Plain interface to db2cli.dll              }
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

unit ZPlainDb2;

interface

{$I ZPlain.inc}

uses
  ZPlainDb2Driver,
{$IFNDEF UNIX}
  Windows,
{$ENDIF}
  ZPlainLoader;

{J+}

const
  WINDOWS_DLL_LOCATION = 'db2cli.dll';
  LINUX_DLL_LOCATION   = 'db2cli.so';

type
  TSQLAllocConnect = function(henv: SQLHENV; phdbc: PSQLHDBC): SQLRETURN; stdcall;

  TSQLAllocEnv = function(phenv: PSQLHENV): SQLRETURN; stdcall;

  TSQLAllocStmt = function(hdbc: SQLHDBC; phstmt: PSQLHSTMT): SQLRETURN; stdcall;

  TSQLAllocHandle = function(fHandleType: SQLSMALLINT; hInput: SQLHANDLE;
    phOutput: PSQLHANDLE): SQLRETURN; stdcall;

  TSQLBindCol = function(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
    fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLCancel = function(hstmt: SQLHSTMT): SQLRETURN; stdcall;

  TSQLColAttribute = function(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
    fDescType: SQLUSMALLINT; rgbDesc: SQLPOINTER; cbDescMax: SQLSMALLINT;
    pcbDesc: PSQLSMALLINT; pfDesc: SQLPOINTER): SQLRETURN; stdcall;

  TSQLConnect = function(hdbc: SQLHDBC; szDSN: PSQLCHAR; cbDSN: SQLSMALLINT;
    szUID: PSQLCHAR; cbUID: SQLSMALLINT; szAuthStr: PSQLCHAR;
    cbAuthStr: SQLSMALLINT): SQLRETURN; stdcall;

  TSQLDescribeCol = function(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
    szColName: PSQLCHAR; cbColNameMax: SQLSMALLINT; pcbColName: PSQLSMALLINT;
    pfSqlType: PSQLSMALLINT; pcbColDef: PSQLUINTEGER; pibScale: PSQLSMALLINT;
    pfNullable: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLDisconnect = function(hdbc: SQLHDBC): SQLRETURN; stdcall;

  TSQLError = function(henv: SQLHENV; hdbc: SQLHDBC; hstmt: SQLHSTMT;
    szSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER; szErrorMsg: PSQLCHAR;
    cbErrorMsgMax: SQLSMALLINT; pcbErrorMsg: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLExecDirect = function(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
    cbSqlStr: SQLINTEGER): SQLRETURN; stdcall;

  TSQLExecute = function(hstmt: SQLHSTMT): SQLRETURN; stdcall;

  TSQLFetch = function(hstmt: SQLHSTMT): SQLRETURN; stdcall;

  TSQLFreeConnect = function(hdbc: SQLHDBC): SQLRETURN; stdcall;

  TSQLFreeEnv = function(henv: SQLHENV): SQLRETURN; stdcall;

  TSQLFreeStmt = function(hstmt: SQLHSTMT; fOption: SQLUSMALLINT): SQLRETURN;
    stdcall;

  TSQLCloseCursor = function(hStmt: SQLHSTMT): SQLRETURN; stdcall;

  TSQLGetCursorName = function(hstmt: SQLHSTMT; szCursor: PSQLCHAR;
    cbCursorMax: SQLSMALLINT; pcbCursor: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLGetData = function(hstmt: SQLHSTMT; icol: SQLUSMALLINT;
    fCType: SQLSMALLINT; rgbValue: SQLPOINTER; cbValueMax: SQLINTEGER;
    pcbValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLNumResultCols = function(hstmt: SQLHSTMT; pccol: PSQLSMALLINT): SQLRETURN;
    stdcall;

  TSQLPrepare = function(hstmt: SQLHSTMT; szSqlStr: PSQLCHAR;
    cbSqlStr: SQLINTEGER): SQLRETURN; stdcall;

  TSQLRowCount = function(hstmt: SQLHSTMT; pcrow: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLSetCursorName = function(hstmt: SQLHSTMT; szCursor: PSQLCHAR;
    cbCursor: SQLSMALLINT): SQLRETURN; stdcall;

  TSQLSetParam = function(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
    fCType: SQLSMALLINT; fSqlType: SQLSMALLINT; cbParamDef: SQLUINTEGER;
    ibScale: SQLSMALLINT; rgbValue: SQLPOINTER; pcbValue: PSQLINTEGER):
    SQLRETURN; stdcall;

  TSQLTransact = function(henv: SQLHENV; hdbc: SQLHDBC; fType: SQLUSMALLINT):
    SQLRETURN; stdcall;

  TSQLEndTran = function(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
    fType: SQLSMALLINT): SQLRETURN; stdcall;

  TSQLFreeHandle = function(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE):
    SQLRETURN; stdcall;

  TSQLGetDiagRec = function(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
    iRecNumber: SQLSMALLINT; pszSqlState: PSQLCHAR; pfNativeError: PSQLINTEGER;
    pszErrorMsg: PSQLCHAR; cbErrorMsgMax: SQLSMALLINT;
    pcbErrorMsg: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLGetDiagField = function(fHandleType: SQLSMALLINT; hHandle: SQLHANDLE;
    iRecNumber: SQLSMALLINT; fDiagIdentifier: SQLSMALLINT; pDiagInfo: SQLPOINTER;
    cbDiagInfoMax: SQLSMALLINT; pcbDiagInfo: PSQLSMALLINT): SQLRETURN; stdcall;

  TSQLCopyDesc = function(hDescSource: SQLHDESC; hDescTarget: SQLHDESC):
    SQLRETURN; stdcall;

  TSQLGetDescField = function(DescriptorHandle: SQLHDESC;
    RecNumber: SQLSMALLINT; FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER;
    BufferLength: SQLINTEGER; StringLength: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLGetDescRec = function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
    Name: PSQLCHAR; BufferLength: SQLSMALLINT; StringLength: PSQLSMALLINT;
    _Type: PSQLSMALLINT; SubType: PSQLSMALLINT; Length: PSQLINTEGER;
    Precision: PSQLSMALLINT; Scale: PSQLSMALLINT;  Nullable: PSQLSMALLINT):
    SQLRETURN; stdcall;

  TSQLSetDescField = function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
    FieldIdentifier: SQLSMALLINT; Value: SQLPOINTER; BufferLength: SQLINTEGER):
    SQLRETURN; stdcall;

  TSQLSetDescRec = function(DescriptorHandle: SQLHDESC; RecNumber: SQLSMALLINT;
    _Type: SQLSMALLINT; SubType: SQLSMALLINT; Length: SQLINTEGER;
    Precision: SQLSMALLINT; Scale: SQLSMALLINT; Data: SQLPOINTER;
    StringLength: PSQLINTEGER; Indicator: PSQLINTEGER): SQLRETURN; stdcall;

  { New added functions }

  TSQLSetConnectAttr = function(hdbc: SQLHDBC; fOption: SQLINTEGER;
    pvParam: SQLPOINTER; fStrLen: SQLINTEGER): SQLRETURN; stdcall;

  TSQLSetConnectOption = function(hdbc: SQLHDBC; fOption: SQLUSMALLINT;
    vParam: SQLUINTEGER): SQLRETURN; stdcall;

  TSQLSetStmtAttr = function(hstmt: SQLHSTMT; fOption: SQLINTEGER;
    pvParam: SQLPOINTER; fStrLen: SQLINTEGER): SQLRETURN; stdcall;

  TSQLSetStmtOption = function(hstmt: SQLHSTMT; fOption: SQLUSMALLINT;
    vParam: SQLUINTEGER): SQLRETURN; stdcall;

  TSQLGetSubString = function(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
    SourceLocator: SQLINTEGER; FromPosition: SQLUINTEGER;
    ForLength: SQLUINTEGER; TargetCType: SQLSMALLINT; rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER; StringLength: PSQLINTEGER;
    IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLGetLength = function(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
    Locator: SQLINTEGER; StringLength: PSQLINTEGER;
    IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLGetPosition = function(hstmt: SQLHSTMT; LocatorCType: SQLSMALLINT;
    SourceLocator: SQLINTEGER; SearchLocator: SQLINTEGER; SearchLiteral: PSQLCHAR;
    SearchLiteralLength: SQLINTEGER; FromPosition: SQLUINTEGER;
    LocatedAt: PSQLUINTEGER; IndicatorValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLBindParameter = function(hstmt: SQLHSTMT; ipar: SQLUSMALLINT;
    fParamType: SQLSMALLINT; fCType: SQLSMALLINT; fSqlType: SQLSMALLINT;
    cbColDef: SQLUINTEGER; ibScale: SQLSMALLINT; rgbValue: SQLPOINTER;
    cbValueMax: SQLINTEGER; pcbValue: PSQLINTEGER): SQLRETURN; stdcall;

  TSQLParamData = function(hstmt: SQLHSTMT; prgbValue: SQLPOINTER):
    SQLRETURN; stdcall;

  TSQLPutData = function(hstmt: SQLHSTMT; rgbValue: SQLPOINTER;
    cbValue: SQLINTEGER): SQLRETURN; stdcall;

  {add new1 functions}
  TSQLColumns = function(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR; cbSchemaName: SQLSMALLINT;
    szTableName: PSQLCHAR; cbTableName: SQLSMALLINT; szColumnName: PSQLCHAR;
    cbColumnName: SQLSMALLINT): SQLRETURN; stdcall;
  TSQLDataSources = function(henv: SQLHENV; fDirection: SQLUSMALLINT; szDSN: PSQLCHAR;
    cbDSNMax: SQLSMALLINT; pcbDSN: PSQLSMALLINT; szDescription: PSQLCHAR;
    cbDescriptionMax: SQLSMALLINT; pcbDescription: PSQLSMALLINT): SQLRETURN; stdcall;
  TSQLFetchScroll = function(StatementHandle: SQLHSTMT;
    FetchOrientation: SQLSMALLINT; FetchOffset: SQLLEN): SQLRETURN; stdcall;
  TSQLGetFunctions = function(hdbc: SQLHDBC; fFunction: SQLUSMALLINT;
    pfExists: PSQLUSMALLINT): SQLRETURN; stdcall;
  TSQLGetInfo = function(hdbc: SQLHDBC; fInfoType: SQLUSMALLINT;
    rgbInfoValue: SQLPOINTER; cbInfoValueMax: SQLSMALLINT;
    pcbInfoValue: PSQLSMALLINT): SQLRETURN; stdcall;
  TSQLGetStmtAttr = function(StatementHandle: SQLHSTMT; Attribute: SQLINTEGER;
    Value: SQLPOINTER; BufferLength: SQLINTEGER;
    StringLength: SQLINTEGER): SQLRETURN; stdcall;
  TSQLGetStmtOption = function(hstmt: SQLHSTMT; fOption: SQLUSMALLINT;
    pvParam: SQLPOINTER): SQLRETURN; stdcall;
  TSQLGetTypeInfo = function(hstmt: SQLHSTMT; fSqlType: SQLSMALLINT): SQLRETURN; stdcall;
  TSQLSpecialColumns = function(hstmt: SQLHSTMT; fColType: SQLUSMALLINT;
    szCatalogName: PSQLCHAR; cbCatalogName: SQLUSMALLINT;
    szSchemaName: PSQLCHAR; cbSchemaName: SQLUSMALLINT; szTableName: PSQLCHAR;
    cbTableName: SQLUSMALLINT; fScope: SQLUSMALLINT;
    fNullable: SQLUSMALLINT): SQLRETURN; stdcall;
  TSQLStatistics = function(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR; cbTableName: SQLSMALLINT;
    fUnique: SQLUSMALLINT; fAccuracy: SQLUSMALLINT): SQLRETURN; stdcall;
  TSQLTables = function(hstmt: SQLHSTMT; szCatalogName: PSQLCHAR;
    cbCatalogName: SQLSMALLINT; szSchemaName: PSQLCHAR;
    cbSchemaName: SQLSMALLINT; szTableName: PSQLCHAR; cbTableName: SQLSMALLINT;
    szTableType: SQLSMALLINT; cbTableType: SQLSMALLINT): SQLRETURN; stdcall;
  TSQLNextResult = function(hstmtSource: SQLHSTMT;
    hstmtTarget: SQLHSTMT): SQLRETURN; stdcall;

{************* Plain API Function variables definition ************}
var
  SQLAllocConnect: TSQLAllocConnect;
  SQLAllocEnv: TSQLAllocEnv;
  SQLAllocStmt: TSQLAllocStmt;
  SQLAllocHandle: TSQLAllocHandle;
  SQLBindCol: TSQLBindCol;
  SQLCancel: TSQLCancel;
  SQLColAttribute: TSQLColAttribute;
  SQLConnect: TSQLConnect;
  SQLDescribeCol: TSQLDescribeCol;
  SQLDisconnect: TSQLDisconnect;
  SQLError: TSQLError;
  SQLExecDirect: TSQLExecDirect;
  SQLExecute: TSQLExecute;
  SQLFetch: TSQLFetch;
  SQLFreeConnect: TSQLFreeConnect;
  SQLFreeEnv: TSQLFreeEnv;
  SQLFreeStmt: TSQLFreeStmt;
  SQLCloseCursor: TSQLCloseCursor;
  SQLGetCursorName: TSQLGetCursorName;
  SQLGetData: TSQLGetData;
  SQLNumResultCols: TSQLNumResultCols;
  SQLPrepare: TSQLPrepare;
  SQLRowCount: TSQLRowCount;
  SQLSetCursorName: TSQLSetCursorName;
  SQLSetParam: TSQLSetParam;
  SQLTransact: TSQLTransact;
  SQLEndTran: TSQLEndTran;
  SQLFreeHandle: TSQLFreeHandle;
  SQLGetDiagRec: TSQLGetDiagRec;
  SQLGetDiagField: TSQLGetDiagField;
  SQLCopyDesc: TSQLCopyDesc;
  SQLGetDescField: TSQLGetDescField;
  SQLGetDescRec: TSQLGetDescRec;
  SQLSetDescField: TSQLSetDescField;
  SQLSetDescRec: TSQLSetDescRec;
  { New added functions }
  SQLSetConnectAttr: TSQLSetConnectAttr;
  SQLSetConnectOption: TSQLSetConnectOption;
  SQLSetStmtAttr: TSQLSetStmtAttr;
  SQLSetStmtOption: TSQLSetStmtOption;
  SQLGetSubString: TSQLGetSubString;
  SQLGetLength: TSQLGetLength;
  SQLGetPosition: TSQLGetPosition;
  SQLBindParameter: TSQLBindParameter;
  SQLParamData: TSQLParamData;
  SQLPutData: TSQLPutData;
  {add new1 functions}
  SQLColumns: TSQLColumns;
  SQLDataSources: TSQLDataSources;
  SQLFetchScroll: TSQLFetchScroll;
  SQLGetFunctions: TSQLGetFunctions;
  SQLGetInfo: TSQLGetInfo;
  SQLGetStmtAttr: TSQLGetStmtAttr;
  SQLGetStmtOption: TSQLGetStmtOption;
  SQLGetTypeInfo: TSQLGetTypeInfo;
  SQLSpecialColumns: TSQLSpecialColumns;
  SQLStatistics: TSQLStatistics;
  SQLTables: TSQLTables;
  SQLNextResult: TSQLNextResult;

var
  LibraryLoader: TZNativeLibraryLoader;

implementation

type
   {** Implements a loader for DB2 native library. }
  TZDb2NativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
  end;

{ TZDb2NativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZDb2NativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;
  
  SQLAllocConnect := GetAddress('SQLAllocConnect');
  SQLAllocEnv := GetAddress('SQLAllocEnv');
  SQLAllocStmt := GetAddress('SQLAllocStmt');
  SQLAllocHandle := GetAddress('SQLAllocHandle');
  SQLBindCol := GetAddress('SQLBindCol');
  SQLCancel := GetAddress('SQLCancel');
  SQLColAttribute := GetAddress('SQLColAttribute');
  SQLConnect := GetAddress('SQLConnect');
  SQLDescribeCol := GetAddress('SQLDescribeCol');
  SQLDisconnect := GetAddress('SQLDisconnect');
  SQLError := GetAddress('SQLError');
  SQLExecDirect := GetAddress('SQLExecDirect');
  SQLExecute := GetAddress('SQLExecute');
  SQLFetch := GetAddress('SQLFetch');
  SQLFreeConnect := GetAddress('SQLFreeConnect');
  SQLFreeEnv := GetAddress('SQLFreeEnv');
  SQLFreeStmt := GetAddress('SQLFreeStmt');
  SQLCloseCursor := GetAddress('SQLCloseCursor');
  SQLGetCursorName := GetAddress('SQLGetCursorName');
  SQLGetData := GetAddress('SQLGetData');
  SQLNumResultCols := GetAddress('SQLNumResultCols');
  SQLPrepare := GetAddress('SQLPrepare');
  SQLRowCount := GetAddress('SQLRowCount');
  SQLSetCursorName := GetAddress('SQLSetCursorName');
  SQLSetParam := GetAddress('SQLSetParam');
  SQLTransact := GetAddress('SQLTransact');
  SQLEndTran := GetAddress('SQLEndTran');
  SQLFreeHandle := GetAddress('SQLFreeHandle');
  SQLGetDiagRec := GetAddress('SQLGetDiagRec');
  SQLGetDiagField := GetAddress('SQLGetDiagField');
  SQLCopyDesc := GetAddress('SQLCopyDesc');
  SQLGetDescField := GetAddress('SQLGetDescField');
  SQLGetDescRec := GetAddress('SQLGetDescRec');
  SQLSetDescField := GetAddress('SQLSetDescField');
  SQLSetDescRec := GetAddress('SQLSetDescRec');
  { New added functions }
  SQLSetConnectAttr := GetAddress('SQLSetConnectAttr');
  SQLSetConnectOption := GetAddress('SQLSetConnectOption');
  SQLSetStmtAttr := GetAddress('SQLSetStmtAttr');
  SQLSetStmtOption := GetAddress('SQLSetStmtOption');
  SQLGetSubString := GetAddress('SQLGetSubString');
  SQLGetLength := GetAddress('SQLGetLength');
  SQLGetPosition := GetAddress('SQLGetPosition');
  SQLBindParameter := GetAddress('SQLBindParameter');
  SQLParamData := GetAddress('SQLParamData');
  SQLPutData := GetAddress('SQLPutData');
  {add new1 functions}
  SQLColumns := GetAddress('SQLColumns');
  SQLDataSources := GetAddress('SQLDataSources');
  SQLFetchScroll := GetAddress('SQLFetchScroll');
  SQLGetFunctions := GetAddress('SQLGetFunctions');
  SQLGetInfo := GetAddress('SQLGetInfo');
  SQLGetStmtAttr := GetAddress('SQLGetStmtAttr');
  SQLGetStmtOption := GetAddress('SQLGetStmtOption');
  SQLGetTypeInfo := GetAddress('SQLGetTypeInfo');
  SQLSpecialColumns := GetAddress('SQLSpecialColumns');
  SQLStatistics := GetAddress('SQLStatistics');
  SQLTables := GetAddress('SQLTables');
  SQLNextResult := GetAddress('SQLNextResult');
end;

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZDb2NativeLibraryLoader.Create(
    [WINDOWS_DLL_LOCATION]);
{$ELSE}
  LibraryLoader := TZDb2NativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION]);
{$ENDIF}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
end.
