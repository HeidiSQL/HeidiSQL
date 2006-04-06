{****************************************************************************}
{*                                                                          *}
{*  Dateiname:     ODBCUTIL.PAS                                             *}
{*                                                                          *}
{*  Beschreibung:  Funktionen für ODBC-Treiber und -Datenquellen            *}
{*                                                                          *}
{*  Autor:         Dietmar Zucker                                           *}
{*                 Frankenstraße 5                                          *}
{*                 D-78048 Villingen-Schwenningen                           *}
{*                                                                          *}
{*                 dietmar.zucker@t-online.de                               *}
{*                 http://home.t-online.de/home/dietmar.zucker              *}
{*                                                                          *}
{*  Copyright:     (c) 2001 Dietmar Zucker                                  *}
{*                                                                          *}
{****************************************************************************}

unit ODBCUtil;

interface

uses
  Classes;

function GetODBCDrivers( List: TStrings ): boolean;
function GetODBCDataSourceNames( List: TStrings ): boolean;

function ODBCDataSourceExists( Name: string ): boolean;

implementation

uses
  SysUtils;

{****************************************************************************}
{*  Allgemeine Definitionen                                                 *}
{****************************************************************************}

const
  SQL_SUCCESS           = 0;
  SQL_SUCCESS_WITH_INFO = 1;
  SQL_NO_DATA           = 100;
  SQL_INVALID_HANDLE    = -2;

type
  SQLHANDLE    = pointer;
  SQLPHANDLE   = ^SQLHANDLE;
  SQLHENV      = SQLHANDLE;
  SQLPCHAR     = pchar;
  SQLUSMALLINT = word;
  SQLSMALLINT  = smallint;
  SQLPSMALLINT = ^SQLSMALLINT;
  SQLINTEGER   = longint;
  SQLPINTEGER  = ^SQLINTEGER;
  SQLPOINTER   = pointer;
  SQLRETURN    = SQLSMALLINT;

{****************************************************************************}
{*  Definitionen für SQLAllocHandle()                                       *}
{****************************************************************************}

const
  SQL_HANDLE_ENV  = 1;
  SQL_HANDLE_DBC  = 2;
  SQL_HANDLE_STMT = 3;
  SQL_HANDLE_DESC = 4;

{****************************************************************************}
{*  Definitionen für SQLSetEnvAttr()                                        *}
{****************************************************************************}

const
  SQL_ATTR_ODBC_VERSION       = 200;
  SQL_ATTR_CONNECTION_POOLING = 201;
  SQL_ATTR_CP_MATCH           = 202;

  SQL_OV_ODBC2                = 2;
  SQL_OV_ODBC3                = 3;

  SQL_IS_POINTER              = -4;
  SQL_IS_UINTEGER             = -5;
  SQL_IS_INTEGER              = -6;
  SQL_IS_USMALLINT            = -7;
  SQL_IS_SMALLINT             = -8;

{****************************************************************************}
{*  Definitionen für SQLDrivers(), SQLDataSources()                         *}
{****************************************************************************}

const
  SQL_FETCH_NEXT         = 1;
  SQL_FETCH_FIRST        = 2;
  SQL_FETCH_FIRST_USER   = 31;
  SQL_FETCH_FIRST_SYSTEM = 32;

{****************************************************************************}
{*  ODBC-API-Funktionen                                                     *}
{****************************************************************************}

function SQLAllocHandle(
           HandleType: SQLSMALLINT;
           InputHandle: SQLHANDLE;
           OutputHandlePtr: SQLPHANDLE ): SQLSMALLINT;
           stdcall; external 'ODBC32.DLL' name 'SQLAllocHandle';

function SQLFreeHandle(
           HandleType: SQLSMALLINT;
           Handle: SQLHANDLE ): SQLSMALLINT;
           stdcall; external 'ODBC32.DLL' name 'SQLFreeHandle';

function SQLSetEnvAttr(
           EnvironmentHandle: SQLHENV;
           Attribute: SQLINTEGER;
           ValuePtr: SQLINTEGER;
           StringLength: SQLPOINTER ): SQLSMALLINT;
           stdcall; external 'ODBC32.DLL' name 'SQLSetEnvAttr';

function SQLDrivers(
           EnvironmentHandle: SQLHENV;
           Direction: SQLUSMALLINT;
           DriverDesc: SQLPCHAR;
           DriverDescMax: SQLSMALLINT;
           DriverDescLenPtr: SQLPSMALLINT;
           DriverAttributes: SQLPCHAR;
           DriverAttributesMax: SQLSMALLINT;
           DriverAttributesLenPtr: SQLPSMALLINT ): SQLSMALLINT;
           stdcall; external 'ODBC32.DLL' name 'SQLDrivers';

function SQLDataSources(
           EnvironmentHandle: SQLHENV;
           Direction: SQLUSMALLINT;
           ServerName: SQLPCHAR;
           BufferLength1: SQLSMALLINT;
           NameLength1Ptr: SQLPSMALLINT;
           Description: SQLPCHAR;
           BufferLength2: SQLSMALLINT;
           NameLength2Ptr: SQLPSMALLINT ): SQLSMALLINT;
           stdcall; external 'ODBC32.DLL' name 'SQLDataSources';

{****************************************************************************}
{*                                                                          *}
{*  Name:          GetODBCDrivers()                                         *}
{*                                                                          *}
{*  Beschreibung:  Ermittelt die Namen aller verfügbaren ODBC-Treiber.      *}
{*                                                                          *}
{*  Input:         List - Stringliste zum Eintragen der Treiber             *}
{*                                                                          *}
{*  Return:        true  - Ermitteln der Treiber erfolgreich                *}
{*                 false - sonst                                            *}
{*                                                                          *}
{****************************************************************************}

function GetODBCDrivers( List: TStrings ): boolean;
var
  ptrHEnv:      pointer;
  chrsDriver:   array [0..128] of char;
  intDriverLen: smallint;
  chrsAttr:     array [0..256] of char;
  intAttrLen:   smallint;
  intRetCode:   smallint;
begin
  Result := false;

  List.BeginUpdate;
  List.Clear;

  // Environment-Handle allokieren
  intRetCode := SQLAllocHandle(SQL_HANDLE_ENV,nil,@ptrHEnv);

  if (intRetCode = SQL_SUCCESS) then begin
    // ODBC Version 3.x deklarieren
    SQLSetEnvAttr(ptrHEnv,SQL_ATTR_ODBC_VERSION,SQL_OV_ODBC3,
      SQLPOINTER(SQL_IS_INTEGER));

    // ersten Treiber suchen
    intRetCode := SQLDrivers(ptrHEnv,SQL_FETCH_FIRST,chrsDriver,
      sizeof(chrsDriver),@intDriverLen,chrsAttr,sizeof(chrsAttr),
      @intAttrLen);

    // Solange Treiber vorhanden...
    while (intRetCode in [SQL_SUCCESS,SQL_SUCCESS_WITH_INFO]) do begin
      // Treiber in Liste eintragen
      List.Add(StrPas(chrsDriver));

      // weiteren Treiber suchen
      intRetCode := SQLDrivers(ptrHEnv,SQL_FETCH_NEXT,chrsDriver,
        sizeof(chrsDriver),@intDriverLen,chrsAttr,sizeof(chrsAttr),
        @intAttrLen);
    end;

    if (intRetCode = SQL_NO_DATA) then
      Result := true;

    // Environment-Handle freigeben
    SQLFreeHandle(SQL_HANDLE_ENV,ptrHEnv);
  end;

  List.EndUpdate;
end;

{****************************************************************************}
{*                                                                          *}
{*  Name:          GetODBCDataSourceNames()                                 *}
{*                                                                          *}
{*  Beschreibung:  Ermittelt die Namen aller verfügbaren Benutzer- und      *}
{*                 System-Datenquellen.                                     *}
{*                                                                          *}
{*  Input:         List - Stringliste zum Eintragen der Datenquellen        *}
{*                                                                          *}
{*  Return:        true  - Ermitteln der Datenquellen erfolgreich           *}
{*                 false - sonst                                            *}
{*                                                                          *}
{****************************************************************************}

function GetODBCDataSourceNames( List: TStrings ): boolean;
var
  ptrHEnv:    pointer;
  chrsDSN:    array [0..32] of char;
  intDSNLen:  smallint;
  chrsDesc:   array [0..128] of char;
  intDescLen: smallint;
  intRetCode: smallint;
begin
  Result := false;

  List.BeginUpdate;
  List.Clear;

  // Environment-Handle allokieren
  intRetCode := SQLAllocHandle(SQL_HANDLE_ENV,nil,@ptrHEnv);

  if (intRetCode = SQL_SUCCESS) then begin
    // ODBC Version 3.x deklarieren
    SQLSetEnvAttr(ptrHEnv,SQL_ATTR_ODBC_VERSION,SQL_OV_ODBC3,
      SQLPOINTER(SQL_IS_INTEGER));

    // erste DataSource suchen
    intRetCode := SQLDataSources(ptrHEnv,SQL_FETCH_FIRST,chrsDSN,
      sizeof(chrsDSN),@intDSNLen,chrsDesc,sizeof(chrsDesc),@intDescLen);

    // Solange DataSource vorhanden...
    while (intRetCode in [SQL_SUCCESS,SQL_SUCCESS_WITH_INFO]) do begin
      // DataSource in Liste eintragen
      List.Add(StrPas(chrsDSN));

      // weitere DataSource suchen
      intRetCode := SQLDataSources(ptrHEnv,SQL_FETCH_NEXT,chrsDSN,
        sizeof(chrsDSN),@intDSNLen,chrsDesc,sizeof(chrsDesc),@intDescLen);
    end;

    if (intRetCode = SQL_NO_DATA) then
      Result := true;

    // Environment-Handle freigeben
    SQLFreeHandle(SQL_HANDLE_ENV,ptrHEnv);
  end;

  List.EndUpdate;
end;

{****************************************************************************}
{*                                                                          *}
{*  Name:          ODBCDataSourceExists()                                   *}
{*                                                                          *}
{*  Beschreibung:  Prüft, ob die übergebene ODBC-Datenquelle existiert.     *}
{*                                                                          *}
{*  Input:         Name - Name der Datenquelle                              *}
{*                                                                          *}
{*  Return:        true  - Datenquelle existiert                            *}
{*                 false - sonst                                            *}
{*                                                                          *}
{****************************************************************************}

function ODBCDataSourceExists( Name: string ): boolean;
var
  List: TStringList;
begin
  Result := false;
  List := TStringList.Create;

  try
    if (GetODBCDataSourceNames(List)) then
      if (List.IndexOf(Name) >= 0) then
         Result := true;
  finally
    List.Free;
  end;
end;

end.
