//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ZDbc.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclado50.bpi");
USEPACKAGE("ZPlain.bpi");
USEPACKAGE("ZParseSql.bpi");
USEPACKAGE("ZCore.bpi");
USEUNIT("..\..\src\dbc\ZDbcUtils.pas");
USEUNIT("..\..\src\dbc\ZDbcAdo.pas");
USEUNIT("..\..\src\dbc\ZDbcAdoMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcAdoResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcAdoStatement.pas");
USEUNIT("..\..\src\dbc\ZDbcAdoUtils.pas");
USEUNIT("..\..\src\dbc\ZDbcCache.pas");
USEUNIT("..\..\src\dbc\ZDbcCachedResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcConnection.pas");
USEUNIT("..\..\src\dbc\ZDbcDbLib.pas");
USEUNIT("..\..\src\dbc\ZDbcDbLibMsSqlMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcDbLibResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcDbLibStatement.pas");
USEUNIT("..\..\src\dbc\ZDbcDbLibSybaseMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcDbLibUtils.pas");
USEUNIT("..\..\src\dbc\ZDbcGenericResolver.pas");
USEUNIT("..\..\src\dbc\ZDbcInterbase6.pas");
USEUNIT("..\..\src\dbc\ZDbcInterbase6Metadata.pas");
USEUNIT("..\..\src\dbc\ZDbcInterbase6ResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcInterbase6Statement.pas");
USEUNIT("..\..\src\dbc\ZDbcInterbase6Utils.pas");
USEUNIT("..\..\src\dbc\ZDbcIntfs.pas");
USEUNIT("..\..\src\dbc\ZDbcLogging.pas");
USEUNIT("..\..\src\dbc\ZDbcMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcMySql.pas");
USEUNIT("..\..\src\dbc\ZDbcMySqlMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcMySqlResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcMySqlStatement.pas");
USEUNIT("..\..\src\dbc\ZDbcMySqlUtils.pas");
USEUNIT("..\..\src\dbc\ZDbcPostgreSql.pas");
USEUNIT("..\..\src\dbc\ZDbcPostgreSqlMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcPostgreSqlResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcPostgreSqlStatement.pas");
USEUNIT("..\..\src\dbc\ZDbcPostgreSqlUtils.pas");
USEUNIT("..\..\src\dbc\ZDbcResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcResultSetMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcStatement.pas");
USEUNIT("..\..\src\dbc\ZDbcSqLiteUtils.pas");
USEUNIT("..\..\src\dbc\ZDbcOracle.pas");
USEUNIT("..\..\src\dbc\ZDbcOracleMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcOracleResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcOracleStatement.pas");
USEUNIT("..\..\src\dbc\ZDbcOracleUtils.pas");
USEUNIT("..\..\src\dbc\ZDbcSqLite.pas");
USEUNIT("..\..\src\dbc\ZDbcSqLiteMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcSqLiteResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcSqLiteStatement.pas");
USEUNIT("..\..\src\dbc\ZDbcASA.pas");
USEUNIT("..\..\src\dbc\ZDbcASAMetadata.pas");
USEUNIT("..\..\src\dbc\ZDbcASAResultSet.pas");
USEUNIT("..\..\src\dbc\ZDbcASAStatement.pas");
USEUNIT("..\..\src\dbc\ZDbcASAUtils.pas");

//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
