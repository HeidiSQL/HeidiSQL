//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ZParseSql.res");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("ZCore.bpi");
USEUNIT("..\..\src\parsesql\ZSybaseToken.pas");
USEUNIT("..\..\src\parsesql\ZGenericSqlAnalyser.pas");
USEUNIT("..\..\src\parsesql\ZGenericSqlToken.pas");
USEUNIT("..\..\src\parsesql\ZInterbaseAnalyser.pas");
USEUNIT("..\..\src\parsesql\ZInterbaseToken.pas");
USEUNIT("..\..\src\parsesql\ZMySqlAnalyser.pas");
USEUNIT("..\..\src\parsesql\ZMySqlToken.pas");
USEUNIT("..\..\src\parsesql\ZPostgreSqlAnalyser.pas");
USEUNIT("..\..\src\parsesql\ZPostgreSqlToken.pas");
USEUNIT("..\..\src\parsesql\ZScriptParser.pas");
USEUNIT("..\..\src\parsesql\ZSelectSchema.pas");
USEUNIT("..\..\src\parsesql\ZSybaseAnalyser.pas");
USEUNIT("..\..\src\parsesql\ZSqLiteToken.pas");
USEUNIT("..\..\src\parsesql\ZOracleAnalyser.pas");
USEUNIT("..\..\src\parsesql\ZOracleToken.pas");
USEUNIT("..\..\src\parsesql\ZSqLiteAnalyser.pas");
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
