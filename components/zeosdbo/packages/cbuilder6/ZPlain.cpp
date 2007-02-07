//---------------------------------------------------------------------------



#include <vcl.h>

#pragma hdrstop

USERES("ZPlain.res");

USEPACKAGE("rtl.bpi");

USEPACKAGE("ZCore.bpi");

USEUNIT("..\..\src\plain\ZPlainPostgreSqlDriver.pas");

USEUNIT("..\..\src\plain\ZPlainAdo.pas");

USEUNIT("..\..\src\plain\ZPlainAdoDriver.pas");

USEUNIT("..\..\src\plain\ZPlainDb2.pas");

USEUNIT("..\..\src\plain\ZPlainDb2Driver.pas");

USEUNIT("..\..\src\plain\ZPlainDbLibDriver.pas");

USEUNIT("..\..\src\plain\ZPlainDbLibMsSql7.pas");

USEUNIT("..\..\src\plain\ZPlainDbLibSybaseAse125.pas");

USEUNIT("..\..\src\plain\ZPlainDriver.pas");

USEUNIT("..\..\src\plain\ZPlainFirebird10.pas");

USEUNIT("..\..\src\plain\ZPlainFirebird15.pas");

USEUNIT("..\..\src\plain\ZPlainFirebird20.pas");

USEUNIT("..\..\src\plain\ZPlainInterbase5.pas");

USEUNIT("..\..\src\plain\ZPlainInterbase6.pas");

USEUNIT("..\..\src\plain\ZPlainInterbaseDriver.pas");

USEUNIT("..\..\src\plain\ZPlainLoader.pas");

USEUNIT("..\..\src\plain\ZPlainMySql40.pas");

USEUNIT("..\..\src\plain\ZPlainMySql41.pas");

USEUNIT("..\..\src\plain\ZPlainMySql320.pas");

USEUNIT("..\..\src\plain\ZPlainMySql323.pas");

USEUNIT("..\..\src\plain\ZPlainMySqlDriver.pas");

USEUNIT("..\..\src\plain\ZPlainPostgreSql7.pas");

USEUNIT("..\..\src\plain\ZPlainPostgreSql8.pas");

USEUNIT("..\..\src\plain\ZPlainSqLiteDriver.pas");

USEUNIT("..\..\src\plain\ZPlainOracle9i.pas");

USEUNIT("..\..\src\plain\ZPlainOracleDriver.pas");

USEUNIT("..\..\src\plain\ZPlainSqLite28.pas");

USEUNIT("..\..\src\plain\ZPlainSqLite3.pas");

USEUNIT("..\..\src\plain\ZPlainASADriver.pas");

USEUNIT("..\..\src\plain\ZPlainASA9.pas");

USEUNIT("..\..\src\plain\ZPlainASA8.pas");

USEUNIT("..\..\src\plain\ZPlainASA7.pas");

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

