//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("ZCore.res");
USEPACKAGE("vcl50.bpi");
USEUNIT("..\..\src\core\ZVariant.pas");
USEUNIT("..\..\src\core\ZClasses.pas");
USEUNIT("..\..\src\core\ZCollections.pas");
USEUNIT("..\..\src\core\ZCompatibility.pas");
USEUNIT("..\..\src\core\ZExpression.pas");
USEUNIT("..\..\src\core\ZExprParser.pas");
USEUNIT("..\..\src\core\ZExprToken.pas");
USEUNIT("..\..\src\core\ZFunctions.pas");
USEUNIT("..\..\src\core\ZMatchPattern.pas");
USEUNIT("..\..\src\core\ZMessages.pas");
USEUNIT("..\..\src\core\ZSysUtils.pas");
USEUNIT("..\..\src\core\ZTokenizer.pas");
USEUNIT("..\..\src\core\ZVariables.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
