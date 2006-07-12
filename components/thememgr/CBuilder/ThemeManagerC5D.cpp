//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ThemeManagerC5D.res");
USERES("..\Design\ThemeManager.dcr");
USEUNIT("..\Design\ThemeManagerReg.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclx50.bpi");
USEPACKAGE("ThemeManagerC5.bpi");
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
