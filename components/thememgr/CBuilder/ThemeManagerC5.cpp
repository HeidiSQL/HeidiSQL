//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ThemeManagerC5.res");
USEUNIT("..\Source\ThemeMgr.pas");
USEUNIT("..\Source\ThemeMgrDB.pas");
USEUNIT("..\Source\ThemeSrv.pas");
USEUNIT("..\Source\TmSchema.pas");
USEUNIT("..\Source\UxTheme.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclx50.bpi");
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
