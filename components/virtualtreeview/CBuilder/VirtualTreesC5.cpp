//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("VirtualTreesC5.res");
USEUNIT("..\Source\VirtualTrees.pas");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("VCLX50.bpi");
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
