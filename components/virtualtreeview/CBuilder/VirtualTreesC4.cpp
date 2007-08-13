//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("VirtualTreesC4.res");
USEUNIT("..\VirtualTrees.pas");
USERES("..\VirtualTrees.dcr");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("VCLX40.bpi");
USEPACKAGE("ThemeManagerC4.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package-Code.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
 return 1;
}
//---------------------------------------------------------------------------
