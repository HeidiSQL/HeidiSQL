//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEUNIT("..\VirtualTreesReg.pas");
USEPACKAGE("vcl40.bpi");
USEPACKAGE("dsnide40.bpi");
USEPACKAGE("vclx40.bpi");
USEPACKAGE("VirtualTreesC4.bpi");
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
