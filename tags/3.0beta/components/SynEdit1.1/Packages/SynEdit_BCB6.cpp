//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("SynEdit_BCB6.res");
USEPACKAGE("vcl60.bpi");
USEUNIT("..\Source\SynEditReg.pas");
USERES("..\Source\SynEditReg.dcr");
USEUNIT("..\Source\SynEditPropertyReg.pas");
USEPACKAGE("vclx60.bpi");
USEPACKAGE("vcldb60.bpi");
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
