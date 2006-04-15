//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEUNIT("utils\ZUtilsRes.pas");
USEUNIT("utils\ZHexEdit.pas");
USERES("utils\ZHexEdit.dcr");
USEUNIT("utils\ZParams.pas");
USEUNIT("utils\ZSocket.pas");
USEUNIT("utils\ZUtilsConst.pas");
USEUNIT("utils\ZUtilsReg.pas");
USEUNIT("utils\ZHash.pas");
USEPACKAGE("ZCommonCB5.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
USEUNIT("UTILS\ZGif.pas");
USEUNIT("UTILS\ZGraph.pas");
USEUNIT("UTILS\ZDataGrid.pas");
USERES("UTILS\ZDataGrid.dcr");
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
