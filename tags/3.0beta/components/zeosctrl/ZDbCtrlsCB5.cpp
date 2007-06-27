//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEUNIT("dbctrls\ZImage.pas");
USEUNIT("dbctrls\ZDbCtrlsReg.pas");
USEFORMNS("dbctrls\ZFilterDlg.pas", Zfilterdlg, frmFilterDlg);
USERES("dbctrls\ZFilterDlg.dcr");
USEFORMNS("dbctrls\ZFindDlg.pas", Zfinddlg, frmFindDlg);
USERES("dbctrls\ZFindDlg.dcr");
USEUNIT("dbctrls\ZGrid.pas");
USERES("dbctrls\ZGrid.dcr");
USEUNIT("dbctrls\ZDbCtrlsConst.pas");
USEPACKAGE("ZCommonCB5.bpi");
USEPACKAGE("ZDbwareCB5.bpi");
USEPACKAGE("ZUtilsCB5.bpi");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("vcldb50.bpi");
USEPACKAGE("vclbde50.bpi");
USEPACKAGE("vcljpg50.bpi");
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
