//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("vcljpg40.bpi");
USEUNIT("dbctrls\ZImage.pas");
USERES("dbctrls\ZImage.dcr");
USEUNIT("dbctrls\ZDbCtrlsReg.pas");
USEFORMNS("dbctrls\ZFilterDlg.pas", Zfilterdlg, frmFilterDlg);
USERES("dbctrls\ZFilterDlg.dcr");
USEFORMNS("dbctrls\ZFindDlg.pas", Zfinddlg, frmFindDlg);
USERES("dbctrls\ZFindDlg.dcr");
USEUNIT("dbctrls\ZGrid.pas");
USERES("dbctrls\ZGrid.dcr");
USEUNIT("dbctrls\ZDbCtrlsConst.pas");
USEPACKAGE("ZCommonCB4.bpi");
USEPACKAGE("ZDbwareCB4.bpi");
USEPACKAGE("ZUtilsCB4.bpi");
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
