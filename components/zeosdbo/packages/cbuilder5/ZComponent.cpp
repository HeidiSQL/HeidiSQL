//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("ZComponent.res");
USERES("..\..\src\component\ZComponent.dcr");
USEPACKAGE("vcl50.bpi");
USEPACKAGE("ZPlain.bpi");
USEPACKAGE("ZDbc.bpi");
USEPACKAGE("ZParseSql.bpi");
USEPACKAGE("ZCore.bpi");
USEPACKAGE("Vcldb50.bpi");
USEUNIT("..\..\src\component\ZAbstractDataset.pas");
USEUNIT("..\..\src\component\ZAbstractRODataset.pas");
USEUNIT("..\..\src\component\ZAbstractTable.pas");
USEUNIT("..\..\src\component\ZComponentReg.pas");
USEUNIT("..\..\src\component\ZConnection.pas");
USEUNIT("..\..\src\component\ZDataset.pas");
USEUNIT("..\..\src\component\ZDatasetUtils.pas");
USEUNIT("..\..\src\component\ZPropertyEditor.pas");
USEUNIT("..\..\src\component\ZSqlMetadata.pas");
USEUNIT("..\..\src\component\ZSqlMonitor.pas");
USEUNIT("..\..\src\component\ZSqlProcessor.pas");
USEUNIT("..\..\src\component\ZSqlStrings.pas");
USEUNIT("..\..\src\component\ZSqlUpdate.pas");
USEUNIT("..\..\src\component\ZStoredProcedure.pas");
USEUNIT("..\..\src\component\ZStreamBlob.pas");
USEPACKAGE("vclado50.bpi");
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
