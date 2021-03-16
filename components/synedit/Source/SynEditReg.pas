{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditReg.pas, released 2000-04-07.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditReg.pas,v 1.33.2.2 2004/10/18 15:25:00 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditReg;

{$I SynEdit.inc}

interface

uses
  // SynEdit components
  SynEdit,
  SynMemo,
  SynEditDocumentManager,
  {$IFNDEF SYN_DELPHI_PE}
  SynDBEdit,
  {$ENDIF}
  SynEditStrConst,
  SynEditHighlighter,
  SynEditMiscClasses,
  SynEditPlugins,
  SynEditExport,
  SynExportHTML,
  SynExportRTF,
  SynExportTeX,
  SynHighlighterMulti,
  SynCompletionProposal,
  SynEditPythonBehaviour,
  SynEditPrint,
  SynEditPrintPreview,
  SynMacroRecorder,
  SynAutoCorrect,
  SynEditSearch,
  SynEditRegexSearch,
  {$IFDEF SYN_COMPILER_4_UP}
  SynHighlighterManager,
  {$ENDIF}
  SynEditOptionsDialog,
  SynHighlighterADSP21xx,
  SynHighlighterAsm,
  SynHighlighterAWK,
  SynHighlighterBaan,
  SynHighlighterBat,
  SynHighlighterCAC,
  SynHighlighterCache,
  SynHighlighterCobol,
  SynHighlighterCpp,
  SynHighlighterCS,
  SynHighlighterCss,
  SynHighlighterDfm,
  SynHighlighterDml,
  SynHighlighterDOT,
  {$ifdef SYN_DELPHI_2010_UP}
  SynHighlighterAsmMASM,
  {$endif}
  {$ifdef SYN_DELPHI_2009_UP}
  SynHighlighterDWS,
  {$endif}
  SynHighlighterECMAScript,
  SynHighlighterEiffel,
  SynHighlighterFortran,
  SynHighlighterFoxpro,
  SynHighlighterGalaxy,
  SynHighlighterGeneral,
  SynHighlighterGo,
  SynHighlighterGLSL,
  SynHighlighterHaskell,
  SynHighlighterHC11,
  SynHighlighterHP48, 
  SynHighlighterHtml,
  SynHighlighterIni,
  SynHighlighterInno,
  SynHighlighterJava,
  SynHighlighterJScript,
  SynHighlighterJSON,
  SynHighlighterKix,
  SynHighlighterModelica,
  SynHighlighterM3,   
  SynHighlighterPas,
  SynHighlighterPerl, 
  SynHighlighterPHP,
  SynHighlighterProgress, 
  SynHighlighterPython,
  SynHighlighterRC,
  SynHighlighterRuby, 
  SynHighlighterSml,
  SynHighlighterSQL,  
  SynHighlighterTclTk,
  SynHighlighterTeX,
  SynHighlighterUNIXShellScript,
  SynHighlighterURI,
  SynHighlighterVB,
  SynHighlighterVBScript,
  SynHighlighterVrml97,  
  SynHighlighterGWS,
  SynHighlighterCPM, 
  SynHighlighterSDD,
  SynHighlighterXML,
  SynHighlighterMsg, 
  SynHighlighterIDL,
  SynHighlighterUnreal,
  SynHighlighterST,
  SynHighlighterLDraw,   
  SynURIOpener,
  Classes;

procedure Register;

implementation

procedure Register;
begin
// SynEdit main components
  RegisterComponents(SYNS_ComponentsPage, [TSynEdit, TSynMemo]);

{$IFNDEF SYN_DELPHI_PE}
  RegisterComponents(SYNS_ComponentsPage, [TDBSynEdit]);
{$ENDIF}

{$IFDEF SYN_COMPILER_6_UP}
  GroupDescendentsWith(TSynCustomHighlighter, TSynEdit);
  GroupDescendentsWith(TSynEditSearchCustom, TSynEdit);
  GroupDescendentsWith(TSynCustomExporter, TSynEdit);
  GroupDescendentsWith(TSynMultiSyn, TSynEdit);
  GroupDescendentsWith(TSynBaseCompletionProposal, TSynEdit);
  GroupDescendentsWith(TSynAutoComplete, TSynEdit);
  GroupDescendentsWith(TAbstractSynPlugin, TSynEdit);
  GroupDescendentsWith(TCustomSynAutoCorrect, TSynEdit);
  GroupDescendentsWith(TSynEditPrint, TSynEdit);
  GroupDescendentsWith(TSynEditPrintPreview, TSynEdit);
  GroupDescendentsWith(TSynEditPythonBehaviour, TSynEdit);
  GroupDescendentsWith(TSynHighlighterManager, TSynEdit);
  GroupDescendentsWith(TSynEditOptionsDialog, TSynEdit);
  GroupDescendentsWith(TSynURIOpener, TSynEdit);
{$ENDIF}

// SynEdit extra components
  RegisterComponents(SYNS_ComponentsPage, [TSynExporterHTML, TSynExporterRTF,
    TSynExporterTeX, TSynEditPythonBehaviour, TSynMultiSyn,
    TSynCompletionProposal, TSynAutoComplete, TSynMacroRecorder,
    TSynEditPrint, TSynEditPrintPreview, TSynAutoCorrect,
    TSynEditSearch, TSynEditRegexSearch, TSynEditOptionsDialog, TSynURIOpener,
    TSynEditDocumentManager]);
{$IFDEF SYN_COMPILER_4_UP}
  RegisterComponents(SYNS_ComponentsPage, [TSynHighlighterManager]);
{$ENDIF}

// SynEdit highlighters
  RegisterComponents(SYNS_HighlightersPage, [
    //classic
    TSynCppSyn, TSynEiffelSyn, TSynFortranSyn, TSynGeneralSyn, TSynJavaSyn,
    TSynM3Syn, TSynPasSyn, TSynVBSyn, TSynCobolSyn, TSynCSSyn, TSynGoSyn,
    // internet
    TSynCssSyn, TSynHTMLSyn, TSynJScriptSyn, TSynPHPSyn, TSynVBScriptSyn,
    TSynXMLSyn, TSynJSONSyn, TSynVrml97Syn, TSynECMAScriptSyn,
    //interpreted
    TSynAWKSyn, TSynBATSyn,
    {$ifdef SYN_DELPHI_2009_UP}
    TSynDWSSyn,
    {$endif}
    TSynKixSyn, TSynPerlSyn, TSynPythonSyn, TSynGLSLSyn,
    TSynTclTkSyn, TSynGWScriptSyn, TSynRubySyn, TSynUNIXShellScriptSyn,
    //database
    TSynCACSyn, TSynCacheSyn, TSynFoxproSyn, TSynSQLSyn, TSynSDDSyn,
    //assembler
    TSynADSP21xxSyn, TSynAsmSyn, TSynHC11Syn, TSynHP48Syn, TSynSTSyn,
    {$ifdef SYN_DELPHI_2010_UP}
    TSynAsmMASMSyn,
    {$endif}
    //data modeling
    TSynDmlSyn, TSynModelicaSyn, TSynSMLSyn,
    //data
    TSynDfmSyn, TSynIniSyn, TSynInnoSyn,
    // other
    TSynBaanSyn, TSynGalaxySyn, TSynProgressSyn, TSynMsgSyn, 
    TSynIdlSyn, TSynUnrealSyn, TSynCPMSyn, TSynTeXSyn,
    TSynHaskellSyn, TSynLDRSyn, TSynURISyn, TSynDOTSyn, TSynRCSyn
  ]);
end;

end.
