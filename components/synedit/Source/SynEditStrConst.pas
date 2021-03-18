{-------------------------------------------------------------------------------
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditStrConst.pas, released 2000-04-07.
The Original Code is based on mwLocalStr.pas by Michael Hieke, part of the
mwEdit component suite.
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

$Id: SynEditStrConst.pas,v 1.41.2.5 2009/01/06 16:26:01 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditStrConst;

{$I SynEdit.inc}

interface

// NOTE: this is design-time stuff, so no need to have it in stringtables
const
  SYNS_ComponentsPage           =  'SynEdit';
  SYNS_HighlightersPage         =  'SynEdit Highlighters';

// NOTE: the following constants are used to store information to the registry,
//       INI files or XML files. For maximum compatibility only the chars
//       'A'..'Z', 'a'..'z', '0'..'9', '_' and '-' are allowed!
//
//       If you want translated/"pretty"/more detailed descriptions use the
//       resourcestrings, i.e. the "friendly" versions below.

// constant names for highlighter attributes
//
//
const
  SYNS_AttrAreaAIdentifier      =  'Area_A_Identifier';
  SYNS_AttrArrowHead            =  'ArrowHead';
  SYNS_AttrAsm                  =  'Asm';
  SYNS_AttrAsmComment           =  'AsmComment';
  SYNS_AttrAsmKey               =  'AsmKey';
  SYNS_AttrAssembler            =  'Assembler';
  SYNS_AttrAtRules              =  'AtRules';
  SYNS_AttrAttribute            =  'Attribute';
  SYNS_AttrAttributeName        =  'AttributeName';
  SYNS_AttrAttributeValue       =  'AttributeValue';
  SYNS_AttrBasicTypes           =  'BasicTypes';
  SYNS_AttrBlock                =  'Block';
  SYNS_AttrBoolean              =  'Booleanvalue';
  SYNS_AttrBrackets             =  'Brackets';
  SYNS_AttrCDATASection         =  'CDATA-Section';
  SYNS_AttrCharacter            =  'Character';
  SYNS_AttrClass                =  'Class';
  SYNS_AttrColor                =  'ColorValue';
  SYNS_AttrConstant             =  'Constant';
  SYNS_AttrComment              =  'Comment';
  SYNS_AttrCondition            =  'Condition';
  SYNS_AttrConditionalComment   =  'ConditionalComment';
  SYNS_AttrConsoleOutput        =  'ConsoleOutput';
  SYNS_AttrDataType             =  'DataType';
  SYNS_AttrDebugLines           =  'DebuggingLines';
  SYNS_AttrDefaultPackage       =  'DefaultPackages';
  SYNS_AttrDelimitedIdentifier  =  'DelimitedIdentifier';
  SYNS_AttrDir                  =  'Direction';
  SYNS_AttrDirections           =  'Directions';
  SYNS_AttrDirective            =  'Directive';
  SYNS_AttrDOCTYPESection       =  'DOCTYPE-Section';
  SYNS_AttrDocumentation        =  'Documentation';
  SYNS_AttrElementName          =  'ElementName';
  SYNS_AttrEmbedSQL             =  'EmbeddedSQL';
  SYNS_AttrEmbedText            =  'EmbeddedText';
  SYNS_AttrEntityReference      =  'EntityReference';
  SYNS_AttrEscapeAmpersand      =  'EscapeAmpersand';
  SYNS_AttrEvent                =  'Event';
  SYNS_AttrException            =  'Exception';
  SYNS_AttrFirstTri             =  'FirstTri';
  SYNS_AttrFloat                =  'Float';
  SYNS_AttrForm                 =  'Form';
  SYNS_AttrFourthTri            =  'FourthTri';
  SYNS_AttrFunction             =  'Function';
  SYNS_AttrHexadecimal          =  'Hexadecimal';
  SYNS_AttrIcon                 =  'IconReference';
  SYNS_AttrIdentifier           =  'Identifier';
  SYNS_AttrIllegalChar          =  'IllegalChar';
  SYNS_AttrInclude              =  'Include';
  SYNS_AttrIndicator            =  'IndicatorArea';
  SYNS_AttrIndirect             =  'Indirect';
  SYNS_AttrInstructions         =  'Instructions';
  SYNS_AttrInvalidSymbol        =  'InvalidSymbol';
  SYNS_AttrInterfaceQualifier   =  'InterfaceQualifier';
  SYNS_AttrInternalFunction     =  'InternalFunction';
  SYNS_AttrKey                  =  'Key';
  SYNS_AttrLabel                =  'Label';
  SYNS_AttrLace                 =  'Lace';
  SYNS_AttrLine                 =  'Line';
  SYNS_AttrMacro                =  'Macro';
  SYNS_AttrMarker               =  'Marker';
  SYNS_AttrMathMode             =  'MathMode';
  SYNS_AttrMessage              =  'Message';
  SYNS_AttrMiscellaneous        =  'Miscellaneous';
  SYNS_AttrNamespaceAttrName    =  'NamespaceAttributeName';
  SYNS_AttrNamespaceAttrValue   =  'NamespaceAttributeValue';
  SYNS_AttrNonReservedKeyword   =  'NonreservedKeyword';
  SYNS_AttrNull                 =  'Null';
  SYNS_AttrNumber               =  'Number';
  SYNS_AttrOctal                =  'Octal';
  SYNS_AttrOperator             =  'Operator';
  SYNS_AttrOperatorAndSymbols   =  'OperatorAndSymbols';
  SYNS_AttrOpLine               =  'OpLine';
  SYNS_AttrOptions              =  'Options';
  SYNS_AttrPath                 =  'PathName';
  SYNS_AttrPLSQL                =  'PLSQL-ReservedWord';
  SYNS_AttrPragma               =  'Pragma';
  SYNS_AttrPredefined           =  'Predefined';
  SYNS_AttrPreprocessor         =  'Preprocessor';
  SYNS_AttrProcessingInstr      =  'ProcessingInstruction';
  SYNS_AttrProcName             =  'ProcName';
  SYNS_AttrProperty             =  'Property';
  SYNS_AttrQuad                 =  'Quad';
  SYNS_AttrQualifier            =  'Qualifier';
  SYNS_AttrRegister             =  'Register';
  SYNS_AttrReservedWord         =  'ReservedWord';
  SYNS_AttrResultValue          =  'ResultValue';
  SYNS_AttrRoundBracket         =  'RoundBracket';
  SYNS_AttrRpl                  =  'Rpl';
  SYNS_AttrRplKey               =  'Rpl-Key';
  SYNS_AttrRplComment           =  'Rpl-Comment';
  SYNS_AttrSASM                 =  'SASM';
  SYNS_AttrSASMComment          =  'SASM-Comment';
  SYNS_AttrSASMKey              =  'SASM-Key';
  SYNS_AttrSecondReservedWord   =  'SecondReservedWord';
  SYNS_AttrSecondTri            =  'SecondTri';
  SYNS_AttrSection              =  'Section';
  SYNS_AttrSequence             =  'SequenceNumberArea';
  SYNS_AttrShape                =  'Shape';
  SYNS_AttrSingleString         =  'SingleQuotedString';
  SYNS_AttrSpace                =  'Space';
  SYNS_AttrSpecialVariable      =  'SpecialVariable';
  SYNS_AttrSQLKey               =  'SQL-Keyword';
  SYNS_AttrSQLPlus              =  'SQLPlus-Command';
  SYNS_AttrSquareBracket        =  'SquareBracket';
  SYNS_AttrString               =  'String';
  SYNS_AttrSymbol               =  'Symbol';
  SYNS_AttrSyntaxError          =  'SyntaxError';
  SYNS_AttrSystem               =  'SystemFunctionsAndVariables';
  SYNS_AttrSystemValue          =  'SystemValue';
  SYNS_AttrTagArea              =  'TagArea';
  SYNS_AttrTableName            =  'TableName';
  SYNS_AttrTerminator           =  'Terminator';
  SYNS_AttrTeXCommand           =  'TeX-Command';
  SYNS_AttrText                 =  'Text';
  SYNS_AttrTextMathMode         =  'TextInMathMode';
  SYNS_AttrThirdTri             =  'ThirdTri';
  SYNS_AttrTixKeyWords          =  'Tix-Keywords';
  SYNS_AttrTriangle             =  'Triangle';
  SYNS_AttrUndefinedProperty    =  'UndefinedProperty';
  SYNS_AttrUnknownWord          =  'UnknownWord';
  SYNS_AttrURI                  =  'URI';
  SYNS_AttrUser                 =  'UserFunctionsAndVariables';
  SYNS_AttrUserFunction         =  'UserFunctions';
  SYNS_AttrValue                =  'Value';
  SYNS_AttrVariable             =  'Variable';
  SYNS_AttrVisitedURI           =  'VisitedURI';
  SYNS_AttrVrmlAppearance       =  'Vrml_Appearance';
  SYNS_AttrVrmlAttribute        =  'Vrml_Attribute';
  SYNS_AttrVrmlDefinition       =  'Vrml_Definition';
  SYNS_AttrVrmlEvent            =  'Vrml_Event';
  SYNS_AttrVrmlGrouping         =  'Vrml_Grouping';
  SYNS_AttrVrmlInterpolator     =  'Vrml_Interpolator';
  SYNS_AttrVrmlLight            =  'Vrml_Light';
  SYNS_AttrVrmlNode             =  'Vrml_Node';
  SYNS_AttrVrmlParameter        =  'Vrml_Parameter';
  SYNS_AttrVrmlProto            =  'Vrml_Proto';
  SYNS_AttrVrmlSensor           =  'Vrml_Sensor';
  SYNS_AttrVrmlShape            =  'Vrml_Shape';
  SYNS_AttrVrmlShape_Hint       =  'Vrml_Shape_Hint';
  SYNS_AttrVrmlTime_dependent   =  'Vrml_Time_dependent';
  SYNS_AttrVrmlViewpoint        =  'Vrml_Viewpoint';
  SYNS_AttrVrmlWorldInfo        =  'Vrml_WorldInfo';
  SYNS_AttrWhitespace           =  'Whitespace';
  SYNS_AttrWidgetWords          =  'Widget-Keywords';
  SYNS_AttrX3DDocType           =  'X3DDocType';
  SYNS_AttrX3DHeader            =  'X3DHeader';

  // constant language names
  SYNS_Lang68HC11               =  '68HC11_Assembler';
  SYNS_LangADSP21xx             =  'ADSP21xx';
  SYNS_LangAWK                  =  'AWK';
  SYNS_LangBaan                 =  'Baan_4GL';
  SYNS_LangBatch                =  'MS-DOS_Batch';
  SYNS_LangCache                =  'CacheObjectScript';
  SYNS_LangCAClipper            =  'CA-Clipper';
  SYNS_LangCOBOL                =  'COBOL';
  SYNS_LangCORBAIDL             =  'CORBA_IDL';
  SYNS_LangCPM                  =  'COAS_Product_Manager_Report';
  SYNS_LangCPP                  =  'CandCPlusPlus';
  SYNS_LangCS                   =  'CSharp';
  SYNS_LangCSS                  =  'CascadingStyleSheet';
  SYNS_LangDfm                  =  'BorlandForms';
  SYNS_LangDOT                  =  'DOT_Graph_Drawing_Description_language';
  SYNS_LangECMAScript           =  'ECMAScript';
  SYNS_LangEiffel               =  'Eiffel';
  SYNS_LangFortran              =  'Fortran';
  SYNS_LangFoxpro               =  'Foxpro';
  SYNS_LangGalaxy               =  'Galaxy';
  SYNS_LangGembase              =  'Gembase';
  SYNS_LangGeneral              =  'General';
  SYNS_LangGeneralMulti         =  'General_Multi-Highlighter';
  SYNS_LangGLSL                 =  'OpenGL Shader Language';
  SYNS_LangGo                   =  'Go';
  SYNS_LangGWS                  =  'GW-TEL';
  SYNS_LangHaskell              =  'Haskell';
  SYNS_LangHP48                 =  'HP48';
  SYNS_LangHTML                 =  'HTML';
  SYNS_LangINI                  =  'INI';
  SYNS_LangInno                 =  'InnoSetupScript';
  SYNS_LangJava                 =  'Java';
  SYNS_LangJScript              =  'JavaScript';
  SYNS_LangJSON                 =  'JSON';
  SYNS_LangKIX                  =  'KiXtart';
  SYNS_LangLDraw                =  'LEGO_LDraw';
  SYNS_LangLLVMIR               =  'LLVM IR';
  SYNS_LangMASM                 =  'x86Assembly MASM';
  SYNS_LangModelica             =  'Modelica';
  SYNS_LangModula3              =  'Modula3';
  SYNS_LangNameUNIXShellScript  =  'UNIXShellScript';
  SYNS_LangPascal               =  'ObjectPascal';
  SYNS_LangPerl                 =  'Perl';
  SYNS_LangPHP                  =  'PHP';
  SYNS_LangProgress             =  'Progress';
  SYNS_LangPython               =  'Python';
  SYNS_LangRC                   =  'Resource';
  SYNS_LangRuby                 =  'Ruby';
  SYNS_LangSDD                  =  'SemantaDataDictionary';
  SYNS_LangSML                  =  'StandardML';
  SYNS_LangSQL                  =  'SQL';
  SYNS_LangST                   =  'StructuredText';
  SYNS_LangSybaseSQL            =  'SybaseSQL';
  SYNS_LangSynGenMsgfiles       =  'SynGen_Msg';
  SYNS_LangTclTk                =  'TclTk';
  SYNS_LangTeX                  =  'TeX';
  SYNS_LangUnknown              =  '<Unknown>';
  SYNS_LangUnreal               =  'Unreal';
  SYNS_LangURI                  =  'URI';
  SYNS_LangVBSScript            =  'MS-VBScript';
  SYNS_LangVisualBASIC          =  'VisualBasic';
  SYNS_LangVrml97               =  'Vrml97';
  SYNS_LangX86Asm               =  'x86Assembly';
  SYNS_LangXML                  =  'XML';

resourcestring
  SYNS_NoSearchEngineError      = 'No search engine has been assigned';

  SYNS_Untitled                 =  'Untitled';

  // Friendly names for highlighter attributes
  SYNS_FriendlyAttrAreaAIdentifier      =  'Area A Identifier';
  SYNS_FriendlyAttrArrowHead            =  'ArrowHead';
  SYNS_FriendlyAttrAsm                  =  'Asm';
  SYNS_FriendlyAttrAsmComment           =  'Asm Comment';
  SYNS_FriendlyAttrAsmKey               =  'Asm Key';
  SYNS_FriendlyAttrAssembler            =  'Assembler';
  SYNS_FriendlyAttrAtRules              =  '@-Rules';
  SYNS_FriendlyAttrAttribute            =  'Attribute';
  SYNS_FriendlyAttrAttributeName        =  'Attribute Name';
  SYNS_FriendlyAttrAttributeValue       =  'Attribute Value';
  SYNS_FriendlyAttrBasicTypes           =  'Basic Types';
  SYNS_FriendlyAttrBlock                =  'Block';
  SYNS_FriendlyAttrBoolean              =  'Boolean value';
  SYNS_FriendlyAttrBrackets             =  'Brackets';
  SYNS_FriendlyAttrCDATASection         =  'CDATA Section';
  SYNS_FriendlyAttrCharacter            =  'Character';
  SYNS_FriendlyAttrClass                =  'Class';
  SYNS_FriendlyAttrColor                =  'Color Value';
  SYNS_FriendlyAttrComment              =  'Comment';
  SYNS_FriendlyAttrCondition            =  'Condition';
  SYNS_FriendlyAttrConditionalComment   =  'Conditional Comment';
  SYNS_FriendlyAttrConsoleOutput        =  'Console output';
  SYNS_FriendlyAttrConstant             =  'Constant';
  SYNS_FriendlyAttrDataType             =  'Data Type';
  SYNS_FriendlyAttrDebugLines           =  'Debugging Lines';
  SYNS_FriendlyAttrDefaultPackage       =  'Default Packages';
  SYNS_FriendlyAttrDelimitedIdentifier  =  'Delimited Identifier';
  SYNS_FriendlyAttrDir                  =  'Direction';
  SYNS_FriendlyAttrDirections           =  'Directions';
  SYNS_FriendlyAttrDirective            =  'Directive';
  SYNS_FriendlyAttrDOCTYPESection       =  'DOCTYPE Section';
  SYNS_FriendlyAttrDocumentation        =  'Documentation';
  SYNS_FriendlyAttrElementName          =  'Element Name';
  SYNS_FriendlyAttrEmbedSQL             =  'Embedded SQL';
  SYNS_FriendlyAttrEmbedText            =  'Embedded Text';
  SYNS_FriendlyAttrEntityReference      =  'Entity Reference';
  SYNS_FriendlyAttrEscapeAmpersand      =  'Escape Ampersand';
  SYNS_FriendlyAttrEvent                =  'Event';
  SYNS_FriendlyAttrException            =  'Exception';
  SYNS_FriendlyAttrFirstTri             =  'FirstTri';
  SYNS_FriendlyAttrFloat                =  'Float';
  SYNS_FriendlyAttrForm                 =  'Form';
  SYNS_FriendlyAttrFourthTri            =  'FourthTri';
  SYNS_FriendlyAttrFunction             =  'Function';
  SYNS_FriendlyAttrHexadecimal          =  'Hexadecimal';
  SYNS_FriendlyAttrIcon                 =  'Icon Reference';
  SYNS_FriendlyAttrIdentifier           =  'Identifier';
  SYNS_FriendlyAttrIllegalChar          =  'Illegal Char';
  SYNS_FriendlyAttrInclude              =  'Include';
  SYNS_FriendlyAttrIndicator            =  'Indicator Area';
  SYNS_FriendlyAttrIndirect             =  'Indirect';
  SYNS_FriendlyAttrInstructions         =  'Instructions';
  SYNS_FriendlyAttrInterfaceQualifier   =  'Interface Qualifier';
  SYNS_FriendlyAttrInternalFunction     =  'Internal Function';
  SYNS_FriendlyAttrInvalidSymbol        =  'Invalid Symbol';
  SYNS_FriendlyAttrKey                  =  'Key';
  SYNS_FriendlyAttrLabel                =  'Label';
  SYNS_FriendlyAttrLace                 =  'Lace';
  SYNS_FriendlyAttrLine                 =  'Line';
  SYNS_FriendlyAttrMacro                =  'Macro';
  SYNS_FriendlyAttrMarker               =  'Marker';
  SYNS_FriendlyAttrMathMode             =  'Math Mode';
  SYNS_FriendlyAttrMessage              =  'Message';
  SYNS_FriendlyAttrMiscellaneous        =  'Miscellaneous';
  SYNS_FriendlyAttrNamespaceAttrName    =  'Namespace Attribute Name';
  SYNS_FriendlyAttrNamespaceAttrValue   =  'Namespace Attribute Value';
  SYNS_FriendlyAttrNonReservedKeyword   =  'Non-reserved Keyword';
  SYNS_FriendlyAttrNull                 =  'Null';
  SYNS_FriendlyAttrNumber               =  'Number';
  SYNS_FriendlyAttrOctal                =  'Octal';
  SYNS_FriendlyAttrOperator             =  'Operator';
  SYNS_FriendlyAttrOperatorAndSymbols   =  'Operator And Symbols';
  SYNS_FriendlyAttrOpLine               =  'OpLine';
  SYNS_FriendlyAttrOptions              =  'Options';
  SYNS_FriendlyAttrPath                 =  'Pathname';
  SYNS_FriendlyAttrPLSQL                =  'PL/SQL Reserved Word';
  SYNS_FriendlyAttrPragma               =  'Pragma';
  SYNS_FriendlyAttrPredefined           =  'Predefined';
  SYNS_FriendlyAttrPreprocessor         =  'Preprocessor';
  SYNS_FriendlyAttrProcessingInstr      =  'Processing Instruction';
  SYNS_FriendlyAttrProcName             =  'Procedure name';
  SYNS_FriendlyAttrProperty             =  'Property';
  SYNS_FriendlyAttrQuad                 =  'Quad';
  SYNS_FriendlyAttrQualifier            =  'Qualifier';
  SYNS_FriendlyAttrRegister             =  'Register';
  SYNS_FriendlyAttrReservedWord         =  'Reserved Word';
  SYNS_FriendlyAttrResultValue          =  'Result Value';
  SYNS_FriendlyAttrRoundBracket         =  'Round Bracket';
  SYNS_FriendlyAttrRpl                  =  'Rpl';
  SYNS_FriendlyAttrRplKey               =  'Rpl Key';
  SYNS_FriendlyAttrRplComment           =  'Rpl Comment';
  SYNS_FriendlyAttrSASM                 =  'SASM';
  SYNS_FriendlyAttrSASMComment          =  'SASM Comment';
  SYNS_FriendlyAttrSASMKey              =  'SASM Key';
  SYNS_FriendlyAttrSecondReservedWord   =  'Second Reserved Word';
  SYNS_FriendlyAttrSecondTri            =  'SecondTri';
  SYNS_FriendlyAttrSection              =  'Section';
  SYNS_FriendlyAttrSequence             =  'Sequence Number Area';
  SYNS_FriendlyAttrShape                =  'Shape';
  SYNS_FriendlyAttrSingleString         =  'Single Quoted String';
  SYNS_FriendlyAttrSpace                =  'Space';
  SYNS_FriendlyAttrSpecialVariable      =  'Special Variable';
  SYNS_FriendlyAttrSQLKey               =  'SQL Keyword';
  SYNS_FriendlyAttrSQLPlus              =  'SQL*Plus Command';
  SYNS_FriendlyAttrSquareBracket        =  'Square Bracket';
  SYNS_FriendlyAttrString               =  'String';
  SYNS_FriendlyAttrSymbol               =  'Symbol';
  SYNS_FriendlyAttrSyntaxError          =  'Syntax Error';
  SYNS_FriendlyAttrSystem               =  'System Functions and Variables';
  SYNS_FriendlyAttrSystemValue          =  'System Value';
  SYNS_FriendlyAttrTagArea              =  'Tag Area';
  SYNS_FriendlyAttrTableName            =  'Table Name';
  SYNS_FriendlyAttrTerminator           =  'Terminator';
  SYNS_FriendlyAttrTeXCommand           =  'TeX Command';
  SYNS_FriendlyAttrText                 =  'Text';
  SYNS_FriendlyAttrTextMathMode         =  'Text in Math Mode';
  SYNS_FriendlyAttrThirdTri             =  'ThirdTri';
  SYNS_FriendlyAttrTixKeyWords          =  'Tix Keywords';
  SYNS_FriendlyAttrTriangle             =  'Triangle';
  SYNS_FriendlyAttrUndefinedProperty    =  'Undefined Property';
  SYNS_FriendlyAttrUnknownWord          =  'Unknown Word';
  SYNS_FriendlyAttrURI                  =  'URI';
  SYNS_FriendlyAttrUser                 =  'User Functions and Variables';
  SYNS_FriendlyAttrUserFunction         =  'User Functions';
  SYNS_FriendlyAttrValue                =  'Value';
  SYNS_FriendlyAttrVariable             =  'Variable';
  SYNS_FriendlyAttrVisitedURI           =  'Visited URI';
  SYNS_FriendlyAttrVrmlAppearance       =  'Vrml_Appearance';
  SYNS_FriendlyAttrVrmlAttribute        =  'Vrml_Attribute';
  SYNS_FriendlyAttrVrmlDefinition       =  'Vrml_Definition';
  SYNS_FriendlyAttrVrmlEvent            =  'Vrml_Event';
  SYNS_FriendlyAttrVrmlGrouping         =  'Vrml_Grouping';
  SYNS_FriendlyAttrVrmlInterpolator     =  'Vrml_Interpolator';
  SYNS_FriendlyAttrVrmlLight            =  'Vrml_Light';
  SYNS_FriendlyAttrVrmlNode             =  'Vrml_Node';
  SYNS_FriendlyAttrVrmlParameter        =  'Vrml_Parameter';
  SYNS_FriendlyAttrVrmlProto            =  'Vrml_Proto';
  SYNS_FriendlyAttrVrmlSensor           =  'Vrml_Sensor';
  SYNS_FriendlyAttrVrmlShape            =  'Vrml_Shape';
  SYNS_FriendlyAttrVrmlShape_Hint       =  'Vrml_Shape_Hint';
  SYNS_FriendlyAttrVrmlTime_dependent   =  'Vrml_Time_dependent';
  SYNS_FriendlyAttrVrmlViewpoint        =  'Vrml_Viewpoint';
  SYNS_FriendlyAttrVrmlWorldInfo        =  'Vrml_WorldInfo';
  SYNS_FriendlyAttrWhitespace           =  'Whitespace';
  SYNS_FriendlyAttrWidgetWords          =  'Widget Keywords';  
  SYNS_FriendlyAttrX3DDocType           =  'X3DDocType';
  SYNS_FriendlyAttrX3DHeader            =  'X3DHeader';

  // names of exporter output formats
  SYNS_ExporterFormatHTML       =  'HTML';
  SYNS_ExporterFormatRTF        =  'RTF';
  SYNS_ExporterFormatTeX        =  'TeX';

  // TCustomSynEdit scroll hint window caption
  SYNS_ScrollInfoFmt            =  '%d - %d';
  SYNS_ScrollInfoFmtTop         =  'Top Line: %d';
  // TSynEditPrintPreview page number
  SYNS_PreviewScrollInfoFmt     =  'Page: %d';

  // strings for property editors etc
  SYNS_EDuplicateShortcut       =  'Shortcut already exists';
  SYNS_ShortcutNone             =  '<none>';
  SYNS_DuplicateShortcutMsg     =  'The keystroke "%s" is already assigned ' +
                                   'to another editor command. (%s)';
  SYNS_DuplicateShortcutMsg2    =  'The keystroke "%s" is already assigned ' +
                                   'to another editor command.'#13#10'The ' +
                                   'shortcut for this item has not been changed.';

  // Filters used for open/save dialog
  SYNS_FilterADSP21xx           =  'DSP Files (*.dsp;*.inc)|*.dsp;*.inc';
  SYNS_FilterAsm68HC11          =  '68HC11 Assembler Files (*.hc11;*.asm;*.asc)|*.hc11;*.asm;*.asc';
  SYNS_FilterAWK                =  'AWK Scripts (*.awk)|*.awk';
  SYNS_FilterBaan               =  'Baan 4GL Files (*.cln)|*.cln';
  SYNS_FilterBatch              =  'MS-DOS Batch Files (*.bat;*.cmd)|*.bat;*.cmd';
  SYNS_FilterCache              =  'Cache Files (*.mac;*.inc;*.int)|*.mac;*.inc;*.int';
  SYNS_FilterCAClipper          =  'CA-Clipper Files (*.prg;*.ch;*.inc)|*.prg;*.ch;*.inc';
  SYNS_FilterCOBOL              =  'COBOL Files (*.cbl;*.cob)|*.cbl;*.cob';
  SYNS_FilterCORBAIDL           =  'CORBA IDL Files (*.idl)|*.idl';
  SYNS_FilterCPM                =  'CPM Reports (*.rdf;*.rif;*.rmf;*.rxf)|*.rdf;*.rif;*.rmf;*.rxf';
  SYNS_FilterCPP                =  'C/C++ Files (*.c;*.cpp;*.cc;*.h;*.hpp;*.hh;*.cxx;*.hxx;*.cu)|*.c;*.cpp;*.cc;*.h;*.hpp;*.hh;*.cxx;*.hxx;*.cu';
  SYNS_FilterCS                 =  'C# Files (*.cs)|*.cs';
  SYNS_FilterCSS                =  'Cascading Stylesheets (*.css)|*.css';
  SYNS_FilterDFM                =  'Borland Form Files (*.dfm;*.xfm)|*.dfm;*.xfm';
  SYNS_FilterDOT                =  'DOT Graph Drawing Description (*.dot)|*.dot';
  SYNS_FilterDWS                =  'DWScript Files (*.dws;*.pas;*.inc)|*.dws;*.pas;*.inc';
  SYNS_FilterEcmaScript         =  'Javascript Files (*.js)|*.js';
  SYNS_FilterEiffel             =  'Eiffel (*.e;*.ace)|*.e;*.ace';
  SYNS_FilterFortran            =  'Fortran Files (*.for)|*.for';
  SYNS_FilterFoxpro             =  'Foxpro Files (*.prg)|*.prg';
  SYNS_FilterGalaxy             =  'Galaxy Files (*.gtv;*.galrep;*.txt)|*.gtv;*.galrep;*.txt';
  SYNS_FilterGembase            =  'GEMBASE Files (*.dml;*.gem)|*.dml;*.gem';
  SYNS_FilterGo                 =  'Go files (*.go)|*.go';
  SYNS_FilterGLSL               =  'GLSL files (*.glsl)|*.glsl';
  SYNS_FilterGWS                =  'GW-TEL Scripts (*.gws)|*.gws';
  SYNS_FilterHaskell            =  'Haskell Files (*.hs;*.lhs)|*.hs;*.lhs';
  SYNS_FilterHP48               =  'HP48 Files (*.s;*.sou;*.a;*.hp)|*.s;*.sou;*.a;*.hp';
  SYNS_FilterHTML               =  'HTML Documents (*.htm;*.html)|*.htm;*.html';
  SYNS_FilterINI                =  'INI Files (*.ini)|*.ini';
  SYNS_FilterInno               =  'Inno Setup Scripts (*.iss)|*.iss';
  SYNS_FilterJava               =  'Java Files (*.java)|*.java';
  SYNS_FilterJScript            =  'Javascript Files (*.js)|*.js';
  SYNS_FilterJSON               =  'JSON Files (*.json)|*.json';
  SYNS_FilterKIX                =  'KiXtart Scripts (*.kix)|*.kix';
  SYNS_FilterLDraw              =  'LEGO LDraw Files (*.ldr)|*.ldr';
  SYNS_FilterLLVMIR             =  'LLVM IR files (*.ll)|*.ll';
  SYNS_FilterModelica           =  'Modelica Files (*.mo)|*.mo';
  SYNS_FilterModula3            =  'Modula-3 Files (*.m3)|*.m3';
  SYNS_FilterPascal             =  'Pascal Files (*.pas;*.pp;*.dpr;*.dpk;*.inc)|*.pas;*.pp;*.dpr;*.dpk;*.inc';
  SYNS_FilterPerl               =  'Perl Files (*.pl;*.pm;*.cgi)|*.pl;*.pm;*.cgi';
  SYNS_FilterPHP                =  'PHP Files (*.php;*.php3;*.phtml;*.inc)|*.php;*.php3;*.phtml;*.inc';
  SYNS_FilterProgress           =  'Progress Files (*.w;*.p;*.i)|*.w;*.p;*.i';
  SYNS_FilterPython             =  'Python Files (*.py)|*.py';
  SYNS_FilterRC                 =  'Resource Files (*.rc)|*.rc';
  SYNS_FilterRTF                =  'Rich Text Format Documents (*.rtf)|*.rtf';
  SYNS_FilterRuby               =  'Ruby Files (*.rb;*.rbw)|*.rb;*.rbw';
  SYNS_FilterSDD                =  'Semanta DD Files (*.sdd)|*.sdd';
  SYNS_FilterSML                =  'Standard ML Files (*.sml)|*.sml';
  SYNS_FilterSQL                =  'SQL Files (*.sql)|*.sql';
  SYNS_FilterST                 =  'Structured Text Files (*.st)|*.st';
  SYNS_FilterSynGenMsgfiles     =  'Msg Files (*.msg)|*.msg';
  SYNS_FilterTclTk              =  'Tcl/Tk Files (*.tcl)|*.tcl';
  SYNS_FilterTeX                =  'TeX Files (*.tex)|*.tex';
  SYNS_FilterUNIXShellScript    =  'UNIX Shell Scripts (*.sh)|*.sh';
  SYNS_FilterURI                =  'All Files (*.*)|*.*';
  SYNS_FilterVBScript           =  'VBScript Files (*.vbs)|*.vbs';
  SYNS_FilterVisualBASIC        =  'Visual Basic Files (*.bas)|*.bas';
  SYNS_FilterVrml97             =  'Vrml97/X3D World (*.wrl;*.wrml;*.vrl;*.vrml;*.x3d)|*.wrl;*.wrml;*.vrl;*.vrml;*.x3d';
  SYNS_FilterX86Assembly        =  'x86 Assembly Files (*.asm)|*.asm';
  SYNS_FilterXML                =  'XML Files (*.xml;*.xsd;*.xsl;*.xslt;*.dtd)|*.xml;*.xsd;*.xsl;*.xslt;*.dtd';

  // friendly language names
  SYNS_FriendlyLang68HC11               =  '68HC11 Assembler';
  SYNS_FriendlyLangADSP21xx             =  'ADSP21xx';
  SYNS_FriendlyLangAWK                  =  'AWK';
  SYNS_FriendlyLangBaan                 =  'Baan 4GL';
  SYNS_FriendlyLangBatch                =  'MS-DOS Batch';
  SYNS_FriendlyLangCache                =  'Cache Object Script';
  SYNS_FriendlyLangCAClipper            =  'CA-Clipper';
  SYNS_FriendlyLangCOBOL                =  'COBOL';
  SYNS_FriendlyLangCORBAIDL             =  'CORBA IDL';
  SYNS_FriendlyLangCPM                  =  'COAS Product Manager Report';
  SYNS_FriendlyLangCPP                  =  'C/C++';
  SYNS_FriendlyLangCS                   =  'C#';
  SYNS_FriendlyLangCSS                  =  'Cascading Style Sheet';
  SYNS_FriendlyLangDfm                  =  'Borland Forms';
  SYNS_FriendlyLangDOT                  =  'DOT Graph Drawing Description language';
  SYNS_FriendlyLangEcmaScript           =  'ECMA Script';
  SYNS_FriendlyLangEiffel               =  'Eiffel';
  SYNS_FriendlyLangFortran              =  'Fortran';
  SYNS_FriendlyLangFoxpro               =  'Foxpro';
  SYNS_FriendlyLangGalaxy               =  'Galaxy';
  SYNS_FriendlyLangGembase              =  'Gembase';
  SYNS_FriendlyLangGeneral              =  'General';
  SYNS_FriendlyLangGeneralMulti         =  'General Multi-Highlighter';
  SYNS_FriendlyLangGo                   =  'Go';
  SYNS_FriendlyLangGLSL                 =  'GLSL';
  SYNS_FriendlyLangGWS                  =  'GW-TEL';
  SYNS_FriendlyLangHaskell              =  'Haskell';
  SYNS_FriendlyLangHP48                 =  'HP48';
  SYNS_FriendlyLangHTML                 =  'HTML';
  SYNS_FriendlyLangINI                  =  'INI';
  SYNS_FriendlyLangInno                 =  'Inno Setup Script';
  SYNS_FriendlyLangJava                 =  'Java';
  SYNS_FriendlyLangJavaScript           =  'JavaScript';
  SYNS_FriendlyLangJScript              =  'JavaScript';
  SYNS_FriendlyLangJSON                 =  'JSON';
  SYNS_FriendlyLangKIX                  =  'KiXtart';
  SYNS_FriendlyLangLDraw                =  'LEGO LDraw';
  SYNS_FriendlyLangLLVMIR               =  'LLVM Intermediate Representation';
  SYNS_FriendlyLangMASM                 =  'x86 Assembly MASM';
  SYNS_FriendlyLangModelica             =  'Modelica';
  SYNS_FriendlyLangModula3              =  'Modula 3';
  SYNS_FriendlyLangNameUNIXShellScript  =  'UNIX Shell Script';
  SYNS_FriendlyLangPascal               =  'Object Pascal';
  SYNS_FriendlyLangPerl                 =  'Perl';
  SYNS_FriendlyLangPHP                  =  'PHP';
  SYNS_FriendlyLangProgress             =  'Progress';
  SYNS_FriendlyLangPython               =  'Python';
  SYNS_FriendlyLangRC                   =  'Resource';
  SYNS_FriendlyLangRuby                 =  'Ruby';
  SYNS_FriendlyLangSDD                  =  'Semanta Data Dictionary';
  SYNS_FriendlyLangSML                  =  'Standard ML';
  SYNS_FriendlyLangSQL                  =  'SQL';
  SYNS_FriendlyLangST                   =  'Structured Text';
  SYNS_FriendlyLangSybaseSQL            =  'Sybase SQL';
  SYNS_FriendlyLangSynGenMsgfiles       =  'SynGen Msg';
  SYNS_FriendlyLangTclTk                =  'Tcl/Tk';
  SYNS_FriendlyLangTeX                  =  'TeX';
  SYNS_FriendlyLangUnknown              =  '<Unknown>';
  SYNS_FriendlyLangUnreal               =  'Unreal';
  SYNS_FriendlyLangURI                  =  'URI';
  SYNS_FriendlyLangVBSScript            =  'MS VBScript';
  SYNS_FriendlyLangVisualBASIC          =  'Visual Basic';
  SYNS_FriendlyLangVrml97               =  'Vrml97';
  SYNS_FriendlyLangX86Asm               =  'x86 Assembly';
  SYNS_FriendlyLangXML                  =  'XML';

implementation

end.
