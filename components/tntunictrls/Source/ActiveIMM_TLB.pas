
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit ActiveIMM_TLB;

{$INCLUDE TntCompilers.inc}

{TNT-IGNORE-UNIT}

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88.1.0.1.0  $
// File generated on 04/03/2001 11:32:13 PM from Type Library described below.

// *************************************************************************//
// NOTE:
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties
// which return objects that may need to be explicitly created via a function
// call prior to any access via the property. These items have been disabled
// in order to prevent accidental use from within the object inspector. You
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively
// removing them from the $IFDEF blocks. However, such items must still be
// programmatically created via a method of the appropriate CoClass before
// they can be used.
// ************************************************************************ //
// Type Lib: C:\Program Files\Microsoft Platform SDK\Include\dimm.tlb (1)
// IID\LCID: {4955DD30-B159-11D0-8FCF-00AA006BCC59}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\Stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// Errors:
//   Hint: Member 'End' of 'IActiveIMMMessagePumpOwner' changed to 'End_'
//   Error creating palette bitmap of (TCActiveIMM) : Server D:\D5Addons\Dimm\dimm.dll contains no icons
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses
  Windows, ActiveX, Classes, OleServer;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  ActiveIMMMajorVersion = 0;
  ActiveIMMMinorVersion = 1;

  LIBID_ActiveIMM: TGUID = '{4955DD30-B159-11D0-8FCF-00AA006BCC59}';

  IID_IEnumRegisterWordA: TGUID = '{08C03412-F96B-11D0-A475-00AA006BCC59}';
  IID_IEnumRegisterWordW: TGUID = '{4955DD31-B159-11D0-8FCF-00AA006BCC59}';
  IID_IEnumInputContext: TGUID = '{09B5EAB0-F997-11D1-93D4-0060B067B86E}';
  IID_IActiveIMMRegistrar: TGUID = '{B3458082-BD00-11D1-939B-0060B067B86E}';
  IID_IActiveIMMMessagePumpOwner: TGUID = '{B5CF2CFA-8AEB-11D1-9364-0060B067B86E}';
  IID_IActiveIMMApp: TGUID = '{08C0E040-62D1-11D1-9326-0060B067B86E}';
  IID_IActiveIMMIME: TGUID = '{08C03411-F96B-11D0-A475-00AA006BCC59}';
  IID_IActiveIME: TGUID = '{6FE20962-D077-11D0-8FE7-00AA006BCC59}';
  IID_IActiveIME2: TGUID = '{E1C4BF0E-2D53-11D2-93E1-0060B067B86E}';
  CLASS_CActiveIMM: TGUID = '{4955DD33-B159-11D0-8FCF-00AA006BCC59}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IEnumRegisterWordA = interface;
  IEnumRegisterWordW = interface;
  IEnumInputContext = interface;
  IActiveIMMRegistrar = interface;
  IActiveIMMMessagePumpOwner = interface;
  IActiveIMMApp = interface;
  IActiveIMMIME = interface;
  IActiveIME = interface;
  IActiveIME2 = interface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CActiveIMM = IActiveIMMApp;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  wireHBITMAP = ^_userHBITMAP; 
  wireHWND = ^_RemotableHandle; 
  PUserType1 = ^TGUID; {*}
  PUserType2 = ^tagMSG; {*}
  PUserType3 = ^REGISTERWORDA; {*}
  PUserType4 = ^REGISTERWORDW; {*}
  PUserType5 = ^CANDIDATEFORM; {*}
  PUserType6 = ^LOGFONTA; {*}
  PUserType7 = ^LOGFONTW; {*}
  PUserType8 = ^COMPOSITIONFORM; {*}
  PUserType9 = ^tagPOINT; {*}
  PWord1 = ^Word; {*}
  PUserType10 = ^IMEMENUITEMINFOA; {*}
  PUserType11 = ^IMEMENUITEMINFOW; {*}
  PUserType12 = ^INPUTCONTEXT; {*}
  PByte1 = ^Byte; {*}

  __MIDL___MIDL_itf_dimm_0000_0001 = packed record
    lpReading: PAnsiChar;
    lpWord: PAnsiChar;
  end;

  REGISTERWORDA = __MIDL___MIDL_itf_dimm_0000_0001; 

  __MIDL___MIDL_itf_dimm_0000_0002 = packed record
    lpReading: PWideChar;
    lpWord: PWideChar;
  end;

  REGISTERWORDW = __MIDL___MIDL_itf_dimm_0000_0002; 

  __MIDL___MIDL_itf_dimm_0000_0003 = packed record
    lfHeight: Integer;
    lfWidth: Integer;
    lfEscapement: Integer;
    lfOrientation: Integer;
    lfWeight: Integer;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..31] of Shortint;
  end;

  LOGFONTA = __MIDL___MIDL_itf_dimm_0000_0003; 

  __MIDL___MIDL_itf_dimm_0000_0004 = packed record
    lfHeight: Integer;
    lfWidth: Integer;
    lfEscapement: Integer;
    lfOrientation: Integer;
    lfWeight: Integer;
    lfItalic: Byte;
    lfUnderline: Byte;
    lfStrikeOut: Byte;
    lfCharSet: Byte;
    lfOutPrecision: Byte;
    lfClipPrecision: Byte;
    lfQuality: Byte;
    lfPitchAndFamily: Byte;
    lfFaceName: array[0..31] of Word;
  end;

  LOGFONTW = __MIDL___MIDL_itf_dimm_0000_0004; 

  tagPOINT = packed record
    x: Integer;
    y: Integer;
  end;

  tagRECT = packed record
    left: Integer;
    top: Integer;
    right: Integer;
    bottom: Integer;
  end;

  __MIDL___MIDL_itf_dimm_0000_0005 = packed record
    dwIndex: LongWord;
    dwStyle: LongWord;
    ptCurrentPos: tagPOINT;
    rcArea: tagRECT;
  end;

  CANDIDATEFORM = __MIDL___MIDL_itf_dimm_0000_0005; 

  __MIDL___MIDL_itf_dimm_0000_0006 = packed record
    dwStyle: LongWord;
    ptCurrentPos: tagPOINT;
    rcArea: tagRECT;
  end;

  COMPOSITIONFORM = __MIDL___MIDL_itf_dimm_0000_0006; 

  __MIDL___MIDL_itf_dimm_0000_0007 = packed record
    dwSize: LongWord;
    dwStyle: LongWord;
    dwCount: LongWord;
    dwSelection: LongWord;
    dwPageStart: LongWord;
    dwPageSize: LongWord;
    dwOffset: array[0..0] of LongWord;
  end;

  CANDIDATELIST = __MIDL___MIDL_itf_dimm_0000_0007; 

  __MIDL___MIDL_itf_dimm_0000_0008 = packed record
    dwStyle: LongWord;
    szDescription: array[0..31] of Shortint;
  end;

  STYLEBUFA = __MIDL___MIDL_itf_dimm_0000_0008; 

  __MIDL___MIDL_itf_dimm_0000_0009 = packed record
    dwStyle: LongWord;
    szDescription: array[0..31] of Word;
  end;

  STYLEBUFW = __MIDL___MIDL_itf_dimm_0000_0009; 

  __MIDL___MIDL_itf_dimm_0000_0010 = packed record
    cbSize: SYSUINT;
    fType: SYSUINT;
    fState: SYSUINT;
    wID: SYSUINT;
    hbmpChecked: wireHBITMAP;
    hbmpUnchecked: wireHBITMAP;
    dwItemData: LongWord;
    szString: array[0..79] of Shortint;
    hbmpItem: wireHBITMAP;
  end;

  IMEMENUITEMINFOA = __MIDL___MIDL_itf_dimm_0000_0010; 

  _userBITMAP = packed record
    bmType: Integer;
    bmWidth: Integer;
    bmHeight: Integer;
    bmWidthBytes: Integer;
    bmPlanes: Word;
    bmBitsPixel: Word;
    cbSize: LongWord;
    pBuffer: ^Byte;
  end;

  __MIDL_IWinTypes_0007 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: ^_userBITMAP);
  end;

  _userHBITMAP = packed record
    fContext: Integer;
    u: __MIDL_IWinTypes_0007;
  end;

  __MIDL___MIDL_itf_dimm_0000_0011 = packed record
    cbSize: SYSUINT;
    fType: SYSUINT;
    fState: SYSUINT;
    wID: SYSUINT;
    hbmpChecked: wireHBITMAP;
    hbmpUnchecked: wireHBITMAP;
    dwItemData: LongWord;
    szString: array[0..79] of Word;
    hbmpItem: wireHBITMAP;
  end;

  IMEMENUITEMINFOW = __MIDL___MIDL_itf_dimm_0000_0011; 

  __MIDL___MIDL_itf_dimm_0000_0013 = record
    case Integer of
      0: (A: LOGFONTA);
      1: (W: LOGFONTW);
  end;

  __MIDL___MIDL_itf_dimm_0000_0012 = packed record
    hWnd: wireHWND;
    fOpen: Integer;
    ptStatusWndPos: tagPOINT;
    ptSoftKbdPos: tagPOINT;
    fdwConversion: LongWord;
    fdwSentence: LongWord;
    lfFont: __MIDL___MIDL_itf_dimm_0000_0013;
    cfCompForm: COMPOSITIONFORM;
    cfCandForm: array[0..3] of CANDIDATEFORM;
    hCompStr: LongWord;
    hCandInfo: LongWord;
    hGuideLine: LongWord;
    hPrivate: LongWord;
    dwNumMsgBuf: LongWord;
    hMsgBuf: LongWord;
    fdwInit: LongWord;
    dwReserve: array[0..2] of LongWord;
  end;

  __MIDL_IWinTypes_0009 = record
    case Integer of
      0: (hInproc: Integer);
      1: (hRemote: Integer);
  end;

  _RemotableHandle = packed record
    fContext: Integer;
    u: __MIDL_IWinTypes_0009;
  end;

  INPUTCONTEXT = __MIDL___MIDL_itf_dimm_0000_0012; 

  __MIDL___MIDL_itf_dimm_0000_0014 = packed record
    dwPrivateDataSize: LongWord;
    fdwProperty: LongWord;
    fdwConversionCaps: LongWord;
    fdwSentenceCaps: LongWord;
    fdwUICaps: LongWord;
    fdwSCSCaps: LongWord;
    fdwSelectCaps: LongWord;
  end;

  IMEINFO = __MIDL___MIDL_itf_dimm_0000_0014;
  UINT_PTR = LongWord; 
  LONG_PTR = Integer; 

  tagMSG = packed record
    hWnd: wireHWND;
    message: SYSUINT;
    wParam: UINT_PTR;
    lParam: LONG_PTR;
    time: LongWord;
    pt: tagPOINT;
  end;


// *********************************************************************//
// Interface: IEnumRegisterWordA
// Flags:     (0)
// GUID:      {08C03412-F96B-11D0-A475-00AA006BCC59}
// *********************************************************************//
  IEnumRegisterWordA = interface(IUnknown)
    ['{08C03412-F96B-11D0-A475-00AA006BCC59}']
    function  Clone(out ppEnum: IEnumRegisterWordA): HResult; stdcall;
    function  Next(ulCount: LongWord; out rgRegisterWord: REGISTERWORDA; out pcFetched: LongWord): HResult; stdcall;
    function  Reset: HResult; stdcall;
    function  Skip(ulCount: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumRegisterWordW
// Flags:     (0)
// GUID:      {4955DD31-B159-11D0-8FCF-00AA006BCC59}
// *********************************************************************//
  IEnumRegisterWordW = interface(IUnknown)
    ['{4955DD31-B159-11D0-8FCF-00AA006BCC59}']
    function  Clone(out ppEnum: IEnumRegisterWordW): HResult; stdcall;
    function  Next(ulCount: LongWord; out rgRegisterWord: REGISTERWORDW; out pcFetched: LongWord): HResult; stdcall;
    function  Reset: HResult; stdcall;
    function  Skip(ulCount: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumInputContext
// Flags:     (0)
// GUID:      {09B5EAB0-F997-11D1-93D4-0060B067B86E}
// *********************************************************************//
  IEnumInputContext = interface(IUnknown)
    ['{09B5EAB0-F997-11D1-93D4-0060B067B86E}']
    function  Clone(out ppEnum: IEnumInputContext): HResult; stdcall;
    function  Next(ulCount: LongWord; out rgInputContext: LongWord; out pcFetched: LongWord): HResult; stdcall;
    function  Reset: HResult; stdcall;
    function  Skip(ulCount: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IActiveIMMRegistrar
// Flags:     (0)
// GUID:      {B3458082-BD00-11D1-939B-0060B067B86E}
// *********************************************************************//
  IActiveIMMRegistrar = interface(IUnknown)
    ['{B3458082-BD00-11D1-939B-0060B067B86E}']
    function  RegisterIME(var rclsid: TGUID; lgid: Word; pszIconFile: PWideChar; pszDesc: PWideChar): HResult; stdcall;
    function  UnregisterIME(var rclsid: TGUID): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IActiveIMMMessagePumpOwner
// Flags:     (0)
// GUID:      {B5CF2CFA-8AEB-11D1-9364-0060B067B86E}
// *********************************************************************//
  IActiveIMMMessagePumpOwner = interface(IUnknown)
    ['{B5CF2CFA-8AEB-11D1-9364-0060B067B86E}']
    function  Start: HResult; stdcall;
    function  End_: HResult; stdcall;
    function  OnTranslateMessage(var pMsg: tagMSG): HResult; stdcall;
    function  Pause(out pdwCookie: LongWord): HResult; stdcall;
    function  Resume(dwCookie: LongWord): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IActiveIMMApp
// Flags:     (0)
// GUID:      {08C0E040-62D1-11D1-9326-0060B067B86E}
// *********************************************************************//
  IActiveIMMApp = interface(IUnknown)
    ['{08C0E040-62D1-11D1-9326-0060B067B86E}']
    function  AssociateContext(var hWnd: _RemotableHandle; hIME: LongWord; out phPrev: LongWord): HResult; stdcall;
    function  ConfigureIMEA(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                            var pData: REGISTERWORDA): HResult; stdcall;
    function  ConfigureIMEW(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                            var pData: REGISTERWORDW): HResult; stdcall;
    function  CreateContext(out phIMC: LongWord): HResult; stdcall;
    function  DestroyContext(hIME: LongWord): HResult; stdcall;
    function  EnumRegisterWordA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                                szRegister: PAnsiChar; var pData: Pointer; out pEnum: IEnumRegisterWordA): HResult; stdcall;
    function  EnumRegisterWordW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                                szRegister: PWideChar; var pData: Pointer; 
                                out pEnum: IEnumRegisterWordW): HResult; stdcall;
    function  EscapeA(var hKL: Pointer; hIMC: LongWord; uEscape: SYSUINT; var pData: Pointer; 
                      out plResult: LONG_PTR): HResult; stdcall;
    function  EscapeW(var hKL: Pointer; hIMC: LongWord; uEscape: SYSUINT; var pData: Pointer; 
                      out plResult: LONG_PTR): HResult; stdcall;
    function  GetCandidateListA(hIMC: LongWord; dwIndex: LongWord; uBufLen: SYSUINT; 
                                out pCandList: CANDIDATELIST; out puCopied: SYSUINT): HResult; stdcall;
    function  GetCandidateListW(hIMC: LongWord; dwIndex: LongWord; uBufLen: SYSUINT; 
                                out pCandList: CANDIDATELIST; out puCopied: SYSUINT): HResult; stdcall;
    function  GetCandidateListCountA(hIMC: LongWord; out pdwListSize: LongWord; 
                                     out pdwBufLen: LongWord): HResult; stdcall;
    function  GetCandidateListCountW(hIMC: LongWord; out pdwListSize: LongWord; 
                                     out pdwBufLen: LongWord): HResult; stdcall;
    function  GetCandidateWindow(hIMC: LongWord; dwIndex: LongWord; out pCandidate: CANDIDATEFORM): HResult; stdcall;
    function  GetCompositionFontA(hIMC: LongWord; out plf: LOGFONTA): HResult; stdcall;
    function  GetCompositionFontW(hIMC: LongWord; out plf: LOGFONTW): HResult; stdcall;
    function  GetCompositionStringA(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                    out plCopied: Integer; out pBuf: Pointer): HResult; stdcall;
    function  GetCompositionStringW(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                    out plCopied: Integer; out pBuf: Pointer): HResult; stdcall;
    function  GetCompositionWindow(hIMC: LongWord; out pCompForm: COMPOSITIONFORM): HResult; stdcall;
    function  GetContext(var hWnd: _RemotableHandle; out phIMC: LongWord): HResult; stdcall;
    function  GetConversionListA(var hKL: Pointer; hIMC: LongWord; pSrc: PAnsiChar; uBufLen: SYSUINT; 
                                 uFlag: SYSUINT; out pDst: CANDIDATELIST; out puCopied: SYSUINT): HResult; stdcall;
    function  GetConversionListW(var hKL: Pointer; hIMC: LongWord; pSrc: PWideChar; 
                                 uBufLen: SYSUINT; uFlag: SYSUINT; out pDst: CANDIDATELIST; 
                                 out puCopied: SYSUINT): HResult; stdcall;
    function  GetConversionStatus(hIMC: LongWord; out pfdwConversion: LongWord; 
                                  out pfdwSentence: LongWord): HResult; stdcall;
    function  GetDefaultIMEWnd(var hWnd: _RemotableHandle; out phDefWnd: wireHWND): HResult; stdcall;
    function  GetDescriptionA(var hKL: Pointer; uBufLen: SYSUINT; szDescription: PAnsiChar; 
                              out puCopied: SYSUINT): HResult; stdcall;
    function  GetDescriptionW(var hKL: Pointer; uBufLen: SYSUINT; szDescription: PWideChar; 
                              out puCopied: SYSUINT): HResult; stdcall;
    function  GetGuideLineA(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; pBuf: PAnsiChar; 
                            out pdwResult: LongWord): HResult; stdcall;
    function  GetGuideLineW(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; pBuf: PWideChar; 
                            out pdwResult: LongWord): HResult; stdcall;
    function  GetIMEFileNameA(var hKL: Pointer; uBufLen: SYSUINT; szFileName: PAnsiChar; 
                              out puCopied: SYSUINT): HResult; stdcall;
    function  GetIMEFileNameW(var hKL: Pointer; uBufLen: SYSUINT; szFileName: PWideChar; 
                              out puCopied: SYSUINT): HResult; stdcall;
    function  GetOpenStatus(hIMC: LongWord): HResult; stdcall;
    function  GetProperty(var hKL: Pointer; fdwIndex: LongWord; out pdwProperty: LongWord): HResult; stdcall;
    function  GetRegisterWordStyleA(var hKL: Pointer; nItem: SYSUINT; out pStyleBuf: STYLEBUFA; 
                                    out puCopied: SYSUINT): HResult; stdcall;
    function  GetRegisterWordStyleW(var hKL: Pointer; nItem: SYSUINT; out pStyleBuf: STYLEBUFW; 
                                    out puCopied: SYSUINT): HResult; stdcall;
    function  GetStatusWindowPos(hIMC: LongWord; out pptPos: tagPOINT): HResult; stdcall;
    function  GetVirtualKey(var hWnd: _RemotableHandle; out puVirtualKey: SYSUINT): HResult; stdcall;
    function  InstallIMEA(szIMEFileName: PAnsiChar; szLayoutText: PAnsiChar; out phKL: Pointer): HResult; stdcall;
    function  InstallIMEW(szIMEFileName: PWideChar; szLayoutText: PWideChar; out phKL: Pointer): HResult; stdcall;
    function  IsIME(var hKL: Pointer): HResult; stdcall;
    function  IsUIMessageA(var hWndIME: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                           lParam: LONG_PTR): HResult; stdcall;
    function  IsUIMessageW(var hWndIME: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                           lParam: LONG_PTR): HResult; stdcall;
    function  NotifyIME(hIMC: LongWord; dwAction: LongWord; dwIndex: LongWord; dwValue: LongWord): HResult; stdcall;
    function  REGISTERWORDA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; szRegister: PAnsiChar): HResult; stdcall;
    function  REGISTERWORDW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                            szRegister: PWideChar): HResult; stdcall;
    function  ReleaseContext(var hWnd: _RemotableHandle; hIMC: LongWord): HResult; stdcall;
    function  SetCandidateWindow(hIMC: LongWord; var pCandidate: CANDIDATEFORM): HResult; stdcall;
    function  SetCompositionFontA(hIMC: LongWord; var plf: LOGFONTA): HResult; stdcall;
    function  SetCompositionFontW(hIMC: LongWord; var plf: LOGFONTW): HResult; stdcall;
    function  SetCompositionStringA(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                    dwCompLen: LongWord; var pRead: Pointer; dwReadLen: LongWord): HResult; stdcall;
    function  SetCompositionStringW(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                    dwCompLen: LongWord; var pRead: Pointer; dwReadLen: LongWord): HResult; stdcall;
    function  SetCompositionWindow(hIMC: LongWord; var pCompForm: COMPOSITIONFORM): HResult; stdcall;
    function  SetConversionStatus(hIMC: LongWord; fdwConversion: LongWord; fdwSentence: LongWord): HResult; stdcall;
    function  SetOpenStatus(hIMC: LongWord; fOpen: Integer): HResult; stdcall;
    function  SetStatusWindowPos(hIMC: LongWord; var pptPos: tagPOINT): HResult; stdcall;
    function  SimulateHotKey(var hWnd: _RemotableHandle; dwHotKeyID: LongWord): HResult; stdcall;
    function  UnregisterWordA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                              szUnregister: PAnsiChar): HResult; stdcall;
    function  UnregisterWordW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                              szUnregister: PWideChar): HResult; stdcall;
    function  Activate(fRestoreLayout: Integer): HResult; stdcall;
    function  Deactivate: HResult; stdcall;
    function  OnDefWindowProc(var hWnd: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                              lParam: LONG_PTR; out plResult: LONG_PTR): HResult; stdcall;
    function  FilterClientWindows(var aaClassList: Word; uSize: SYSUINT): HResult; stdcall;
    function  GetCodePageA(var hKL: Pointer; out uCodePage: SYSUINT): HResult; stdcall;
    function  GetLangId(var hKL: Pointer; out plid: Word): HResult; stdcall;
    function  AssociateContextEx(var hWnd: _RemotableHandle; hIMC: LongWord; dwFlags: LongWord): HResult; stdcall;
    function  DisableIME(idThread: LongWord): HResult; stdcall;
    function  GetImeMenuItemsA(hIMC: LongWord; dwFlags: LongWord; dwType: LongWord; 
                               var pImeParentMenu: IMEMENUITEMINFOA; 
                               out pImeMenu: IMEMENUITEMINFOA; dwSize: LongWord; 
                               out pdwResult: LongWord): HResult; stdcall;
    function  GetImeMenuItemsW(hIMC: LongWord; dwFlags: LongWord; dwType: LongWord; 
                               var pImeParentMenu: IMEMENUITEMINFOW; 
                               out pImeMenu: IMEMENUITEMINFOW; dwSize: LongWord; 
                               out pdwResult: LongWord): HResult; stdcall;
    function  EnumInputContext(idThread: LongWord; out ppEnum: IEnumInputContext): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IActiveIMMIME
// Flags:     (0)
// GUID:      {08C03411-F96B-11D0-A475-00AA006BCC59}
// *********************************************************************//
  IActiveIMMIME = interface(IUnknown)
    ['{08C03411-F96B-11D0-A475-00AA006BCC59}']
    function  AssociateContext(var hWnd: _RemotableHandle; hIME: LongWord; out phPrev: LongWord): HResult; stdcall;
    function  ConfigureIMEA(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                            var pData: REGISTERWORDA): HResult; stdcall;
    function  ConfigureIMEW(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                            var pData: REGISTERWORDW): HResult; stdcall;
    function  CreateContext(out phIMC: LongWord): HResult; stdcall;
    function  DestroyContext(hIME: LongWord): HResult; stdcall;
    function  EnumRegisterWordA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                                szRegister: PAnsiChar; var pData: Pointer; out pEnum: IEnumRegisterWordA): HResult; stdcall;
    function  EnumRegisterWordW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                                szRegister: PWideChar; var pData: Pointer; 
                                out pEnum: IEnumRegisterWordW): HResult; stdcall;
    function  EscapeA(var hKL: Pointer; hIMC: LongWord; uEscape: SYSUINT; var pData: Pointer; 
                      out plResult: LONG_PTR): HResult; stdcall;
    function  EscapeW(var hKL: Pointer; hIMC: LongWord; uEscape: SYSUINT; var pData: Pointer; 
                      out plResult: LONG_PTR): HResult; stdcall;
    function  GetCandidateListA(hIMC: LongWord; dwIndex: LongWord; uBufLen: SYSUINT; 
                                out pCandList: CANDIDATELIST; out puCopied: SYSUINT): HResult; stdcall;
    function  GetCandidateListW(hIMC: LongWord; dwIndex: LongWord; uBufLen: SYSUINT; 
                                out pCandList: CANDIDATELIST; out puCopied: SYSUINT): HResult; stdcall;
    function  GetCandidateListCountA(hIMC: LongWord; out pdwListSize: LongWord; 
                                     out pdwBufLen: LongWord): HResult; stdcall;
    function  GetCandidateListCountW(hIMC: LongWord; out pdwListSize: LongWord; 
                                     out pdwBufLen: LongWord): HResult; stdcall;
    function  GetCandidateWindow(hIMC: LongWord; dwIndex: LongWord; out pCandidate: CANDIDATEFORM): HResult; stdcall;
    function  GetCompositionFontA(hIMC: LongWord; out plf: LOGFONTA): HResult; stdcall;
    function  GetCompositionFontW(hIMC: LongWord; out plf: LOGFONTW): HResult; stdcall;
    function  GetCompositionStringA(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                    out plCopied: Integer; out pBuf: Pointer): HResult; stdcall;
    function  GetCompositionStringW(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                    out plCopied: Integer; out pBuf: Pointer): HResult; stdcall;
    function  GetCompositionWindow(hIMC: LongWord; out pCompForm: COMPOSITIONFORM): HResult; stdcall;
    function  GetContext(var hWnd: _RemotableHandle; out phIMC: LongWord): HResult; stdcall;
    function  GetConversionListA(var hKL: Pointer; hIMC: LongWord; pSrc: PAnsiChar; uBufLen: SYSUINT; 
                                 uFlag: SYSUINT; out pDst: CANDIDATELIST; out puCopied: SYSUINT): HResult; stdcall;
    function  GetConversionListW(var hKL: Pointer; hIMC: LongWord; pSrc: PWideChar; 
                                 uBufLen: SYSUINT; uFlag: SYSUINT; out pDst: CANDIDATELIST; 
                                 out puCopied: SYSUINT): HResult; stdcall;
    function  GetConversionStatus(hIMC: LongWord; out pfdwConversion: LongWord; 
                                  out pfdwSentence: LongWord): HResult; stdcall;
    function  GetDefaultIMEWnd(var hWnd: _RemotableHandle; out phDefWnd: wireHWND): HResult; stdcall;
    function  GetDescriptionA(var hKL: Pointer; uBufLen: SYSUINT; szDescription: PAnsiChar; 
                              out puCopied: SYSUINT): HResult; stdcall;
    function  GetDescriptionW(var hKL: Pointer; uBufLen: SYSUINT; szDescription: PWideChar; 
                              out puCopied: SYSUINT): HResult; stdcall;
    function  GetGuideLineA(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; pBuf: PAnsiChar; 
                            out pdwResult: LongWord): HResult; stdcall;
    function  GetGuideLineW(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; pBuf: PWideChar; 
                            out pdwResult: LongWord): HResult; stdcall;
    function  GetIMEFileNameA(var hKL: Pointer; uBufLen: SYSUINT; szFileName: PAnsiChar; 
                              out puCopied: SYSUINT): HResult; stdcall;
    function  GetIMEFileNameW(var hKL: Pointer; uBufLen: SYSUINT; szFileName: PWideChar; 
                              out puCopied: SYSUINT): HResult; stdcall;
    function  GetOpenStatus(hIMC: LongWord): HResult; stdcall;
    function  GetProperty(var hKL: Pointer; fdwIndex: LongWord; out pdwProperty: LongWord): HResult; stdcall;
    function  GetRegisterWordStyleA(var hKL: Pointer; nItem: SYSUINT; out pStyleBuf: STYLEBUFA; 
                                    out puCopied: SYSUINT): HResult; stdcall;
    function  GetRegisterWordStyleW(var hKL: Pointer; nItem: SYSUINT; out pStyleBuf: STYLEBUFW; 
                                    out puCopied: SYSUINT): HResult; stdcall;
    function  GetStatusWindowPos(hIMC: LongWord; out pptPos: tagPOINT): HResult; stdcall;
    function  GetVirtualKey(var hWnd: _RemotableHandle; out puVirtualKey: SYSUINT): HResult; stdcall;
    function  InstallIMEA(szIMEFileName: PAnsiChar; szLayoutText: PAnsiChar; out phKL: Pointer): HResult; stdcall;
    function  InstallIMEW(szIMEFileName: PWideChar; szLayoutText: PWideChar; out phKL: Pointer): HResult; stdcall;
    function  IsIME(var hKL: Pointer): HResult; stdcall;
    function  IsUIMessageA(var hWndIME: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                           lParam: LONG_PTR): HResult; stdcall;
    function  IsUIMessageW(var hWndIME: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                           lParam: LONG_PTR): HResult; stdcall;
    function  NotifyIME(hIMC: LongWord; dwAction: LongWord; dwIndex: LongWord; dwValue: LongWord): HResult; stdcall;
    function  REGISTERWORDA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; szRegister: PAnsiChar): HResult; stdcall;
    function  REGISTERWORDW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                            szRegister: PWideChar): HResult; stdcall;
    function  ReleaseContext(var hWnd: _RemotableHandle; hIMC: LongWord): HResult; stdcall;
    function  SetCandidateWindow(hIMC: LongWord; var pCandidate: CANDIDATEFORM): HResult; stdcall;
    function  SetCompositionFontA(hIMC: LongWord; var plf: LOGFONTA): HResult; stdcall;
    function  SetCompositionFontW(hIMC: LongWord; var plf: LOGFONTW): HResult; stdcall;
    function  SetCompositionStringA(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                    dwCompLen: LongWord; var pRead: Pointer; dwReadLen: LongWord): HResult; stdcall;
    function  SetCompositionStringW(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                    dwCompLen: LongWord; var pRead: Pointer; dwReadLen: LongWord): HResult; stdcall;
    function  SetCompositionWindow(hIMC: LongWord; var pCompForm: COMPOSITIONFORM): HResult; stdcall;
    function  SetConversionStatus(hIMC: LongWord; fdwConversion: LongWord; fdwSentence: LongWord): HResult; stdcall;
    function  SetOpenStatus(hIMC: LongWord; fOpen: Integer): HResult; stdcall;
    function  SetStatusWindowPos(hIMC: LongWord; var pptPos: tagPOINT): HResult; stdcall;
    function  SimulateHotKey(var hWnd: _RemotableHandle; dwHotKeyID: LongWord): HResult; stdcall;
    function  UnregisterWordA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                              szUnregister: PAnsiChar): HResult; stdcall;
    function  UnregisterWordW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                              szUnregister: PWideChar): HResult; stdcall;
    function  GenerateMessage(hIMC: LongWord): HResult; stdcall;
    function  LockIMC(hIMC: LongWord; out ppIMC: PUserType12): HResult; stdcall;
    function  UnlockIMC(hIMC: LongWord): HResult; stdcall;
    function  GetIMCLockCount(hIMC: LongWord; out pdwLockCount: LongWord): HResult; stdcall;
    function  CreateIMCC(dwSize: LongWord; out phIMCC: LongWord): HResult; stdcall;
    function  DestroyIMCC(hIMCC: LongWord): HResult; stdcall;
    function  LockIMCC(hIMCC: LongWord; out ppv: Pointer): HResult; stdcall;
    function  UnlockIMCC(hIMCC: LongWord): HResult; stdcall;
    function  ReSizeIMCC(hIMCC: LongWord; dwSize: LongWord; out phIMCC: LongWord): HResult; stdcall;
    function  GetIMCCSize(hIMCC: LongWord; out pdwSize: LongWord): HResult; stdcall;
    function  GetIMCCLockCount(hIMCC: LongWord; out pdwLockCount: LongWord): HResult; stdcall;
    function  GetHotKey(dwHotKeyID: LongWord; out puModifiers: SYSUINT; out puVKey: SYSUINT;
                        out phKL: Pointer): HResult; stdcall;
    function  SetHotKey(dwHotKeyID: LongWord; uModifiers: SYSUINT; uVKey: SYSUINT; var hKL: Pointer): HResult; stdcall;
    function  CreateSoftKeyboard(uType: SYSUINT; var hOwner: _RemotableHandle; x: SYSINT; 
                                 y: SYSINT; out phSoftKbdWnd: wireHWND): HResult; stdcall;
    function  DestroySoftKeyboard(var hSoftKbdWnd: _RemotableHandle): HResult; stdcall;
    function  ShowSoftKeyboard(var hSoftKbdWnd: _RemotableHandle; nCmdShow: SYSINT): HResult; stdcall;
    function  GetCodePageA(var hKL: Pointer; out uCodePage: SYSUINT): HResult; stdcall;
    function  GetLangId(var hKL: Pointer; out plid: Word): HResult; stdcall;
    function  KeybdEvent(lgidIME: Word; bVk: Byte; bScan: Byte; dwFlags: LongWord; 
                         dwExtraInfo: LongWord): HResult; stdcall;
    function  LockModal: HResult; stdcall;
    function  UnlockModal: HResult; stdcall;
    function  AssociateContextEx(var hWnd: _RemotableHandle; hIMC: LongWord; dwFlags: LongWord): HResult; stdcall;
    function  DisableIME(idThread: LongWord): HResult; stdcall;
    function  GetImeMenuItemsA(hIMC: LongWord; dwFlags: LongWord; dwType: LongWord; 
                               var pImeParentMenu: IMEMENUITEMINFOA; 
                               out pImeMenu: IMEMENUITEMINFOA; dwSize: LongWord; 
                               out pdwResult: LongWord): HResult; stdcall;
    function  GetImeMenuItemsW(hIMC: LongWord; dwFlags: LongWord; dwType: LongWord; 
                               var pImeParentMenu: IMEMENUITEMINFOW; 
                               out pImeMenu: IMEMENUITEMINFOW; dwSize: LongWord; 
                               out pdwResult: LongWord): HResult; stdcall;
    function  EnumInputContext(idThread: LongWord; out ppEnum: IEnumInputContext): HResult; stdcall;
    function  RequestMessageA(hIMC: LongWord; wParam: UINT_PTR; lParam: LONG_PTR; 
                              out plResult: LONG_PTR): HResult; stdcall;
    function  RequestMessageW(hIMC: LongWord; wParam: UINT_PTR; lParam: LONG_PTR; 
                              out plResult: LONG_PTR): HResult; stdcall;
    function  SendIMCA(var hWnd: _RemotableHandle; uMsg: SYSUINT; wParam: UINT_PTR; 
                       lParam: LONG_PTR; out plResult: LONG_PTR): HResult; stdcall;
    function  SendIMCW(var hWnd: _RemotableHandle; uMsg: SYSUINT; wParam: UINT_PTR; 
                       lParam: LONG_PTR; out plResult: LONG_PTR): HResult; stdcall;
    function  IsSleeping: HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IActiveIME
// Flags:     (0)
// GUID:      {6FE20962-D077-11D0-8FE7-00AA006BCC59}
// *********************************************************************//
  IActiveIME = interface(IUnknown)
    ['{6FE20962-D077-11D0-8FE7-00AA006BCC59}']
    function  Inquire(dwSystemInfoFlags: LongWord; out pIMEInfo: IMEINFO; szWndClass: PWideChar; 
                      out pdwPrivate: LongWord): HResult; stdcall;
    function  ConversionList(hIMC: LongWord; szSource: PWideChar; uFlag: SYSUINT; uBufLen: SYSUINT; 
                             out pDest: CANDIDATELIST; out puCopied: SYSUINT): HResult; stdcall;
    function  Configure(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                        var pRegisterWord: REGISTERWORDW): HResult; stdcall;
    function  Destroy(uReserved: SYSUINT): HResult; stdcall;
    function  Escape(hIMC: LongWord; uEscape: SYSUINT; var pData: Pointer; out plResult: LONG_PTR): HResult; stdcall;
    function  SetActiveContext(hIMC: LongWord; fFlag: Integer): HResult; stdcall;
    function  ProcessKey(hIMC: LongWord; uVirKey: SYSUINT; lParam: LongWord; var pbKeyState: Byte): HResult; stdcall;
    function  Notify(hIMC: LongWord; dwAction: LongWord; dwIndex: LongWord; dwValue: LongWord): HResult; stdcall;
    function  Select(hIMC: LongWord; fSelect: Integer): HResult; stdcall;
    function  SetCompositionString(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                   dwCompLen: LongWord; var pRead: Pointer; dwReadLen: LongWord): HResult; stdcall;
    function  ToAsciiEx(uVirKey: SYSUINT; uScanCode: SYSUINT; var pbKeyState: Byte; 
                        fuState: SYSUINT; hIMC: LongWord; out pdwTransBuf: LongWord; 
                        out puSize: SYSUINT): HResult; stdcall;
    function  RegisterWord(szReading: PWideChar; dwStyle: LongWord; szString: PWideChar): HResult; stdcall;
    function  UnregisterWord(szReading: PWideChar; dwStyle: LongWord; szString: PWideChar): HResult; stdcall;
    function  GetRegisterWordStyle(nItem: SYSUINT; out pStyleBuf: STYLEBUFW; out puBufSize: SYSUINT): HResult; stdcall;
    function  EnumRegisterWord(szReading: PWideChar; dwStyle: LongWord; szRegister: PWideChar; 
                               var pData: Pointer; out ppEnum: IEnumRegisterWordW): HResult; stdcall;
    function  GetCodePageA(out uCodePage: SYSUINT): HResult; stdcall;
    function  GetLangId(out plid: Word): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IActiveIME2
// Flags:     (0)
// GUID:      {E1C4BF0E-2D53-11D2-93E1-0060B067B86E}
// *********************************************************************//
  IActiveIME2 = interface(IActiveIME)
    ['{E1C4BF0E-2D53-11D2-93E1-0060B067B86E}']
    function  Sleep: HResult; stdcall;
    function  Unsleep(fDead: Integer): HResult; stdcall;
  end;

// *********************************************************************//
// The Class CoCActiveIMM provides a Create and CreateRemote method to          
// create instances of the default interface IActiveIMMApp exposed by              
// the CoClass CActiveIMM. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCActiveIMM = class
    class function Create: IActiveIMMApp;
    class function CreateRemote(const MachineName: AnsiString): IActiveIMMApp;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCActiveIMM
// Help String      : 
// Default Interface: IActiveIMMApp
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCActiveIMMProperties= class;
{$ENDIF}
  TCActiveIMM = class(TOleServer)
  private
    FIntf:        IActiveIMMApp;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TCActiveIMMProperties;
    function      GetServerProperties: TCActiveIMMProperties;
{$ENDIF}
    function      GetDefaultInterface: IActiveIMMApp;
  protected
    procedure InitServerData; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IActiveIMMApp);
    procedure Disconnect; override;
    function  AssociateContext(var hWnd: _RemotableHandle; hIME: LongWord; out phPrev: LongWord): HResult;
    function  ConfigureIMEA(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                            var pData: REGISTERWORDA): HResult;
    function  ConfigureIMEW(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                            var pData: REGISTERWORDW): HResult;
    function  CreateContext(out phIMC: LongWord): HResult;
    function  DestroyContext(hIME: LongWord): HResult;
    function  EnumRegisterWordA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                                szRegister: PAnsiChar; var pData: Pointer; out pEnum: IEnumRegisterWordA): HResult;
    function  EnumRegisterWordW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                                szRegister: PWideChar; var pData: Pointer; 
                                out pEnum: IEnumRegisterWordW): HResult;
    function  EscapeA(var hKL: Pointer; hIMC: LongWord; uEscape: SYSUINT; var pData: Pointer; 
                      out plResult: LONG_PTR): HResult;
    function  EscapeW(var hKL: Pointer; hIMC: LongWord; uEscape: SYSUINT; var pData: Pointer; 
                      out plResult: LONG_PTR): HResult;
    function  GetCandidateListA(hIMC: LongWord; dwIndex: LongWord; uBufLen: SYSUINT; 
                                out pCandList: CANDIDATELIST; out puCopied: SYSUINT): HResult;
    function  GetCandidateListW(hIMC: LongWord; dwIndex: LongWord; uBufLen: SYSUINT; 
                                out pCandList: CANDIDATELIST; out puCopied: SYSUINT): HResult;
    function  GetCandidateListCountA(hIMC: LongWord; out pdwListSize: LongWord; 
                                     out pdwBufLen: LongWord): HResult;
    function  GetCandidateListCountW(hIMC: LongWord; out pdwListSize: LongWord; 
                                     out pdwBufLen: LongWord): HResult;
    function  GetCandidateWindow(hIMC: LongWord; dwIndex: LongWord; out pCandidate: CANDIDATEFORM): HResult;
    function  GetCompositionFontA(hIMC: LongWord; out plf: LOGFONTA): HResult;
    function  GetCompositionFontW(hIMC: LongWord; out plf: LOGFONTW): HResult;
    function  GetCompositionStringA(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                    out plCopied: Integer; out pBuf: Pointer): HResult;
    function  GetCompositionStringW(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                    out plCopied: Integer; out pBuf: Pointer): HResult;
    function  GetCompositionWindow(hIMC: LongWord; out pCompForm: COMPOSITIONFORM): HResult;
    function  GetContext(var hWnd: _RemotableHandle; out phIMC: LongWord): HResult;
    function  GetConversionListA(var hKL: Pointer; hIMC: LongWord; pSrc: PAnsiChar; uBufLen: SYSUINT; 
                                 uFlag: SYSUINT; out pDst: CANDIDATELIST; out puCopied: SYSUINT): HResult;
    function  GetConversionListW(var hKL: Pointer; hIMC: LongWord; pSrc: PWideChar; 
                                 uBufLen: SYSUINT; uFlag: SYSUINT; out pDst: CANDIDATELIST; 
                                 out puCopied: SYSUINT): HResult;
    function  GetConversionStatus(hIMC: LongWord; out pfdwConversion: LongWord; 
                                  out pfdwSentence: LongWord): HResult;
    function  GetDefaultIMEWnd(var hWnd: _RemotableHandle; out phDefWnd: wireHWND): HResult;
    function  GetDescriptionA(var hKL: Pointer; uBufLen: SYSUINT; szDescription: PAnsiChar; 
                              out puCopied: SYSUINT): HResult;
    function  GetDescriptionW(var hKL: Pointer; uBufLen: SYSUINT; szDescription: PWideChar; 
                              out puCopied: SYSUINT): HResult;
    function  GetGuideLineA(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; pBuf: PAnsiChar; 
                            out pdwResult: LongWord): HResult;
    function  GetGuideLineW(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; pBuf: PWideChar; 
                            out pdwResult: LongWord): HResult;
    function  GetIMEFileNameA(var hKL: Pointer; uBufLen: SYSUINT; szFileName: PAnsiChar; 
                              out puCopied: SYSUINT): HResult;
    function  GetIMEFileNameW(var hKL: Pointer; uBufLen: SYSUINT; szFileName: PWideChar; 
                              out puCopied: SYSUINT): HResult;
    function  GetOpenStatus(hIMC: LongWord): HResult;
    function  GetProperty(var hKL: Pointer; fdwIndex: LongWord; out pdwProperty: LongWord): HResult;
    function  GetRegisterWordStyleA(var hKL: Pointer; nItem: SYSUINT; out pStyleBuf: STYLEBUFA; 
                                    out puCopied: SYSUINT): HResult;
    function  GetRegisterWordStyleW(var hKL: Pointer; nItem: SYSUINT; out pStyleBuf: STYLEBUFW; 
                                    out puCopied: SYSUINT): HResult;
    function  GetStatusWindowPos(hIMC: LongWord; out pptPos: tagPOINT): HResult;
    function  GetVirtualKey(var hWnd: _RemotableHandle; out puVirtualKey: SYSUINT): HResult;
    function  InstallIMEA(szIMEFileName: PAnsiChar; szLayoutText: PAnsiChar; out phKL: Pointer): HResult;
    function  InstallIMEW(szIMEFileName: PWideChar; szLayoutText: PWideChar; out phKL: Pointer): HResult;
    function  IsIME(var hKL: Pointer): HResult;
    function  IsUIMessageA(var hWndIME: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                           lParam: LONG_PTR): HResult;
    function  IsUIMessageW(var hWndIME: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                           lParam: LONG_PTR): HResult;
    function  NotifyIME(hIMC: LongWord; dwAction: LongWord; dwIndex: LongWord; dwValue: LongWord): HResult;
    function  REGISTERWORDA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; szRegister: PAnsiChar): HResult;
    function  REGISTERWORDW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                            szRegister: PWideChar): HResult;
    function  ReleaseContext(var hWnd: _RemotableHandle; hIMC: LongWord): HResult;
    function  SetCandidateWindow(hIMC: LongWord; var pCandidate: CANDIDATEFORM): HResult;
    function  SetCompositionFontA(hIMC: LongWord; var plf: LOGFONTA): HResult;
    function  SetCompositionFontW(hIMC: LongWord; var plf: LOGFONTW): HResult;
    function  SetCompositionStringA(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                    dwCompLen: LongWord; var pRead: Pointer; dwReadLen: LongWord): HResult;
    function  SetCompositionStringW(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                    dwCompLen: LongWord; var pRead: Pointer; dwReadLen: LongWord): HResult;
    function  SetCompositionWindow(hIMC: LongWord; var pCompForm: COMPOSITIONFORM): HResult;
    function  SetConversionStatus(hIMC: LongWord; fdwConversion: LongWord; fdwSentence: LongWord): HResult;
    function  SetOpenStatus(hIMC: LongWord; fOpen: Integer): HResult;
    function  SetStatusWindowPos(hIMC: LongWord; var pptPos: tagPOINT): HResult;
    function  SimulateHotKey(var hWnd: _RemotableHandle; dwHotKeyID: LongWord): HResult;
    function  UnregisterWordA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                              szUnregister: PAnsiChar): HResult;
    function  UnregisterWordW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                              szUnregister: PWideChar): HResult;
    function  Activate(fRestoreLayout: Integer): HResult;
    function  Deactivate: HResult;
    function  OnDefWindowProc(var hWnd: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                              lParam: LONG_PTR; out plResult: LONG_PTR): HResult;
    function  FilterClientWindows(var aaClassList: Word; uSize: SYSUINT): HResult;
    function  GetCodePageA(var hKL: Pointer; out uCodePage: SYSUINT): HResult;
    function  GetLangId(var hKL: Pointer; out plid: Word): HResult;
    function  AssociateContextEx(var hWnd: _RemotableHandle; hIMC: LongWord; dwFlags: LongWord): HResult;
    function  DisableIME(idThread: LongWord): HResult;
    function  GetImeMenuItemsA(hIMC: LongWord; dwFlags: LongWord; dwType: LongWord; 
                               var pImeParentMenu: IMEMENUITEMINFOA; 
                               out pImeMenu: IMEMENUITEMINFOA; dwSize: LongWord; 
                               out pdwResult: LongWord): HResult;
    function  GetImeMenuItemsW(hIMC: LongWord; dwFlags: LongWord; dwType: LongWord; 
                               var pImeParentMenu: IMEMENUITEMINFOW; 
                               out pImeMenu: IMEMENUITEMINFOW; dwSize: LongWord; 
                               out pdwResult: LongWord): HResult;
    function  EnumInputContext(idThread: LongWord; out ppEnum: IEnumInputContext): HResult;
    property  DefaultInterface: IActiveIMMApp read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCActiveIMMProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCActiveIMM
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCActiveIMMProperties = class(TPersistent)
  private
    FServer:    TCActiveIMM;
    function    GetDefaultInterface: IActiveIMMApp;
    constructor Create(AServer: TCActiveIMM);
  protected
  public
    property DefaultInterface: IActiveIMMApp read GetDefaultInterface;
  published
  end;
{$ENDIF}

implementation

uses
  ComObj;

class function CoCActiveIMM.Create: IActiveIMMApp;
begin
  Result := CreateComObject(CLASS_CActiveIMM) as IActiveIMMApp;
end;

class function CoCActiveIMM.CreateRemote(const MachineName: AnsiString): IActiveIMMApp;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CActiveIMM) as IActiveIMMApp;
end;

procedure TCActiveIMM.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{4955DD33-B159-11D0-8FCF-00AA006BCC59}';
    IntfIID:   '{08C0E040-62D1-11D1-9326-0060B067B86E}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCActiveIMM.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IActiveIMMApp;
  end;
end;

procedure TCActiveIMM.ConnectTo(svrIntf: IActiveIMMApp);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TCActiveIMM.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TCActiveIMM.GetDefaultInterface: IActiveIMMApp;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCActiveIMM.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCActiveIMMProperties.Create(Self);
{$ENDIF}
end;

destructor TCActiveIMM.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCActiveIMM.GetServerProperties: TCActiveIMMProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TCActiveIMM.AssociateContext(var hWnd: _RemotableHandle; hIME: LongWord; 
                                       out phPrev: LongWord): HResult;
begin
  Result := DefaultInterface.AssociateContext(hWnd, hIME, phPrev);
end;

function  TCActiveIMM.ConfigureIMEA(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                                    var pData: REGISTERWORDA): HResult;
begin
  Result := DefaultInterface.ConfigureIMEA(hKL, hWnd, dwMode, pData);
end;

function  TCActiveIMM.ConfigureIMEW(var hKL: Pointer; var hWnd: _RemotableHandle; dwMode: LongWord; 
                                    var pData: REGISTERWORDW): HResult;
begin
  Result := DefaultInterface.ConfigureIMEW(hKL, hWnd, dwMode, pData);
end;

function  TCActiveIMM.CreateContext(out phIMC: LongWord): HResult;
begin
  Result := DefaultInterface.CreateContext(phIMC);
end;

function  TCActiveIMM.DestroyContext(hIME: LongWord): HResult;
begin
  Result := DefaultInterface.DestroyContext(hIME);
end;

function  TCActiveIMM.EnumRegisterWordA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                                        szRegister: PAnsiChar; var pData: Pointer; 
                                        out pEnum: IEnumRegisterWordA): HResult;
begin
  Result := DefaultInterface.EnumRegisterWordA(hKL, szReading, dwStyle, szRegister, pData, pEnum);
end;

function  TCActiveIMM.EnumRegisterWordW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                                        szRegister: PWideChar; var pData: Pointer; 
                                        out pEnum: IEnumRegisterWordW): HResult;
begin
  Result := DefaultInterface.EnumRegisterWordW(hKL, szReading, dwStyle, szRegister, pData, pEnum);
end;

function  TCActiveIMM.EscapeA(var hKL: Pointer; hIMC: LongWord; uEscape: SYSUINT; 
                              var pData: Pointer; out plResult: LONG_PTR): HResult;
begin
  Result := DefaultInterface.EscapeA(hKL, hIMC, uEscape, pData, plResult);
end;

function  TCActiveIMM.EscapeW(var hKL: Pointer; hIMC: LongWord; uEscape: SYSUINT; 
                              var pData: Pointer; out plResult: LONG_PTR): HResult;
begin
  Result := DefaultInterface.EscapeW(hKL, hIMC, uEscape, pData, plResult);
end;

function  TCActiveIMM.GetCandidateListA(hIMC: LongWord; dwIndex: LongWord; uBufLen: SYSUINT; 
                                        out pCandList: CANDIDATELIST; out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetCandidateListA(hIMC, dwIndex, uBufLen, pCandList, puCopied);
end;

function  TCActiveIMM.GetCandidateListW(hIMC: LongWord; dwIndex: LongWord; uBufLen: SYSUINT; 
                                        out pCandList: CANDIDATELIST; out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetCandidateListW(hIMC, dwIndex, uBufLen, pCandList, puCopied);
end;

function  TCActiveIMM.GetCandidateListCountA(hIMC: LongWord; out pdwListSize: LongWord; 
                                             out pdwBufLen: LongWord): HResult;
begin
  Result := DefaultInterface.GetCandidateListCountA(hIMC, pdwListSize, pdwBufLen);
end;

function  TCActiveIMM.GetCandidateListCountW(hIMC: LongWord; out pdwListSize: LongWord; 
                                             out pdwBufLen: LongWord): HResult;
begin
  Result := DefaultInterface.GetCandidateListCountW(hIMC, pdwListSize, pdwBufLen);
end;

function  TCActiveIMM.GetCandidateWindow(hIMC: LongWord; dwIndex: LongWord; 
                                         out pCandidate: CANDIDATEFORM): HResult;
begin
  Result := DefaultInterface.GetCandidateWindow(hIMC, dwIndex, pCandidate);
end;

function  TCActiveIMM.GetCompositionFontA(hIMC: LongWord; out plf: LOGFONTA): HResult;
begin
  Result := DefaultInterface.GetCompositionFontA(hIMC, plf);
end;

function  TCActiveIMM.GetCompositionFontW(hIMC: LongWord; out plf: LOGFONTW): HResult;
begin
  Result := DefaultInterface.GetCompositionFontW(hIMC, plf);
end;

function  TCActiveIMM.GetCompositionStringA(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                            out plCopied: Integer; out pBuf: Pointer): HResult;
begin
  Result := DefaultInterface.GetCompositionStringA(hIMC, dwIndex, dwBufLen, plCopied, pBuf);
end;

function  TCActiveIMM.GetCompositionStringW(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                            out plCopied: Integer; out pBuf: Pointer): HResult;
begin
  Result := DefaultInterface.GetCompositionStringW(hIMC, dwIndex, dwBufLen, plCopied, pBuf);
end;

function  TCActiveIMM.GetCompositionWindow(hIMC: LongWord; out pCompForm: COMPOSITIONFORM): HResult;
begin
  Result := DefaultInterface.GetCompositionWindow(hIMC, pCompForm);
end;

function  TCActiveIMM.GetContext(var hWnd: _RemotableHandle; out phIMC: LongWord): HResult;
begin
  Result := DefaultInterface.GetContext(hWnd, phIMC);
end;

function  TCActiveIMM.GetConversionListA(var hKL: Pointer; hIMC: LongWord; pSrc: PAnsiChar; 
                                         uBufLen: SYSUINT; uFlag: SYSUINT; out pDst: CANDIDATELIST; 
                                         out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetConversionListA(hKL, hIMC, pSrc, uBufLen, uFlag, pDst, puCopied);
end;

function  TCActiveIMM.GetConversionListW(var hKL: Pointer; hIMC: LongWord; pSrc: PWideChar; 
                                         uBufLen: SYSUINT; uFlag: SYSUINT; out pDst: CANDIDATELIST; 
                                         out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetConversionListW(hKL, hIMC, pSrc, uBufLen, uFlag, pDst, puCopied);
end;

function  TCActiveIMM.GetConversionStatus(hIMC: LongWord; out pfdwConversion: LongWord; 
                                          out pfdwSentence: LongWord): HResult;
begin
  Result := DefaultInterface.GetConversionStatus(hIMC, pfdwConversion, pfdwSentence);
end;

function  TCActiveIMM.GetDefaultIMEWnd(var hWnd: _RemotableHandle; out phDefWnd: wireHWND): HResult;
begin
  Result := DefaultInterface.GetDefaultIMEWnd(hWnd, phDefWnd);
end;

function  TCActiveIMM.GetDescriptionA(var hKL: Pointer; uBufLen: SYSUINT; szDescription: PAnsiChar; 
                                      out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetDescriptionA(hKL, uBufLen, szDescription, puCopied);
end;

function  TCActiveIMM.GetDescriptionW(var hKL: Pointer; uBufLen: SYSUINT; szDescription: PWideChar; 
                                      out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetDescriptionW(hKL, uBufLen, szDescription, puCopied);
end;

function  TCActiveIMM.GetGuideLineA(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                    pBuf: PAnsiChar; out pdwResult: LongWord): HResult;
begin
  Result := DefaultInterface.GetGuideLineA(hIMC, dwIndex, dwBufLen, pBuf, pdwResult);
end;

function  TCActiveIMM.GetGuideLineW(hIMC: LongWord; dwIndex: LongWord; dwBufLen: LongWord; 
                                    pBuf: PWideChar; out pdwResult: LongWord): HResult;
begin
  Result := DefaultInterface.GetGuideLineW(hIMC, dwIndex, dwBufLen, pBuf, pdwResult);
end;

function  TCActiveIMM.GetIMEFileNameA(var hKL: Pointer; uBufLen: SYSUINT; szFileName: PAnsiChar; 
                                      out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetIMEFileNameA(hKL, uBufLen, szFileName, puCopied);
end;

function  TCActiveIMM.GetIMEFileNameW(var hKL: Pointer; uBufLen: SYSUINT; szFileName: PWideChar; 
                                      out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetIMEFileNameW(hKL, uBufLen, szFileName, puCopied);
end;

function  TCActiveIMM.GetOpenStatus(hIMC: LongWord): HResult;
begin
  Result := DefaultInterface.GetOpenStatus(hIMC);
end;

function  TCActiveIMM.GetProperty(var hKL: Pointer; fdwIndex: LongWord; out pdwProperty: LongWord): HResult;
begin
  Result := DefaultInterface.GetProperty(hKL, fdwIndex, pdwProperty);
end;

function  TCActiveIMM.GetRegisterWordStyleA(var hKL: Pointer; nItem: SYSUINT; 
                                            out pStyleBuf: STYLEBUFA; out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetRegisterWordStyleA(hKL, nItem, pStyleBuf, puCopied);
end;

function  TCActiveIMM.GetRegisterWordStyleW(var hKL: Pointer; nItem: SYSUINT; 
                                            out pStyleBuf: STYLEBUFW; out puCopied: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetRegisterWordStyleW(hKL, nItem, pStyleBuf, puCopied);
end;

function  TCActiveIMM.GetStatusWindowPos(hIMC: LongWord; out pptPos: tagPOINT): HResult;
begin
  Result := DefaultInterface.GetStatusWindowPos(hIMC, pptPos);
end;

function  TCActiveIMM.GetVirtualKey(var hWnd: _RemotableHandle; out puVirtualKey: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetVirtualKey(hWnd, puVirtualKey);
end;

function  TCActiveIMM.InstallIMEA(szIMEFileName: PAnsiChar; szLayoutText: PAnsiChar; out phKL: Pointer): HResult;
begin
  Result := DefaultInterface.InstallIMEA(szIMEFileName, szLayoutText, phKL);
end;

function  TCActiveIMM.InstallIMEW(szIMEFileName: PWideChar; szLayoutText: PWideChar; 
                                  out phKL: Pointer): HResult;
begin
  Result := DefaultInterface.InstallIMEW(szIMEFileName, szLayoutText, phKL);
end;

function  TCActiveIMM.IsIME(var hKL: Pointer): HResult;
begin
  Result := DefaultInterface.IsIME(hKL);
end;

function  TCActiveIMM.IsUIMessageA(var hWndIME: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                                   lParam: LONG_PTR): HResult;
begin
  Result := DefaultInterface.IsUIMessageA(hWndIME, msg, wParam, lParam);
end;

function  TCActiveIMM.IsUIMessageW(var hWndIME: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                                   lParam: LONG_PTR): HResult;
begin
  Result := DefaultInterface.IsUIMessageW(hWndIME, msg, wParam, lParam);
end;

function  TCActiveIMM.NotifyIME(hIMC: LongWord; dwAction: LongWord; dwIndex: LongWord; 
                                dwValue: LongWord): HResult;
begin
  Result := DefaultInterface.NotifyIME(hIMC, dwAction, dwIndex, dwValue);
end;

function  TCActiveIMM.REGISTERWORDA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                                    szRegister: PAnsiChar): HResult;
begin
  Result := DefaultInterface.REGISTERWORDA(hKL, szReading, dwStyle, szRegister);
end;

function  TCActiveIMM.REGISTERWORDW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                                    szRegister: PWideChar): HResult;
begin
  Result := DefaultInterface.REGISTERWORDW(hKL, szReading, dwStyle, szRegister);
end;

function  TCActiveIMM.ReleaseContext(var hWnd: _RemotableHandle; hIMC: LongWord): HResult;
begin
  Result := DefaultInterface.ReleaseContext(hWnd, hIMC);
end;

function  TCActiveIMM.SetCandidateWindow(hIMC: LongWord; var pCandidate: CANDIDATEFORM): HResult;
begin
  Result := DefaultInterface.SetCandidateWindow(hIMC, pCandidate);
end;

function  TCActiveIMM.SetCompositionFontA(hIMC: LongWord; var plf: LOGFONTA): HResult;
begin
  Result := DefaultInterface.SetCompositionFontA(hIMC, plf);
end;

function  TCActiveIMM.SetCompositionFontW(hIMC: LongWord; var plf: LOGFONTW): HResult;
begin
  Result := DefaultInterface.SetCompositionFontW(hIMC, plf);
end;

function  TCActiveIMM.SetCompositionStringA(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                            dwCompLen: LongWord; var pRead: Pointer; 
                                            dwReadLen: LongWord): HResult;
begin
  Result := DefaultInterface.SetCompositionStringA(hIMC, dwIndex, pComp, dwCompLen, pRead, dwReadLen);
end;

function  TCActiveIMM.SetCompositionStringW(hIMC: LongWord; dwIndex: LongWord; var pComp: Pointer; 
                                            dwCompLen: LongWord; var pRead: Pointer; 
                                            dwReadLen: LongWord): HResult;
begin
  Result := DefaultInterface.SetCompositionStringW(hIMC, dwIndex, pComp, dwCompLen, pRead, dwReadLen);
end;

function  TCActiveIMM.SetCompositionWindow(hIMC: LongWord; var pCompForm: COMPOSITIONFORM): HResult;
begin
  Result := DefaultInterface.SetCompositionWindow(hIMC, pCompForm);
end;

function  TCActiveIMM.SetConversionStatus(hIMC: LongWord; fdwConversion: LongWord; 
                                          fdwSentence: LongWord): HResult;
begin
  Result := DefaultInterface.SetConversionStatus(hIMC, fdwConversion, fdwSentence);
end;

function  TCActiveIMM.SetOpenStatus(hIMC: LongWord; fOpen: Integer): HResult;
begin
  Result := DefaultInterface.SetOpenStatus(hIMC, fOpen);
end;

function  TCActiveIMM.SetStatusWindowPos(hIMC: LongWord; var pptPos: tagPOINT): HResult;
begin
  Result := DefaultInterface.SetStatusWindowPos(hIMC, pptPos);
end;

function  TCActiveIMM.SimulateHotKey(var hWnd: _RemotableHandle; dwHotKeyID: LongWord): HResult;
begin
  Result := DefaultInterface.SimulateHotKey(hWnd, dwHotKeyID);
end;

function  TCActiveIMM.UnregisterWordA(var hKL: Pointer; szReading: PAnsiChar; dwStyle: LongWord; 
                                      szUnregister: PAnsiChar): HResult;
begin
  Result := DefaultInterface.UnregisterWordA(hKL, szReading, dwStyle, szUnregister);
end;

function  TCActiveIMM.UnregisterWordW(var hKL: Pointer; szReading: PWideChar; dwStyle: LongWord; 
                                      szUnregister: PWideChar): HResult;
begin
  Result := DefaultInterface.UnregisterWordW(hKL, szReading, dwStyle, szUnregister);
end;

function  TCActiveIMM.Activate(fRestoreLayout: Integer): HResult;
begin
  Result := DefaultInterface.Activate(fRestoreLayout);
end;

function  TCActiveIMM.Deactivate: HResult;
begin
  Result := DefaultInterface.Deactivate;
end;

function  TCActiveIMM.OnDefWindowProc(var hWnd: _RemotableHandle; msg: SYSUINT; wParam: UINT_PTR; 
                                      lParam: LONG_PTR; out plResult: LONG_PTR): HResult;
begin
  Result := DefaultInterface.OnDefWindowProc(hWnd, msg, wParam, lParam, plResult);
end;

function  TCActiveIMM.FilterClientWindows(var aaClassList: Word; uSize: SYSUINT): HResult;
begin
  Result := DefaultInterface.FilterClientWindows(aaClassList, uSize);
end;

function  TCActiveIMM.GetCodePageA(var hKL: Pointer; out uCodePage: SYSUINT): HResult;
begin
  Result := DefaultInterface.GetCodePageA(hKL, uCodePage);
end;

function  TCActiveIMM.GetLangId(var hKL: Pointer; out plid: Word): HResult;
begin
  Result := DefaultInterface.GetLangId(hKL, plid);
end;

function  TCActiveIMM.AssociateContextEx(var hWnd: _RemotableHandle; hIMC: LongWord; 
                                         dwFlags: LongWord): HResult;
begin
  Result := DefaultInterface.AssociateContextEx(hWnd, hIMC, dwFlags);
end;

function  TCActiveIMM.DisableIME(idThread: LongWord): HResult;
begin
  Result := DefaultInterface.DisableIME(idThread);
end;

function  TCActiveIMM.GetImeMenuItemsA(hIMC: LongWord; dwFlags: LongWord; dwType: LongWord; 
                                       var pImeParentMenu: IMEMENUITEMINFOA; 
                                       out pImeMenu: IMEMENUITEMINFOA; dwSize: LongWord; 
                                       out pdwResult: LongWord): HResult;
begin
  Result := DefaultInterface.GetImeMenuItemsA(hIMC, dwFlags, dwType, pImeParentMenu, pImeMenu, 
                                              dwSize, pdwResult);
end;

function  TCActiveIMM.GetImeMenuItemsW(hIMC: LongWord; dwFlags: LongWord; dwType: LongWord; 
                                       var pImeParentMenu: IMEMENUITEMINFOW; 
                                       out pImeMenu: IMEMENUITEMINFOW; dwSize: LongWord; 
                                       out pdwResult: LongWord): HResult;
begin
  Result := DefaultInterface.GetImeMenuItemsW(hIMC, dwFlags, dwType, pImeParentMenu, pImeMenu, 
                                              dwSize, pdwResult);
end;

function  TCActiveIMM.EnumInputContext(idThread: LongWord; out ppEnum: IEnumInputContext): HResult;
begin
  Result := DefaultInterface.EnumInputContext(idThread, ppEnum);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCActiveIMMProperties.Create(AServer: TCActiveIMM);
begin
  inherited Create;
  FServer := AServer;
end;

function TCActiveIMMProperties.GetDefaultInterface: IActiveIMMApp;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

end.
