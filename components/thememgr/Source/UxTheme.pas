{******************************************************************************}
{                                                                              }
{ Visual Styles (Themes) API interface Unit for Object Pascal                  }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: uxtheme.h, released June 2001. The original Pascal     }
{ code is: UxTheme.pas, released July 2001. The initial developer of the       }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Portions created by Mike Lischke are Copyright (C) 1999-2002                 }
{ Mike Lischke. All Rights Reserved.                                           }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI home    }
{ page, located at http://delphi-jedi.org or my personal homepage located at   }
{ http://members.chello.nl/m.vanbrakel2                                        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

unit UxTheme;

{$HPPEMIT ''}
{$HPPEMIT '#include "uxtheme.h"'}
{$HPPEMIT ''}

interface

uses
  Windows;

procedure FreeThemeLibrary;
function InitThemeLibrary: Boolean;
function UseThemes: Boolean;

const
  WM_THEMECHANGED = $031A;
  
type
  HIMAGELIST = THANDLE; // TODO TEMPORARY
  HTHEME = THANDLE;     // handle to a section of theme data for class
  {$EXTERNALSYM HTHEME}

//----------------------------------------------------------------------------------------------------------------------
// NOTE: PartId's and StateId's used in the theme API are defined in the
//       hdr file <tmschema.h> using the TM_PART and TM_STATE macros.  For
//       example, "TM_PART(BP, PUSHBUTTON)" defines the PartId "BP_PUSHBUTTON".
//----------------------------------------------------------------------------------------------------------------------
//  OpenThemeData()     - Open the theme data for the specified HWND and
//                        semi-colon separated list of class names.
//
//                        OpenThemeData() will try each class name, one at
//                        a time, and use the first matching theme info
//                        found.  If a match is found, a theme handle
//                        to the data is returned.  If no match is found,
//                        a "NULL" handle is returned.
//
//                        When the window is destroyed or a WM_THEMECHANGED
//                        msg is received, "CloseThemeData()" should be
//                        called to close the theme handle.
//
//  hwnd                - window handle of the control/window to be themed
//
//  pszClassList        - class name (or list of names) to match to theme data
//                        section.  if the list contains more than one name,
//                        the names are tested one at a time for a match.
//                        If a match is found, OpenThemeData() returns a
//                        theme handle associated with the matching class.
//                        This param is a list (instead of just a single
//                        class name) to provide the class an opportunity
//                        to get the "best" match between the class and
//                        the current theme.  For example, a button might
//                        pass L"OkButton, Button" if its ID=ID_OK.  If
//                        the current theme has an entry for OkButton,
//                        that will be used.  Otherwise, we fall back on
//                        the normal Button entry.
//----------------------------------------------------------------------------------------------------------------------

var
  OpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
{$EXTERNALSYM OpenThemeData}

//----------------------------------------------------------------------------------------------------------------------
//  CloseTHemeData()    - closes the theme data handle.  This should be done
//                        when the window being themed is destroyed or
//                        whenever a WM_THEMECHANGED msg is received
//                        (followed by an attempt to create a new Theme data
//                        handle).
//
//  hTheme              - open theme data handle (returned from prior call
//                        to OpenThemeData() API).
//----------------------------------------------------------------------------------------------------------------------

var
  CloseThemeData: function(hTheme: HTHEME): HRESULT; stdcall;
{$EXTERNALSYM CloseThemeData}

//----------------------------------------------------------------------------------------------------------------------
//    functions for basic drawing support
//----------------------------------------------------------------------------------------------------------------------
// The following methods are the theme-aware drawing services.
// Controls/Windows are defined in drawable "parts" by their author: a
// parent part and 0 or more child parts.  Each of the parts can be
// described in "states" (ex: disabled, hot, pressed).
//----------------------------------------------------------------------------------------------------------------------
// For the list of all themed classes and the definition of all
// parts and states, see the file "tmschmea.h".
//----------------------------------------------------------------------------------------------------------------------
// Each of the below methods takes a "iPartId" param to specify the
// part and a "iStateId" to specify the state of the part.
// "iStateId=0" refers to the root part.  "iPartId" = "0" refers to
// the root class.
//----------------------------------------------------------------------------------------------------------------------
// Note: draw operations are always scaled to fit (and not to exceed)
// the specified "Rect".
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
//  DrawThemeBackground()
//                      - draws the theme-specified border and fill for
//                        the "iPartId" and "iStateId".  This could be
//                        based on a bitmap file, a border and fill, or
//                        other image description.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part) to draw
//  pRect               - defines the size/location of the part
//  pClipRect           - optional clipping rect (don't draw outside it)
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
    pClipRect: PRECT): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeBackground}

//----------------------------------------------------------------------------------------------------------------------
//----- DrawThemeText() flags ----

const
  DTT_GRAYED = $1;         // draw a grayed-out string
  {$EXTERNALSYM DTT_GRAYED}

//----------------------------------------------------------------------------------------------------------------------
//  DrawThemeText()     - draws the text using the theme-specified
//                        color and font for the "iPartId" and
//                        "iStateId".
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part) to draw
//  pszText             - actual text to draw
//  dwCharCount         - number of chars to draw (-1 for all)
//  dwTextFlags         - same as DrawText() "uFormat" param
//  dwTextFlags2        - additional drawing options
//  pRect               - defines the size/location of the part
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeText: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR; iCharCount: Integer;
    dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeText}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeBackgroundContentRect()
//                      - gets the size of the content for the theme-defined
//                        background.  This is usually the area inside
//                        the borders or Margins.
//
//      hTheme          - theme data handle
//      hdc             - (optional) device content to be used for drawing
//      iPartId         - part number to draw
//      iStateId        - state number (of the part) to draw
//      pBoundingRect   - the outer RECT of the part being drawn
//      pContentRect    - RECT to receive the content area
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeBackgroundContentRect: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    const pBoundingRect: TRect; pContentRect: PRECT): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBackgroundContentRect}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeBackgroundExtent() - calculates the size/location of the theme-
//                               specified background based on the
//                               "pContentRect".
//
//      hTheme          - theme data handle
//      hdc             - (optional) device content to be used for drawing
//      iPartId         - part number to draw
//      iStateId        - state number (of the part) to draw
//      pContentRect    - RECT that defines the content area
//      pBoundingRect   - RECT to receive the overall size/location of part
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeBackgroundExtent: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pContentRect: TRect;
    var pExtentRect: TRect): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBackgroundExtent}

//----------------------------------------------------------------------------------------------------------------------

type
  THEMESIZE = (
    TS_MIN,             // minimum size
    TS_TRUE,            // size without stretching
    TS_DRAW             // size that theme mgr will use to draw part
  );
  {$EXTERNALSYM THEMESIZE}
  TThemeSize = THEMESIZE;

//----------------------------------------------------------------------------------------------------------------------
//  GetThemePartSize() - returns the specified size of the theme part
//
//  hTheme              - theme data handle
//  hdc                 - HDC to select font into & measure against
//  iPartId             - part number to retrieve size for
//  iStateId            - state number (of the part)
//  prc                 - (optional) rect for part drawing destination
//  eSize               - the type of size to be retreived
//  psz                 - receives the specified size of the part
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemePartSize: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; prc: PRECT; eSize: THEMESIZE;
    var psz: TSize): HRESULT; stdcall;
{$EXTERNALSYM GetThemePartSize}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeTextExtent() - calculates the size/location of the specified
//                         text when rendered in the Theme Font.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to select font & measure into
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  pszText             - the text to be measured
//  dwCharCount         - number of chars to draw (-1 for all)
//  dwTextFlags         - same as DrawText() "uFormat" param
//  pszBoundingRect     - optional: to control layout of text
//  pszExtentRect       - receives the RECT for text size/location
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeTextExtent: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; pszText: LPCWSTR;
    iCharCount: Integer; dwTextFlags: DWORD; pBoundingRect: PRECT; var pExtentRect: TRect): HRESULT; stdcall;
{$EXTERNALSYM GetThemeTextExtent}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeTextMetrics()
//                      - returns info about the theme-specified font
//                        for the part/state passed in.
//
//  hTheme              - theme data handle
//  hdc                 - optional: HDC for screen context
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  ptm                 - receives the font info
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeTextMetrics: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;
    var ptm: TEXTMETRIC): HRESULT; stdcall;
{$EXTERNALSYM GetThemeTextMetrics}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeBackgroundRegion()
//                      - computes the region for a regular or partially
//                        transparent theme-specified background that is
//                        bound by the specified "pRect".
//                        If the rectangle is empty, sets the HRGN to NULL
//                        and return S_FALSE.
//
//  hTheme              - theme data handle
//  hdc                 - optional HDC to draw into (DPI scaling)
//  iPartId             - part number to draw
//  iStateId            - state number (of the part)
//  pRect               - the RECT used to draw the part
//  pRegion             - receives handle to calculated region
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeBackgroundRegion: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect;
    var pRegion: HRGN): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBackgroundRegion}

//----------------------------------------------------------------------------------------------------------------------
//----- HitTestThemeBackground, HitTestThemeBackgroundRegion flags ----

//  Theme background segment hit test flag (default). possible return values are:
//  HTCLIENT: hit test succeeded in the middle background segment
//  HTTOP, HTLEFT, HTTOPLEFT, etc:  // hit test succeeded in the the respective theme background segment.

const
  HTTB_BACKGROUNDSEG         = $0000;
  {$EXTERNALSYM HTTB_BACKGROUNDSEG}

//  Fixed border hit test option.  possible return values are:
//  HTCLIENT: hit test succeeded in the middle background segment
//  HTBORDER: hit test succeeded in any other background segment

  HTTB_FIXEDBORDER           = $0002;  // Return code may be either HTCLIENT or HTBORDER.
  {$EXTERNALSYM HTTB_FIXEDBORDER}

//  Caption hit test option.  Possible return values are:
//  HTCAPTION: hit test succeeded in the top, top left, or top right background segments
//  HTNOWHERE or another return code, depending on absence or presence of accompanying flags, resp.

  HTTB_CAPTION               = $0004;
  {$EXTERNALSYM HTTB_CAPTION}

//  Resizing border hit test flags.  Possible return values are:
//  HTCLIENT: hit test succeeded in middle background segment
//  HTTOP, HTTOPLEFT, HTLEFT, HTRIGHT, etc:    hit test succeeded in the respective system resizing zone
//  HTBORDER: hit test failed in middle segment and resizing zones, but succeeded in a background border segment

  HTTB_RESIZINGBORDER_LEFT   = $0010; // Hit test left resizing border,
  {$EXTERNALSYM HTTB_RESIZINGBORDER_LEFT}
  HTTB_RESIZINGBORDER_TOP    = $0020; // Hit test top resizing border
  {$EXTERNALSYM HTTB_RESIZINGBORDER_TOP}
  HTTB_RESIZINGBORDER_RIGHT  = $0040; // Hit test right resizing border
  {$EXTERNALSYM HTTB_RESIZINGBORDER_RIGHT}
  HTTB_RESIZINGBORDER_BOTTOM = $0080; // Hit test bottom resizing border
  {$EXTERNALSYM HTTB_RESIZINGBORDER_BOTTOM}

  HTTB_RESIZINGBORDER        = (HTTB_RESIZINGBORDER_LEFT or HTTB_RESIZINGBORDER_TOP or
                                HTTB_RESIZINGBORDER_RIGHT or HTTB_RESIZINGBORDER_BOTTOM);
  {$EXTERNALSYM HTTB_RESIZINGBORDER}

// Resizing border is specified as a template, not just window edges.
// This option is mutually exclusive with HTTB_SYSTEMSIZINGWIDTH; HTTB_SIZINGTEMPLATE takes precedence

  HTTB_SIZINGTEMPLATE        = $0100;
  {$EXTERNALSYM HTTB_SIZINGTEMPLATE}

// Use system resizing border width rather than theme content margins.
// This option is mutually exclusive with HTTB_SIZINGTEMPLATE, which takes precedence.

  HTTB_SYSTEMSIZINGMARGINS   = $0200;
  {$EXTERNALSYM HTTB_SYSTEMSIZINGMARGINS}

//----------------------------------------------------------------------------------------------------------------------
//  HitTestThemeBackground()
//                      - returns a HitTestCode (a subset of the values
//                        returned by WM_NCHITTEST) for the point "ptTest"
//                        within the theme-specified background
//                        (bound by pRect).  "pRect" and "ptTest" should
//                        both be in the same coordinate system
//                        (client, screen, etc).
//
//      hTheme          - theme data handle
//      hdc             - HDC to draw into
//      iPartId         - part number to test against
//      iStateId        - state number (of the part)
//      pRect           - the RECT used to draw the part
//      hrgn            - optional region to use; must be in same coordinates as
//                      -    pRect and pTest.
//      ptTest          - the hit point to be tested
//      dwOptions       - HTTB_xxx constants
//      pwHitTestCode   - receives the returned hit test code - one of:
//
//                        HTNOWHERE, HTLEFT, HTTOPLEFT, HTBOTTOMLEFT,
//                        HTRIGHT, HTTOPRIGHT, HTBOTTOMRIGHT,
//                        HTTOP, HTBOTTOM, HTCLIENT
//----------------------------------------------------------------------------------------------------------------------

var
  HitTestThemeBackground: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; dwOptions: DWORD;
    const pRect: TRect; hrgn: HRGN; ptTest: TPoint; var pwHitTestCode: WORD): HRESULT; stdcall;
{$EXTERNALSYM HitTestThemeBackground}

//----------------------------------------------------------------------------------------------------------------------
//  DrawThemeEdge()     - Similar to the DrawEdge() API, but uses part colors
//                        and is high-DPI aware
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number of part
//  pDestRect           - the RECT used to draw the line(s)
//  uEdge               - Same as DrawEdge() API
//  uFlags              - Same as DrawEdge() API
//  pContentRect        - Receives the interior rect if (uFlags & BF_ADJUST)
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeEdge: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pDestRect: TRect; uEdge,
    uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeEdge}

//----------------------------------------------------------------------------------------------------------------------
//  DrawThemeIcon()     - draws an image within an imagelist based on
//                        a (possible) theme-defined effect.
//
//  hTheme              - theme data handle
//  hdc                 - HDC to draw into
//  iPartId             - part number to draw
//  iStateId            - state number of part
//  pRect               - the RECT to draw the image within
//  himl                - handle to IMAGELIST
//  iImageIndex         - index into IMAGELIST (which icon to draw)
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeIcon: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; himl: HIMAGELIST;
    iImageIndex: Integer): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeIcon}

//----------------------------------------------------------------------------------------------------------------------
//  IsThemePartDefined() - returns TRUE if the theme has defined parameters
//                         for the specified "iPartId" and "iStateId".
//
//  hTheme              - theme data handle
//  iPartId             - part number to find definition for
//  iStateId            - state number of part
//----------------------------------------------------------------------------------------------------------------------

var
  IsThemePartDefined: function(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; stdcall;
{$EXTERNALSYM IsThemePartDefined}

//----------------------------------------------------------------------------------------------------------------------
//  IsThemeBackgroundPartiallyTransparent()
//                      - returns TRUE if the theme specified background for
//                        the part/state has transparent pieces or
//                        alpha-blended pieces.
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//----------------------------------------------------------------------------------------------------------------------

var
  IsThemeBackgroundPartiallyTransparent: function(hTheme: HTHEME; iPartId, iStateId: Integer): BOOL; stdcall;
{$EXTERNALSYM IsThemeBackgroundPartiallyTransparent}

//----------------------------------------------------------------------------------------------------------------------
//    lower-level theme information services
//----------------------------------------------------------------------------------------------------------------------
// The following methods are getter routines for each of the Theme Data types.
// Controls/Windows are defined in drawable "parts" by their author: a
// parent part and 0 or more child parts.  Each of the parts can be
// described in "states" (ex: disabled, hot, pressed).
//----------------------------------------------------------------------------------------------------------------------
// Each of the below methods takes a "iPartId" param to specify the
// part and a "iStateId" to specify the state of the part.
// "iStateId=0" refers to the root part.  "iPartId" = "0" refers to
// the root class.
//----------------------------------------------------------------------------------------------------------------------
// Each method also take a "iPropId" param because multiple instances of
// the same primitive type can be defined in the theme schema.
//----------------------------------------------------------------------------------------------------------------------


//----------------------------------------------------------------------------------------------------------------------
//  GetThemeColor()     - Get the value for the specified COLOR property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pColor              - receives the value of the property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeColor: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT; stdcall;
{$EXTERNALSYM GetThemeColor}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeMetric()    - Get the value for the specified metric/size
//                        property
//
//  hTheme              - theme data handle
//  hdc                 - (optional) hdc to be drawn into (DPI scaling)
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeMetric: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var piVal: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeMetric}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeString()    - Get the value for the specified string property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pszBuff             - receives the string property value
//  cchMaxBuffChars     - max. number of chars allowed in pszBuff
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeString: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; pszBuff: LPWSTR;
    cchMaxBuffChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeString}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeBool()      - Get the value for the specified BOOL property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pfVal               - receives the value of the property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeBool: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pfVal: BOOL): HRESULT; stdcall;
{$EXTERNALSYM GetThemeBool}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeInt()       - Get the value for the specified int property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeInt: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var piVal: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeInt}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeEnumValue() - Get the value for the specified ENUM property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  piVal               - receives the value of the enum (cast to int*)
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeEnumValue: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var piVal: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeEnumValue}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemePosition()  - Get the value for the specified position
//                        property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pPoint              - receives the value of the position property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemePosition: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;var pPoint: TPoint): HRESULT; stdcall;
{$EXTERNALSYM GetThemePosition}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeFont()      - Get the value for the specified font property
//
//  hTheme              - theme data handle
//  hdc                 - (optional) hdc to be drawn to (DPI scaling)
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pFont               - receives the value of the LOGFONT property
//                        (scaled for the current logical screen dpi)
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeFont: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer;
    var pFont: LOGFONT): HRESULT; stdcall;
{$EXTERNALSYM GetThemeFont}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeRect()      - Get the value for the specified RECT property
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to get the value for
//  pRect               - receives the value of the RECT property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeRect: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pRect: TRect): HRESULT; stdcall;
{$EXTERNALSYM GetThemeRect}

//----------------------------------------------------------------------------------------------------------------------

type
  _MARGINS = record
    cxLeftWidth: Integer;      // width of left border that retains its size
    cxRightWidth: Integer;     // width of right border that retains its size
    cyTopHeight: Integer;      // height of top border that retains its size
    cyBottomHeight: Integer;   // height of bottom border that retains its size
  end;
  {$EXTERNALSYM _MARGINS}
  MARGINS = _MARGINS;
  {$EXTERNALSYM MARGINS}
  PMARGINS = ^MARGINS;
  {$EXTERNALSYM PMARGINS}
  TMargins = MARGINS;

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeMargins()   - Get the value for the specified MARGINS property
//
//      hTheme          - theme data handle
//      hdc             - (optional) hdc to be used for drawing
//      iPartId         - part number
//      iStateId        - state number of part
//      iPropId         - the property number to get the value for
//      prc             - RECT for area to be drawn into
//      pMargins        - receives the value of the MARGINS property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeMargins: function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId, iPropId: Integer; prc: PRECT;
    var pMargins: MARGINS): HRESULT; stdcall;
{$EXTERNALSYM GetThemeMargins}

//----------------------------------------------------------------------------------------------------------------------

const
  MAX_INTLIST_COUNT = 10;
  {$EXTERNALSYM MAX_INTLIST_COUNT}

type
  _INTLIST = record
    iValueCount: Integer;      // number of values in iValues
    iValues: array [0..MAX_INTLIST_COUNT - 1] of Integer;
  end;
  {$EXTERNALSYM _INTLIST}
  INTLIST = _INTLIST;
  {$EXTERNALSYM INTLIST}
  PINTLIST = ^INTLIST;
  {$EXTERNALSYM PINTLIST}
  TIntList = INTLIST;

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeIntList()   - Get the value for the specified INTLIST struct
//
//      hTheme          - theme data handle
//      iPartId         - part number
//      iStateId        - state number of part
//      iPropId         - the property number to get the value for
//      pIntList        - receives the value of the INTLIST property
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeIntList: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pIntList: INTLIST): HRESULT; stdcall;
{$EXTERNALSYM GetThemeIntList}

//----------------------------------------------------------------------------------------------------------------------

type
  PROPERTYORIGIN = (
    PO_STATE,           // property was found in the state section
    PO_PART,            // property was found in the part section
    PO_CLASS,           // property was found in the class section
    PO_GLOBAL,          // property was found in [globals] section
    PO_NOTFOUND);       // property was not found
  {$EXTERNALSYM PROPERTYORIGIN}
  TPropertyOrigin = PROPERTYORIGIN;

//----------------------------------------------------------------------------------------------------------------------
//  GetThemePropertyOrigin()
//                      - searches for the specified theme property
//                        and sets "pOrigin" to indicate where it was
//                        found (or not found)
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to search for
//  pOrigin             - receives the value of the property origin
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemePropertyOrigin: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer;
    var pOrigin: PROPERTYORIGIN): HRESULT; stdcall;
{$EXTERNALSYM GetThemePropertyOrigin}

//----------------------------------------------------------------------------------------------------------------------
//  SetWindowTheme()
//                      - redirects an existing Window to use a different
//                        section of the current theme information than its
//                        class normally asks for.
//
//  hwnd                - the handle of the window (cannot be NULL)
//
//  pszSubAppName       - app (group) name to use in place of the calling
//                        app's name.  If NULL, the actual calling app
//                        name will be used.
//
//  pszSubIdList        - semicolon separated list of class Id names to
//                        use in place of actual list passed by the
//                        window's class.  if NULL, the id list from the
//                        calling class is used.
//----------------------------------------------------------------------------------------------------------------------
// The Theme Manager will remember the "pszSubAppName" and the
// "pszSubIdList" associations thru the lifetime of the window (even
// if themes are subsequently changed).  The window is sent a
// "WM_THEMECHANGED" msg at the end of this call, so that the new
// theme can be found and applied.
//----------------------------------------------------------------------------------------------------------------------
// When "pszSubAppName" or "pszSubIdList" are NULL, the Theme Manager
// removes the previously remember association.  To turn off theme-ing for
// the specified window, you can pass an empty string (L"") so it
// won't match any section entries.
//----------------------------------------------------------------------------------------------------------------------

var
  SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
{$EXTERNALSYM SetWindowTheme}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeFilename()  - Get the value for the specified FILENAME property.
//
//  hTheme              - theme data handle
//  iPartId             - part number
//  iStateId            - state number of part
//  iPropId             - the property number to search for
//  pszThemeFileName    - output buffer to receive the filename
//  cchMaxBuffChars     - the size of the return buffer, in chars
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeFilename: function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; pszThemeFileName: LPWSTR;
    cchMaxBuffChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeFilename}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysColor()  - Get the value of the specified System color.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        color from [SysMetrics] section of theme.
//                        if NULL, will return the global system color.
//
//  iColorId            - the system color index defined in winuser.h
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysColor: function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;
{$EXTERNALSYM GetThemeSysColor}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysColorBrush()
//                      - Get the brush for the specified System color.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        brush matching color from [SysMetrics] section of
//                        theme.  if NULL, will return the brush matching
//                        global system color.
//
//  iColorId            - the system color index defined in winuser.h
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysColorBrush: function(hTheme: HTHEME; iColorId: Integer): HBRUSH; stdcall;
{$EXTERNALSYM GetThemeSysColorBrush}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysBool()   - Get the boolean value of specified System metric.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        BOOL from [SysMetrics] section of theme.
//                        if NULL, will return the specified system boolean.
//
//  iBoolId             - the TMT_XXX BOOL number (first BOOL
//                        is TMT_FLATMENUS)
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysBool: function(hTheme: HTHEME; iBoolId: Integer): BOOL; stdcall;
{$EXTERNALSYM GetThemeSysBool}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysSize()   - Get the value of the specified System size metric.
//                        (scaled for the current logical screen dpi)
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        size from [SysMetrics] section of theme.
//                        if NULL, will return the global system metric.
//
//  iSizeId             - the following values are supported when
//                        hTheme is non-NULL:
//
//                          SM_CXBORDER   (border width)
//                          SM_CXVSCROLL  (scrollbar width)
//                          SM_CYHSCROLL  (scrollbar height)
//                          SM_CXSIZE     (caption width)
//                          SM_CYSIZE     (caption height)
//                          SM_CXSMSIZE   (small caption width)
//                          SM_CYSMSIZE   (small caption height)
//                          SM_CXMENUSIZE (menubar width)
//                          SM_CYMENUSIZE (menubar height)
//
//                        when hTheme is NULL, iSizeId is passed directly
//                        to the GetSystemMetrics() function
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysSize: function(hTheme: HTHEME; iSizeId: Integer): Integer; stdcall;
{$EXTERNALSYM GetThemeSysSize}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysFont()   - Get the LOGFONT for the specified System font.
//
//  hTheme              - the theme data handle.  if non-NULL, will return
//                        font from [SysMetrics] section of theme.
//                        if NULL, will return the specified system font.
//
//  iFontId             - the TMT_XXX font number (first font
//                        is TMT_CAPTIONFONT)
//
//  plf                 - ptr to LOGFONT to receive the font value.
//                        (scaled for the current logical screen dpi)
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysFont: function(hTheme: HTHEME; iFontId: Integer; var plf: LOGFONT): HRESULT; stdcall;
{$EXTERNALSYM GetThemeSysFont}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysString() - Get the value of specified System string metric.
//
//  hTheme              - the theme data handle (required)
//
//  iStringId           - must be one of the following values:
//
//                          TMT_CSSNAME
//                          TMT_XMLNAME
//
//  pszStringBuff       - the buffer to receive the string value
//
//  cchMaxStringChars   - max. number of chars that pszStringBuff can hold
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysString: function(hTheme: HTHEME; iStringId: Integer; pszStringBuff: LPWSTR;
    cchMaxStringChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeSysString}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeSysInt() - Get the value of specified System int.
//
//  hTheme              - the theme data handle (required)
//
//  iIntId              - must be one of the following values:
//
//                          TMT_DPIX
//                          TMT_DPIY
//                          TMT_MINCOLORDEPTH
//
//  piValue             - ptr to int to receive value
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeSysInt: function(hTheme: HTHEME; iIntId: Integer; var piValue: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeSysInt}

//----------------------------------------------------------------------------------------------------------------------
//  IsThemeActive()     - can be used to test if a system theme is active
//                        for the current user session.
//
//                        use the API "IsAppThemed()" to test if a theme is
//                        active for the calling process.
//----------------------------------------------------------------------------------------------------------------------

var
  IsThemeActive: function: BOOL; stdcall;
{$EXTERNALSYM IsThemeActive}

//----------------------------------------------------------------------------------------------------------------------
//  IsAppThemed()       - returns TRUE if a theme is active and available to
//                        the current process
//----------------------------------------------------------------------------------------------------------------------

var
  IsAppThemed: function: BOOL; stdcall;
{$EXTERNALSYM IsAppThemed}

//----------------------------------------------------------------------------------------------------------------------
//  GetWindowTheme()    - if window is themed, returns its most recent
//                        HTHEME from OpenThemeData() - otherwise, returns
//                        NULL.
//
//      hwnd            - the window to get the HTHEME of
//----------------------------------------------------------------------------------------------------------------------

var
  GetWindowTheme: function(hwnd: HWND): HTHEME; stdcall;
{$EXTERNALSYM GetWindowTheme}

//----------------------------------------------------------------------------------------------------------------------
//  EnableThemeDialogTexture()
//
//  - Enables/disables dialog background theme.  This method can be used to
//    tailor dialog compatibility with child windows and controls that
//    may or may not coordinate the rendering of their client area backgrounds
//    with that of their parent dialog in a manner that supports seamless
//    background texturing.
//
//      hdlg         - the window handle of the target dialog
//      dwFlags      - ETDT_ENABLE to enable the theme-defined dialog background texturing,
//                     ETDT_DISABLE to disable background texturing,
//                     ETDT_ENABLETAB to enable the theme-defined background
//                          texturing using the Tab texture
//----------------------------------------------------------------------------------------------------------------------

const
  ETDT_DISABLE       = $00000001;
  {$EXTERNALSYM ETDT_DISABLE}
  ETDT_ENABLE        = $00000002;
  {$EXTERNALSYM ETDT_ENABLE}
  ETDT_USETABTEXTURE = $00000004;
  {$EXTERNALSYM ETDT_USETABTEXTURE}
  ETDT_ENABLETAB     = (ETDT_ENABLE or ETDT_USETABTEXTURE);
  {$EXTERNALSYM ETDT_ENABLETAB}

var
  EnableThemeDialogTexture: function(hwnd: HWND; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM EnableThemeDialogTexture}

//----------------------------------------------------------------------------------------------------------------------
//  IsThemeDialogTextureEnabled()
//
//  - Reports whether the dialog supports background texturing.
//
//      hdlg         - the window handle of the target dialog
//----------------------------------------------------------------------------------------------------------------------

var
  IsThemeDialogTextureEnabled: function(hwnd: HWND): BOOL; stdcall;
{$EXTERNALSYM IsThemeDialogTextureEnabled}

//----------------------------------------------------------------------------------------------------------------------
//---- flags to control theming within an app ----

const
  STAP_ALLOW_NONCLIENT   = (1 shl 0);
  {$EXTERNALSYM STAP_ALLOW_NONCLIENT}
  STAP_ALLOW_CONTROLS    = (1 shl 1);
  {$EXTERNALSYM STAP_ALLOW_CONTROLS}
  STAP_ALLOW_WEBCONTENT  = (1 shl 2);
  {$EXTERNALSYM STAP_ALLOW_WEBCONTENT}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeAppProperties()
//                      - returns the app property flags that control theming
//----------------------------------------------------------------------------------------------------------------------

var
  GetThemeAppProperties: function: DWORD; stdcall;
{$EXTERNALSYM GetThemeAppProperties}

//----------------------------------------------------------------------------------------------------------------------
//  SetThemeAppProperties()
//                      - sets the flags that control theming within the app
//
//      dwFlags         - the flag values to be set
//----------------------------------------------------------------------------------------------------------------------

var
  SetThemeAppProperties: procedure(dwFlags: DWORD); stdcall;
{$EXTERNALSYM SetThemeAppProperties}

//----------------------------------------------------------------------------------------------------------------------
//  GetCurrentThemeName()
//                      - Get the name of the current theme in-use.
//                        Optionally, return the ColorScheme name and the
//                        Size name of the theme.
//
//  pszThemeFileName    - receives the theme path & filename
//  cchMaxNameChars     - max chars allowed in pszNameBuff
//
//  pszColorBuff        - (optional) receives the canonical color scheme name
//                        (not the display name)
//  cchMaxColorChars    - max chars allowed in pszColorBuff
//
//  pszSizeBuff         - (optional) receives the canonical size name
//                        (not the display name)
//  cchMaxSizeChars     - max chars allowed in pszSizeBuff
//----------------------------------------------------------------------------------------------------------------------

var
  GetCurrentThemeName: function(pszThemeFileName: LPWSTR; cchMaxNameChars: Integer; pszColorBuff: LPWSTR;
    cchMaxColorChars: Integer; pszSizeBuff: LPWSTR; cchMaxSizeChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetCurrentThemeName}

//----------------------------------------------------------------------------------------------------------------------
//  GetThemeDocumentationProperty()
//                      - Get the value for the specified property name from
//                        the [documentation] section of the themes.ini file
//                        for the specified theme.  If the property has been
//                        localized in the theme files string table, the
//                        localized version of the property value is returned.
//
//  pszThemeFileName    - filename of the theme file to query
//  pszPropertyName     - name of the string property to retreive a value for
//  pszValueBuff        - receives the property string value
//  cchMaxValChars      - max chars allowed in pszValueBuff
//----------------------------------------------------------------------------------------------------------------------

const
  SZ_THDOCPROP_DISPLAYNAME               = WideString('DisplayName');
  {$EXTERNALSYM SZ_THDOCPROP_DISPLAYNAME}
  SZ_THDOCPROP_CANONICALNAME             = WideString('ThemeName');
  {$EXTERNALSYM SZ_THDOCPROP_CANONICALNAME}
  SZ_THDOCPROP_TOOLTIP                   = WideString('ToolTip');
  {$EXTERNALSYM SZ_THDOCPROP_TOOLTIP}
  SZ_THDOCPROP_AUTHOR                    = WideString('author');
  {$EXTERNALSYM SZ_THDOCPROP_AUTHOR}

var
  GetThemeDocumentationProperty: function(pszThemeName, pszPropertyName: LPCWSTR; pszValueBuff: LPWSTR;
    cchMaxValChars: Integer): HRESULT; stdcall;
{$EXTERNALSYM GetThemeDocumentationProperty}

//----------------------------------------------------------------------------------------------------------------------
//  Theme API Error Handling
//
//      All functions in the Theme API not returning an HRESULT (THEMEAPI_)
//      use the WIN32 function "SetLastError()" to record any call failures.
//
//      To retreive the error code of the last failure on the
//      current thread for these type of API's, use the WIN32 function
//      "GetLastError()".
//
//      All Theme API error codes (HRESULT's and GetLastError() values)
//      should be normal win32 errors which can be formatted into
//      strings using the Win32 API FormatMessage().
//----------------------------------------------------------------------------------------------------------------------

//----------------------------------------------------------------------------------------------------------------------
// DrawThemeParentBackground()
//                      - used by partially-transparent or alpha-blended
//                        child controls to draw the part of their parent
//                        that they appear in front of.
//
//  hwnd                - handle of the child control
//  hdc                 - hdc of the child control
//  prc                 - (optional) rect that defines the area to be
//                        drawn (CHILD coordinates)
//----------------------------------------------------------------------------------------------------------------------

var
  DrawThemeParentBackground: function(hwnd: HWND; hdc: HDC; prc: PRECT): HRESULT; stdcall;
{$EXTERNALSYM DrawThemeParentBackground}

//----------------------------------------------------------------------------------------------------------------------
//  EnableTheming()     - enables or disables themeing for the current user
//                        in the current and future sessions.
//
//  fEnable             - if FALSE, disable theming & turn themes off.
//                      - if TRUE, enable themeing and, if user previously
//                        had a theme active, make it active now.
//----------------------------------------------------------------------------------------------------------------------

var
  EnableTheming: function(fEnable: BOOL): HRESULT; stdcall;
{$EXTERNALSYM EnableTheming}

implementation

uses
  SyncObjs;

//----------------------------------------------------------------------------------------------------------------------

const
  themelib = 'uxtheme.dll';

var
  ThemeLibrary: THandle;
  ReferenceCount: Integer;  // We have to keep track of several load/unload calls.
  Lock: TCriticalSection;

procedure FreeThemeLibrary;

begin
  Lock.Enter;
  try
    if ReferenceCount > 0 then
      Dec(ReferenceCount);

    if (ThemeLibrary <> 0) and (ReferenceCount = 0) then
    begin
      FreeLibrary(ThemeLibrary);
      ThemeLibrary := 0;

      OpenThemeData := nil;
      CloseThemeData := nil;
      DrawThemeBackground := nil;
      DrawThemeText := nil;
      GetThemeBackgroundContentRect := nil;
      GetThemeBackgroundExtent := nil;
      GetThemePartSize := nil;
      GetThemeTextExtent := nil;
      GetThemeTextMetrics := nil;
      GetThemeBackgroundRegion := nil;
      HitTestThemeBackground := nil;
      DrawThemeEdge := nil;
      DrawThemeIcon := nil;
      IsThemePartDefined := nil;
      IsThemeBackgroundPartiallyTransparent := nil;
      GetThemeColor := nil;
      GetThemeMetric := nil;
      GetThemeString := nil;
      GetThemeBool := nil;
      GetThemeInt := nil;
      GetThemeEnumValue := nil;
      GetThemePosition := nil;
      GetThemeFont := nil;
      GetThemeRect := nil;
      GetThemeMargins := nil;
      GetThemeIntList := nil;
      GetThemePropertyOrigin := nil;
      SetWindowTheme := nil;
      GetThemeFilename := nil;
      GetThemeSysColor := nil;
      GetThemeSysColorBrush := nil;
      GetThemeSysBool := nil;
      GetThemeSysSize := nil;
      GetThemeSysFont := nil;
      GetThemeSysString := nil;
      GetThemeSysInt := nil;
      IsThemeActive := nil;
      IsAppThemed := nil;
      GetWindowTheme := nil;
      EnableThemeDialogTexture := nil;
      IsThemeDialogTextureEnabled := nil;
      GetThemeAppProperties := nil;
      SetThemeAppProperties := nil;
      GetCurrentThemeName := nil;
      GetThemeDocumentationProperty := nil;
      DrawThemeParentBackground := nil;
      EnableTheming := nil;
    end;
  finally
    Lock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function InitThemeLibrary: Boolean;

begin
  Lock.Enter;
  try
    Inc(ReferenceCount);

    if ThemeLibrary = 0 then
    begin
      ThemeLibrary := LoadLibrary(themelib);
      if ThemeLibrary > 0 then
      begin
        OpenThemeData := GetProcAddress(ThemeLibrary, 'OpenThemeData');
        CloseThemeData := GetProcAddress(ThemeLibrary, 'CloseThemeData');
        DrawThemeBackground := GetProcAddress(ThemeLibrary, 'DrawThemeBackground');
        DrawThemeText := GetProcAddress(ThemeLibrary, 'DrawThemeText');
        GetThemeBackgroundContentRect := GetProcAddress(ThemeLibrary, 'GetThemeBackgroundContentRect');
        GetThemeBackgroundExtent := GetProcAddress(ThemeLibrary, 'GetThemeBackgroundExtent');
        GetThemePartSize := GetProcAddress(ThemeLibrary, 'GetThemePartSize');
        GetThemeTextExtent := GetProcAddress(ThemeLibrary, 'GetThemeTextExtent');
        GetThemeTextMetrics := GetProcAddress(ThemeLibrary, 'GetThemeTextMetrics');
        GetThemeBackgroundRegion := GetProcAddress(ThemeLibrary, 'GetThemeBackgroundRegion');
        HitTestThemeBackground := GetProcAddress(ThemeLibrary, 'HitTestThemeBackground');
        DrawThemeEdge := GetProcAddress(ThemeLibrary, 'DrawThemeEdge');
        DrawThemeIcon := GetProcAddress(ThemeLibrary, 'DrawThemeIcon');
        IsThemePartDefined := GetProcAddress(ThemeLibrary, 'IsThemePartDefined');
        IsThemeBackgroundPartiallyTransparent := GetProcAddress(ThemeLibrary, 'IsThemeBackgroundPartiallyTransparent');
        GetThemeColor := GetProcAddress(ThemeLibrary, 'GetThemeColor');
        GetThemeMetric := GetProcAddress(ThemeLibrary, 'GetThemeMetric');
        GetThemeString := GetProcAddress(ThemeLibrary, 'GetThemeString');
        GetThemeBool := GetProcAddress(ThemeLibrary, 'GetThemeBool');
        GetThemeInt := GetProcAddress(ThemeLibrary, 'GetThemeInt');
        GetThemeEnumValue := GetProcAddress(ThemeLibrary, 'GetThemeEnumValue');
        GetThemePosition := GetProcAddress(ThemeLibrary, 'GetThemePosition');
        GetThemeFont := GetProcAddress(ThemeLibrary, 'GetThemeFont');
        GetThemeRect := GetProcAddress(ThemeLibrary, 'GetThemeRect');
        GetThemeMargins := GetProcAddress(ThemeLibrary, 'GetThemeMargins');
        GetThemeIntList := GetProcAddress(ThemeLibrary, 'GetThemeIntList');
        GetThemePropertyOrigin := GetProcAddress(ThemeLibrary, 'GetThemePropertyOrigin');
        SetWindowTheme := GetProcAddress(ThemeLibrary, 'SetWindowTheme');
        GetThemeFilename := GetProcAddress(ThemeLibrary, 'GetThemeFilename');
        GetThemeSysColor := GetProcAddress(ThemeLibrary, 'GetThemeSysColor');
        GetThemeSysColorBrush := GetProcAddress(ThemeLibrary, 'GetThemeSysColorBrush');
        GetThemeSysBool := GetProcAddress(ThemeLibrary, 'GetThemeSysBool');
        GetThemeSysSize := GetProcAddress(ThemeLibrary, 'GetThemeSysSize');
        GetThemeSysFont := GetProcAddress(ThemeLibrary, 'GetThemeSysFont');
        GetThemeSysString := GetProcAddress(ThemeLibrary, 'GetThemeSysString');
        GetThemeSysInt := GetProcAddress(ThemeLibrary, 'GetThemeSysInt');
        IsThemeActive := GetProcAddress(ThemeLibrary, 'IsThemeActive');
        IsAppThemed := GetProcAddress(ThemeLibrary, 'IsAppThemed');
        GetWindowTheme := GetProcAddress(ThemeLibrary, 'GetWindowTheme');
        EnableThemeDialogTexture := GetProcAddress(ThemeLibrary, 'EnableThemeDialogTexture');
        IsThemeDialogTextureEnabled := GetProcAddress(ThemeLibrary, 'IsThemeDialogTextureEnabled');
        GetThemeAppProperties := GetProcAddress(ThemeLibrary, 'GetThemeAppProperties');
        SetThemeAppProperties := GetProcAddress(ThemeLibrary, 'SetThemeAppProperties');
        GetCurrentThemeName := GetProcAddress(ThemeLibrary, 'GetCurrentThemeName');
        GetThemeDocumentationProperty := GetProcAddress(ThemeLibrary, 'GetThemeDocumentationProperty');
        DrawThemeParentBackground := GetProcAddress(ThemeLibrary, 'DrawThemeParentBackground');
        EnableTheming := GetProcAddress(ThemeLibrary, 'EnableTheming');
      end;
    end;
    Result := ThemeLibrary > 0;
  finally
    Lock.Leave;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function UseThemes: Boolean;

begin
  Result := ThemeLibrary > 0;
  if Result then
    Result := IsAppThemed and IsThemeActive;
end;

//----------------------------------------------------------------------------------------------------------------------

initialization
  Lock := TCriticalSection.Create;
finalization
  while ReferenceCount > 0 do
    FreeThemeLibrary;
  Lock.Free;
end.

