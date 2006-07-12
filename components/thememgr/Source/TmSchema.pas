{******************************************************************************}
{                                                                              }
{ Visual Styles (Themes) API interface Unit for Object Pascal                  }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: tmschema.h, released June 2001. The original Pascal    }
{ code is: TmSchema.pas, released July 2001. The initial developer of the      }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Portions created by Mike Lischke are Copyright (C) 1999-2001                 }
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

unit TmSchema;

{$WEAKPACKAGEUNIT}
                                         
{$HPPEMIT ''}
{$HPPEMIT '#include "tmschema.h"'}
{$HPPEMIT ''}

// TODO EXTERNALSYMs

interface

//----------------------------------------------------------------------------------------------------------------------
//   TmSchema.h - Theme Manager schema (properties, parts, etc)
//----------------------------------------------------------------------------------------------------------------------

const
  THEMEMGR_VERSION = 1;     // increment if order of props changes or
                            // any props are deleted (will prevent loading
                            // of controlsets that use older version
  {$EXTERNALSYM THEMEMGR_VERSION}

//----------------------------------------------------------------------------------------------------------------------
//   TM_ENUM (must also be declared in PROPERTIES section)
//
//    these cannot be renumbered (part of uxtheme API)
//----------------------------------------------------------------------------------------------------------------------

type
  BGTYPE = Cardinal;

const
  BT_IMAGEFILE   = 0;
  BT_BORDERFILL  = 1;
  BT_NONE        = 2;

type
  IMAGELAYOUT = Cardinal;

const
  IL_VERTICAL    = 0;
  IL_HORIZONTAL  = 1;

type
  BORDERTYPE = Cardinal;

const
  BT_RECT       = 0;
  BT_ROUNDRECT  = 1;
  BT_ELLIPSE    = 2;

type
  FILLTYPE = Cardinal;

const
  FT_SOLID           = 0;
  FT_VERTGRADIENT    = 1;
  FT_HORZGRADIENT    = 2;
  FT_RADIALGRADIENT  = 3;
  FT_TILEIMAGE       = 4;

type
  SIZINGTYPE = Cardinal;

const
  ST_TRUESIZE  = 0;
  ST_STRETCH   = 1;
  ST_TILE      = 2;

type
  HALIGN = Cardinal;

const
  HA_LEFT    = 0;
  HA_CENTER  = 1;
  HA_RIGHT   = 2;

type
  CONTENTALIGNMENT = Cardinal;

const
  CA_LEFT    = 0;
  CA_CENTER  = 1;
  CA_RIGHT   = 2;

type
  VALIGN = Cardinal;

const
  VA_TOP     = 0;
  VA_CENTER  = 1;
  VA_BOTTOM  = 2;

type
  OFFSETTYPE = Cardinal;

const
  OT_TOPLEFT            = 0;
  OT_TOPRIGHT           = 1;
  OT_TOPMIDDLE          = 2;
  OT_BOTTOMLEFT         = 3;
  OT_BOTTOMRIGHT        = 4;
  OT_BOTTOMMIDDLE       = 5;
  OT_MIDDLELEFT         = 6;
  OT_MIDDLERIGHT        = 7;
  OT_LEFTOFCAPTION      = 8;
  OT_RIGHTOFCAPTION     = 9;
  OT_LEFTOFLASTBUTTON   = 10;
  OT_RIGHTOFLASTBUTTON  = 11;
  OT_ABOVELASTBUTTON    = 12;
  OT_BELOWLASTBUTTON    = 13;

type
  ICONEFFECT = Cardinal;

const
  ICE_NONE    = 0;
  ICE_GLOW    = 1;
  ICE_SHADOW  = 2;
  ICE_PULSE   = 3;
  ICE_ALPHA   = 4;

type
  TEXTSHADOWTYPE = Cardinal;

const
  TST_NONE        = 0;
  TST_SINGLE      = 1;
  TST_CONTINUOUS  = 2;

type
  GLYPHTYPE = Cardinal;

const
  GT_NONE        = 0;
  GT_IMAGEGLYPH  = 1;
  GT_FONTGLYPH   = 2;

type
  IMAGESELECTTYPE = Cardinal;

const
  IST_NONE  = 0;
  IST_SIZE  = 1;
  IST_DPI   = 2;

type
  TRUESIZESCALINGTYPE = Cardinal;

const
  TSST_NONE  = 0;
  TSST_SIZE  = 1;
  TSST_DPI   = 2;

type
  GLYPHFONTSIZINGTYPE = Cardinal;

const
  GFST_NONE  = 0;
  GFST_SIZE  = 1;
  GFST_DPI   = 2;

//----------------------------------------------------------------------------------------------------------------------
//    PROPERTIES - used by uxtheme rendering and controls
//
//    these cannot be renumbered (part of uxtheme API)
//----------------------------------------------------------------------------------------------------------------------

const

  //---- primitive types ----

  TMT_STRING = 201;
  TMT_INT = 202;
  TMT_BOOL = 203;
  TMT_COLOR = 204;
  TMT_MARGINS = 205;
  TMT_FILENAME = 206;
  TMT_SIZE = 207;
  TMT_POSITION = 208;
  TMT_RECT = 209;
  TMT_FONT = 210;
  TMT_INTLIST = 211;

  //---- special misc. properties ----

  TMT_COLORSCHEMES = 401;
  TMT_SIZES = 402;
  TMT_CHARSET = 403;

  //---- [documentation] properties ----

  TMT_DISPLAYNAME = 601;
  TMT_TOOLTIP = 602;
  TMT_COMPANY = 603;
  TMT_AUTHOR = 604;
  TMT_COPYRIGHT = 605;
  TMT_URL = 606;
  TMT_VERSION = 607;
  TMT_DESCRIPTION = 608;

  {$ifndef BCB}
    TMT_FIRST_RCSTRING_NAME = TMT_DISPLAYNAME;
    TMT_LAST_RCSTRING_NAME  = TMT_DESCRIPTION;
  {$endif BCB}

  //---- theme metrics: fonts ----

  TMT_CAPTIONFONT = 801;
  TMT_SMALLCAPTIONFONT = 802;
  TMT_MENUFONT = 803;
  TMT_STATUSFONT = 804;
  TMT_MSGBOXFONT = 805;
  TMT_ICONTITLEFONT = 806;

  {$ifndef BCB}
    TMT_FIRSTFONT = TMT_CAPTIONFONT;
    TMT_LASTFONT  = TMT_ICONTITLEFONT;
  {$endif BCB}
  
  //---- theme metrics: bools ----

  TMT_FLATMENUS = 1001;

  {$ifndef BCB}
    TMT_FIRSTBOOL = TMT_FLATMENUS;
    TMT_LASTBOOL  = TMT_FLATMENUS;
  {$endif BCB}
  
  //---- theme metrics: sizes ----

  TMT_SIZINGBORDERWIDTH = 1201;
  TMT_SCROLLBARWIDTH = 1202;
  TMT_SCROLLBARHEIGHT = 1203;
  TMT_CAPTIONBARWIDTH = 1204;
  TMT_CAPTIONBARHEIGHT = 1205;
  TMT_SMCAPTIONBARWIDTH = 1206;
  TMT_SMCAPTIONBARHEIGHT = 1207;
  TMT_MENUBARWIDTH = 1208;
  TMT_MENUBARHEIGHT = 1209;

  {$ifndef BCB}
    TMT_FIRSTSIZE  = TMT_SIZINGBORDERWIDTH;
    TMT_LASTSIZE  = TMT_MENUBARHEIGHT;
  {$endif BCB}
  
  //---- theme metrics: ints ----

  TMT_MINCOLORDEPTH = 1301;

  {$ifndef BCB}
    TMT_FIRSTINT = TMT_MINCOLORDEPTH;
    TMT_LASTINT  = TMT_MINCOLORDEPTH;
  {$endif BCB}
  
  //---- theme metrics: strings ----

  TMT_CSSNAME = 1401;
  TMT_XMLNAME = 1402;

  {$ifndef BCB}
    TMT_FIRSTSTRING  = TMT_CSSNAME;
    TMT_LASTSTRING   = TMT_XMLNAME;
  {$endif BCB}

  //---- theme metrics: colors ----

  TMT_SCROLLBAR = 1601;
  TMT_BACKGROUND = 1602;
  TMT_ACTIVECAPTION = 1603;
  TMT_INACTIVECAPTION = 1604;
  TMT_MENU = 1605;
  TMT_WINDOW = 1606;
  TMT_WINDOWFRAME = 1607;
  TMT_MENUTEXT = 1608;
  TMT_WINDOWTEXT = 1609;
  TMT_CAPTIONTEXT = 1610;
  TMT_ACTIVEBORDER = 1611;
  TMT_INACTIVEBORDER = 1612;
  TMT_APPWORKSPACE = 1613;
  TMT_HIGHLIGHT = 1614;
  TMT_HIGHLIGHTTEXT = 1615;
  TMT_BTNFACE = 1616;
  TMT_BTNSHADOW = 1617;
  TMT_GRAYTEXT = 1618;
  TMT_BTNTEXT = 1619;
  TMT_INACTIVECAPTIONTEXT = 1620;
  TMT_BTNHIGHLIGHT = 1621;
  TMT_DKSHADOW3D = 1622;
  TMT_LIGHT3D = 1623;
  TMT_INFOTEXT = 1624;
  TMT_INFOBK = 1625;
  TMT_BUTTONALTERNATEFACE = 1626;
  TMT_HOTTRACKING = 1627;
  TMT_GRADIENTACTIVECAPTION = 1628;
  TMT_GRADIENTINACTIVECAPTION = 1629;
  TMT_MENUHILIGHT = 1630;
  TMT_MENUBAR = 1631;

  {$ifndef BCB}
    TMT_FIRSTCOLOR = TMT_SCROLLBAR;
    TMT_LASTCOLOR  = TMT_MENUBAR;
  {$endif BCB}

  //---- hue substitutions ----

  TMT_FROMHUE1 = 1801;
  TMT_FROMHUE2 = 1802;
  TMT_FROMHUE3 = 1803;
  TMT_FROMHUE4 = 1804;
  TMT_FROMHUE5 = 1805;
  TMT_TOHUE1 = 1806;
  TMT_TOHUE2 = 1807;
  TMT_TOHUE3 = 1808;
  TMT_TOHUE4 = 1809;
  TMT_TOHUE5 = 1810;

  //---- color substitutions ----

  TMT_FROMCOLOR1 = 2001;
  TMT_FROMCOLOR2 = 2002;
  TMT_FROMCOLOR3 = 2003;
  TMT_FROMCOLOR4 = 2004;
  TMT_FROMCOLOR5 = 2005;
  TMT_TOCOLOR1 = 2006;
  TMT_TOCOLOR2 = 2007;
  TMT_TOCOLOR3 = 2008;
  TMT_TOCOLOR4 = 2009;
  TMT_TOCOLOR5 = 2010;

  //---- rendering BOOL properties ----

  TMT_TRANSPARENT = 2201;
  TMT_AUTOSIZE = 2202;
  TMT_BORDERONLY = 2203;
  TMT_COMPOSITED = 2204;
  TMT_BGFILL = 2205;
  TMT_GLYPHTRANSPARENT = 2206;
  TMT_GLYPHONLY = 2207;
  TMT_ALWAYSSHOWSIZINGBAR = 2208;
  TMT_MIRRORIMAGE = 2209;
  TMT_UNIFORMSIZING = 2210;
  TMT_INTEGRALSIZING = 2211;
  TMT_SOURCEGROW = 2212;
  TMT_SOURCESHRINK = 2213;

  //---- rendering INT properties ----

  TMT_IMAGECOUNT = 2401;
  TMT_ALPHALEVEL = 2402;
  TMT_BORDERSIZE = 2403;
  TMT_ROUNDCORNERWIDTH = 2404;
  TMT_ROUNDCORNERHEIGHT = 2405;
  TMT_GRADIENTRATIO1 = 2406;
  TMT_GRADIENTRATIO2 = 2407;
  TMT_GRADIENTRATIO3 = 2408;
  TMT_GRADIENTRATIO4 = 2409;
  TMT_GRADIENTRATIO5 = 2410;
  TMT_PROGRESSCHUNKSIZE = 2411;
  TMT_PROGRESSSPACESIZE = 2412;
  TMT_SATURATION = 2413;
  TMT_TEXTBORDERSIZE = 2414;
  TMT_ALPHATHRESHOLD = 2415;
  TMT_WIDTH = 2416;
  TMT_HEIGHT = 2417;
  TMT_GLYPHINDEX = 2418;
  TMT_TRUESIZESTRETCHMARK = 2419;
  TMT_MINDPI1 = 2420;
  TMT_MINDPI2 = 2421;
  TMT_MINDPI3 = 2422;
  TMT_MINDPI4 = 2423;
  TMT_MINDPI5 = 2424;

  //---- rendering FONT properties ----

  TMT_GLYPHFONT = 2601;

  //---- rendering INTLIST properties ----
  // start with 2801
                                              // (from smallest to largest)
  //---- rendering FILENAME properties ----

  TMT_IMAGEFILE = 3001;
  TMT_IMAGEFILE1 = 3002;
  TMT_IMAGEFILE2 = 3003;
  TMT_IMAGEFILE3 = 3004;
  TMT_IMAGEFILE4 = 3005;
  TMT_IMAGEFILE5 = 3006;
  TMT_STOCKIMAGEFILE = 3007;
  TMT_GLYPHIMAGEFILE = 3008;

  //---- rendering STRING properties ----

  TMT_TEXT = 3201;

  //---- rendering POSITION (x and y values) properties ----

  TMT_OFFSET = 3401;
  TMT_TEXTSHADOWOFFSET = 3402;
  TMT_MINSIZE = 3403;
  TMT_MINSIZE1 = 3404;
  TMT_MINSIZE2 = 3405;
  TMT_MINSIZE3 = 3406;
  TMT_MINSIZE4 = 3407;
  TMT_MINSIZE5 = 3408;
  TMT_NORMALSIZE = 3409;

  //---- rendering MARGIN properties ----

  TMT_SIZINGMARGINS = 3601;
  TMT_CONTENTMARGINS = 3602;
  TMT_CAPTIONMARGINS = 3603;

  //---- rendering COLOR properties ----

  TMT_BORDERCOLOR = 3801;
  TMT_FILLCOLOR = 3802;
  TMT_TEXTCOLOR = 3803;
  TMT_EDGELIGHTCOLOR = 3804;
  TMT_EDGEHIGHLIGHTCOLOR = 3805;
  TMT_EDGESHADOWCOLOR = 3806;
  TMT_EDGEDKSHADOWCOLOR = 3807;
  TMT_EDGEFILLCOLOR = 3808;
  TMT_TRANSPARENTCOLOR = 3809;
  TMT_GRADIENTCOLOR1 = 3810;
  TMT_GRADIENTCOLOR2 = 3811;
  TMT_GRADIENTCOLOR3 = 3812;
  TMT_GRADIENTCOLOR4 = 3813;
  TMT_GRADIENTCOLOR5 = 3814;
  TMT_SHADOWCOLOR = 3815;
  TMT_GLOWCOLOR = 3816;
  TMT_TEXTBORDERCOLOR = 3817;
  TMT_TEXTSHADOWCOLOR = 3818;
  TMT_GLYPHTEXTCOLOR = 3819;
  TMT_GLYPHTRANSPARENTCOLOR = 3820;
  TMT_FILLCOLORHINT = 3821;
  TMT_BORDERCOLORHINT = 3822;
  TMT_ACCENTCOLORHINT = 3823;

  //---- rendering enum properties (must be declared in TM_ENUM section above) ----

  TMT_BGTYPE = 4001;
  TMT_BORDERTYPE = 4002;
  TMT_FILLTYPE = 4003;
  TMT_SIZINGTYPE = 4004;
  TMT_HALIGN = 4005;
  TMT_CONTENTALIGNMENT = 4006;
  TMT_VALIGN = 4007;
  TMT_OFFSETTYPE = 4008;
  TMT_ICONEFFECT = 4009;
  TMT_TEXTSHADOWTYPE = 4010;
  TMT_IMAGELAYOUT = 4011;
  TMT_GLYPHTYPE = 4012;
  TMT_IMAGESELECTTYPE = 4013;
  TMT_GLYPHFONTSIZINGTYPE = 4014;
  TMT_TRUESIZESCALINGTYPE = 4015;

  //---- custom properties (used only by controls/shell) ----

  TMT_USERPICTURE = 5001;
  TMT_DEFAULTPANESIZE = 5002;
  TMT_BLENDCOLOR = 5003;

//----------------------------------------------------------------------------------------------------------------------
//   "Window" (i.e., non-client) Parts & States
//
//    these cannot be renumbered (part of uxtheme API)
//----------------------------------------------------------------------------------------------------------------------

type
  WINDOWPARTS = Cardinal;

const
  WINDOWPartFiller0                  = 0;
  WP_CAPTION                         = 1;
  WP_SMALLCAPTION                    = 2;
  WP_MINCAPTION                      = 3;
  WP_SMALLMINCAPTION                 = 4;
  WP_MAXCAPTION                      = 5;
  WP_SMALLMAXCAPTION                 = 6;
  WP_FRAMELEFT                       = 7;
  WP_FRAMERIGHT                      = 8;
  WP_FRAMEBOTTOM                     = 9;
  WP_SMALLFRAMELEFT                  = 10;
  WP_SMALLFRAMERIGHT                 = 11;
  WP_SMALLFRAMEBOTTOM                = 12;
  WP_SYSBUTTON                       = 13;
  WP_MDISYSBUTTON                    = 14;
  WP_MINBUTTON                       = 15;
  WP_MDIMINBUTTON                    = 16;
  WP_MAXBUTTON                       = 17;
  WP_CLOSEBUTTON                     = 18;
  WP_SMALLCLOSEBUTTON                = 19;
  WP_MDICLOSEBUTTON                  = 20;
  WP_RESTOREBUTTON                   = 21;
  WP_MDIRESTOREBUTTON                = 22;
  WP_HELPBUTTON                      = 23;
  WP_MDIHELPBUTTON                   = 24;
  WP_HORZSCROLL                      = 25;
  WP_HORZTHUMB                       = 26;
  WP_VERTSCROLL                      = 27;
  WP_VERTTHUMB                       = 28;
  WP_DIALOG                          = 29;
  WP_CAPTIONSIZINGTEMPLATE           = 30;
  WP_SMALLCAPTIONSIZINGTEMPLATE      = 31;
  WP_FRAMELEFTSIZINGTEMPLATE         = 32;
  WP_SMALLFRAMELEFTSIZINGTEMPLATE    = 33;
  WP_FRAMERIGHTSIZINGTEMPLATE        = 34;
  WP_SMALLFRAMERIGHTSIZINGTEMPLATE   = 35;
  WP_FRAMEBOTTOMSIZINGTEMPLATE       = 36;
  WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE  = 37;

type
  FRAMESTATES = Cardinal;

const
  FRAMEStateFiller0  = 0;
  FS_ACTIVE          = 1;
  FS_INACTIVE        = 2;

type
  CAPTIONSTATES = Cardinal;

const
  CAPTIONStateFiller0  = 0;
  CS_ACTIVE            = 1;
  CS_INACTIVE          = 2;
  CS_DISABLED          = 3;

type
  MAXCAPTIONSTATES = Cardinal;

const
  MAXCAPTIONStateFiller0  = 0;
  MXCS_ACTIVE             = 1;
  MXCS_INACTIVE           = 2;
  MXCS_DISABLED           = 3;

type
  MINCAPTIONSTATES = Cardinal;

const
  MINCAPTIONStateFiller0  = 0;
  MNCS_ACTIVE             = 1;
  MNCS_INACTIVE           = 2;
  MNCS_DISABLED           = 3;

type
  HORZSCROLLSTATES = Cardinal;

const
  HORZSCROLLStateFiller0  = 0;
  HSS_NORMAL              = 1;
  HSS_HOT                 = 2;
  HSS_PUSHED              = 3;
  HSS_DISABLED            = 4;

type
  HORZTHUMBSTATES = Cardinal;

const
  HORZTHUMBStateFiller0  = 0;
  HTS_NORMAL             = 1;
  HTS_HOT                = 2;
  HTS_PUSHED             = 3;
  HTS_DISABLED           = 4;

type
  VERTSCROLLSTATES = Cardinal;

const
  VERTSCROLLStateFiller0  = 0;
  VSS_NORMAL              = 1;
  VSS_HOT                 = 2;
  VSS_PUSHED              = 3;
  VSS_DISABLED            = 4;

type
  VERTTHUMBSTATES = Cardinal;

const
  VERTTHUMBStateFiller0  = 0;
  VTS_NORMAL             = 1;
  VTS_HOT                = 2;
  VTS_PUSHED             = 3;
  VTS_DISABLED           = 4;

type
  SYSBUTTONSTATES = Cardinal;

const
  SYSBUTTONStateFiller0  = 0;
  SBS_NORMAL             = 1;
  SBS_HOT                = 2;
  SBS_PUSHED             = 3;
  SBS_DISABLED           = 4;

type
  MINBUTTONSTATES = Cardinal;

const
  MINBUTTONStateFiller0  = 0;
  MINBS_NORMAL           = 1;
  MINBS_HOT              = 2;
  MINBS_PUSHED           = 3;
  MINBS_DISABLED         = 4;
  MINBS_INACTIVE         = 5;

type
  MAXBUTTONSTATES = Cardinal;

const
  MAXBUTTONStateFiller0  = 0;
  MAXBS_NORMAL           = 1;
  MAXBS_HOT              = 2;
  MAXBS_PUSHED           = 3;
  MAXBS_DISABLED         = 4;
  MAXBS_INACTIVE         = 5;

type
  RESTOREBUTTONSTATES = Cardinal;

const
  RESTOREBUTTONStateFiller0  = 0;
  RBS_NORMAL                 = 1;
  RBS_HOT                    = 2;
  RBS_PUSHED                 = 3;
  RBS_DISABLED               = 4;
  RBS_INACTIVE               = 5;

type
  HELPBUTTONSTATES = Cardinal;

const
  HELPBUTTONStateFiller0  = 0;
  HBS_NORMAL              = 1;
  HBS_HOT                 = 2;
  HBS_PUSHED              = 3;
  HBS_DISABLED            = 4;
  HBS_INACTIVE            = 5;

type
  CLOSEBUTTONSTATES = Cardinal;

const
  CLOSEBUTTONStateFiller0  = 0;
  CBS_NORMAL               = 1;
  CBS_HOT                  = 2;
  CBS_PUSHED               = 3;
  CBS_DISABLED             = 4;
  CBS_INACTIVE             = 5;

//----------------------------------------------------------------------------------------------------------------------
//   "Button" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  BUTTONPARTS = Cardinal;

const
  BUTTONPartFiller0  = 0;
  BP_PUSHBUTTON      = 1;
  BP_RADIOBUTTON     = 2;
  BP_CHECKBOX        = 3;
  BP_GROUPBOX        = 4;
  BP_USERBUTTON      = 5;

type
  PUSHBUTTONSTATES = Cardinal;

const
  PUSHBUTTONStateFiller0  = 0;
  PBS_NORMAL              = 1;
  PBS_HOT                 = 2;
  PBS_PRESSED             = 3;
  PBS_DISABLED            = 4;
  PBS_DEFAULTED           = 5;

type
  RADIOBUTTONSTATES = Cardinal;

const
  RADIOBUTTONStateFiller0  = 0;
  RBS_UNCHECKEDNORMAL      = 1;
  RBS_UNCHECKEDHOT         = 2;
  RBS_UNCHECKEDPRESSED     = 3;
  RBS_UNCHECKEDDISABLED    = 4;
  RBS_CHECKEDNORMAL        = 5;
  RBS_CHECKEDHOT           = 6;
  RBS_CHECKEDPRESSED       = 7;
  RBS_CHECKEDDISABLED      = 8;

type
  CHECKBOXSTATES = Cardinal;

const
  CHECKBOXStateFiller0   = 0;
  CBS_UNCHECKEDNORMAL    = 1;
  CBS_UNCHECKEDHOT       = 2;
  CBS_UNCHECKEDPRESSED   = 3;
  CBS_UNCHECKEDDISABLED  = 4;
  CBS_CHECKEDNORMAL      = 5;
  CBS_CHECKEDHOT         = 6;
  CBS_CHECKEDPRESSED     = 7;
  CBS_CHECKEDDISABLED    = 8;
  CBS_MIXEDNORMAL        = 9;
  CBS_MIXEDHOT           = 10;
  CBS_MIXEDPRESSED       = 11;
  CBS_MIXEDDISABLED      = 12;

type
  GROUPBOXSTATES = Cardinal;

const
  GROUPBOXStateFiller0  = 0;
  GBS_NORMAL            = 1;
  GBS_DISABLED          = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "Rebar" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  REBARPARTS = Cardinal;

const
  REBARPartFiller0  = 0;
  RP_GRIPPER        = 1;
  RP_GRIPPERVERT    = 2;
  RP_BAND           = 3;
  RP_CHEVRON        = 4;
  RP_CHEVRONVERT    = 5;

type
  CHEVRONSTATES = Cardinal;

const
  CHEVRONStateFiller0  = 0;
  CHEVS_NORMAL         = 1;
  CHEVS_HOT            = 2;
  CHEVS_PRESSED        = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "Toolbar" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  TOOLBARPARTS = Cardinal;

const
  TOOLBARPartFiller0      = 0;
  TP_BUTTON               = 1;
  TP_DROPDOWNBUTTON       = 2;
  TP_SPLITBUTTON          = 3;
  TP_SPLITBUTTONDROPDOWN  = 4;
  TP_SEPARATOR            = 5;
  TP_SEPARATORVERT        = 6;

type
  TOOLBARSTATES = Cardinal;

const
  TOOLBARStateFiller0  = 0;
  TS_NORMAL            = 1;
  TS_HOT               = 2;
  TS_PRESSED           = 3;
  TS_DISABLED          = 4;
  TS_CHECKED           = 5;
  TS_HOTCHECKED        = 6;

//----------------------------------------------------------------------------------------------------------------------
//   "Status" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  STATUSPARTS = Cardinal;

const
  STATUSPartFiller0  = 0;
  SP_PANE            = 1;
  SP_GRIPPERPANE     = 2;
  SP_GRIPPER         = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "Menu" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  MENUPARTS = Cardinal;

const
  MENUPartFiller0     = 0;
  MP_MENUITEM         = 1;
  MP_MENUDROPDOWN     = 2;
  MP_MENUBARITEM      = 3;
  MP_MENUBARDROPDOWN  = 4;
  MP_CHEVRON          = 5;
  MP_SEPARATOR        = 6;

type
  MENUSTATES = Cardinal;

const
  MENUStateFiller0  = 0;
  MS_NORMAL         = 1;
  MS_SELECTED       = 2;
  MS_DEMOTED        = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "ListView" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  LISTVIEWPARTS = Cardinal;

const
  LISTVIEWPartFiller0   = 0;
  LVP_LISTITEM          = 1;
  LVP_LISTGROUP         = 2;
  LVP_LISTDETAIL        = 3;
  LVP_LISTSORTEDDETAIL  = 4;
  LVP_EMPTYTEXT         = 5;

type
  LISTITEMSTATES = Cardinal;

const
  LISTITEMStateFiller0  = 0;
  LIS_NORMAL            = 1;
  LIS_HOT               = 2;
  LIS_SELECTED          = 3;
  LIS_DISABLED          = 4;
  LIS_SELECTEDNOTFOCUS  = 5;

//----------------------------------------------------------------------------------------------------------------------
//   "Header" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  HEADERPARTS = Cardinal;

const
  HEADERPartFiller0   = 0;
  HP_HEADERITEM       = 1;
  HP_HEADERITEMLEFT   = 2;
  HP_HEADERITEMRIGHT  = 3;
  HP_HEADERSORTARROW  = 4;

type
  HEADERITEMSTATES = Cardinal;

const
  HEADERITEMStateFiller0  = 0;
  HIS_NORMAL              = 1;
  HIS_HOT                 = 2;
  HIS_PRESSED             = 3;

type
  HEADERITEMLEFTSTATES = Cardinal;

const
  HEADERITEMLEFTStateFiller0  = 0;
  HILS_NORMAL                 = 1;
  HILS_HOT                    = 2;
  HILS_PRESSED                = 3;

type
  HEADERITEMRIGHTSTATES = Cardinal;

const
  HEADERITEMRIGHTStateFiller0  = 0;
  HIRS_NORMAL                  = 1;
  HIRS_HOT                     = 2;
  HIRS_PRESSED                 = 3;

type
  HEADERSORTARROWSTATES = Cardinal;

const
  HEADERSORTARROWStateFiller0  = 0;
  HSAS_SORTEDUP                = 1;
  HSAS_SORTEDDOWN              = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "Progress" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  PROGRESSPARTS = Cardinal;

const
  PROGRESSPartFiller0  = 0;
  PP_BAR               = 1;
  PP_BARVERT           = 2;
  PP_CHUNK             = 3;
  PP_CHUNKVERT         = 4;

//----------------------------------------------------------------------------------------------------------------------
//   "Tab" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  TABPARTS = Cardinal;

const
  TABPartFiller0            = 0;
  TABP_TABITEM              = 1;
  TABP_TABITEMLEFTEDGE      = 2;
  TABP_TABITEMRIGHTEDGE     = 3;
  TABP_TABITEMBOTHEDGE      = 4;
  TABP_TOPTABITEM           = 5;
  TABP_TOPTABITEMLEFTEDGE   = 6;
  TABP_TOPTABITEMRIGHTEDGE  = 7;
  TABP_TOPTABITEMBOTHEDGE   = 8;
  TABP_PANE                 = 9;
  TABP_BODY                 = 10;

type
  TABITEMSTATES = Cardinal;

const
  TABITEMStateFiller0  = 0;
  TIS_NORMAL           = 1;
  TIS_HOT              = 2;
  TIS_SELECTED         = 3;
  TIS_DISABLED         = 4;
  TIS_FOCUSED          = 5;

type
  TABITEMLEFTEDGESTATES = Cardinal;

const
  TABITEMLEFTEDGEStateFiller0  = 0;
  TILES_NORMAL                 = 1;
  TILES_HOT                    = 2;
  TILES_SELECTED               = 3;
  TILES_DISABLED               = 4;
  TILES_FOCUSED                = 5;

type
  TABITEMRIGHTEDGESTATES = Cardinal;

const
  TABITEMRIGHTEDGEStateFiller0  = 0;
  TIRES_NORMAL                  = 1;
  TIRES_HOT                     = 2;
  TIRES_SELECTED                = 3;
  TIRES_DISABLED                = 4;
  TIRES_FOCUSED                 = 5;

type
  TABITEMBOTHEDGESSTATES = Cardinal;

const
  TABITEMBOTHEDGESStateFiller0  = 0;
  TIBES_NORMAL                  = 1;
  TIBES_HOT                     = 2;
  TIBES_SELECTED                = 3;
  TIBES_DISABLED                = 4;
  TIBES_FOCUSED                 = 5;

type
  TOPTABITEMSTATES = Cardinal;

const
  TOPTABITEMStateFiller0  = 0;
  TTIS_NORMAL             = 1;
  TTIS_HOT                = 2;
  TTIS_SELECTED           = 3;
  TTIS_DISABLED           = 4;
  TTIS_FOCUSED            = 5;

type
  TOPTABITEMLEFTEDGESTATES = Cardinal;

const
  TOPTABITEMLEFTEDGEStateFiller0  = 0;
  TTILES_NORMAL                   = 1;
  TTILES_HOT                      = 2;
  TTILES_SELECTED                 = 3;
  TTILES_DISABLED                 = 4;
  TTILES_FOCUSED                  = 5;

type
  TOPTABITEMRIGHTEDGESTATES = Cardinal;

const
  TOPTABITEMRIGHTEDGEStateFiller0  = 0;
  TTIRES_NORMAL                    = 1;
  TTIRES_HOT                       = 2;
  TTIRES_SELECTED                  = 3;
  TTIRES_DISABLED                  = 4;
  TTIRES_FOCUSED                   = 5;

type
  TOPTABITEMBOTHEDGESSTATES = Cardinal;

const
  TOPTABITEMBOTHEDGESStateFiller0  = 0;
  TTIBES_NORMAL                    = 1;
  TTIBES_HOT                       = 2;
  TTIBES_SELECTED                  = 3;
  TTIBES_DISABLED                  = 4;
  TTIBES_FOCUSED                   = 5;

//----------------------------------------------------------------------------------------------------------------------
//   "Trackbar" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  TRACKBARPARTS = Cardinal;

const
  TRACKBARPartFiller0  = 0;
  TKP_TRACK            = 1;
  TKP_TRACKVERT        = 2;
  TKP_THUMB            = 3;
  TKP_THUMBBOTTOM      = 4;
  TKP_THUMBTOP         = 5;
  TKP_THUMBVERT        = 6;
  TKP_THUMBLEFT        = 7;
  TKP_THUMBRIGHT       = 8;
  TKP_TICS             = 9;
  TKP_TICSVERT         = 10;

type
  TRACKBARSTATES = Cardinal;

const
  TRACKBARStateFiller0  = 0;
  TKS_NORMAL            = 1;

type
  TRACKSTATES = Cardinal;

const
  TRACKStateFiller0  = 0;
  TRS_NORMAL         = 1;

type
  TRACKVERTSTATES = Cardinal;

const
  TRACKVERTStateFiller0  = 0;
  TRVS_NORMAL            = 1;

type
  THUMBSTATES = Cardinal;

const
  THUMBStateFiller0  = 0;
  TUS_NORMAL         = 1;
  TUS_HOT            = 2;
  TUS_PRESSED        = 3;
  TUS_FOCUSED        = 4;
  TUS_DISABLED       = 5;

type
  THUMBBOTTOMSTATES = Cardinal;

const
  THUMBBOTTOMStateFiller0  = 0;
  TUBS_NORMAL              = 1;
  TUBS_HOT                 = 2;
  TUBS_PRESSED             = 3;
  TUBS_FOCUSED             = 4;
  TUBS_DISABLED            = 5;

type
  THUMBTOPSTATES = Cardinal;

const
  THUMBTOPStateFiller0  = 0;
  TUTS_NORMAL           = 1;
  TUTS_HOT              = 2;
  TUTS_PRESSED          = 3;
  TUTS_FOCUSED          = 4;
  TUTS_DISABLED         = 5;

type
  THUMBVERTSTATES = Cardinal;

const
  THUMBVERTStateFiller0  = 0;
  TUVS_NORMAL            = 1;
  TUVS_HOT               = 2;
  TUVS_PRESSED           = 3;
  TUVS_FOCUSED           = 4;
  TUVS_DISABLED          = 5;

type
  THUMBLEFTSTATES = Cardinal;

const
  THUMBLEFTStateFiller0  = 0;
  TUVLS_NORMAL           = 1;
  TUVLS_HOT              = 2;
  TUVLS_PRESSED          = 3;
  TUVLS_FOCUSED          = 4;
  TUVLS_DISABLED         = 5;

type
  THUMBRIGHTSTATES = Cardinal;

const
  THUMBRIGHTStateFiller0  = 0;
  TUVRS_NORMAL            = 1;
  TUVRS_HOT               = 2;
  TUVRS_PRESSED           = 3;
  TUVRS_FOCUSED           = 4;
  TUVRS_DISABLED          = 5;

type
  TICSSTATES = Cardinal;

const
  TICSStateFiller0  = 0;
  TSS_NORMAL        = 1;

type
  TICSVERTSTATES = Cardinal;

const
  TICSVERTStateFiller0  = 0;
  TSVS_NORMAL           = 1;

//----------------------------------------------------------------------------------------------------------------------
//   "Tooltips" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  TOOLTIPPARTS = Cardinal;

const
  TOOLTIPPartFiller0  = 0;
  TTP_STANDARD        = 1;
  TTP_STANDARDTITLE   = 2;
  TTP_BALLOON         = 3;
  TTP_BALLOONTITLE    = 4;
  TTP_CLOSE           = 5;

type
  CLOSESTATES = Cardinal;

const
  CLOSEStateFiller0  = 0;
  TTCS_NORMAL        = 1;
  TTCS_HOT           = 2;
  TTCS_PRESSED       = 3;

type
  STANDARDSTATES = Cardinal;

const
  STANDARDStateFiller0  = 0;
  TTSS_NORMAL           = 1;
  TTSS_LINK             = 2;

type
  BALLOONSTATES = Cardinal;

const
  BALLOONStateFiller0  = 0;
  TTBS_NORMAL          = 1;
  TTBS_LINK            = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "TreeView" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  TREEVIEWPARTS = Cardinal;

const
  TREEVIEWPartFiller0  = 0;
  TVP_TREEITEM         = 1;
  TVP_GLYPH            = 2;
  TVP_BRANCH           = 3;

type
  TREEITEMSTATES = Cardinal;

const
  TREEITEMStateFiller0    = 0;
  TREIS_NORMAL            = 1;
  TREIS_HOT               = 2;
  TREIS_SELECTED          = 3;
  TREIS_DISABLED          = 4;
  TREIS_SELECTEDNOTFOCUS  = 5;

type
  GLYPHSTATES = Cardinal;

const
  GLYPHStateFiller0  = 0;
  GLPS_CLOSED        = 1;
  GLPS_OPENED        = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "Spin" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  SPINPARTS = Cardinal;

const
  SPINPartFiller0  = 0;
  SPNP_UP          = 1;
  SPNP_DOWN        = 2;
  SPNP_UPHORZ      = 3;
  SPNP_DOWNHORZ    = 4;

type
  UPSTATES = Cardinal;

const
  UPStateFiller0  = 0;
  UPS_NORMAL      = 1;
  UPS_HOT         = 2;
  UPS_PRESSED     = 3;
  UPS_DISABLED    = 4;

type
  DOWNSTATES = Cardinal;

const
  DOWNStateFiller0  = 0;
  DNS_NORMAL        = 1;
  DNS_HOT           = 2;
  DNS_PRESSED       = 3;
  DNS_DISABLED      = 4;

type
  UPHORZSTATES = Cardinal;

const
  UPHORZStateFiller0  = 0;
  UPHZS_NORMAL        = 1;
  UPHZS_HOT           = 2;
  UPHZS_PRESSED       = 3;
  UPHZS_DISABLED      = 4;

type
  DOWNHORZSTATES = Cardinal;

const
  DOWNHORZStateFiller0  = 0;
  DNHZS_NORMAL          = 1;
  DNHZS_HOT             = 2;
  DNHZS_PRESSED         = 3;
  DNHZS_DISABLED        = 4;

//----------------------------------------------------------------------------------------------------------------------
//   "Page" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  PAGEPARTS = Cardinal;

const
  PAGEPartFiller0  = 0;
  PGRP_UP          = 1;
  PGRP_DOWN        = 2;
  PGRP_UPHORZ      = 3;
  PGRP_DOWNHORZ    = 4;

//--- Pager uses same states as Spin ---

//----------------------------------------------------------------------------------------------------------------------
//   "Scrollbar" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  SCROLLBARPARTS = Cardinal;

const
  SCROLLBARPartFiller0  = 0;
  SBP_ARROWBTN          = 1;
  SBP_THUMBBTNHORZ      = 2;
  SBP_THUMBBTNVERT      = 3;
  SBP_LOWERTRACKHORZ    = 4;
  SBP_UPPERTRACKHORZ    = 5;
  SBP_LOWERTRACKVERT    = 6;
  SBP_UPPERTRACKVERT    = 7;
  SBP_GRIPPERHORZ       = 8;
  SBP_GRIPPERVERT       = 9;
  SBP_SIZEBOX           = 10;

type
  ARROWBTNSTATES = Cardinal;

const
  ARROWBTNStateFiller0  = 0;
  ABS_UPNORMAL          = 1;
  ABS_UPHOT             = 2;
  ABS_UPPRESSED         = 3;
  ABS_UPDISABLED        = 4;
  ABS_DOWNNORMAL        = 5;
  ABS_DOWNHOT           = 6;
  ABS_DOWNPRESSED       = 7;
  ABS_DOWNDISABLED      = 8;
  ABS_LEFTNORMAL        = 9;
  ABS_LEFTHOT           = 10;
  ABS_LEFTPRESSED       = 11;
  ABS_LEFTDISABLED      = 12;
  ABS_RIGHTNORMAL       = 13;
  ABS_RIGHTHOT          = 14;
  ABS_RIGHTPRESSED      = 15;
  ABS_RIGHTDISABLED     = 16;

type
  SCROLLBARSTATES = Cardinal;

const
  SCROLLBARStateFiller0  = 0;
  SCRBS_NORMAL           = 1;
  SCRBS_HOT              = 2;
  SCRBS_PRESSED          = 3;
  SCRBS_DISABLED         = 4;

type
  SIZEBOXSTATES = Cardinal;

const
  SIZEBOXStateFiller0  = 0;
  SZB_RIGHTALIGN       = 1;
  SZB_LEFTALIGN        = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "Edit" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  EDITPARTS = Cardinal;

const
  EDITPartFiller0  = 0;
  EP_EDITTEXT      = 1;
  EP_CARET         = 2;

type
  EDITTEXTSTATES = Cardinal;

const
  EDITTEXTStateFiller0  = 0;
  ETS_NORMAL            = 1;
  ETS_HOT               = 2;
  ETS_SELECTED          = 3;
  ETS_DISABLED          = 4;
  ETS_FOCUSED           = 5;
  ETS_READONLY          = 6;
  ETS_ASSIST            = 7;

//----------------------------------------------------------------------------------------------------------------------
//   "ComboBox" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  COMBOBOXPARTS = Cardinal;

const
  COMBOBOXPartFiller0  = 0;
  CP_DROPDOWNBUTTON    = 1;

type
  COMBOBOXSTATES = Cardinal;

const
  COMBOBOXStateFiller0  = 0;
  CBXS_NORMAL           = 1;
  CBXS_HOT              = 2;
  CBXS_PRESSED          = 3;
  CBXS_DISABLED         = 4;

//----------------------------------------------------------------------------------------------------------------------
//   "Taskbar Clock" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  CLOCKPARTS = Cardinal;

const
  CLOCKPartFiller0  = 0;
  CLP_TIME          = 1;

type
  CLOCKSTATES = Cardinal;

const
  CLOCKStateFiller0  = 0;
  CLS_NORMAL         = 1;

//----------------------------------------------------------------------------------------------------------------------
//   "Tray Notify" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  TRAYNOTIFYPARTS = Cardinal;

const
  TRAYNOTIFYPartFiller0  = 0;
  TNP_BACKGROUND         = 1;
  TNP_ANIMBACKGROUND     = 2;

//----------------------------------------------------------------------------------------------------------------------
//   "TaskBar" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  TASKBARPARTS = Cardinal;

const
  TASKBARPartFiller0    = 0;
  TBP_BACKGROUNDBOTTOM  = 1;
  TBP_BACKGROUNDRIGHT   = 2;
  TBP_BACKGROUNDTOP     = 3;
  TBP_BACKGROUNDLEFT    = 4;
  TBP_SIZINGBARBOTTOM   = 5;
  TBP_SIZINGBARRIGHT    = 6;
  TBP_SIZINGBARTOP      = 7;
  TBP_SIZINGBARLEFT     = 8;

//----------------------------------------------------------------------------------------------------------------------
//   "TaskBand" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  TASKBANDPARTS = Cardinal;

const
  TASKBANDPartFiller0       = 0;
  TDP_GROUPCOUNT            = 1;
  TDP_FLASHBUTTON           = 2;
  TDP_FLASHBUTTONGROUPMENU  = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "StartPanel" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  STARTPANELPARTS = Cardinal;

const
  STARTPANELPartFiller0    = 0;
  SPP_USERPANE             = 1;
  SPP_MOREPROGRAMS         = 2;
  SPP_MOREPROGRAMSARROW    = 3;
  SPP_PROGLIST             = 4;
  SPP_PROGLISTSEPARATOR    = 5;
  SPP_PLACESLIST           = 6;
  SPP_PLACESLISTSEPARATOR  = 7;
  SPP_LOGOFF               = 8;
  SPP_LOGOFFBUTTONS        = 9;
  SPP_USERPICTURE          = 10;
  SPP_PREVIEW              = 11;

type
  MOREPROGRAMSARROWSTATES = Cardinal;

const
  MOREPROGRAMSARROWStateFiller0  = 0;
  SPS_NORMAL                     = 1;
  SPS_HOT                        = 2;
  SPS_PRESSED                    = 3;

type
  LOGOFFBUTTONSSTATES = Cardinal;

const
  LOGOFFBUTTONSStateFiller0  = 0;
  SPLS_NORMAL                = 1;
  SPLS_HOT                   = 2;
  SPLS_PRESSED               = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "ExplorerBar" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  EXPLORERBARPARTS = Cardinal;

const
  EXPLORERBARPartFiller0      = 0;
  EBP_HEADERBACKGROUND        = 1;
  EBP_HEADERCLOSE             = 2;
  EBP_HEADERPIN               = 3;
  EBP_IEBARMENU               = 4;
  EBP_NORMALGROUPBACKGROUND   = 5;
  EBP_NORMALGROUPCOLLAPSE     = 6;
  EBP_NORMALGROUPEXPAND       = 7;
  EBP_NORMALGROUPHEAD         = 8;
  EBP_SPECIALGROUPBACKGROUND  = 9;
  EBP_SPECIALGROUPCOLLAPSE    = 10;
  EBP_SPECIALGROUPEXPAND      = 11;
  EBP_SPECIALGROUPHEAD        = 12;

type
  HEADERCLOSESTATES = Cardinal;

const
  HEADERCLOSEStateFiller0  = 0;
  EBHC_NORMAL              = 1;
  EBHC_HOT                 = 2;
  EBHC_PRESSED             = 3;

type
  HEADERPINSTATES = Cardinal;

const
  HEADERPINStateFiller0  = 0;
  EBHP_NORMAL            = 1;
  EBHP_HOT               = 2;
  EBHP_PRESSED           = 3;
  EBHP_SELECTEDNORMAL    = 4;
  EBHP_SELECTEDHOT       = 5;
  EBHP_SELECTEDPRESSED   = 6;

type
  IEBARMENUSTATES = Cardinal;

const
  IEBARMENUStateFiller0  = 0;
  EBM_NORMAL             = 1;
  EBM_HOT                = 2;
  EBM_PRESSED            = 3;

type
  NORMALGROUPCOLLAPSESTATES = Cardinal;

const
  NORMALGROUPCOLLAPSEStateFiller0  = 0;
  EBNGC_NORMAL                     = 1;
  EBNGC_HOT                        = 2;
  EBNGC_PRESSED                    = 3;

type
  NORMALGROUPEXPANDSTATES = Cardinal;

const
  NORMALGROUPEXPANDStateFiller0  = 0;
  EBNGE_NORMAL                   = 1;
  EBNGE_HOT                      = 2;
  EBNGE_PRESSED                  = 3;

type
  SPECIALGROUPCOLLAPSESTATES = Cardinal;

const
  SPECIALGROUPCOLLAPSEStateFiller0  = 0;
  EBSGC_NORMAL                      = 1;
  EBSGC_HOT                         = 2;
  EBSGC_PRESSED                     = 3;

type
  SPECIALGROUPEXPANDSTATES = Cardinal;

const
  SPECIALGROUPEXPANDStateFiller0  = 0;
  EBSGE_NORMAL                    = 1;
  EBSGE_HOT                       = 2;
  EBSGE_PRESSED                   = 3;

//----------------------------------------------------------------------------------------------------------------------
//   "TaskBand" Parts & States
//----------------------------------------------------------------------------------------------------------------------

type
  MENUBANDPARTS = Cardinal;

const
  MENUBANDPartFiller0  = 0;
  MDP_NEWAPPBUTTON     = 1;
  MDP_SEPERATOR        = 2;

type
  MENUBANDSTATES = Cardinal;

const
  MENUBANDStateFiller0  = 0;
  MDS_NORMAL            = 1;
  MDS_HOT               = 2;
  MDS_PRESSED           = 3;
  MDS_DISABLED          = 4;
  MDS_CHECKED           = 5;
  MDS_HOTCHECKED        = 6;

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------------------------------------------------------------------------------------------------------------

end.
