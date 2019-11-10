(******************************************************************************)
(* SynEdit Include File.  This file was adapted from Brad Stowers' DFS.INC    *)
(* file and used with permission.  This will help reduce headaches when new   *)
(* versions of Delphi and C++Builder are released, among other things.        *)
(******************************************************************************)
(* Brad Stowers: bstowers@pobox.com                                           *)
(* Delphi Free Stuff: http://delphifreestuff.com/                             *)
(* February 24, 1999                                                          *)
(******************************************************************************)
(*                                                                            *)
(* Complete Boolean Evaluation compiler directive is turned off by including  *)
(*   this file.                                                               *)
(* The $ObjExportAll directive is turned on if compiling with C++Builder 3 or *)
(*   higher.  This is required for Delphi components built in Builder with    *)
(*   run-time packages.                                                       *)
(*                                                                            *)
(* Here is a brief explanation of what each of the defines mean:              *)
(* SYN_WIN32            : Compilation target is 32-bit Windows                *)
(* SYN_COMPILER_2       : Delphi 2 or C++Builder 1 is the compiler.           *)
(* SYN_COMPILER_2_UP    : Delphi 2 or higher, or C++Builder 1 or higher is    *)
(*                        the compiler.                                       *)
(* SYN_COMPILER_3       : Delphi 3 or C++Builder 3 is the compiler.           *)
(* SYN_COMPILER_3_UP    : Delphi 3 or higher, or C++Builder 3 or higher is    *)
(*                        the compiler.                                       *)
(* SYN_COMPILER_4       : Delphi 4 or C++Builder 4 is the compiler.           *)
(* SYN_COMPILER_4_UP    : Delphi 4 or higher, or C++Builder 4 or higher is    *)
(*                        the compiler.                                       *)
(* SYN_COMPILER_5       : Delphi 5 or C++Builder 5 is the compiler.           *)
(* SYN_COMPILER_5_UP    : Delphi 5 or higher, or C++Builder 5 or higher is    *)
(*                        the compiler.                                       *)
(* SYN_COMPILER_6       : Delphi 6, C++Builder 6, Kylix 1 or Kylix 2 is       *)
(*                        the compiler.                                       *)
(* SYN_COMPILER_6_UP    : Delphi 6, C++Builder 6, Kylix 1, Kylix 2 or higher  *)
(*                        is the compiler.                                    *)
(* SYN_COMPILER_7       : Delphi 7 or Kylix 3 is the compiler.                *)
(* SYN_COMPILER_7_UP    : Delphi 7, Kylix 3 or higher is the compiler.        *)
(* SYN_COMPILER_9       : Delphi 2005 is the compiler.                        *)
(* SYN_COMPILER_9_UP    : Delphi 2005 or higher is the compiler.              *)
(* SYN_COMPILER_10      : Delphi 2006 is the compiler.                        *)
(* SYN_COMPILER_10_UP   : Delphi 2006 or higher is the compiler.              *)
(* SYN_COMPILER_11      : Delphi 2007 is the compiler.                        *)
(* SYN_COMPILER_11_UP   : Delphi 2007 or higher is the compiler.              *)
(* SYN_COMPILER_12      : Delphi 2009 is the compiler.                        *)
(* SYN_COMPILER_12_UP   : Delphi 2009 or higher is the compiler.              *)
(* SYN_COMPILER_14      : Delphi 2010 is the compiler.                        *)
(* SYN_COMPILER_14_UP   : Delphi 2010 or higher is the compiler.              *)
(* SYN_COMPILER_15      : Delphi XE   is the compiler.                        *)
(* SYN_COMPILER_15_UP   : Delphi XE   or higher is the compiler.              *)
(* SYN_COMPILER_16      : Delphi XE2  is the compiler.                        *)
(* SYN_COMPILER_16_UP   : Delphi XE2  or higher is the compiler.              *)
(* SYN_COMPILER_17      : Delphi XE3  is the compiler.                        *)
(* SYN_COMPILER_17_UP   : Delphi XE3  or higher is the compiler.              *)
(* SYN_COMPILER_18      : Delphi XE4  is the compiler.                        *)
(* SYN_COMPILER_18_UP   : Delphi XE4  or higher is the compiler.              *)
(* SYN_COMPILER_19      : Delphi XE5  is the compiler.                        *)
(* SYN_COMPILER_19_UP   : Delphi XE5  or higher is the compiler.              *)
(* SYN_COMPILER_20      : Delphi XE6  is the compiler.                        *)
(* SYN_COMPILER_20_UP   : Delphi XE6  or higher is the compiler.              *)
(* SYN_COMPILER_21      : Delphi XE7  is the compiler.                        *)
(* SYN_COMPILER_21_UP   : Delphi XE7  or higher is the compiler.              *)
(* SYN_COMPILER_22      : Delphi XE8  is the compiler.                        *)
(* SYN_COMPILER_22_UP   : Delphi XE8  or higher is the compiler.              *)
(* SYN_CPPB             : Any version of C++Builder is being used.            *)
(* SYN_CPPB_1           : C++B v1.0x is being used.                           *)
(* SYN_CPPB_3           : C++B v3.0x is being used.                           *)
(* SYN_CPPB_3_UP        : C++B v3.0x or higher is being used.                 *)
(* SYN_CPPB_4           : C++B v4.0x is being used.                           *)
(* SYN_CPPB_4_UP        : C++B v4.0x or higher is being used.                 *)
(* SYN_CPPB_5           : C++B v5.0x is being used.                           *)
(* SYN_CPPB_5_UP        : C++B v5.0x or higher is being used.                 *)
(* SYN_CPPB_6           : C++B v6.0x is being used.                           *)
(* SYN_CPPB_6_UP        : C++B v6.0x or higher is being used.                 *)
(* SYN_DELPHI           : Any version of Delphi is being used.                *)
(* SYN_DELPHI_PE        : The personal edition of Delphi is being used.       *)
(* SYN_DELPHI_2         : Delphi 2 is being used.                             *)
(* SYN_DELPHI_2_UP      : Delphi 2 or higher is being used.                   *)
(* SYN_DELPHI_3         : Delphi 3 is being used.                             *)
(* SYN_DELPHI_3_UP      : Delphi 3 or higher is being used.                   *)
(* SYN_DELPHI_4         : Delphi 4 is being used.                             *)
(* SYN_DELPHI_4_UP      : Delphi 4 or higher is being used.                   *)
(* SYN_DELPHI_5         : Delphi 5 is being used.                             *)
(* SYN_DELPHI_5_UP      : Delphi 5 or higher is being used.                   *)
(* SYN_DELPHI_6         : Delphi 6 is being used.                             *)
(* SYN_DELPHI_6_UP      : Delphi 6 or higher is being used.                   *)
(* SYN_DELPHI_7         : Delphi 7 is being used.                             *)
(* SYN_DELPHI_7_UP      : Delphi 7 or higher is being used.                   *)
(* SYN_DELPHI_8         : Delphi 8 is being used.                             *)
(* SYN_DELPHI_8_UP      : Delphi 8 or higher is being used.                   *)
(* SYN_DELPHI_2005      : Delphi 2005 is being used.                          *)
(* SYN_DELPHI_2005_UP   : Delphi 2005 or higher is being used.                *)
(* SYN_DELPHI_2006      : Delphi 2006 is being used.                          *)
(* SYN_DELPHI_2006_UP   : Delphi 2006 or higher is being used.                *)
(* SYN_DELPHI_2007      : Delphi 2007 is being used.                          *)
(* SYN_DELPHI_2007_UP   : Delphi 2007 or higher is being used.                *)
(* SYN_DELPHI_2009      : Delphi 2009 is being used.                          *)
(* SYN_DELPHI_2009_UP   : Delphi 2009 or higher is being used.                *)
(* SYN_DELPHI_2010      : Delphi 2010 is being used.                          *)
(* SYN_DELPHI_2010_UP   : Delphi 2010 or higher is being used.                *)
(* SYN_DELPHI_XE        : Delphi XE is being used.                            *)
(* SYN_DELPHI_XE_UP     : Delphi XE or higher is being used.                  *)
(* SYN_DELPHI_XE2       : Delphi XE2 is being used.                           *)
(* SYN_DELPHI_XE2_UP    : Delphi XE2 or higher is being used.                 *)
(* SYN_DELPHI_XE3       : Delphi XE3 is being used.                           *)
(* SYN_DELPHI_XE3_UP    : Delphi XE3 or higher is being used.                 *)
(* SYN_DELPHI_XE4       : Delphi XE4 is being used.                           *)
(* SYN_DELPHI_XE4_UP    : Delphi XE4 or higher is being used.                 *)
(* SYN_DELPHI_XE5       : Delphi XE5 is being used.                           *)
(* SYN_DELPHI_XE5_UP    : Delphi XE5 or higher is being used.                 *)
(* SYN_DELPHI_XE6       : Delphi XE6 is being used.                           *)
(* SYN_DELPHI_XE6_UP    : Delphi XE6 or higher is being used.                 *)
(* SYN_DELPHI_XE7       : Delphi XE7 is being used.                           *)
(* SYN_DELPHI_XE7_UP    : Delphi XE7 or higher is being used.                 *)
(* SYN_DELPHI_XE8       : Delphi XE8 is being used.                           *)
(* SYN_DELPHI_XE8_UP    : Delphi XE8 or higher is being used.                 *)
(* SYN_DELPHI_10        : Delphi RX 10 (Seattle) is being used.               *)
(* SYN_DELPHI_10_UP     : Delphi RX 10 or higher is being used.               *)
(* SYN_DELPHI_10_1      : Delphi RX 10.1 (Berlin) is being used.              *)
(* SYN_DELPHI_10_1_UP   : Delphi RX 10.1 or higher is being used.             *)
(* SYN_DELPHI_10_2      : Delphi RX 10.2 (Tokyo) is being used.               *)
(* SYN_DELPHI_10_2_UP   : Delphi RX 10.2 or higher is being used.             *)
(* SYN_DELPHI_10_3      : Delphi RX 10.3 (Rio) is being used.                 *)
(* SYN_DELPHI_10_3_UP   : Delphi RX 10.3 or higher is being used.             *)
(* SYN_KYLIX            : Kylix 1.0 is being using.                           *)
(******************************************************************************)

{$DEFINE SYNEDIT_INCLUDE}

{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{ (remove the dot in front of a define to enable it)                           }
{------------------------------------------------------------------------------}

{$B-,H+} // defaults are short evaluation of boolean values and long strings

{.$DEFINE SYN_DEVELOPMENT_CHECKS} // additional tests for debugging
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  {$R+,Q+,S+,T+}
{$ENDIF}

{------------------------------------------------------------------------------}
{ Pull in all defines from SynEditJedi.inc (must be done after the common      }
{ compiler defines to  work correctly). Use SynEdit-prefix to avoid problems   }
{ with other versions of jedi.inc in the search-path.                          }
{------------------------------------------------------------------------------}

{$I SynEditJedi.inc}

{------------------------------------------------------------------------------}
{ SYN_WIN32, SYN_LINUX and SYN_KYLIX defines                                   }
{------------------------------------------------------------------------------}

{$IFDEF WIN32}
  {$DEFINE SYN_WIN32}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE SYN_LINUX}
  {$DEFINE SYN_KYLIX}
  {$DEFINE SYN_COMPILER_6}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_6}
{$ENDIF}

{------------------------------------------------------------------------------}
{ VERXXX to SYN_COMPILERX, SYN_DELPHIX and SYN_CPPBX mappings                  }
{------------------------------------------------------------------------------}

{$IFDEF VER330}
  {$DEFINE SYN_COMPILER_26}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_10_3}
{$ENDIF}

{$IFDEF VER320}
  {$DEFINE SYN_COMPILER_25}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_10_2}
{$ENDIF}

{$IFDEF VER310}
  {$DEFINE SYN_COMPILER_24}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_10_1}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE SYN_COMPILER_23}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_10}
{$ENDIF}

{$IFDEF VER290}
  {$DEFINE SYN_COMPILER_22}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_XE8}
{$ENDIF}

{$IFDEF VER280}
  {$DEFINE SYN_COMPILER_21}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_XE7}
{$ENDIF}

{$IFDEF VER270}
  {$DEFINE SYN_COMPILER_20}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_XE6}
{$ENDIF}

{$IFDEF VER260}
  {$DEFINE SYN_COMPILER_19}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_XE5}
{$ENDIF}

{$IFDEF VER250}
  {$DEFINE SYN_COMPILER_18}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_XE4}
{$ENDIF}

{$IFDEF VER240}
  {$DEFINE SYN_COMPILER_17}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_XE3}
{$ENDIF}

{$IFDEF VER230}
  {$DEFINE SYN_COMPILER_16}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_XE2}
{$ENDIF}

{$IFDEF VER220}
  {$DEFINE SYN_COMPILER_15}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_XE}
{$ENDIF}

{$IFDEF VER210}
  {$DEFINE SYN_COMPILER_14}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_2010}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SYN_COMPILER_12}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_2009}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SYN_COMPILER_11}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_2007}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SYN_COMPILER_10}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_2006}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SYN_COMPILER_9}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_2005}
{$ENDIF}

{$IFDEF VER160}
  {$DEFINE SYN_COMPILER_8}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_8}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SYN_COMPILER_7}
  {$IFNDEF BCB}
    {$DEFINE SYN_DELPHI}
    {$DEFINE SYN_DELPHI_7}
  {$ELSE}
    {.$DEFINE SYN_CPPB}
    {.$DEFINE SYN_CPPB_7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SYN_COMPILER_6}
  {$IFNDEF BCB}
    {$DEFINE SYN_DELPHI}
    {$DEFINE SYN_DELPHI_6}
  {$ELSE}
    {$DEFINE SYN_CPPB}
    {$DEFINE SYN_CPPB_6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SYN_COMPILER_5}
  {$IFNDEF BCB}
    {$DEFINE SYN_DELPHI}
    {$DEFINE SYN_DELPHI_5}
  {$ELSE}
    {$DEFINE SYN_CPPB}
    {$DEFINE SYN_CPPB_5}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SYN_COMPILER_4}
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SYN_COMPILER_4}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_4}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SYN_COMPILER_3}
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE SYN_COMPILER_3}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_3}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE SYN_COMPILER_2}  { C++B v1 compiler is really v2 }
  {$DEFINE SYN_CPPB}
  {$DEFINE SYN_CPPB_1}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE SYN_COMPILER_2}
  {$DEFINE SYN_DELPHI}
  {$DEFINE SYN_DELPHI_2}
{$ENDIF}

{$IFDEF SYN_COMPILER_2}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_3}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_4}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_5}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_6}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_7}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_8}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_9}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_10}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_11}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_12}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_14}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_15}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_16}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_17}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_18}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_19}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
  {$DEFINE SYN_COMPILER_19_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_20}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
  {$DEFINE SYN_COMPILER_19_UP}
  {$DEFINE SYN_COMPILER_20_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_21}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
  {$DEFINE SYN_COMPILER_19_UP}
  {$DEFINE SYN_COMPILER_20_UP}
  {$DEFINE SYN_COMPILER_21_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_22}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
  {$DEFINE SYN_COMPILER_19_UP}
  {$DEFINE SYN_COMPILER_20_UP}
  {$DEFINE SYN_COMPILER_21_UP}
  {$DEFINE SYN_COMPILER_22_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_23}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
  {$DEFINE SYN_COMPILER_19_UP}
  {$DEFINE SYN_COMPILER_20_UP}
  {$DEFINE SYN_COMPILER_21_UP}
  {$DEFINE SYN_COMPILER_22_UP}
  {$DEFINE SYN_COMPILER_23_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_24}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
  {$DEFINE SYN_COMPILER_19_UP}
  {$DEFINE SYN_COMPILER_20_UP}
  {$DEFINE SYN_COMPILER_21_UP}
  {$DEFINE SYN_COMPILER_22_UP}
  {$DEFINE SYN_COMPILER_23_UP}
  {$DEFINE SYN_COMPILER_24_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_25}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
  {$DEFINE SYN_COMPILER_19_UP}
  {$DEFINE SYN_COMPILER_20_UP}
  {$DEFINE SYN_COMPILER_21_UP}
  {$DEFINE SYN_COMPILER_22_UP}
  {$DEFINE SYN_COMPILER_23_UP}
  {$DEFINE SYN_COMPILER_24_UP}
  {$DEFINE SYN_COMPILER_25_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_26}
  {$DEFINE SYN_COMPILER_1_UP}
  {$DEFINE SYN_COMPILER_2_UP}
  {$DEFINE SYN_COMPILER_3_UP}
  {$DEFINE SYN_COMPILER_4_UP}
  {$DEFINE SYN_COMPILER_5_UP}
  {$DEFINE SYN_COMPILER_6_UP}
  {$DEFINE SYN_COMPILER_7_UP}
  {$DEFINE SYN_COMPILER_8_UP}
  {$DEFINE SYN_COMPILER_9_UP}
  {$DEFINE SYN_COMPILER_10_UP}
  {$DEFINE SYN_COMPILER_11_UP}
  {$DEFINE SYN_COMPILER_12_UP}
  {$DEFINE SYN_COMPILER_14_UP}
  {$DEFINE SYN_COMPILER_15_UP}
  {$DEFINE SYN_COMPILER_16_UP}
  {$DEFINE SYN_COMPILER_17_UP}
  {$DEFINE SYN_COMPILER_18_UP}
  {$DEFINE SYN_COMPILER_19_UP}
  {$DEFINE SYN_COMPILER_20_UP}
  {$DEFINE SYN_COMPILER_21_UP}
  {$DEFINE SYN_COMPILER_22_UP}
  {$DEFINE SYN_COMPILER_23_UP}
  {$DEFINE SYN_COMPILER_24_UP}
  {$DEFINE SYN_COMPILER_25_UP}
  {$DEFINE SYN_COMPILER_26_UP}
{$ENDIF}


{$IFDEF SYN_DELPHI_2}
  {$DEFINE SYN_DELPHI_2_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_3}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_4}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_5}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_6}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_7}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_8}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_2005}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_2006}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_2007}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_2009}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_2010}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_XE}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_XE2}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_XE3}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_XE4}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_XE5}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
  {$DEFINE SYN_DELPHI_XE5_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_XE6}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
  {$DEFINE SYN_DELPHI_XE5_UP}
  {$DEFINE SYN_DELPHI_XE6_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_XE7}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
  {$DEFINE SYN_DELPHI_XE5_UP}
  {$DEFINE SYN_DELPHI_XE6_UP}
  {$DEFINE SYN_DELPHI_XE7_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_XE8}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
  {$DEFINE SYN_DELPHI_XE5_UP}
  {$DEFINE SYN_DELPHI_XE6_UP}
  {$DEFINE SYN_DELPHI_XE7_UP}
  {$DEFINE SYN_DELPHI_XE8_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_10}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
  {$DEFINE SYN_DELPHI_XE5_UP}
  {$DEFINE SYN_DELPHI_XE6_UP}
  {$DEFINE SYN_DELPHI_XE7_UP}
  {$DEFINE SYN_DELPHI_XE8_UP}
  {$DEFINE SYN_DELPHI_10_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_10_1}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
  {$DEFINE SYN_DELPHI_XE5_UP}
  {$DEFINE SYN_DELPHI_XE6_UP}
  {$DEFINE SYN_DELPHI_XE7_UP}
  {$DEFINE SYN_DELPHI_XE8_UP}
  {$DEFINE SYN_DELPHI_10_1_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_10_2}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
  {$DEFINE SYN_DELPHI_XE5_UP}
  {$DEFINE SYN_DELPHI_XE6_UP}
  {$DEFINE SYN_DELPHI_XE7_UP}
  {$DEFINE SYN_DELPHI_XE8_UP}
  {$DEFINE SYN_DELPHI_10_1_UP}
  {$DEFINE SYN_DELPHI_10_2_UP}
{$ENDIF}

{$IFDEF SYN_DELPHI_10_3}
  {$DEFINE SYN_DELPHI_2_UP}
  {$DEFINE SYN_DELPHI_3_UP}
  {$DEFINE SYN_DELPHI_4_UP}
  {$DEFINE SYN_DELPHI_5_UP}
  {$DEFINE SYN_DELPHI_6_UP}
  {$DEFINE SYN_DELPHI_7_UP}
  {$DEFINE SYN_DELPHI_8_UP}
  {$DEFINE SYN_DELPHI_2005_UP}
  {$DEFINE SYN_DELPHI_2006_UP}
  {$DEFINE SYN_DELPHI_2007_UP}
  {$DEFINE SYN_DELPHI_2009_UP}
  {$DEFINE SYN_DELPHI_2010_UP}
  {$DEFINE SYN_DELPHI_XE_UP}
  {$DEFINE SYN_DELPHI_XE2_UP}
  {$DEFINE SYN_DELPHI_XE3_UP}
  {$DEFINE SYN_DELPHI_XE4_UP}
  {$DEFINE SYN_DELPHI_XE5_UP}
  {$DEFINE SYN_DELPHI_XE6_UP}
  {$DEFINE SYN_DELPHI_XE7_UP}
  {$DEFINE SYN_DELPHI_XE8_UP}
  {$DEFINE SYN_DELPHI_10_1_UP}
  {$DEFINE SYN_DELPHI_10_2_UP}
  {$DEFINE SYN_DELPHI_10_3_UP}
{$ENDIF}

{$IFDEF SYN_CPPB_6}
  {$DEFINE SYN_CPPB_3_UP}
  {$DEFINE SYN_CPPB_4_UP}
  {$DEFINE SYN_CPPB_5_UP}
  {$DEFINE SYN_CPPB_6_UP}
{$ENDIF}

{$IFDEF SYN_CPPB_3}
  {$DEFINE SYN_CPPB_3_UP}
{$ENDIF}

{$IFDEF SYN_COMPILER_3_UP}
  {$DEFINE SYN_NO_COM_CLEANUP}
{$ENDIF}

{$IFDEF SYN_CPPB_3_UP}
  // C++Builder requires this if you use Delphi components in run-time packages.
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF SYN_KYLIX}
  Kylix is no longer supported. Please refer to the branch 'Kylix'
{$ELSE}
  {$DEFINE SYN_WIN32}
{$ENDIF}

{------------------------------------------------------------------------------}
{  Please change this to suit your needs (to activate an option remove the dot }
{  in front of a DEFINE)                                                       }
{------------------------------------------------------------------------------}

// "Heredoc" syntax highlighting
// If you enable the following statement and use highlighter(s) that have
// support for "Heredoc" strings as scheme(s) in SynMultiSyn, you must
// implement your own SynMultiSyn OnCustomRange event handler in order to
// properly store Range State information
{.$DEFINE SYN_HEREDOC}

// Define OWN_UnicodeString_MEMMGR to speed up WideStrings-handling
{$IFDEF SYN_WIN32}
  {$IFNDEF UNICODE}
    {$DEFINE OWN_UnicodeString_MEMMGR}
  {$ENDIF}
{$ENDIF}

// Turn this off if you don't need complex script support, since it is slower
{-$DEFINE SYN_UNISCRIBE}

// Code Folding
{$IFDEF SYN_DELPHI_XE_UP}
  {$DEFINE SYN_CodeFolding}
{$ENDIF}



