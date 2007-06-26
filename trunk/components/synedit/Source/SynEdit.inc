{$I ../../compilerdetection/Compilers.inc}

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
(* SYN_KYLIX            : Kylix 1.0 is being using.                           *)
(* SYN_CLX              : A CLX application is being created.                 *)
(******************************************************************************)

{$DEFINE SYNEDIT_INCLUDE}

{$IFDEF WIN32}           {$DEFINE SYN_WIN32}           {$ENDIF}
{$IFDEF LINUX}           {$DEFINE SYN_KYLIX}           {$ENDIF}
{$IFDEF LINUX}           {$DEFINE SYN_LINUX}           {$ENDIF}

{$IFDEF COMPILER_2}      {$DEFINE SYN_COMPILER_2}      {$ENDIF}
{$IFDEF COMPILER_2_UP}   {$DEFINE SYN_COMPILER_2_UP}   {$ENDIF}
{$IFDEF COMPILER_3}      {$DEFINE SYN_COMPILER_3}      {$ENDIF}
{$IFDEF COMPILER_3_UP}   {$DEFINE SYN_COMPILER_3_UP}   {$ENDIF}
{$IFDEF COMPILER_4}      {$DEFINE SYN_COMPILER_4}      {$ENDIF}
{$IFDEF COMPILER_4_UP}   {$DEFINE SYN_COMPILER_4_UP}   {$ENDIF}
{$IFDEF COMPILER_5}      {$DEFINE SYN_COMPILER_5}      {$ENDIF}
{$IFDEF COMPILER_5_UP}   {$DEFINE SYN_COMPILER_5_UP}   {$ENDIF}
{$IFDEF COMPILER_6}      {$DEFINE SYN_COMPILER_6}      {$ENDIF}
{$IFDEF COMPILER_6_UP}   {$DEFINE SYN_COMPILER_6_UP}   {$ENDIF}
{$IFDEF COMPILER_7}      {$DEFINE SYN_COMPILER_7}      {$ENDIF}
{$IFDEF COMPILER_7_UP}   {$DEFINE SYN_COMPILER_7_UP}   {$ENDIF}
{$IFDEF COMPILER_8}      {$DEFINE SYN_COMPILER_8}      {$ENDIF}
{$IFDEF COMPILER_8_UP}   {$DEFINE SYN_COMPILER_8_UP}   {$ENDIF}
{$IFDEF COMPILER_9}      {$DEFINE SYN_COMPILER_9}      {$ENDIF}
{$IFDEF COMPILER_9_UP}   {$DEFINE SYN_COMPILER_9_UP}   {$ENDIF}
{$IFDEF COMPILER_10}     {$DEFINE SYN_COMPILER_10}     {$ENDIF}
{$IFDEF COMPILER_10_UP}  {$DEFINE SYN_COMPILER_10_UP}  {$ENDIF}

{$IFDEF CPPB}            {$DEFINE SYN_CPPB}            {$ENDIF}

{$IFDEF CPPB_1}          {$DEFINE SYN_CPPB_1}          {$ENDIF}
{$IFDEF CPPB_3}          {$DEFINE SYN_CPPB_3}          {$ENDIF}
{$IFDEF CPPB_3_UP}       {$DEFINE SYN_CPPB_3_UP}       {$ENDIF}
{$IFDEF CPPB_4}          {$DEFINE SYN_CPPB_4}          {$ENDIF}
{$IFDEF CPPB_4_UP}       {$DEFINE SYN_CPPB_4_UP}       {$ENDIF}
{$IFDEF CPPB_5}          {$DEFINE SYN_CPPB_5}          {$ENDIF}
{$IFDEF CPPB_5_UP}       {$DEFINE SYN_CPPB_5_UP}       {$ENDIF}
{$IFDEF CPPB_6}          {$DEFINE SYN_CPPB_6}          {$ENDIF}
{$IFDEF CPPB_6_UP}       {$DEFINE SYN_CPPB_6_UP}       {$ENDIF}

{$IFDEF DELPHI}          {$DEFINE SYN_DELPHI}          {$ENDIF}

{$IFDEF DELPHI_2}        {$DEFINE SYN_DELPHI_2}        {$ENDIF}
{$IFDEF DELPHI_2_UP}     {$DEFINE SYN_DELPHI_2_UP}     {$ENDIF}
{$IFDEF DELPHI_3}        {$DEFINE SYN_DELPHI_3}        {$ENDIF}
{$IFDEF DELPHI_3_UP}     {$DEFINE SYN_DELPHI_3_UP}     {$ENDIF}
{$IFDEF DELPHI_4}        {$DEFINE SYN_DELPHI_4}        {$ENDIF}
{$IFDEF DELPHI_4_UP}     {$DEFINE SYN_DELPHI_4_UP}     {$ENDIF}
{$IFDEF DELPHI_5}        {$DEFINE SYN_DELPHI_5}        {$ENDIF}
{$IFDEF DELPHI_5_UP}     {$DEFINE SYN_DELPHI_5_UP}     {$ENDIF}
{$IFDEF DELPHI_6}        {$DEFINE SYN_DELPHI_6}        {$ENDIF}
{$IFDEF DELPHI_6_UP}     {$DEFINE SYN_DELPHI_6_UP}     {$ENDIF}
{$IFDEF DELPHI_7}        {$DEFINE SYN_DELPHI_7}        {$ENDIF}
{$IFDEF DELPHI_7_UP}     {$DEFINE SYN_DELPHI_7_UP}     {$ENDIF}
{$IFDEF DELPHI_8}        {$DEFINE SYN_DELPHI_8}        {$ENDIF}
{$IFDEF DELPHI_8_UP}     {$DEFINE SYN_DELPHI_8_UP}     {$ENDIF}
{$IFDEF DELPHI_9}        {$DEFINE SYN_DELPHI_2005}     {$ENDIF}
{$IFDEF DELPHI_9_UP}     {$DEFINE SYN_DELPHI_2005_UP}  {$ENDIF}
{$IFDEF DELPHI_10}       {$DEFINE SYN_DELPHI_2006}     {$ENDIF}
{$IFDEF DELPHI_10_UP}    {$DEFINE SYN_DELPHI_2006_UP}  {$ENDIF}

{$IFDEF KYLIX}
  {$DEFINE SYN_KYLIX}
  {$DEFINE SYN_CLX}
{$ENDIF}


{------------------------------------------------------------------------------}
{ Common compiler defines                                                      }
{------------------------------------------------------------------------------}

// defaults are short evaluation of boolean values and long strings

{$B-,H+}

{------------------------------------------------------------------------------}
{ Please change this to suit your needs                                        }
{------------------------------------------------------------------------------}

// "Heredoc" syntax highlighting
// If you enable the following statement and use highlighter(s) that have
// support for "Heredoc" strings as scheme(s) in SynMultiSyn, you must
// implement your own SynMultiSyn OnCustomRange event handler in order to
// properly store Range State information
{.$DEFINE SYN_HEREDOC}

// support for multibyte character sets
{$IFDEF SYN_COMPILER_3_UP}

{$IFNDEF SYN_CLX}
{$DEFINE SYN_MBCSSUPPORT}
{$ENDIF}

{$ENDIF}

// additional tests for debugging

{.$DEFINE SYN_DEVELOPMENT_CHECKS}

{$IFDEF SYN_DEVELOPMENT_CHECKS}

{$R+,Q+,S+,T+}

{$ENDIF}

// $Id: SynEdit.inc,v 1.20 2005/12/31 07:34:36 skyweb Exp $
