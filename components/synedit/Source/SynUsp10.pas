{******************************************************************************}
{*                                                                            *}
{*  Copyright (c) Microsoft Corporation. All rights reserved.                 *}
{*                                                                            *}
{*  File:       usp10.h                                                       *}
{*  Content:    USP - Unicode Complex Script processor                        *}
{*                                                                            *}
{*  Delphi / FreePascal adaptation by Alexey Barkovoy (clootie@clootie.ru)    *}
(*                                                                            *)
{*  The original version from Alexey Barkovoy can be downloaded from:         *}
{*     http://clootie.ru                                                      *}
(*                                                                            *)
(*  Dynamic linking logic (similar to what the JCL does) by Maël Hörz.        *)
(*                                                                            *)
(*  Latest version can be downloaded from http://mh-nexus.de/unisynedit.htm   *)
(*  or checked out from the Unicode branch of SynEdit CVS.                    *) 
(*                                                                            *)
{******************************************************************************}
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

// $Id: SynUsp10.pas,v 1.1.2.2 2008/09/17 13:59:12 maelh Exp $

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}

// necessary for dynamic linking
{$STACKFRAMES ON}
{$WARNINGS OFF}

unit SynUsp10;

interface

uses
  Windows;

const
  ///// Uniscribe build number
  USPBUILD = 0400;



  /////   USP - Unicode Complex Script processor
  //
  //      Copyright (c) Microsoft Corporation. All rights reserved.




  /////   SCRIPT
  //
  //      The SCRIPT enum is an opaque type used internally to identify
  //      which shaping engine functions are used to process a given run.
  //
  //
  SCRIPT_UNDEFINED  = 0;
  //
  //p     SCRIPT_UNDEFINED: This is the only public script ordinal. May be
  //      forced into the eScript field of a SCRIPT_ANALYSIS to disable shaping.
  //      SCRIPT_UNDEFINED is supported by all fonts - ScriptShape will display
  //      whatever glyph is defined in the font CMAP table, or, if none, the
  //      missing glyph.



  /////   USP Status Codes
  //
  USP_E_SCRIPT_NOT_IN_FONT = DWord((SEVERITY_ERROR shl 31) or
    (FACILITY_ITF shl 16)) or $200; // MAKE_HRESULT(SEVERITY_ERROR,FACILITY_ITF,0x200)    // Script doesn't exist in font






  /////   SCRIPT_CACHE
  //
  //      Many script APIs take a combination of HDC and SCRIPT_CACHE parameter.
  //
  //      A SCRIPT_CACHE is an opaque pointer to a Uniscribe font metric cache
  //      structure.

type
  SCRIPT_CACHE = Pointer;
  {$EXTERNALSYM SCRIPT_CACHE}
  TScriptCache = SCRIPT_CACHE;
  PScriptCache = ^TScriptCache;


//      The client must allocate and retain one SCRIPT_CACHE variable for each
//      character style used. It must be initialised by the client to NULL.
//
//      APIs are passed an HDC and the address of a SCRIPT_CACHE variable.
//      Uniscribe will first attempt to access font data via the SCRIPT_CACHE
//      and will only inspect the HDC if the required data is not already
//      cached.
//
//      The HDC may be passed as NULL. If data required by Uniscribe is
//      already cached, the HDC won't be accessed and operation continues
//      normally.
//
//      If the HDC is passed as NULL, and Uniscribe needs to access it for
//      any reason, Uniscribe will return E_PENDING.
//
//      E_PENDING is returned quickly, allowing the client to avoid time
//      consuming SelectObject calls. The following example applies to all
//      APIs that take a SCRIPT_CACHE and an optional HDC.
//
//c     hr = ScriptShape(NULL, &sc, ..);
//c     if (hr == E_PENDING) {
//c         ... select font into hdc ...
//c         hr = ScriptShape(hdc, &sc, ...);
//c     }




/////   ScriptFreeCache
//
//      The client may free a SCRIPT_CACHE at any time. Uniscribe maintains
//      reference counts in it's font and shaper caches, and frees font data
//      only when all sizes of the font are free, and shaper data only when
//      all fonts it supports are freed.
//
//      The client should free the SCRIPT_CACHE for a style when it discards
//      that style.
//
//      ScriptFreeCache always sets it's parameter to NULL to help avoid
//      mis-referencing.


function ScriptFreeCache(
    psc: PScriptCache //InOut  Cache handle
 ): HRESULT; stdcall;


type
  /////   SCRIPT_CONTROL
  //
  //      The SCRIPT_CONTROL structure provides itemization control flags to the
  //      ScriptItemize function.
  //
  //

  TScriptControlFlag = (
    scContextDigits,       // Means use previous script instead of uDefaultLanguage
    // The following flags provide legacy support for GetCharacterPlacement features
    scInvertPreBoundDir,   // Reading order of virtual item immediately prior to string
    scInvertPostBoundDir,  // Reading order of virtual item immediately following string
    scLinkStringBefore,    // Equivalent to presence of ZWJ before string
    scLinkStringAfter,     // Equivalent to presence of ZWJ after string
    scNeutralOverride,     // Causes all neutrals to be strong in the current embedding direction
    scNumericOverride,     // Causes all numerals to be strong in the current embedding direction
    scLegacyBidiClass      // Causes plus and minus to be reated as neutrals, slash as a common separator
  );
  TScriptControlFlags = set of TScriptControlFlag;

  PScriptControl = ^TScriptControl;
  tag_SCRIPT_CONTROL = packed record
    uDefaultLanguage: Word;     // For NADS, also default for context
    fFlags: TScriptControlFlags;
    fReserved: Byte;
  end;
(*  uDefaultLanguage: DWORD    {:16}; // For NADS, also default for context
    fContextDigits: DWORD      {:1};  // Means use previous script instead of uDefaultLanguage

    // The following flags provide legacy support for GetCharacterPlacement features
    fInvertPreBoundDir: DWORD  {:1};  // Reading order of virtual item immediately prior to string
    fInvertPostBoundDir: DWORD {:1};  // Reading order of virtual item immediately following string
    fLinkStringBefore: DWORD   {:1};  // Equivalent to presence of ZWJ before string
    fLinkStringAfter: DWORD    {:1};  // Equivalent to presence of ZWJ after string
    fNeutralOverride: DWORD    {:1};  // Causes all neutrals to be strong in the current embedding direction
    fNumericOverride: DWORD    {:1};  // Causes all numerals to be strong in the current embedding direction
    fLegacyBidiClass: DWORD    {:1};  // Causes plus and minus to be reated as neutrals, slash as a common separator
    fReserved: DWORD           {:8};
  end; *)
  {$EXTERNALSYM tag_SCRIPT_CONTROL}
  SCRIPT_CONTROL = tag_SCRIPT_CONTROL;
  {$EXTERNALSYM SCRIPT_CONTROL}
  TScriptControl = tag_SCRIPT_CONTROL;

//
//
//p     uDefaultLanguage: Language to use when Unicode values are ambiguous.
//              Used by numeric processing to select digit shape when
//              ssDigitSubstitute (see SCRIPT_STATE) is in force.
//
//p     scContextDigits: Specifies that national digits are chosen according to
//              the nearest previous strong text, rather than using
//              uDefaultLanguage.
//
//p     scInvertPreBoundDir: By default text at the start of the string is
//              laid out as if it follows strong text of the same direction
//              as the base embedding level. Set scInvertPreBoundDir to change
//              the initial context to the opposite of the base embedding
//              level. This flag is for GetCharacterPlacement legacy support.
//
//p     scInvertPostBoundDir: By default text at the end of the string is
//              laid out as if it preceeds strong text of the same direction
//              as the base embedding level. Set scInvertPostBoundDir to change
//              the final context to the opposite of the base embedding
//              level. This flag is for GetCharacterPlacement legacy support.
//
//p     scLinkStringBefore: Causes the first character of the string to be
//              shaped as if were joined to a previous character.
//
//p     scLinkStringAfter: Causes the last character of the string to be
//              shaped as if were joined to a following character.
//
//p     scNeutralOverride: Causes all neutral characters in the string to be
//              treated as if they were strong characters of their enclosing
//              embedding level. This effectively locks neutrals in place,
//              reordering occuring only between neutrals.
//
//p     scNumericOverride: Causes all numeric characters in the string to be
//              treated as if they were strong characters of their enclosing
//              embedding level. This effectively locks numerics in place,
//              reordering occuring only between numerics.
//
//p     fReserved: Reserved. Always initialise to 0.



  /////   SCRIPT_STATE
  //
  //      The SCRIPT_STATE structure is used both to initialise the unicode
  //      algorithm state as an input parameter to ScriptItemize, and is also
  //      a component of each item analysis returned by ScriptItemize.
  //
  //
  TScriptStateFlag = (
    uBidiLevel_reserved1, uBidiLevel_r2, uBidiLevel_r3, uBidiLevel_r4, uBidiLevel_r5,
    ssOverrideDirection, // Set when in LRO/RLO embedding
    ssInhibitSymSwap,    // Set by U+206A (ISS), cleared by U+206B (ASS)
    ssCharShape,         // Set by U+206D (AAFS), cleared by U+206C (IAFS)
    ssDigitSubstitute,   // Set by U+206E (NADS), cleared by U+206F (NODS)
    ssInhibitLigate,     // Equiv !GCP_Ligate, no Unicode control chars yet
    ssDisplayZWG,        // Equiv GCP_DisplayZWG, no Unicode control characters yet
    ssArabicNumContext,  // For EN->AN Unicode rule
    ssGcpClusters        // For Generating Backward Compatible GCP Clusters (legacy Apps)
  );
  TScriptStateFlags = set of TScriptStateFlag;

  PScriptState = ^TScriptState;
  tag_SCRIPT_STATE = packed record
   case Byte of
    0: (uBidiLevel: Byte)    {:5};  // Unicode Bidi algorithm embedding level (0-16)
    1: (fFlags: TScriptStateFlags)
  end;
(*  uBidiLevel: Word         {:5};  // Unicode Bidi algorithm embedding level (0-16)
    fOverrideDirection: Word {:1};  // Set when in LRO/RLO embedding
    fInhibitSymSwap: Word    {:1};  // Set by U+206A (ISS), cleared by U+206B (ASS)
    fCharShape: Word         {:1};  // Set by U+206D (AAFS), cleared by U+206C (IAFS)
    fDigitSubstitute: Word   {:1};  // Set by U+206E (NADS), cleared by U+206F (NODS)
    fInhibitLigate: Word     {:1};  // Equiv !GCP_Ligate, no Unicode control chars yet
    fDisplayZWG: Word        {:1};  // Equiv GCP_DisplayZWG, no Unicode control characters yet
    fArabicNumContext: Word  {:1};  // For EN->AN Unicode rule
    fcpClusters: Word       {:1};  // For Generating Backward Compatible GCP Clusters (legacy Apps)
    fReserved: Word          {:1};
    fEngineReserved: Word    {:2};  // For use by shaping engine
  end; *)
  {$EXTERNALSYM tag_SCRIPT_STATE}
  SCRIPT_STATE = tag_SCRIPT_STATE;
  {$EXTERNALSYM SCRIPT_STATE}
  TScriptState = tag_SCRIPT_STATE;

const
  MASK_uBidiLevel = $1F; // Mask to apply to TScriptState.uBidiLevel
type

//
//
//p     uBidiLevel: The embedding level associated with all characters in this
//              run according to the Unicode bidi algorithm. When passed to
//              ScriptItemize, should be initialised to 0 for an LTR base
//              embedding level, or 1 for RTL.
//
//p     ssOverrideDirection: TRUE if this level is an override level (LRO/RLO).
//              In an override level, characters are layed out purely
//              left to right, or purely right to left. No reordering of digits
//              or strong characters of opposing direction takes place.
//              Note that this initial value is reset by LRE, RLE, LRO or
//              RLO codes in the string.
//
//p     ssInhibitSymSwap: TRUE if the shaping engine is to bypass mirroring of
//              Unicode Mirrored glyphs such as brackets. Set by Unicode
//              character ISS, cleared by ASS.
//
//p     ssCharShape: TRUE if character codes in the Arabic Presentation Forms
//              areas of Unicode should be shaped. (Not implemented).
//
//p     ssDigitSubstitute: TRUE if character codes U+0030 through U+0039
//              (European digits) are to be substituted by national digits.
//              Set by Unicode NADS, Cleared by NODS.
//
//p     ssInhibitLigate: TRUE if ligatures are not to be used in the shaping
//              of Arabic or Hebrew characters.
//
//p     ssDisplayZWG: TRUE if control characters are to be shaped as
//              representational glyphs. (Normally, control characters are
//              shaped to the blank glyph and given a width of zero).
//
//p     ssArabicNumContext: TRUE indicates prior strong characters were Arabic
//              for the purposes of rule P0 on page 3-19 of 'The Unicode
//              Standard, version 2.0'. Should normally be set TRUE before
//              itemizing an RTL paragraph in an Arabic language, FALSE
//              otherwise.
//
//p     ssGcpClusters: For GetCharaterPlacement legacy support only.
//              Initialise to TRUE to request ScriptShape to generate
//              the LogClust array the same way as GetCharacterPlacement
//              does in Arabic and Hebrew Windows95. Affects only Arabic
//              and Hebrew items.
//
//p     fReserved: Reserved. Always initialise to 0.
//
//p     fEngineReserved: Reserved. Always initialise to 0.






  /////   SCRIPT_ANALYSIS
  //
  //      Each analysed item is described by a SCRIPT_ANALYSIS structure.
  //      It also includes a copy of the Unicode algorithm state (SCRIPT_STATE).
  //
  //
  TScriptAnalysis_enum = (
    eScript_r1, eScript_r2, eScript_r3, eScript_r4, eScript_r5,  // first 10 bits
    eScript_r6, eScript_r7, eScript_r8, eScript_r9, eScript_r10, // are reserved
    fRTL,             // Rendering direction
    fLayoutRTL,       // Set for GCP classes ARABIC/HEBREW and LOCALNUMBER
    fLinkBefore,      // Implies there was a ZWJ before this item
    fLinkAfter,       // Implies there is a ZWJ following this item.
    fLogicalOrder,    // Set by client as input to ScriptShape/Place
    fNoGlyphIndex     // Generated by ScriptShape/Place - this item does not use glyph indices
  );
  TScriptAnalysis_set = set of TScriptAnalysis_enum;

  PScriptAnalysis = ^TScriptAnalysis;
  tag_SCRIPT_ANALYSIS = packed record
   case Byte of
    0: (eScript: Word)   {:10};    // Shaping engine
    1: (fFlags: TScriptAnalysis_set;
        s: TScriptState)
  end;
(*  eScript: Word         {:10};    // Shaping engine
    fRTL: Word            {:1};     // Rendering direction
    fLayoutRTL: Word      {:1};     // Set for GCP classes ARABIC/HEBREW and LOCALNUMBER
    fLinkBefore: Word     {:1};     // Implies there was a ZWJ before this item
    fLinkAfter: Word      {:1};     // Implies there is a ZWJ following this item.
    fLogicalOrder: Word   {:1};     // Set by client as input to ScriptShape/Place
    fNoGlyphIndex: Word   {:1};     // Generated by ScriptShape/Place - this item does not use glyph indices
    s: TScriptState;
  end; *)
  {$EXTERNALSYM tag_SCRIPT_ANALYSIS}
  SCRIPT_ANALYSIS = tag_SCRIPT_ANALYSIS;
  {$EXTERNALSYM SCRIPT_ANALYSIS}
  TScriptAnalysis = tag_SCRIPT_ANALYSIS;

const
  MASK_eScript = $3FF; // Mask to apply to TScriptAnalysis.eScript
type

//
//
//p     eScript: Opaque value identifying which engine Uniscribe will use to
//              Shape, Place and TextOut this item. The value of eScript is
//              undefined, and will change in future releases, but attributes
//              of eScript may be obtained by calling ScriptGetProperties.
//
//p     fRTL: Rendering direction. Normally identical to the parity of the
//              Unicode embedding level, but may differ if overridden by
//              GetCharacterPlacement legacy support.
//
//p     fLayoutRTL: Logical direction - whether conceptually part of a
//              left-to-right sequenece or a right-to-left sequence. Although
//              this is usually the same as fRTL, for a number in a
//              right-to-left run, fRTL is False (because digits are always
//              displayed LTR), but fLayoutRTL is True (because the number is
//              read as part of the right-to-left sequence).
//
//p     fLinkBefore: If set, the shaping engine will shape the first character
//              of this item as if it were joining with a previous character.
//              Set by ScriptItemize, may be overriden before calling ScriptShape.
//
//p     fLinkAfter: If set, the shaping engine will shape the last character
//              of this item as if it were joining with a subsequient character.
//              Set by ScriptItemize, may be overriden before calling ScriptShape.
//
//p     fLogicalOrder: If set, the shaping engine will generate all glyph
//              related arrays in logical order. By default glyph related
//              arrays are in visual order, the first array entry corresponding
//              to the leftmost glyph.
//              Set to FALSE by ScriptItemize, may be overriden before calling
//              ScriptShape.
//
//p     fNoGlyphIndex: May be set TRUE on input to ScriptShape to disable use
//              of glyphs for this item. Additionally, ScriptShape will set it
//              TRUE for hdcs containing symbolic, unrecognised and device fonts.
//              Disabling glyphing disables complex script shaping. When set,
//              shaping and placing for this item is implemented directly by
//              calls to GetTextExtentExPoint and ExtTextOut.
/////   SCRIPT_ITEM
//
//      The SCRIPT_ITEM structure includes a SCRIPT_ANALYSIS with the string
//      ofset of the first character of the item.
//
//

  PScriptItem = ^TScriptItem;
  tag_SCRIPT_ITEM = record
    iCharPos: Integer;      // Logical offset to first character in this item
    a: TScriptAnalysis;
  end;
  {$EXTERNALSYM tag_SCRIPT_ITEM}
  SCRIPT_ITEM = tag_SCRIPT_ITEM;
  {$EXTERNALSYM SCRIPT_ITEM}
  TScriptItem = SCRIPT_ITEM;

//
//
//p     iCharPos: Offset from beginning of itemised string to first character
//              of this item, counted in Unicode codepoints (i.e. words).
//
//p     a: Script analysis structure containing analysis specific to this
//              item, to be passed to ScriptShape, ScriptPlace etc.



/////   ScriptItemize - break text into items
//
//      Breaks a run of unicode into individually shapeable items.
//      Items are delimited by
//
//      o Change of shaping engine
//      o Change of direction
//
//      The client may create multiple runs from each item returned by
//      ScriptItemize, but should not combine multiple items into a single run.
//
//      Later the client will call ScriptShape for each run (when measuring or
//      rendering), and must pass the SCRIPT_ANALYSIS that ScriptItemize
//      returned.


function ScriptItemize(
    const pwcInChars: PWideChar;     // In   Unicode string to be itemized
    cInChars: Integer;               // In   Codepoint count to itemize
    cMaxItems: Integer;              // In   Max length of itemization array
    const psControl: PScriptControl; // In   Analysis control (optional)
    const psState: PScriptState;     // In   Initial bidi algorithm state (optional)
    pItems: PScriptItem;             // Out  Array to receive itemization
    pcItems: PInteger                // Out  Count of items processed (optional)
 ): HRESULT; stdcall;
{$EXTERNALSYM ScriptItemize}



/////
//
//
//      Returns E_INVALIDARG if pwcInChars == NULL or cInChars == 0
//          or pItems == NULL or cMaxItems < 2.
//
//      Returns E_OUTOFMEMORY if the output buffer length (cMaxItems) is
//          insufficient. Note that in this case, as in all error cases, no
//          items have been fully processed so no part of the output array
//          contains defined values.
//
//      If psControl and psState are NULL on entry, ScriptItemize
//      breaks the unicode string purely by character code.  If they are all
//      non-null, it performs a full Unicode bidi analysis.
//
//      ScriptItemize always adds a terminal item to the item analysis array
//      (pItems) such that the length of an item at pItem is always available as:
//
//c     pItem[1].iCharPos - pItem[0].iCharPos
//
//      For this reason, it is invalid to call ScriptItemize with a buffer
//      of less than two SCRIPT_ANALYSIS items.
//
//      To perform a correct Unicode Bidi analysis, the SCRIPT_STATE should
//      be initialised according to the paragraph reading order at paragraph
//      start, and ScriptItemize should be passed the whole paragraph.
//
//      fRTL and fNumeric together provide the same classification as
//      the lpClass output from GetCharacterPlacement.
//
//      European digits U+0030 through U+0039 may be rendered as national
//      digits as follows:
//
//t     ssDigitSubstitute | scContextDigits | Digit shapes displayed for Unicode U+0030 through U+0039
//t     ---------------- | -------------- | ------------------------------------
//t     False            | Any            | Western (European / American) digits
//t     True             | False          | As specified in SCRIPT_CONTROL.uDefaultLanguage
//t     True             | True           | As prior strong text, defaulting to SCRIPT_CONTROL.uDefaultLanguage
//
//
//      For scContextDigits, any Western digits (U+0030 - U+0039) encountered
//      before the first strongly directed character are substituted by the
//      traditional digits of the SCRIPT_CONTROL.uDefaultLanguage when that
//      language is written in the same direction as SCRIPT_STATE.uBidiLevel.
//
//      Thus, in a right-to-left string, if SCRIPT_CONTROL.uDefaultLanguage is
//      1 (LANG_ARABIC), then leading Western digits will be substituted by
//      traditional Arabic digits.
//
//      However, also in a right-to-left string, if SCRIPT_CONTROL.uDefaultLanguage
//      is 0x1e (LANG_THAI), then no substitution occurs on leading Western
//      digits because the Thai language is written left-to-right.
//
//      Following strongly directed characters, digits are substituted
//      by the traditional digits associated with the closest prior strongly
//      directed character.
//
//      The left-to-right mark (LRM) and right-to-left mark (RLM) are strong
//      characters whose language depends on the SCRIPT_CONTROL.uDefaultLangauge.
//
//      If SCRIPT_CONTROL.uDefaultLangauge is a left-to-right langauge, then
//      LRM causes subsequent Western digits to be substituted by the
//      traditional digits associated with that language, while Western
//      digits following RLM are not substituted.
//
//      Conversly, if SCRIPT_CONTROL.uDefaultLangauge is a right-to-left
//      langauge, then Western digits following LRM are not substituted, while
//      Western digits following RLM are substituted by the traditional digits
//      associated with that language.
//
//
//
//      Effect of Unicode control characters on SCRIPT_STATE:
//
//t     SCRIPT_STATE flag | Set by | Cleared by
//t     ----------------- | ------   ----------
//t     ssDigitSubstitute  |  NADS  |   NODS
//t     ssInhibitSymSwap   |  ISS   |   ASS
//t     ssCharShape        |  AAFS  |   IAFS
//
//      SCRIPT_STATE.ssArabicNumContext controls the Unicode EN->AN rule.
//      It should normally be initialised to TRUE
//      before itemizing an RTL paragraph in an Arabic language, FALSE
//      otherwise.
/////   ScriptLayout
//
//      The ScriptLayout function converts an array of run embedding levels to
//      a map of visual to logical position, and/or logical to visual position.
//
//      pbLevel must contain the embedding levels for all runs on the line,
//      ordered logically.
//
//      On output, piVisualToLogical[0] is the logical index of the run to
//      display at the far left. Subsequent entries should be displayed
//      progressing from left to right.
//
//      piLogicalToVisual[0] is the relative visual position where the first
//      logical run should be displayed - the leftmost display position being zero.
//
//      The caller may request either piLogicalToVisual or piVisualToLogical
//      or both.
//
//      Note: No other input is required since the embedding levels give all
//      necessary information for layout.


function ScriptLayout(
    cRuns: Integer;               // In   Number of runs to process
    const pbLevel: PByte;         // In   Array of run embedding levels
    piVisualToLogical: PInteger;  // Out  List of run indices in visual order
    piLogicalToVisual: PInteger   // Out  List of visual run positions
 ): HRESULT; stdcall;
{$EXTERNALSYM ScriptLayout}





type

  /////   SCRIPT_JUSTIFY
  //
  //      The script justification enumeration provides the client with the
  //      glyph characteristic information it needs to implement justification.

  PScriptJustify = ^TScriptJustify;
  tag_SCRIPT_JUSTIFY = (
    SCRIPT_JUSTIFY_NONE           {= 0},   // Justification can't be applied at this glyph
    SCRIPT_JUSTIFY_ARABIC_BLANK   {= 1},   // This glyph represents a blank in an Arabic run
    SCRIPT_JUSTIFY_CHARACTER      {= 2},   // Inter-character justification point follows this glyph
    SCRIPT_JUSTIFY_RESERVED1      {= 3},   // Reserved #1
    SCRIPT_JUSTIFY_BLANK          {= 4},   // This glyph represents a blank outside an Arabic run
    SCRIPT_JUSTIFY_RESERVED2      {= 5},   // Reserved #2
    SCRIPT_JUSTIFY_RESERVED3      {= 6},   // Reserved #3
    SCRIPT_JUSTIFY_ARABIC_NORMAL  {= 7},   // Normal Middle-Of-Word glyph that connects to the right (begin)
    SCRIPT_JUSTIFY_ARABIC_KASHIDA {= 8},   // Kashida(U+640) in middle of word
    SCRIPT_JUSTIFY_ARABIC_ALEF    {= 9},   // Final form of Alef-like (U+627, U+625, U+623, U+632)
    SCRIPT_JUSTIFY_ARABIC_HA      {= 10},  // Final form of Ha (U+647)
    SCRIPT_JUSTIFY_ARABIC_RA      {= 11},  // Final form of Ra (U+631)
    SCRIPT_JUSTIFY_ARABIC_BA      {= 12},  // Middle-Of-Word form of Ba (U+628)
    SCRIPT_JUSTIFY_ARABIC_BARA    {= 13},  // Ligature of alike (U+628,U+631)
    SCRIPT_JUSTIFY_ARABIC_SEEN    {= 14},  // Highest priority: Initial shape of Seen(U+633) (end)
    SCRIPT_JUSTIFY_RESERVED4      {= 15}   // Reserved #4
  );
  SCRIPT_JUSTIFY = tag_SCRIPT_JUSTIFY;
  {$EXTERNALSYM SCRIPT_JUSTIFY}
  TScriptJustify = SCRIPT_JUSTIFY;



  /////   SCRIPT_VISATTR
  //
  //      The visual (glyph) attribute buffer generated by ScriptShape
  //      identifies clusters and justification points:

  TScriptVisAttr_enum = (
    uJustification_r1, uJustification_r2, uJustification_r3, uJustification_r4,
    fClusterStart,     {:1}  // First glyph of representation of cluster
    fDiacritic,        {:1}  // Diacritic
    fZeroWidth,        {:1}  // Blank, ZWJ, ZWNJ etc, with no width
    fReserved          {:1}  // General reserved
  );
  TScriptVisAttr_set = set of TScriptVisAttr_enum;

  PScriptVisAttr = ^TScriptVisAttr;
  tag_SCRIPT_VISATTR = packed record
   case Byte of
    0: (uJustification: Byte) {:4};  // Justification class
    1: (fFlags: TScriptVisAttr_set;
        fShapeReserved: Byte) {:8};  // Reserved for use by shaping engines
  end;
(*  uJustification: Word   {:4};  // Justification class
    fClusterStart: Word    {:1};  // First glyph of representation of cluster
    fDiacritic: Word       {:1};  // Diacritic
    fZeroWidth: Word       {:1};  // Blank, ZWJ, ZWNJ etc, with no width
    fReserved: Word        {:1};  // General reserved
    fShapeReserved: Word   {:8};  // Reserved for use by shaping engines
  end; *)
  SCRIPT_VISATTR = tag_SCRIPT_VISATTR;
  {$EXTERNALSYM SCRIPT_VISATTR}
  TScriptVisAttr = SCRIPT_VISATTR;

const
  MASK_uJustification = $F; // Mask to apply to TScriptVisAttr.uJustification

//
//
//p     uJustification: Justification class for this glyph. See SCRIPT_JUSTIFY.
//
//p     fClusterStart: Set for the logically first glyph in every cluster,
//          even for clusters containing just one glyph.
//
//p     fDiacritic: Set for glyphs that combine with base characters.
//
//p     fZeroWidth: Set by the shaping engine for some, but not all, zero
//          width characters.


/////   ScriptShape
//
//      The ScriptShape function takes a Unicode run and generates glyphs and
//      visual attributes.
//
//      The number of glyphs generated varies according to the script and the
//      font. Only for simple scripts and fonts does each Unicode code point
//      generates a single glyph.
//
//      There is no limit on the number of glyphs generated by a codepoint.
//      For example, a sophisticated complex script font might choose to
//      constuct characters from components, and so generate many times as
//      many glyphs as characters.
//
//      There are also special cases like invalid character representations,
//      where extra glyphs are added to represent the invalid sequence.
//
//      A reasonable guess might be to provide a glyph buffer 1.5 times the
//      length of the character buffer, plus a 16 glyph fixed addition for
//      rare cases like invalid sequenece representation.
//
//      If ScriptShape returns E_OUTOFMEMORY it will be necessary to recall
//      it, possibly more than once, until a large enough buffer is found.


function ScriptShape(
    hdc: HDC;                    // In    Optional (see under caching)
    psc: PScriptCache;           // InOut Cache handle
    const pwcChars: PWideChar;   // In    Logical unicode run
    cChars: Integer;             // In    Length of unicode run
    cMaxGlyphs: Integer;         // In    Max glyphs to generate
    psa: PScriptAnalysis;        // InOut Result of ScriptItemize (may have fNoGlyphIndex set)
    pwOutGlyphs: PWord;          // Out   Output glyph buffer
    pwLogClust: PWord;           // Out   Logical clusters
    psva: PScriptVisAttr;        // Out   Visual glyph attributes
    pcGlyphs: PInteger           // Out   Count of glyphs generated
 ): HRESULT; stdcall;
{$EXTERNALSYM ScriptShape}






/////
//
//      Returns E_OUTOFMEMORY if the output buffer length (cMaxGlyphs) is
//          insufficient. Note that in this case, as in all error cases, the
//          content of all output parameters are undefined.
//
//p     psa: Pass the SCRIPT_ANALYSIS field of the SCRIPT_ITEM entry for this
//          item. (The SCRIPT_ITEM array is returned by ScriptItemize.)
//
//      Clusters are sequenced uniformly within the run, as are glyphs within
//      the cluster - the fRTL item flag (from ScriptItemize) identifies
//      whether left to right, or right to left.
//
//p     pwLogClust: has cChars elements - each entry in pwLogClust corresponds
//          to a character in the input string (pwcChars). The value in each
//          pwLogCLust entry is the offset of the first glyph in the cluster
//          that contains this character.
//
//      Example: In the following example, there are four clusters:
//      1st cluster: one character represented by one glyph
//      2nd cluster: one character represented by 3 glyphs
//      3rd cluster: three characters represented by one glyph
//      4th cluster: 2 characters represented by three glyphs
//
//      Glyph array: (c<n>g<m> means cluster n glyph m)
//c        0      1    2    3      4      5    6    7
//c     -------------------------------------------------
//c     | c1g1 | c2g1 c2g2 c2g3 | c3g1 | c4g1 c4g2 c4g3 |
//c     -------------------------------------------------
//
//      Character array: (c<n>u<m> means cluster n Unicode codepoint m)
//c        0      1      2    3    4      5    6
//c     --------------------------------------------
//c     | c1u1 | c2u1 | c3u1 c3u2 c3u3 | c4u1 c4u2 |
//c     --------------------------------------------
//
//      LogClust: (one entry per character gives 1st glyph in cluster
//c     --------------------------------------------
//c     |   0  |   1  |   4    4    4  |   5    5  |
//c     --------------------------------------------
//
//      Note that for an RTL run (SCRIPT_ANALYSIS.a.fRTL == TRUE) and when
//      fLogicalOrder == FALSE (the default), glyphs are generated in visual
//      order - the reverse of the codepoint order, and the values in the
//      LogClust array will be descending.
//
//
//p     psva: has one visual attribute per glyph and so has maxGlyphs entries.
//
//
//      ScriptShape may set the fNoGlyphIndex flag in psa if the font or
//      OS cannot support glyph indices.
//
//      If fLogicalOrder is requested in psa, glyphs will be always be
//      generated in the same order as the original Unicode characters.
//
//      If fLogicalOrder is not set, right to left items are generated in
//      reverse order, so ScriptTextOut does not need to reverse them before
//      calling ExtTextOut.
/////   ScriptPlace
//
//      The ScriptPlace function takes the output of a ScriptShape call and
//      generates glyph advance width and 2D offset information.
//
//      The composite ABC width for the whole item identifies how much the
//      glyphs overhang to the left of the start position and to the right of
//      the length implied by the sum of the advance widths.
//
//      The total advance width of the line is exactly abcA + abcB + abcC.
//
//      abcA and abcC are maintained internally by Uniscribe as proportions
//      of the cell height represented in 8 bits and are thus roughly +/- 1%.
//      The total width returned (as the sum of piAdvance, and as the sum of
//      abcA+abcB+abcC) is accurate to the resolution of the TrueType shaping
//      engine.
//
//      All glyph related arrays are in visual order unless the fLogicalOrder
//      flag is set in psa.

type
  PGOffset = ^TGOffset;
  tagGOFFSET = record
    du:  Longint;
    dv:  Longint;
  end;
  GOFFSET = tagGOFFSET;
  {$EXTERNALSYM GOFFSET}
  TGOffset = tagGOFFSET;


function ScriptPlace(
    hdc: HDC;                   // In    Optional (see under caching)
    psc: PScriptCache;          // InOut Cache handle
    const pwGlyphs: PWord;      // In    Glyph buffer from prior ScriptShape call
    cGlyphs: Integer;           // In    Number of glyphs
    const psva: PScriptVisAttr; // In    Visual glyph attributes
    psa: PScriptAnalysis;       // InOut Result of ScriptItemize (may have fNoGlyphIndex set)
    piAdvance: PInteger;        // Out   Advance wdiths
    pGoffset: PGOffset;         // Out   x,y offset for combining glyph
    pABC: PABC                  // Out   Composite ABC for the whole run (Optional)
 ): HRESULT; stdcall;
{$EXTERNALSYM ScriptPlace}






/////   ScriptTextOut
//
//      The ScriptTextOut function takes the output of both ScriptShape and
//      ScriptPlace calls and calls the operating system ExtTextOut function
//      appropriately. If the last parameter is not null, GDI's ExtTextOutW calls
//      are routed to this function.
//
//      All arrays are in visual order unless the fLogicalOrder flag is set in
//      psa.


function ScriptTextOut(
    const hdc: HDC;                // In     OS handle to device context (required)
    psc: PScriptCache;             // InOut  Cache handle
    x: Integer;                    // In     x,y position for first glyph
    y: Integer;                    // In
    fuOptions: LongWord;           // In     ExtTextOut options
    const lprc: PRect;             // In     optional clipping/opaquing rectangle
    const psa: PScriptAnalysis;    // In     Result of ScriptItemize
    const pwcReserved: PWideChar;  // In     Reserved (requires NULL)
    iReserved: Integer;            // In     Reserved (requires 0)
    const pwGlyphs: PWord;         // In     Glyph buffer from prior ScriptShape call
    cGlyphs: Integer;              // In     Number of glyphs
    const piAdvance: PInteger;     // In     Advance widths from ScriptPlace
    const piJustify: PInteger;     // In     Justified advance widths (optional)
    const pGoffset: PGOffset       // In     x,y offset for combining glyph
 ): HRESULT; stdcall;






/////
//
//      The caller should normally use SetTextAlign(hdc, TA_RIGHT) before
//      calling ScriptTextOut with an RTL item inlogical order.
//
//      The piJustify array provides requested cell widths for each glyph.
//      When the piJustify width of a glyph differs from the unjustified
//      width (in PiAdvance), space is added to or removed from the glyph
//      cell at it's trailing edge. The glyph is always aligned with the
//      leading edge of it's cell. (This rule applies even in visual order.)
//
//      When a glyph cell is extended the extra space is uaually made up by
//      the addition of white space, however for Arabic scripts, the extra
//      space is made up by one or more kashida glyphs, unless the extra space
//      is insufficient for the shortest kashida glyph in the font. (The
//      width of the shortest kashida is available by calling
//      ScriptGetFontProperties.)
//
//      piJustify should only be passed if re-justification of the string is
//      required. Normally pass NULL to this parameter.
//
//      fuOptions may contain ETO_CLIPPED or ETO_OPAQUE (or neither or both).
//
//      Do not use ScriptTextOut to write to a metafile unless you are sure
//      that the metafile will eventually be played back without any font
//      substitution. ScriptTextOut record glyph numbers in the metafile.
//      Since glyph numbers vary considerably from one font to another
//      such a metafile is unlikely to play back correctly when differant
//      fonts are substituted.
//
//      For example when a metafile is played back at a different scale
//      CreateFont requests recorded in the metafile may resolve to bitmap
//      instead of truetype fonts, or if the metafile is played back on
//      a different machine requested fonts may not be installed.//
//
//      To write complex scripts in a metafile in a font independant manner,
//      use ExtTextOut to write the logical characters directly, so that
//      glyph generation and placement does not occur until the text is
//      played back.
/////   ScriptJustify
//
//      ScriptJustify provides a simple minded implementation of multilingual
//      justification.
//
//      Sophisticated text formatters may prefer to generate their own delta
//      dx array by combining their own features with the information returned
//      by ScriptShape in the SCRIPT_VISATTR array.
//
//      ScriptJustify establishes how much adjustment to make at each glyph
//      position on the line. It interprets the SCRIPT_VISATTR array generated
//      by a call to ScriptShape, and gives top priority to kashida, then uses
//      inter word spacing if there's no kashida points, then uses
//      intercharacter spacing if there are no inter-word points.
//
//      The justified advance widths generated in ScriptJustify should be
//      passed to ScriptTextOut in the piJustify paramter.
//
//      ScriptJustify creates a justify array containing updated advance
//      widths for each glyph. Where a glyphs advance width is increased, it
//      is expected that the extra width will be rendered to the right of the
//      glyph, with as white space or, for Arabic text, as kashida.
/////
function ScriptJustify(
    const psva:  PScriptVisAttr;  // In   Collected visual attributes for entire line
    const piAdvance: PInteger;    // In   Advance widths from ScriptPlace
    cGlyphs: Integer;             // In   Size of all arrays
    iDx: Integer;                 // In   Desired width change, either increase or descrease
    iMinKashida: Integer;         // In   Minimum length of continuous kashida glyph to generate
    piJustify: PInteger           // Out  Updated advance widths to pass to ScriptTextOut
 ): HRESULT; stdcall;




type
  /////   SCRIPT_LOGATTR
  //
  //      The SCRIPT_LOGATTR structure describes attributes of logical
  //      characters useful when editing and formatting text.
  //
  //      Note that for wordbreaking and linebreaking, if the first character of
  //      the run passed in is not whitespace, the client needs to check whether
  //      the last character of the previous run is whitespace to determine if
  //      the first character of this run is the start of a word.
  //
  //
  TScriptLogAttr_enum = (
    fSoftBreak,  // Potential linebreak point
    fWhiteSpace, // A unicode whitespace character, except NBSP, ZWNBSP
    fCharStop,   // Valid cursor position (for left/right arrow)
    fWordStop,   // Valid cursor position (for ctrl + left/right arrow)
    fInvalid     // Invalid character sequence
  );

  PScriptLogAttr = ^TScriptLogAttr;
  tag_SCRIPT_LOGATTR = set of TScriptLogAttr_enum;
(*  fSoftBreak: Byte      {:1};     // Potential linebreak point
    fWhiteSpace: Byte     {:1};     // A unicode whitespace character, except NBSP, ZWNBSP
    fCharStop: Byte       {:1};     // Valid cursor position (for left/right arrow)
    fWordStop: Byte       {:1};     // Valid cursor position (for ctrl + left/right arrow)
    fInvalid: Byte        {:1};     // Invalid character sequence
    fReserved: Byte       {:3;}
  end; *)
  {$EXTERNALSYM tag_SCRIPT_LOGATTR}
  SCRIPT_LOGATTR = tag_SCRIPT_LOGATTR;
  {$EXTERNALSYM SCRIPT_LOGATTR}
  TScriptLogAttr = SCRIPT_LOGATTR;

  
//
//
//p     fSoftBreak: It would be valid to break the line in front of this
//              character. This flag is set on the first character of
//              South-East Asian words. Note that when linebreaking the
//              client would usually also treat any nonblank following a blank
//              as a softbreak position, by inspecting the fWhiteSPace flag
//              below.
//
//p     fWhiteSpace: This character is one of the many Unicode character
//              that are classified as breakable whitespace.
//
//p     fCharStop: Valid cursor position. Set on most characters, but not
//              on codepoints inside Indian and South East Asian character
//              clusters. May be used to implement left and right arrow
//              operation in editors.
//
//p     fWordStop: Valid position following word advance/retire commonly
//              implemented at ctrl/left-arrow and ctrl/right-arrow.
//              May be used to implement ctrl+left and ctrl+right arrow
//              operation in editors. As with fSoftBreak clients should
//              normally also inspect the fWhiteSpace flag and treat the
//              first character after a run of whitespace as the start of a
//              word.
//
//p     fInvalid: Marks characters which form an invalid or undisplayable
//              combination. Scripts which can set this flag have the flag
//              fInvalidLogAttr set in their SCRIPT_PROPERTIES.






/////   ScriptBreak
//
//      The ScriptBreak function returns cursor movement and formatting break
//      positions for an item as an array of SCRIPT_LOGATTRs. To support
//      mixed formatting within a single word correctly, ScriptBreak should
//      be passed whole items as returned by ScriptItemize.
//
//      ScriptBreak does not require an hdc and does not execute glyph shaping.
//
//      The fCharStop flag marks cluster boundaries for those scripts where
//      it is conventional to restrict from moving inside clusters. The same
//      boundaries could also be inferred by inspecting the pLogCLust array
//      returned by ScriptShape, however ScriptBreak is considerably faster in
//      implementation and does not require an hdc to be prepared.
//
//      The fWordStop, fSoftBreak and fWhiteSpace flags are only available
//      through ScriptBreak.
//
//      Most shaping engines that identify invalid sequences do so by setting
//      the fInvalid flag in ScriptBreak. The fInvalidLogAttr flag in
//      ScriptProperties identifies which scripts do this.


function ScriptBreak(
    const pwcChars: PWideChar;  // In   Logical unicode item
    cChars: Integer;    // In   Length of unicode item
    const psa: PScriptAnalysis;       // In   Result of earlier ScriptItemize call
    psla: PScriptLogAttr     // Out  Logical character attributes
 ): HRESULT; stdcall;
{$EXTERNALSYM ScriptBreak}





/////   ScriptCPtoX
//
//      The ScriptCPtoX function returns the x offset from the left end
//      (!fLogical) or leading edge (fLogical) of a run to either the leading
//      or the trailing edge of a logical character cluster.
//
//      iCP is the offset of any logical character in the cluster.
//
//      For scripts where the caret may conventionally be placed into the
//      middle of clusters (e.g. Arabic, Hebrew), the returned X may be
//      an interpolated position for any codepoint in the line.
//
//      For scripts where the caret is conventionally snapped to the boundaries
//      of clusters, (e.g. Thai, Indian), the resulting X position will be
//      snapped to the requested edge of the cluster containing CP.


function ScriptCPtoX(
    iCP: Integer;               // In   Logical character position in run
    fTrailing: BOOL;            // In   Which edge (default - leading)
    cChars: Integer;            // In   Count of logical codepoints in run
    cGlyphs: Integer;           // In   Count of glyphs in run
    const pwLogClust: PWord;    // In   Logical clusters
    const psva: PScriptVisAttr; // In   Visual glyph attributes array
    const piAdvance: PInteger;  // In   Advance widths
    const psa: PScriptAnalysis; // In   Script analysis from item attributes
    piX: PInteger               // Out  Resulting X position
 ): HRESULT; stdcall;






/////   ScriptXtoCP
//
//      The ScriptXtoCP function converts an x offset from the left end
//      (!fLogical) or leading edge (fLogical) of a run to a logical
//      character position and a flag that indicates whether the X position
//      fell in the leading or the trailing half of the character.
//
//      For scripts where the cursor may conventionally be placed into the
//      middle of clusters (e.g. Arabic, Hebrew), the returned CP may be
//      for any codepoint in the line, and fTrailing will be either zero
//      or one.
//
//      For scripts where the cursor is conventionally snapped to the
//      boundaries of a cluster, the returned CP is always the position of
//      the logically first codepoint in a cluster, and fTrailing is either
//      zero, or the number of codepoints in the cluster.
//
//      Thus the appropriate cursor position for a mouse hit is always the
//      returned CP plus the value of fTrailing.
//
//      If the X positition passed is not in the item at all, the resulting
//      position will be the trailing edge of character -1 (for X positions
//      before the item), or the leading edge of character 'cChars' (for
//      X positions following the item).


function ScriptXtoCP(
    iX: Integer;                 // In   X offset from left of run
    cChars: Integer;             // In   Count of logical codepoints in run
    cGlyphs: Integer;            // In   Count of glyphs in run
    const pwLogClust: PWord;     // In   Logical clusters
    const psva: PScriptVisAttr;  // In   Visual glyph attributes
    const piAdvance: Integer;    // In   Advance widths
    const psa:  PScriptAnalysis; // In   Script analysis from item attributes
    piCP: PInteger;              // Out  Resulting character position
    piTrailing: PInteger         // Out  Leading or trailing half flag
 ): HRESULT; stdcall;






/////   Relationship between caret positions, justifications points and clusters
//
//
//t     Job                              | Uniscribe support
//t     -------------------------------- | --------------------------------------------------------
//t     Caret move by character cluster  | LogClust or VISATTR.fClusterStart or LOGATTR.fCharStop
//t     Line breaking between characters | LogClust or VISATTR.fClusterStart or LOGATTR.fCharStop
//t     Caret move by word               | LOGATTR.fWordStop
//t     Line breaking between words      | LOGATTR.fWordStop
//t     Justification                    | VISATTR.uJustification
//
//
//
/////   Character clusters
//
//      Character clusters are glyph sequences that cannot be split between
//      lines.
//
//      Some languages (e.g. Thai, Indic) restrict caret placement to points
//      betwen clusters. This applies both to keyboard initiated caret
//      movement (e.g. cursor keys) and pointing and clicking with the mouse
//      (hit testing).
//
//      Uniscribe provides cluster information in both the visual and logical
//      attributes. If you've called ScriptShape you'll find the cluster
//      information represented both by sequences of the same value in the
//      pwLogClust array, and by the fClusterStart flag in the psva
//      SCRIPT_VISATTR array.
//
//      ScriptBreak also returns the fCharStop flag in the SCRIPT_LOGATTR
//      array to identify cluster positions.
//
//
//
/////   Word break points
//
//      Valid positions for moving the caret when moving in whole words are
//      marked by the fWordStop flag returned by ScriptBreak.
//
//      Valid positions for breaking lines between words are marked by the
//      fSoftBreak flag returned by ScriptBreak.
//
//
//
/////   Justification
//
//      Justification space or kashida should be inserted where identified by
//      the uJustificaion field of the SCRIPT_VISATTR.
//
//      When performing inter-character justification, insert extra space
//      only after glyphs marked with uJustify == SCRIPT_JUSTIFY_CHARACTER.
//
//
//
/////   Script specific processing
//
//      Uniscribe provides information about special processing for each
//      script in the SCRIPT_PROPERTIES array.
//
//      Use the following code during initialisation to get a pointer to
//      the SCRIPT_PROPERTIES array:
//
//c     const SCRIPT_PROPERTIES **g_ppScriptProperties; // Array of pointers to properties
//c     int iMaxScript;
//c     HRESULT hr;
//
//c     hr = ScriptGetProperties(&g_ppScriptProperties, &g_iMaxScript);
//
//      Then inspect the properties of the script of an item 'iItem' as follows:
//
//c     hr = ScriptItemize( ... , pItems, ... );
//c     ...
//c     if (g_ppScriptProperties[pItems[iItem].a.eScript]->fNeedsCaretInfo) {
//c         // Use ScriptBreak to restrict the caret from entering clusters (for example).
//c     }
//
//
//      SCRIPT_PROPERTIES.fNeedsCaretInfo
//
//      Caret placement should be restricted to cluster
//      edges for scripts such as Thai and Indian. The fNeedsCaretInfo flag
//      in SCRIPT_PROPERTIES identifies such languages.
//
//      Note that ScriptXtoCP and ScriptCPtoX automatically apply caret
//      placement restictions.
//
//
//      SCRIPT_PROPERTIES.fNeedsWordBreaking
//
//      For most scripts, word break placement  may be
//      identified by scanning for characters marked as fWhiteSpace in
//      SCRIPT_LOGATTR, or for glyphs marked as uJustify ==
//      SCRIPT_JUSTIFY_BLANK or SCRIPT_JUSTIFY_ARABIC_BLANK in SCRIPT_VISATTR.
//
//      For languages such as Thai, it is also necessary to call ScriptBreak,
//      and include character positions marked as fWordStop in SCRIPT_LOGATTR.
//      Such scripts are marked as fNeedsWordbreaking in SCRIPT_PROPERTIES.
//
//
//      SCRIPT_PROPERTIES.fNeedsCharacterJustify
//
//      Languages such as Thai also require inter-character spacing when
//      justifying (where uJustify == SCRIPT_JUSTIFY_CHARACTER in the
//      SCRIPT_VISATTR). Such languages are marked as fNeedsCharacterJustify
//      in SCRIPT_PROPERTIES.
//
//
//      SCRIPT_PROPERTIES.fAmbiguousCharSet
//
//      Many Uniscribe scripts do not correspond directly to 8 bit character
//      sets. For example Unicode characters in the range U+100 through U+024F
//      represent extended latin shapes used for many languages, including
//      those supported by EASTEUROPE_CHARSET, TURKISH_CHARSET and
//      VIETNAMESE_CHARSET. However many of these characters are supported by
//      more han one of thsese charsets.
//      fAmbiguousCharset is set for any script token which could contain
//      characters from a number of these charsets. In these cases the bCharSet
//      field may contain ANSI_CHARSET or DEFAULT_CHARSET. The Uniscribe client
//      will generally need to apply futher processing to determine which charset
//      to use when requesting a font suitable for this run. For example it
//      determine that the run consists of multiple languages and split it up
//      to use a different font for each language.






/////   Notes on ScriptXtoCP and ScriptCPtoX
//
//      Both functions work only within runs and require the results of a
//      previous ScriptShape call.
//
//      The client must establish which run a given cursor offset or x
//      position is within before passing it to ScriptCPtoX or ScriptXtoCP.
//
//      Cluster information in the logical cluster array is used to share
//      the width of a cluster of glyphs equally among the logical characters
//      they represent.
//
//      For example, the lam alif glyph is divided into four areas: the
//      leading half of the lam, the trailing half of the lam, the leading
//      half of the alif and the trailing half of the alif.
//
//      ScriptXtoCP Understands the caret position conventions of each script.
//      For Indian and Thai, caret positions are snapped to cluster boundaries,
//      for Arabic and Hebrew, caret positions are interpolated within clusters.
//
//
/////   Translating mouse hit 'x' offset to caret position
//
//      Conventionally, caret position 'cp' may be selected by clicking either
//      on the trailing half of character 'cp-1' or on the leading half of
//      character 'cp'. This may easily be implemented as follows:
//
//c     int iCharPos;
//c     int iCaretPos
//c     int fTrailing;
//
//c     ScriptXtoCP(iMouseX, ..., &iCharPos, &fTrailing);
//c     iCaretPos = iCharPos + fTrailing;
//
//      For scripts that snap the caret to cluster boundaries, ScriptXtoCP
//      returns ftrailing set to either 0, or the width of the cluster in
//      codepoints. Thus the above code correctly returns only valid
//      caret positions.
//
//
/////   Displaying the caret in bidi strings
//
//      In unidirectional text, the leading edge of a character is at the same
//      place as the trailing edge of the previous character, so there is no
//      ambiguity in placing the caret between characters.
//
//      In bidirectional text, the caret position between runs of opposing
//      direction may be ambiguous.
//
//      For example in the left to right paragraph 'helloMAALAS', the last
//      letter of 'hello' immediately preceeds the first letter of 'salaam'.
//      The best position to display the caret depends on whether it is
//      considered to follow the 'o' of 'hello', or to preceed the 's' of
//      'salaam'.
//
/////   Commonly used caret positioning conventions
//
//t     Situation       | Visual caret placement
//t     ---------       | -------------------------------------------
//t     Typing          | Trailing edge of last character typed
//t     Pasting         | Trailing edge of last character pasted
//t     Caret advancing | Trailing edge of last character passed over
//t     Caret retiring  | Leading edge of last character passed over
//t     Home            | Leading edge of line
//t     End             | Trailing edge of line
//
//      The caret may be positioned as follows:
//
//c     if (advancing) {
//c         ScriptCPtoX(iCharPos-1, TRUE, ..., &iCaretX);
//c     } else {
//c         ScriptCPtoX(iCharPos, FALSE, ..., &iCaretX);
//c     }
//
//      Or, more simply, given an fAdvancing BOOL restricted to TRUE or FALSE:
//
//c     ScriptCPtoX(iCharPos-fAdvancing, fAdvancing, ..., &iCaretX);
//
//      ScriptCPtoX handles out of range positions logically: it returns the
//      leading edge of the run for iCharPos <0, and the trailing edge of the
//      run for iCharPos >=length.
/////   ScriptGetLogicalWidths
//
//      Converts visual withs in piAdvance into logical widths,
//      one per original character, in logical order.
//
//      Ligature glyphs widths are divided evenly amongst the characters
//      they represent.


function ScriptGetLogicalWidths(
    const psa: PScriptAnalysis;     // In   Script analysis from item attributes
    cChars: Integer;                // In   Count of logical codepoints in run
    cGlyphs: Integer;               // In   Count of glyphs in run
    const piGlyphWidth: PInteger;   // In   Advance widths
    const pwLogClust: PWord;        // In   Logical clusters
    const psva: PScriptVisAttr;     // In   Visual glyph attributes
    piDx: PInteger                  // Out  Logical widths
 ): HRESULT; stdcall;






/////
//      ScriptGetLogicalWidths is useful for recording widths in a
//      font independant manner. By passing the recorded logical widths
//      to ScriptApplyLogicalWidths, a block of text can be replayed in the
//      same boundaries with acceptable loss of quality even when the original
//      font is not available.
/////   ScriptApplyLogicalWidth
//
//      Accepts an array of advance widths in logical order, corresponding
//      one to one with codepoints, and generates an array of glyph widths
//      suitable for passing to the piJustify parameter of ScriptTextOut.
//
//      ScriptApplyLogicalWidth may be used to reapply logical widths
//      obtained with ScriptGetLogicalWidths. It may be useful in situations
//      such as metafiling, where it is necessary to record and reapply
//      advance width information in a font independant manner.



function ScriptApplyLogicalWidth(
    const piDx: PInteger;        // In     Logical dx array to apply
    cChars: Integer;             // In     Count of logical codepoints in run
    cGlyphs: Integer;            // In     Glyph count
    const pwLogClust: PWORD;     // In     Logical clusters
    const psva: PScriptVisAttr;  // In     Visual attributes from ScriptShape/Place
    const piAdvance: PInteger;   // In     Glyph advance widths from ScriptPlace
    const psa:  PScriptAnalysis; // In     Script analysis from item attributes
    pABC: PABC;                  // InOut  Updated item ABC width (optional)
    piJustify: PInteger          // Out    Resulting glyph advance widths for ScriptTextOut
 ): HRESULT; stdcall;






/////
//p     piDx: Pointer to an array of dx widths in logical order, one per codepoint.
//
//p     cChars: Count of the logical codepoints in the run.
//
//p     cGlyphs: Glyph count.
//
//p     pwLogClust: Pointer to an array of logical clusters from ScriptShape
//
//p     psva: Pointer to an array of visual attributes from ScriptShape and
//          updated by ScriptPlace.
//
//p     piAdvance: Pointer to an array of glyph advance widths from ScriptPlace.
//
//p     psa: Pointer to a SCRIPT_ANALYSIS structure from ScriptItemize and
//          updated by ScriptShape and SriptPlace..
//
//p     pABC: Pointer to the run overall ABC width (optional). If present,
//          when the function is called, it should contain the run ABC width
//          returned by ScriptPlace; when the function returns, the ABC width
//          has been updated to match the new widths.
//
//p     piJustify:Pointer to an array of the resulting glyph advance widths.
//          This is suitable for passing to the piJustify parameter of ScriptTextOut.
/////   ScriptGetCMap
//
//      ScriptGetCMap may be used to determine which characters in a run
//      are supported by the selected font.
//
//      It returns glyph indices of Unicode characters according to Truetype
//      Cmap table, or standard Cmap implemented for old style fonts. The
//      glyph indices are returned in the same order as the input string.
//
//      The caller may scan the returned glyph buffer looking for the default
//      glyph to determine which characters are not available. (The default
//      glyph index for the selected font should be determined by calling
//      ScriptGetFontProperties).
//
//      The return value indicates the presence of any missing glyphs.

const
  SGCM_RTL  = $00000001;      // Return mirrored glyph for mirrorable Unicode codepoints


function ScriptGetCMap(
    hdc: HDC;                     // In    Optional (see notes on caching)
    psc: PScriptCache;            // InOut Address of Cache handle
    const pwcInChars: PWideChar;  // In    Unicode codepoint(s) to look up
    cChars: Integer;              // In    Number of characters
    dwFlags: DWORD;               // In    Flags such as SGCM_RTL
    pwOutGlyphs: PWord            // Out   Array of glyphs, one per input character
 ): HRESULT; stdcall;





/////
//  returns S_OK     - All unicode codepoints were present in the font
//          S_FALSE  - Some of the Unicode codepoints were mapped to the default glyph
//          E_HANDLE - font or system does not support glyph indices
/////   ScriptGetGlyphABCWidth
//
//      Returns ABC width of a given glyph.
//      May be useful for drawing glyph charts. Should not be used for
//      run of the mill complex script text formatting.


function ScriptGetGlyphABCWidth(
    hdc: HDC;            // In    Optional (see notes on caching)
    psc: PScriptCache;   // InOut Address of Cache handle
    wGlyph: Word;        // In    Glyph
    pABC: PABC           // Out   ABC width
 ): HRESULT; stdcall;





type
  /////
  //  returns S_OK     - Glyph width returned
  //          E_HANDLE - font or system does not support glyph indices
  /////   SCRIPT_PROPERTIES
  //

  TScriptProperties_enum = (
    fNumeric,              {:1}
    fComplex,              {:1}  // Script requires special shaping or layout
    fNeedsWordBreaking,    {:1}  // Requires ScriptBreak for word breaking information
    fNeedsCaretInfo,       {:1}  // Requires caret restriction to cluster boundaries
    bCharSet,              {:8}  // Charset to use when creating font
    fControl,              {:1}  // Contains only control characters
    fPrivateUseArea,       {:1}  // This item is from the Unicode range U+E000 through U+F8FF
    fNeedsCharacterJustify,{:1}  // Requires inter-character justification
    fInvalidGlyph,         {:1}  // Invalid combinations generate glyph wgInvalid in the glyph buffer
    fInvalidLogAttr,       {:1}  // Invalid combinations are marked by fInvalid in the logical attributes
    fCDM,                  {:1}  // Contains Combining Diacritical Marks
    fAmbiguousCharSet,     {:1}  // Script does not correspond 1//:1 with a charset
    fClusterSizeVaries,    {:1}  // Measured cluster width depends on adjacent clusters
    fRejectInvalid         {:1}  // Invalid combinations should be rejected
  );
  TScriptProperties_set = set of TScriptProperties_enum;

  PScriptProperties = ^TScriptProperties;
  SCRIPT_PROPERTIES = packed record
    langid: Word                  {:16}; // Primary and sublanguage associated with script
    fFlags: TScriptProperties_set;
  end;
(*  langid: DWORD                 {:16}; // Primary and sublanguage associated with script
    fNumeric: DWORD               {:1};
    fComplex: DWORD               {:1};  // Script requires special shaping or layout
    fNeedsWordBreaking: DWORD     {:1};  // Requires ScriptBreak for word breaking information
    fNeedsCaretInfo: DWORD        {:1};  // Requires caret restriction to cluster boundaries
    bCharSet: DWORD               {:8};  // Charset to use when creating font
    fControl: DWORD               {:1};  // Contains only control characters
    fPrivateUseArea: DWORD        {:1};  // This item is from the Unicode range U+E000 through U+F8FF
    fNeedsCharacterJustify: DWORD {:1};  // Requires inter-character justification
    fInvalidGlyph: DWORD          {:1};  // Invalid combinations generate glyph wgInvalid in the glyph buffer
    fInvalidLogAttr: DWORD        {:1};  // Invalid combinations are marked by fInvalid in the logical attributes
    fCDM: DWORD                   {:1};  // Contains Combining Diacritical Marks
    fAmbiguousCharSet: DWORD      {:1};  // Script does not correspond 1//:1 with a charset
    fClusterSizeVaries: DWORD     {:1};  // Measured cluster width depends on adjacent clusters
    fRejectInvalid: DWORD         {:1};  // Invalid combinations should be rejected
  end; *)
  {$EXTERNALSYM SCRIPT_PROPERTIES}
  TScriptProperties = SCRIPT_PROPERTIES;
  
//
//p     langid: Language associated with this script. When a script is used for many languages,
//          langid id represents a default language. For example, Western script is represented
//          by LANG_ENGLISH although it is also used for French, German, Spanish etc.
//
//p     fNumeric: Script contains numerics and characters used in conjunction with numerics
//          by the rules of the Unicode bidirectional algorithm. For example
//          dollar sign and period are classified as numeric when adjacent to or in between
//          digits.
//
//p     fComplex: Indicates a script that requires complex script handling. If fComplex is false
//          the script contains no combining characters and requires no contextual shaping or reordering.
//
//p     fNeedsWordBreaking: A script, such as Thai, which requires algorithmic wordbreaking.
//          Use ScriptBreak to obtain a wordbreak points using the standard system wordbreaker.
//
//p     fNeedsCaretInfo: A script, such as Thai and Indian, where the caret may not be placed
//          inside a cluster. To determine valid caret positions inspect the fCharStop flag in the
//          logical attributes returned by ScriptBreak, or compare adjacent values in the pwLogClust
//          array returned by ScriptShape.
//
//p     bCharSet: Nominal charset associated with script. May be used in a logfont when creating
//          a font suitable for displaying this script. Note that for new scripts where there
//          is no charset defined, bCharSet may be innapropriate and DEFAULT_CHARSET should
//          be used instead - see the description of fAmbiguousCharSet below.
//
//p     fControl: contains control characters.
//
//p     fPrivateUseArea: The Unicode range U+E000 through U+F8FF.
//
//p     fNeedsCharacterJustify: A script, such as Thai, where justification is conventionally
//          achieved by increasing the space between all letters, not just between words.
//
//p     fInvalidGlyph: A script for which ScriptShape generates an invalid glyph
//          to represent invalid sequences. The glyph index of the invalid glyph for
//          a particular font may be obtained by calling ScriptGetFontProperties.
//
//p     fInvalidLogAttr: A script for which ScriptBreak sets the fInvalid flag
//          in the logical attributes to mark invalid sequences.
//
//p     fCDM: Implies that an item analysed by ScriptItemize included combining
//          diacritical marks (U+0300 through U+36F).
//
//p     fAmbiguousCharSet: No single legacy charset supports this script.
//          For example the extended Latin Extended-A Unicode range includes
//          characters from the EASTUROPE_CHARSET, the TURKISH_CHARSET and the
//          BALTIC_CHARSET. It also contains characters that are not available
//          in any legacy charset. Use DEFAULT_CHARSET when creating fonts to
//          display parts of this run.
//
//p     fClusterSizeVaries: A script, such as Arabic, where contextual shaping
//          may cause a string to increase in size when removing characters.
//
//p     fRejectInvalid: A script, such as Thai, where invalid sequences conventionally
//          cause an editor such as notepad to beep, and ignore keypresses.


/////   ScriptGetProperties
//
//      ScriptGetProperties returns the address of a table that maps a
//      script in a SCRIPT_ANALYSIS uScript field to properties including
//      the primary language associated with that script, whether it's
//      numeric and whether it's complex.


function ScriptGetProperties(
    out ppSp: PScriptProperties;   // Out  Receives pointer to table of pointers to properties indexed by script
    out piNumScripts: Integer      // Out  Receives number of scripts (valid values are 0 through NumScripts-1)
 ): HRESULT; stdcall;





type
  /////   SCRIPT_FONTPROPERTIES
  //
  PScriptFontProperties = ^TScriptFontProperties;
  SCRIPT_FONTPROPERTIES = record
    cBytes: Integer;      // Structure length
    wgBlank: Word;        // Blank glyph
    wgDefault: Word;      // Glyph used for Unicode values not present in the font
    wgInvalid: Word;      // Glyph used for invalid character combinations (especially in Thai)
    wgKashida: Word;      // Shortest continuous kashida glyph in the font, -1 if doesn't exist
    iKashidaWidth: Integer;// Widths of shortest continuous kashida glyph in the font
  end;
  {$EXTERNALSYM SCRIPT_FONTPROPERTIES}
  TScriptFontProperties = SCRIPT_FONTPROPERTIES;


/////   ScriptGetFontProperties
//
//      Returns information from the font cache


function ScriptGetFontProperties(
    hdc: HDC;                    // In    Optional (see notes on caching)
    psc: PScriptCache;           // InOut Address of Cache handle
    sfp:  PScriptFontProperties  // Out   Receives properties for this font
 ): HRESULT; stdcall;






/////   ScriptCacheGetHeight
//
//


function ScriptCacheGetHeight(
    hdc: HDC;            // In    Optional (see notes on caching)
    psc: PScriptCache;   // InOut Address of Cache handle
    tmHeight: PLongint   // Out   Receives font height in pixels
 ): HRESULT; stdcall;




const
  /////   ScriptStringAnalyse
  //
  //
  SSA_PASSWORD         = $00000001;  // Input string contains a single character to be duplicated iLength times
  SSA_TAB              = $00000002;  // Expand tabs
  SSA_CLIP             = $00000004;  // Clip string at iReqWidth
  SSA_FIT              = $00000008;  // Justify string to iReqWidth
  SSA_DZWG             = $00000010;  // Provide representation glyphs for control characters
  SSA_FALLBACK         = $00000020;  // Use fallback fonts
  SSA_BREAK            = $00000040;  // Return break flags (character and word stops)
  SSA_GLYPHS           = $00000080;  // Generate glyphs, positions and attributes
  SSA_RTL              = $00000100;  // Base embedding level 1
  SSA_GCP              = $00000200;  // Return missing glyphs and LogCLust with GetCharacterPlacement conventions
  SSA_HOTKEY           = $00000400;  // Replace '&' with underline on subsequent codepoint
  SSA_METAFILE         = $00000800;  // Write items with ExtTextOutW Unicode calls, not glyphs
  SSA_LINK             = $00001000;  // Apply FE font linking/association to non-complex text
  SSA_HIDEHOTKEY       = $00002000;  // Remove first '&' from displayed string
  SSA_HOTKEYONLY       = $00002400;  // Display underline only.

  SSA_FULLMEASURE      = $04000000;  // Internal - calculate full width and out the number of chars can fit in iReqWidth.
  SSA_LPKANSIFALLBACK  = $08000000;  // Internal - enable FallBack for all LPK Ansi calls Except BiDi hDC calls
  SSA_PIDX             = $10000000;  // Internal
  SSA_LAYOUTRTL        = $20000000;  // Internal - Used when DC is mirrored
  SSA_DONTGLYPH        = $40000000;  // Internal - Used only by GDI during metafiling - Use ExtTextOutA for positioning
  SSA_NOKASHIDA        = $80000000;  // Internal - Used by GCP to justify the non Arabic glyphs only.
//
//
//p     SSA_HOTKEY: Note that SSA_HOTKEY and SSA_HIDEHOTKEY remove the
//          hotkey '&' character from further processing, so functions
//          such as ScriptString_pLogAttr return arrays based on a string
//          which excludes the '&'.



type
  /////   SCRIPT_TABDEF
  //
  //      Defines tabstop positions for ScriptStringAnalyse (ignored unless SSA_TAB passed)
  //
  PScriptTabDef = ^TScriptTabDef;
  tag_SCRIPT_TABDEF = record
    cTabStops: Integer;        // Number of entries in pTabStops array
    iScale: Integer;           // Scale factor for pTabStops (see below)
    pTabStops: PInteger;       // Pointer to array of one or more tab stops
    iTabOrigin: Integer;       // Initial offset for tab stops (logical units)
  end;
  {$EXTERNALSYM tag_SCRIPT_TABDEF}
  SCRIPT_TABDEF = tag_SCRIPT_TABDEF;
  {$EXTERNALSYM SCRIPT_TABDEF}
  TScriptTabDef = tag_SCRIPT_TABDEF;

//
//
//p     cTabStops: Number of entries in the pTabStops array. If zero, tabstops
//          are every 8 average character widths. If one, all tabstops are
//          the length of the first entry in pTabStops. If more than one,
//          the first cTabStops are as specified in the pTabStops array,
//          subsequent tabstops are every 8 average characters from the last
//          tabstop in the array.
//
//p     iScale: Scale factor for iTabOrigin and pTabStops entries. Values are
//          converted to device coordinates by multiplying by iScale then
//          dividing by 4. If values are already in device units, set iScale to
//          4. If values are in dialog units, set iScale to the average char
//          width of the dialog font. If values are multiples of the average
//          character width for the selected font, set iScale to 0.
//
//p     pTabStops: Array of cTabStops entries. Each entry specifies a
//          tabstop position. Positive values give nearedge alignment,
//          negative values give faredge alignment.
//
//p     iTabOrigin: Tabs are considered to start iTabOrigin before the
//          beginning of the string. Helps with multiple tabbed
//          outputs on the same line.






/////   ScriptStringAnalyse
//
//      cString - Input string must contain at least one character
//
//      hdc - required if SSA_GLYPH requested. Optional for SSA_BREAK.
//      If present the current font in the hdc is inspected and if a symbolic
//      font the character string is treated as a single neutral SCRIPT_UNDEFINED item.
//
//      Note that the uBidiLevel field in the initial SCRIPT_STATE value
//      is ignored - the uBidiLevel used is derived from the SSA_RTL
//      flag in combination with the layout of the hdc.


  SCRIPT_STRING_ANALYSIS = Pointer;
  {$EXTERNALSYM SCRIPT_STRING_ANALYSIS}
  TScriptStringAnalysis = SCRIPT_STRING_ANALYSIS;
  PScriptStringAnalysis = ^TScriptStringAnalysis;


function ScriptStringAnalyse(
    hdc: HDC;                  //In  Device context (required)
    const pString: Pointer;    //In  String in 8 or 16 bit characters
    cString: Integer;          //In  Length in characters (Must be at least 1)
    cGlyphs: Integer;          //In  Required glyph buffer size (default cString*1.5 + 16)
    iCharset: Integer;         //In  Charset if an ANSI string, -1 for a Unicode string
    dwFlags: DWORD;            //In  Analysis required
    iReqWidth: Integer;        //In  Required width for fit and/or clip
    psControl: PScriptControl; //In  Analysis control (optional)
    psState: PScriptState;     //In  Analysis initial state (optional)
    const piDx: PInteger;      //In  Requested logical dx array
    pTabdef: PScriptTabDef;    //In  Tab positions (optional)
    const pbInClass: PByte;    //In  Legacy GetCharacterPlacement character classifications (deprecated)

    pssa:  PScriptStringAnalysis //Out Analysis of string
 ): HRESULT; stdcall;






/////   ScriptStringFree - free a string analysis
//
//


function ScriptStringFree(
    pssa: PScriptStringAnalysis  //InOut Address of pointer to analysis
 ): HRESULT; stdcall;






/////   ScriptStringSize
//
//      returns a pointer to the size (width and height) of an analysed string
//
//      Note that the SIZE pointer remains valid only until the
//      SCRIPT_STRING_ANALYSIS is passed to ScriptStringFree.


function ScriptString_pSize(
    ssa: TScriptStringAnalysis
 ): {const} PSize; stdcall;






/////   ScriptString_pcOutChars
//
//      returns pointer to length of string after clipping (requires SSA_CLIP set)
//
//      Note that the int pointer remains valid only until the
//      SCRIPT_STRING_ANALYSIS is passed to ScriptStringFree.


function ScriptString_pcOutChars(
    ssa: TScriptStringAnalysis
 ): {const} PInteger; stdcall;






/////   ScriptString_pLogAttr
//
//      returns pointer to logical attributes buffer in a SCRIPT_STRING_ANALYSIS
//
//      Note that the buffer pointer remains valid only until the
//      SCRIPT_STRING_ANALYSIS is passed to ScriptStringFree.
//
//      The logical attribute array contains *ScriptString_pcOutChars(ssa)
//      entries.


function ScriptString_pLogAttr(
    ssa: TScriptStringAnalysis
 ): {const} PScriptLogAttr; stdcall;






/////   ScriptStringGetOrder
//
//      Creates an array mapping original character position to glyph position.
//
//      Treats clusters as they were in legacy systems - Unless a cluster
//      contains more glyphs than codepoints, each glyph is referenced at
//      least once from the puOrder array.
//
//      Requires SSA_GLYPHS requested in original ScriptStringAnalyse call.
//
//      The puOrder parameter should address a buffer containing room for
//      at least *ScriptString_pcOutChars(ssa) ints.


function ScriptStringGetOrder(
    ssa: TScriptStringAnalysis;
    puOrder: PLongWord
 ): HRESULT; stdcall;






/////   ScriptStringCPtoX
//
//      Return x coordinate for leading or trailing edge of character icp.


function ScriptStringCPtoX(
    ssa: TScriptStringAnalysis;        //In  String analysis
    icp: Integer;                      //In  Caret character position
    fTrailing: BOOL;                   //In  Which edge of icp
    out pX: Integer                    //Out Corresponding x offset
 ): HRESULT; stdcall;





/////   ScriptStringXtoCP
//
//


function ScriptStringXtoCP(
    ssa: TScriptStringAnalysis;        // In
    iX: Integer;                       // In
    piCh: PInteger;                    // Out
    piTrailing: PInteger               // Out
 ): HRESULT; stdcall;





/////   ScriptStringGetLogicalWidths
//
//      Converts visual withs in psa->piAdvance into logical widths,
//      one per original character, in logical order.
//
//      Requires SSA_GLYPHS requested in original ScriptStringAnalyse call.
//
//      The piDx parameter should address a buffer containing room for
//      at least *ScriptString_pcOutChars(ssa) ints.


function ScriptStringGetLogicalWidths(
    ssa: TScriptStringAnalysis;
    out piDx: Integer): HRESULT; stdcall;






/////   ScriptStringValidate
//
//      Scans the string analysis for invalid glyphs.
//
//      Only glyphs generated by scripts that can generate invalid glyphs
//      are scanned.
//
//      returns S_OK    - no invalid glyphs are present
//              S_FALSE - one or more invalid glyphs are present


function ScriptStringValidate(
    ssa: TScriptStringAnalysis): HRESULT; stdcall;






/////   ScriptStringOut
//
//      Displays the string generated by a prior ScriptStringAnalyze call,
//      then optionally adds highlighting corresponding to a logical selection.
//
//      Requires SSA_GLYPHS requested in original ScriptStringAnalyse call.


function ScriptStringOut(
    ssa: TScriptStringAnalysis;         //In  Analysis with glyphs
    iX: Integer;                        //In
    iY: Integer;                        //In
    uOptions: LongWord;                 //In  ExtTextOut options
    const prc: PRect;                   //In  Clipping rectangle (iff ETO_CLIPPED)
    iMinSel: Integer;                   //In  Logical selection. Set iMinSel>=iMaxSel for no selection
    iMaxSel: Integer;                   //In
    fDisabled: BOOL                     //In  If disabled, only the background is highlighted.
 ): HRESULT; stdcall;





const
  /////
  //      uOptions may nclude only ETO_CLIPPED or ETO_OPAQUE.
  /////   ScriptIsComplex
  //
  //      Determines whether a Unicode string requires complex script processing
  //
  //      The dwFlags parameter may include the following requests
  //
  SIC_COMPLEX     = 1;   // Treat complex script letters as complex
  SIC_ASCIIDIGIT  = 2;   // Treat digits U+0030 through U+0039 as complex
  SIC_NEUTRAL     = 4;   // Treat neutrals as complex
  
//
//      SIC_COMPLEX: Should normally set. Causes complex script letters to
//      be treated as complex.
//
//      SIC_ASCIIDIGIT: Set this flag if the string would be displayed with
//      digit substitution enabled. If you are following the users NLS
//      settings using the ScriptRecordDigitSubstitution API, you can pass
//      scriptDigitSubstitute.DigitSubstitute != SCRIPT_DIGITSUBSTITUTE_NONE.
//
//      SIC_NEUTRAL: Set this flag if you may be displaying the string with
//      right-to-left reading order. When this flag is set, neutral characters
//      are considered as complex.
//
//
//      Returns S_OK     if string requires complex script processing,
//              S_FALSE  if string contains only characters laid out side by
//                       side from left to right.


function ScriptIsComplex(
    const pwcInChars: PWideChar;     //In  String to be tested
    cInChars: Integer;               //In  Length in characters
    dwFlags: DWORD                   //In  Flags (see above)
 ): HRESULT; stdcall;





type
  /////   ScriptRecordDigitSubstitution
  //
  //      Reads NLS native digit and digit substitution settings and records
  //      them in the SCRIPT_DIGITSUBSTITUTE structure.
  //
  //
  PScriptDigitSubstitute = ^TScriptDigitSubstitute;
  tag_SCRIPT_DIGITSUBSTITUTE = packed record
    NationalDigitLanguage: Word    {:16};   // Language for native substitution
    TraditionalDigitLanguage: Word {:16};   // Language for traditional substitution
    DigitSubstitute: Byte          {:8};    // Substitution type
    bReserved: Byte;
    wReserved: Word;
    dwReserved: DWORD;                       // Reserved
  end;
(*  NationalDigitLanguage: DWORD    {:16};   // Language for native substitution
    TraditionalDigitLanguage: DWORD {:16};   // Language for traditional substitution
    DigitSubstitute: DWORD          {:8};    // Substitution type
    dwReserved: DWORD;                       // Reserved
  end; *)
  {$EXTERNALSYM tag_SCRIPT_DIGITSUBSTITUTE}
  SCRIPT_DIGITSUBSTITUTE = tag_SCRIPT_DIGITSUBSTITUTE;
  {$EXTERNALSYM SCRIPT_DIGITSUBSTITUTE}
  TScriptDigitSubstitute = tag_SCRIPT_DIGITSUBSTITUTE;
  
//
//
//p     NationalDigitLanguage: Standard digits for the selected locale as
//          defined by the countries standard setting authority.
//
//p     TraditionalDigitLangauge: Digits originally used with the locales
//          script.
//
//p     DigitSubstitute: Selects between None, Context, National and
//          Traditional. See ScriptApplyDigitSubstitution below for
//          constant definitions.
//
//      Although most complex scripts have their own associated digits, many
//      countries using those scripts use western (so called
//      'Arabic') digits as their standard. NationalDigitLanguage reflects the
//      digits used as standard, and is set from
//      the NLS data for the locale.
//      On Windows 2000 the national digit langauge can be
//      adjusted to any digit script with the control panel/regional
//      options/numbers/Standard digits listbox.
//
//      The TraditionalDigitLanguage for a locale is derived directly from the
//      script used by that locale.


function ScriptRecordDigitSubstitution(
    Locale: LCID;                     // In   LOCALE_USER_DEFAULT or desired locale
    out psds: TScriptDigitSubstitute  // Out  Digit substitution settings
 ): HRESULT; stdcall;





/////
//p     Locale: NLS locale to be queried. Should usually be set to
//          LOCALE_USER_DEFAULT. Alternatively may be passed as a locale
//          combined with LOCALE_NOUSEROVERRIDE to obtain default settings
//          for a given locale. Note that context digit substitution is
//          supported only in ARABIC and FARSI locales. In other locales,
//          context digit is mapped to no substitution.
//
//p     psds: Pointer to SCRIPT_DIGITSUBSTITUTE. This structure may be passed
//          later to ScriptApplyDigitSubstitution.
//
//p     returns: E_INVALIDARG if Locale is invalid or not installed. E_POINTER
//          if psds is NULL. Otherwise S_OK.
//
//      For performance reasons, you should not call
//      ScriptRecordDigitSubstitution frequently. In particular it would be a
//      considerable overhead to call it every time you call ScriptItemize
//      or ScriptStringAnalyse.
//
//      Instead, you may choose to save the SCRIPT_DIGITSUBSTITUTE
//      structure, and update it only when you receive a
//      WM_SETTINGCHANGE message or when a RegNotifyChangeKeyValue
//      call in a dedicated thread indicates a change in the registry
//      under HKCU\Control Panel\\International.
//
//      The normal way to call this function is simply
//
//c     SCRIPT_DIGITSUBSTITUTE sds;
//c     ScriptRecordDigitSubstitution(LOCALE_USER_DEFAULT, &sds);
//
//      Then every time you itemize, you'd use the results like this:
//
//c     SCRIPT_CONTROL  sc = {0};
//c     SCRIPT_STATE    ss = {0};
//
//c     ScriptApplyDigitSubstitution(&sds, &sc, &ss);
//
//
/////   ScriptApplyDigitSubstitution
//
//      Aplies the digit substitution settings recorded in a
//      SCRIPT_DIGIT_SUBSTITUTE structure to the SCRIPT_CONTROL and
//      SCRIPT_STATE structures.
//
//      The DigitSubstitute field of the SCRIPT_DIGITSUBSTITUTE structure
//      is normally set by ScriptRecordDigitSubstitution, however it may
//      be replaced by any one of the following values:
//
//
const
  SCRIPT_DIGITSUBSTITUTE_CONTEXT      = 0;  // Substitute to match preceeding letters
  SCRIPT_DIGITSUBSTITUTE_NONE         = 1;  // No substitution
  SCRIPT_DIGITSUBSTITUTE_NATIONAL     = 2;  // Substitute with official national digits
  SCRIPT_DIGITSUBSTITUTE_TRADITIONAL  = 3;  // Substitute with traditional digits of the locale
//
//
//p     SCRIPT_DIGITSUBSTITUTE_CONTEXT: Digits U+0030 - U+0039 will be
//          substituted according to the language of prior letters. Before
//          any letters, digits will be substituted according to the
//          TraditionalDigitLangauge field of the SCRIPT_DIGIT_SUBSTITUTE
//          structure. This field is normally set to the primary language of
//          the Locale passed to ScriptRecordDigitSubstitution.
//
//p     SCRIPT_DIGITSUBSTITUTE_NONE: Digits will not be substituted. Unicode
//          values U+0030 to U+0039 will be displayed with Arabic (i.e.
//          Western) numerals.
//
//p     SCRIPT_DIGITSUBSTITUTE_NATIONAL: Digits U+0030 - U+0039 will be
//          substituted according to the NationalDigitLangauge field of
//          the SCRIPT_DIGIT_SUBSTITUTE structure. This field is normally
//          set to the national digits returned for the NLS LCTYPE
//          LOCALE_SNATIVEDIGITS by ScriptRecordDigitSubstitution.
//
//p     SCRIPT_DIGITSUBSTITUTE_TRADITIONAL: Digits U+0030 - U+0039 will be
//          substituted according to the TraditionalDigitLangauge field of
//          the SCRIPT_DIGIT_SUBSTITUTE structure. This field is normally
//          set to the primary language of the Locale passed to
//          ScriptRecordDigitSubstitution.


function ScriptApplyDigitSubstitution(
    const psds: PScriptDigitSubstitute;   // In   Digit substitution settings
    psc: PScriptControl;                  // Out  Script control structure
    pss: PScriptState                     // Out  Script state structure
 ): HRESULT; stdcall;






/////
//p     psds: Pointer to SCRIPT_DIGITSUBSTITUTE structure recorded earlier.
//          If NULL, ScriptApplyDigitSubstitution calls
//          ScriptRecordDigitSubstitution with LOCALE_USER_DEFAULT.
//
//p     psc: SCRIPT_CONTROL structure. The scContextDigits and uDefaultLanguage
//          fields will be updated.
//
//p     pss: SCRIPT_CONTROL structure. The ssDigitSubstitute field will be
//          updated.
//
//p     returns: E_INVALIDARG if the DigitSubstitute field of the
//          SCRIPT_DIGITSUBSTITUTE structure is unrecognised, else S_OK;

var
  Usp10IsInstalled: Boolean;

implementation

uses
  SysUtils;

const
  Usp10DLL = 'usp10.dll';

var
  Usp10DllModule: HMODULE = 0;

function GetUsp10DllModule: HMODULE;
begin
  if Usp10DllModule = 0 then
  begin
    Usp10DllModule := SafeLoadLibrary(Usp10DLL);
    if Usp10DllModule <= HINSTANCE_ERROR then
      Usp10DllModule := 0;
  end;
  Result := Usp10DllModule;
end;

procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
begin
  if not Assigned(P) then
  begin
    P := Pointer(GetProcAddress(Usp10DllModule, PAnsiChar(AnsiString(ProcName))));
    if not Assigned(P) or (Usp10DllModule = 0) then
      RaiseLastOSError;
  end;
end;

var
  _ScriptFreeCache: Pointer = nil;

function ScriptFreeCache;
begin
  if _ScriptFreeCache = nil then
    GetProcedureAddress(_ScriptFreeCache, Usp10DLL, 'ScriptFreeCache');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptFreeCache]
  end;
end;

var
  _ScriptItemize: Pointer = nil;

function ScriptItemize;
begin
  if _ScriptItemize = nil then
    GetProcedureAddress(_ScriptItemize, Usp10DLL, 'ScriptItemize');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptItemize]
  end;
end;

var
  _ScriptLayout: Pointer = nil;

function ScriptLayout;
begin
  if _ScriptLayout = nil then
    GetProcedureAddress(_ScriptLayout, Usp10DLL, 'ScriptLayout');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptLayout]
  end;
end;

var
  _ScriptShape: Pointer = nil;

function ScriptShape;
begin
  if _ScriptShape = nil then
    GetProcedureAddress(_ScriptShape, Usp10DLL, 'ScriptShape');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptShape]
  end;
end;

var
  _ScriptPlace: Pointer = nil;

function ScriptPlace;
begin
  if _ScriptPlace = nil then
    GetProcedureAddress(_ScriptPlace, Usp10DLL, 'ScriptPlace');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptPlace]
  end;
end;

var
  _ScriptTextOut: Pointer = nil;

function ScriptTextOut;
begin
  if _ScriptTextOut = nil then
    GetProcedureAddress(_ScriptTextOut, Usp10DLL, 'ScriptTextOut');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptTextOut]
  end;
end;

var
  _ScriptJustify: Pointer = nil;

function ScriptJustify;
begin
  if _ScriptJustify = nil then
    GetProcedureAddress(_ScriptJustify, Usp10DLL, 'ScriptJustify');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptJustify]
  end;
end;

var
  _ScriptBreak: Pointer = nil;

function ScriptBreak;
begin
  if _ScriptBreak = nil then
    GetProcedureAddress(_ScriptBreak, Usp10DLL, 'ScriptBreak');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptBreak]
  end;
end;

var
  _ScriptCPtoX: Pointer = nil;

function ScriptCPtoX;
begin
  if _ScriptCPtoX = nil then
    GetProcedureAddress(_ScriptCPtoX, Usp10DLL, 'ScriptCPtoX');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptCPtoX]
  end;
end;

var
  _ScriptXtoCP: Pointer = nil;

function ScriptXtoCP;
begin
  if _ScriptXtoCP = nil then
    GetProcedureAddress(_ScriptXtoCP, Usp10DLL, 'ScriptXtoCP');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptXtoCP]
  end;
end;

var
  _ScriptGetLogicalWidths: Pointer = nil;

function ScriptGetLogicalWidths;
begin
  if _ScriptGetLogicalWidths = nil then
    GetProcedureAddress(_ScriptGetLogicalWidths, Usp10DLL, 'ScriptGetLogicalWidths');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptGetLogicalWidths]
  end;
end;

var
  _ScriptApplyLogicalWidth: Pointer = nil;

function ScriptApplyLogicalWidth;
begin
  if _ScriptApplyLogicalWidth = nil then
    GetProcedureAddress(_ScriptApplyLogicalWidth, Usp10DLL, 'ScriptApplyLogicalWidth');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptApplyLogicalWidth]
  end;
end;

var
  _ScriptGetCMap: Pointer = nil;

function ScriptGetCMap;
begin
  if _ScriptGetCMap = nil then
    GetProcedureAddress(_ScriptGetCMap, Usp10DLL, 'ScriptGetCMap');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptGetCMap]
  end;
end;

var
  _ScriptGetGlyphABCWidth: Pointer = nil;

function ScriptGetGlyphABCWidth;
begin
  if _ScriptGetGlyphABCWidth = nil then
    GetProcedureAddress(_ScriptGetGlyphABCWidth, Usp10DLL, 'ScriptGetGlyphABCWidth');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptGetGlyphABCWidth]
  end;
end;

var
  _ScriptGetProperties: Pointer = nil;

function ScriptGetProperties;
begin
  if _ScriptGetProperties = nil then
    GetProcedureAddress(_ScriptGetProperties, Usp10DLL, 'ScriptGetProperties');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptGetProperties]
  end;
end;

var
  _ScriptGetFontProperties: Pointer = nil;

function ScriptGetFontProperties;
begin
  if _ScriptGetFontProperties = nil then
    GetProcedureAddress(_ScriptGetFontProperties, Usp10DLL, 'ScriptGetFontProperties');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptGetFontProperties]
  end;
end;

var
  _ScriptCacheGetHeight: Pointer = nil;

function ScriptCacheGetHeight;
begin
  if _ScriptCacheGetHeight = nil then
    GetProcedureAddress(_ScriptCacheGetHeight, Usp10DLL, 'ScriptCacheGetHeight');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptCacheGetHeight]
  end;
end;

var
  _ScriptStringAnalyse: Pointer = nil;

function ScriptStringAnalyse;
begin
  if _ScriptStringAnalyse = nil then
    GetProcedureAddress(_ScriptStringAnalyse, Usp10DLL, 'ScriptStringAnalyse');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptStringAnalyse]
  end;
end;

var
  _ScriptStringFree: Pointer = nil;

function ScriptStringFree;
begin
  if _ScriptStringFree = nil then
    GetProcedureAddress(_ScriptStringFree, Usp10DLL, 'ScriptStringFree');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptStringFree]
  end;
end;

var
  _ScriptString_pSize: Pointer = nil;

function ScriptString_pSize;
begin
  if _ScriptString_pSize = nil then
    GetProcedureAddress(_ScriptString_pSize, Usp10DLL, 'ScriptString_pSize');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptString_pSize]
  end;
end;

var
  _ScriptString_pcOutChars: Pointer = nil;

function ScriptString_pcOutChars;
begin
  if _ScriptString_pcOutChars = nil then
    GetProcedureAddress(_ScriptString_pcOutChars, Usp10DLL, 'ScriptString_pcOutChars');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptString_pcOutChars]
  end;
end;

var
  _ScriptString_pLogAttr: Pointer = nil;

function ScriptString_pLogAttr;
begin
  if _ScriptString_pLogAttr = nil then
    GetProcedureAddress(_ScriptString_pLogAttr, Usp10DLL, 'ScriptString_pLogAttr');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptString_pLogAttr]
  end;
end;

var
  _ScriptStringGetOrder: Pointer = nil;

function ScriptStringGetOrder;
begin
  if _ScriptStringGetOrder = nil then
    GetProcedureAddress(_ScriptStringGetOrder, Usp10DLL, 'ScriptStringGetOrder');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptStringGetOrder]
  end;
end;

var
  _ScriptStringCPtoX: Pointer = nil;

function ScriptStringCPtoX;
begin
  if _ScriptStringCPtoX = nil then
    GetProcedureAddress(_ScriptStringCPtoX, Usp10DLL, 'ScriptStringCPtoX');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptStringCPtoX]
  end;
end;

var
  _ScriptStringXtoCP: Pointer = nil;

function ScriptStringXtoCP;
begin
  if _ScriptStringXtoCP = nil then
    GetProcedureAddress(_ScriptStringXtoCP, Usp10DLL, 'ScriptStringXtoCP');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptStringXtoCP]
  end;
end;

var
  _ScriptStringGetLogicalWidths: Pointer = nil;

function ScriptStringGetLogicalWidths;
begin
  if _ScriptStringGetLogicalWidths = nil then
    GetProcedureAddress(_ScriptStringGetLogicalWidths, Usp10DLL, 'ScriptStringGetLogicalWidths');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptStringGetLogicalWidths]
  end;
end;

var
  _ScriptStringValidate: Pointer = nil;

function ScriptStringValidate;
begin
  if _ScriptStringValidate = nil then
    GetProcedureAddress(_ScriptStringValidate, Usp10DLL, 'ScriptStringValidate');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptStringValidate]
  end;
end;

var
  _ScriptStringOut: Pointer = nil;

function ScriptStringOut;
begin
  if _ScriptStringOut = nil then
    GetProcedureAddress(_ScriptStringOut, Usp10DLL, 'ScriptStringOut');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptStringOut]
  end;
end;

var
  _ScriptIsComplex: Pointer = nil;

function ScriptIsComplex;
begin
  if _ScriptIsComplex = nil then
    GetProcedureAddress(_ScriptIsComplex, Usp10DLL, 'ScriptIsComplex');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptIsComplex]
  end;
end;

var
  _ScriptRecordDigitSubstitution: Pointer = nil;

function ScriptRecordDigitSubstitution;
begin
  if _ScriptRecordDigitSubstitution = nil then
    GetProcedureAddress(_ScriptRecordDigitSubstitution, Usp10DLL, 'ScriptRecordDigitSubstitution');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptRecordDigitSubstitution]
  end;
end;

var
  _ScriptApplyDigitSubstitution: Pointer = nil;

function ScriptApplyDigitSubstitution;
begin
  if _ScriptApplyDigitSubstitution = nil then
    GetProcedureAddress(_ScriptApplyDigitSubstitution, Usp10DLL, 'ScriptApplyDigitSubstitution');
  asm
    MOV ESP, EBP
    POP EBP
    JMP [_ScriptApplyDigitSubstitution]
  end;
end;

initialization
  Usp10DllModule := GetUsp10DllModule;
  Usp10IsInstalled := Usp10DllModule <> 0;

finalization
  if Usp10DllModule <> 0 then FreeLibrary(Usp10DllModule);

end.
