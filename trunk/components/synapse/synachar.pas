{==============================================================================|
| Project : Ararat Synapse                                       | 005.002.003 |
|==============================================================================|
| Content: Charset conversion support                                          |
|==============================================================================|
| Copyright (c)1999-2012, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2000-2012.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{: @abstract(Charset conversion support)
This unit contains a routines for lot of charset conversions.

It using built-in conversion tables or external Iconv library. Iconv is used
 when needed conversion is known by Iconv library. When Iconv library is not
 found or Iconv not know requested conversion, then are internal routines used
 for conversion. (You can disable Iconv support from your program too!)

Internal routines knows all major charsets for Europe or America. For East-Asian
 charsets you must use Iconv library!
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$H+}

//old Delphi does not have MSWINDOWS define.
{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit synachar;

interface

uses
{$IFNDEF MSWINDOWS}
  {$IFNDEF FPC}
  Libc,
  {$ENDIF}
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils,
  synautil, synacode, synaicnv;

type
  {:Type with all supported charsets.}
  TMimeChar = (ISO_8859_1, ISO_8859_2, ISO_8859_3, ISO_8859_4, ISO_8859_5,
    ISO_8859_6, ISO_8859_7, ISO_8859_8, ISO_8859_9, ISO_8859_10, ISO_8859_13,
    ISO_8859_14, ISO_8859_15, CP1250, CP1251, CP1252, CP1253, CP1254, CP1255,
    CP1256, CP1257, CP1258, KOI8_R, CP895, CP852, UCS_2, UCS_4, UTF_8, UTF_7,
    UTF_7mod, UCS_2LE, UCS_4LE,
    //next is supported by Iconv only...
    UTF_16, UTF_16LE, UTF_32, UTF_32LE, C99, JAVA, ISO_8859_16, KOI8_U, KOI8_RU,
    CP862, CP866, MAC, MACCE, MACICE, MACCRO, MACRO, MACCYR, MACUK, MACGR, MACTU,
    MACHEB, MACAR, MACTH, ROMAN8, NEXTSTEP, ARMASCII, GEORGIAN_AC, GEORGIAN_PS,
    KOI8_T, MULELAO, CP1133, TIS620, CP874, VISCII, TCVN, ISO_IR_14, JIS_X0201,
    JIS_X0208, JIS_X0212, GB1988_80, GB2312_80, ISO_IR_165, ISO_IR_149, EUC_JP,
    SHIFT_JIS, CP932, ISO_2022_JP, ISO_2022_JP1, ISO_2022_JP2, GB2312, CP936,
    GB18030, ISO_2022_CN, ISO_2022_CNE, HZ, EUC_TW, BIG5, CP950, BIG5_HKSCS,
    EUC_KR, CP949, CP1361, ISO_2022_KR, CP737, CP775, CP853, CP855, CP857,
    CP858, CP860, CP861, CP863, CP864, CP865, CP869, CP1125);

  {:Set of any charsets.}
  TMimeSetChar = set of TMimeChar;

const
  {:Set of charsets supported by Iconv library only.}
  IconvOnlyChars: set of TMimeChar = [UTF_16, UTF_16LE, UTF_32, UTF_32LE,
    C99, JAVA, ISO_8859_16, KOI8_U, KOI8_RU, CP862, CP866, MAC, MACCE, MACICE,
    MACCRO, MACRO, MACCYR, MACUK, MACGR, MACTU, MACHEB, MACAR, MACTH, ROMAN8,
    NEXTSTEP, ARMASCII, GEORGIAN_AC, GEORGIAN_PS, KOI8_T, MULELAO, CP1133,
    TIS620, CP874, VISCII, TCVN, ISO_IR_14, JIS_X0201, JIS_X0208, JIS_X0212,
    GB1988_80, GB2312_80, ISO_IR_165, ISO_IR_149, EUC_JP, SHIFT_JIS, CP932,
    ISO_2022_JP, ISO_2022_JP1, ISO_2022_JP2, GB2312, CP936, GB18030,
    ISO_2022_CN, ISO_2022_CNE, HZ, EUC_TW, BIG5, CP950, BIG5_HKSCS, EUC_KR,
    CP949, CP1361, ISO_2022_KR, CP737, CP775, CP853, CP855, CP857, CP858,
    CP860, CP861, CP863, CP864, CP865, CP869, CP1125];

  {:Set of charsets supported by internal routines only.}
  NoIconvChars: set of TMimeChar = [CP895, UTF_7mod];

  {:null character replace table. (Usable for disable charater replacing.)}
  Replace_None: array[0..0] of Word =
    (0);

  {:Character replace table for remove Czech diakritics.}
  Replace_Czech: array[0..59] of Word =
    (
      $00E1, $0061,
      $010D, $0063,
      $010F, $0064,
      $010E, $0044,
      $00E9, $0065,
      $011B, $0065,
      $00ED, $0069,
      $0148, $006E,
      $00F3, $006F,
      $0159, $0072,
      $0161, $0073,
      $0165, $0074,
      $00FA, $0075,
      $016F, $0075,
      $00FD, $0079,
      $017E, $007A,
      $00C1, $0041,
      $010C, $0043,
      $00C9, $0045,
      $011A, $0045,
      $00CD, $0049,
      $0147, $004E,
      $00D3, $004F,
      $0158, $0052,
      $0160, $0053,
      $0164, $0054,
      $00DA, $0055,
      $016E, $0055,
      $00DD, $0059,
      $017D, $005A
    );

var
  {:By this you can generally disable/enable Iconv support.}
  DisableIconv: Boolean = False;

  {:Default set of charsets for @link(IdealCharsetCoding) function.}
  IdealCharsets: TMimeSetChar =
    [ISO_8859_1, ISO_8859_2, ISO_8859_3, ISO_8859_4, ISO_8859_5,
    ISO_8859_6, ISO_8859_7, ISO_8859_8, ISO_8859_9, ISO_8859_10,
    KOI8_R, KOI8_U
    {$IFNDEF CIL} //error URW778 ??? :-O
    , GB2312, EUC_KR, ISO_2022_JP, EUC_TW
    {$ENDIF}
    ];

{==============================================================================}
{:Convert Value from one charset to another. See: @link(CharsetConversionEx)}
function CharsetConversion(const Value: AnsiString; CharFrom: TMimeChar;
  CharTo: TMimeChar): AnsiString;

{:Convert Value from one charset to another with additional character conversion.
see: @link(Replace_None) and @link(Replace_Czech)}
function CharsetConversionEx(const Value: AnsiString; CharFrom: TMimeChar;
  CharTo: TMimeChar; const TransformTable: array of Word): AnsiString;

{:Convert Value from one charset to another with additional character conversion.
 This funtion is similar to @link(CharsetConversionEx), but you can disable
 transliteration of unconvertible characters.}
function CharsetConversionTrans(Value: AnsiString; CharFrom: TMimeChar;
  CharTo: TMimeChar; const TransformTable: array of Word; Translit: Boolean): AnsiString;

{:Returns charset used by operating system.}
function GetCurCP: TMimeChar;

{:Returns charset used by operating system as OEM charset. (in Windows DOS box,
 for example)}
function GetCurOEMCP: TMimeChar;

{:Converting string with charset name to TMimeChar.}
function GetCPFromID(Value: AnsiString): TMimeChar;

{:Converting TMimeChar to string with name of charset.}
function GetIDFromCP(Value: TMimeChar): AnsiString;

{:return @true when value need to be converted. (It is not 7-bit ASCII)}
function NeedCharsetConversion(const Value: AnsiString): Boolean;

{:Finding best target charset from set of TMimeChars with minimal count of
 unconvertible characters.}
function IdealCharsetCoding(const Value: AnsiString; CharFrom: TMimeChar;
  CharTo: TMimeSetChar): TMimeChar;

{:Return BOM (Byte Order Mark) for given unicode charset.}
function GetBOM(Value: TMimeChar): AnsiString;

{:Convert binary string with unicode content to WideString.}
function StringToWide(const Value: AnsiString): WideString;

{:Convert WideString to binary string with unicode content.}
function WideToString(const Value: WideString): AnsiString;

{==============================================================================}
implementation

//character transcoding tables X to UCS-2
{
//dummy table
$0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
$0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
$0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
$0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
$00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
$00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
$00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
$00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
$00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
$00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
$00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
$00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
$00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
$00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
$00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
$00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF
}

const

{Latin-1
  Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
  Irish, Italian, Norwegian, Portuguese, Spanish and Swedish.
}
  CharISO_8859_1: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF
    );

{Latin-2
  Albanian, Czech, English, German, Hungarian, Polish, Rumanian,
  Serbo-Croatian, Slovak, Slovene and Swedish.
}
  CharISO_8859_2: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7,
    $00A8, $0160, $015E, $0164, $0179, $00AD, $017D, $017B,
    $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7,
    $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
    $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7,
    $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7,
    $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7,
    $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9
    );

{Latin-3
  Afrikaans, Catalan, English, Esperanto, French, Galician,
  German, Italian, Maltese and Turkish.
}
  CharISO_8859_3: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0126, $02D8, $00A3, $00A4, $FFFD, $0124, $00A7,
    $00A8, $0130, $015E, $011E, $0134, $00AD, $FFFD, $017B,
    $00B0, $0127, $00B2, $00B3, $00B4, $00B5, $0125, $00B7,
    $00B8, $0131, $015F, $011F, $0135, $00BD, $FFFD, $017C,
    $00C0, $00C1, $00C2, $FFFD, $00C4, $010A, $0108, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $FFFD, $00D1, $00D2, $00D3, $00D4, $0120, $00D6, $00D7,
    $011C, $00D9, $00DA, $00DB, $00DC, $016C, $015C, $00DF,
    $00E0, $00E1, $00E2, $FFFD, $00E4, $010B, $0109, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $FFFD, $00F1, $00F2, $00F3, $00F4, $0121, $00F6, $00F7,
    $011D, $00F9, $00FA, $00FB, $00FC, $016D, $015D, $02D9
    );

{Latin-4
  Danish, English, Estonian, Finnish, German, Greenlandic,
  Lappish, Latvian, Lithuanian, Norwegian and Swedish.
}
  CharISO_8859_4: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0138, $0156, $00A4, $0128, $013B, $00A7,
    $00A8, $0160, $0112, $0122, $0166, $00AD, $017D, $00AF,
    $00B0, $0105, $02DB, $0157, $00B4, $0129, $013C, $02C7,
    $00B8, $0161, $0113, $0123, $0167, $014A, $017E, $014B,
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E,
    $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $012A,
    $0110, $0145, $014C, $0136, $00D4, $00D5, $00D6, $00D7,
    $00D8, $0172, $00DA, $00DB, $00DC, $0168, $016A, $00DF,
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F,
    $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $012B,
    $0111, $0146, $014D, $0137, $00F4, $00F5, $00F6, $00F7,
    $00F8, $0173, $00FA, $00FB, $00FC, $0169, $016B, $02D9
    );

{CYRILLIC
  Bulgarian, Bielorussian, English, Macedonian, Russian,
  Serbo-Croatian and Ukrainian.
}
  CharISO_8859_5: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0401, $0402, $0403, $0404, $0405, $0406, $0407,
    $0408, $0409, $040A, $040B, $040C, $00AD, $040E, $040F,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
    $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
    $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
    $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
    $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
    $2116, $0451, $0452, $0453, $0454, $0455, $0456, $0457,
    $0458, $0459, $045A, $045B, $045C, $00A7, $045E, $045F
    );

{ARABIC
}
  CharISO_8859_6: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $FFFD, $FFFD, $FFFD, $00A4, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $060C, $00AD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $061B, $FFFD, $FFFD, $FFFD, $061F,
    $FFFD, $0621, $0622, $0623, $0624, $0625, $0626, $0627,
    $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637,
    $0638, $0639, $063A, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647,
    $0648, $0649, $064A, $064B, $064C, $064D, $064E, $064F,
    $0650, $0651, $0652, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD
    );

{GREEK
}
  CharISO_8859_7: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $2018, $2019, $00A3, $FFFD, $FFFD, $00A6, $00A7,
    $00A8, $00A9, $FFFD, $00AB, $00AC, $00AD, $FFFD, $2015,
    $00B0, $00B1, $00B2, $00B3, $0384, $0385, $0386, $00B7,
    $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,
    $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
    $03A0, $03A1, $FFFD, $03A3, $03A4, $03A5, $03A6, $03A7,
    $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7,
    $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7,
    $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $FFFD
    );

{HEBREW
}
  CharISO_8859_8: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $FFFD, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $2017,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7,
    $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7,
    $05E8, $05E9, $05EA, $FFFD, $FFFD, $200E, $200F, $FFFD
    );

{Latin-5
  English, Finnish, French, German, Irish, Italian, Norwegian,
  Portuguese, Spanish, Swedish and Turkish.
}
  CharISO_8859_9: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7,
    $00A8, $0160, $015E, $0164, $0179, $00AD, $017D, $017B,
    $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7,
    $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
    $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF
    );

{Latin-6
  Danish, English, Estonian, Faeroese, Finnish, German, Greenlandic,
  Icelandic, Lappish, Latvian, Lithuanian, Norwegian and Swedish.
}
  CharISO_8859_10: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0112, $0122, $012A, $0128, $0136, $00A7,
    $013B, $0110, $0160, $0166, $017D, $00AD, $016A, $014A,
    $00B0, $0105, $0113, $0123, $012B, $0129, $0137, $00B7,
    $013C, $0111, $0161, $0167, $017E, $2015, $016B, $014B,
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E,
    $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $00CF,
    $00D0, $0145, $014C, $00D3, $00D4, $00D5, $00D6, $0168,
    $00D8, $0172, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F,
    $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $00EF,
    $00F0, $0146, $014D, $00F3, $00F4, $00F5, $00F6, $0169,
    $00F8, $0173, $00FA, $00FB, $00FC, $00FD, $00FE, $0138
    );

  CharISO_8859_13: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $201D, $00A2, $00A3, $00A4, $201E, $00A6, $00A7,
    $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
    $00B0, $00B1, $00B2, $00B3, $201C, $00B5, $00B6, $00B7,
    $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
    $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112,
    $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
    $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7,
    $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
    $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113,
    $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
    $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7,
    $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $2019
    );

  CharISO_8859_14: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $1E02, $1E03, $00A3, $010A, $010B, $1E0A, $00A7,
    $1E80, $00A9, $1E82, $1E0B, $1EF2, $00AD, $00AE, $0178,
    $1E1E, $1E1F, $0120, $0121, $1E40, $1E41, $00B6, $1E56,
    $1E81, $1E57, $1E83, $1E60, $1EF3, $1E84, $1E85, $1E61,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $0174, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $1E6A,
    $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $0176, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $0175, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $1E6B,
    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $0177, $00FF
    );

  CharISO_8859_15: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $20AC, $00A5, $0160, $00A7,
    $0161, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $017D, $00B5, $00B6, $00B7,
    $017E, $00B9, $00BA, $00BB, $0152, $0153, $0178, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF
    );

{Eastern European
}
  CharCP_1250: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $FFFD, $201E, $2026, $2020, $2021,
    $FFFD, $2030, $0160, $2039, $015A, $0164, $017D, $0179,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $FFFD, $2122, $0161, $203A, $015B, $0165, $017E, $017A,
    $00A0, $02C7, $02D8, $0141, $00A4, $0104, $00A6, $00A7,
    $00A8, $00A9, $015E, $00AB, $00AC, $00AD, $00AE, $017B,
    $00B0, $00B1, $02DB, $0142, $00B4, $00B5, $00B6, $00B7,
    $00B8, $0105, $015F, $00BB, $013D, $02DD, $013E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
    $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7,
    $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7,
    $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7,
    $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9
    );

{Cyrillic
}
  CharCP_1251: array[128..255] of Word =
  (
    $0402, $0403, $201A, $0453, $201E, $2026, $2020, $2021,
    $20AC, $2030, $0409, $2039, $040A, $040C, $040B, $040F,
    $0452, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $FFFD, $2122, $0459, $203A, $045A, $045C, $045B, $045F,
    $00A0, $040E, $045E, $0408, $00A4, $0490, $00A6, $00A7,
    $0401, $00A9, $0404, $00AB, $00AC, $00AD, $00AE, $0407,
    $00B0, $00B1, $0406, $0456, $0491, $00B5, $00B6, $00B7,
    $0451, $2116, $0454, $00BB, $0458, $0405, $0455, $0457,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
    $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
    $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
    $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
    $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F
    );

{Latin-1 (US, Western Europe)
}
  CharCP_1252: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0160, $2039, $0152, $FFFD, $017D, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $0161, $203A, $0153, $FFFD, $017E, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF
    );

{Greek
}
  CharCP_1253: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $FFFD, $2030, $FFFD, $2039, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $FFFD, $2122, $FFFD, $203A, $FFFD, $FFFD, $FFFD, $FFFD,
    $00A0, $0385, $0386, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $FFFD, $00AB, $00AC, $00AD, $00AE, $2015,
    $00B0, $00B1, $00B2, $00B3, $0384, $00B5, $00B6, $00B7,
    $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,
    $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
    $03A0, $03A1, $FFFD, $03A3, $03A4, $03A5, $03A6, $03A7,
    $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7,
    $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7,
    $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $FFFD
    );

{Turkish
}
  CharCP_1254: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0160, $2039, $0152, $FFFD, $FFFD, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $0161, $203A, $0153, $FFFD, $FFFD, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF
    );

{Hebrew
}
  CharCP_1255: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $FFFD, $2039, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $FFFD, $203A, $FFFD, $FFFD, $FFFD, $FFFD,
    $00A0, $00A1, $00A2, $00A3, $20AA, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $00BF,
    $05B0, $05B1, $05B2, $05B3, $05B4, $05B5, $05B6, $05B7,
    $05B8, $05B9, $FFFD, $05BB, $05BC, $05BD, $05BE, $05BF,
    $05C0, $05C1, $05C2, $05C3, $05F0, $05F1, $05F2, $05F3,
    $05F4, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7,
    $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7,
    $05E8, $05E9, $05EA, $FFFD, $FFFD, $200E, $200F, $FFFD
    );

{Arabic
}
  CharCP_1256: array[128..255] of Word =
  (
    $20AC, $067E, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0679, $2039, $0152, $0686, $0698, $0688,
    $06AF, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $06A9, $2122, $0691, $203A, $0153, $200C, $200D, $06BA,
    $00A0, $060C, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $06BE, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $061B, $00BB, $00BC, $00BD, $00BE, $061F,
    $06C1, $0621, $0622, $0623, $0624, $0625, $0626, $0627,
    $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $00D7,
    $0637, $0638, $0639, $063A, $0640, $0641, $0642, $0643,
    $00E0, $0644, $00E2, $0645, $0646, $0647, $0648, $00E7,
    $00E8, $00E9, $00EA, $00EB, $0649, $064A, $00EE, $00EF,
    $064B, $064C, $064D, $064E, $00F4, $064F, $0650, $00F7,
    $0651, $00F9, $0652, $00FB, $00FC, $200E, $200F, $06D2
    );

{Baltic
}
  CharCP_1257: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $FFFD, $201E, $2026, $2020, $2021,
    $FFFD, $2030, $FFFD, $2039, $FFFD, $00A8, $02C7, $00B8,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $FFFD, $2122, $FFFD, $203A, $FFFD, $00AF, $02DB, $FFFD,
    $00A0, $FFFD, $00A2, $00A3, $00A4, $FFFD, $00A6, $00A7,
    $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
    $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112,
    $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
    $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7,
    $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
    $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113,
    $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
    $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7,
    $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $02D9
    );

{Vietnamese
}
  CharCP_1258: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $FFFD, $2039, $0152, $FFFD, $FFFD, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $FFFD, $203A, $0153, $FFFD, $FFFD, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $0102, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $0300, $00CD, $00CE, $00CF,
    $0110, $00D1, $0309, $00D3, $00D4, $01A0, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $01AF, $0303, $00DF,
    $00E0, $00E1, $00E2, $0103, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $0301, $00ED, $00EE, $00EF,
    $0111, $00F1, $0323, $00F3, $00F4, $01A1, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $01B0, $20AB, $00FF
    );

{Cyrillic
}
  CharKOI8_R: array[128..255] of Word =
  (
    $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524,
    $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
    $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248,
    $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
    $2550, $2551, $2552, $0451, $2553, $2554, $2555, $2556,
    $2557, $2558, $2559, $255A, $255B, $255C, $255D, $255E,
    $255F, $2560, $2561, $0401, $2562, $2563, $2564, $2565,
    $2566, $2567, $2568, $2569, $256A, $256B, $256C, $00A9,
    $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433,
    $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
    $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432,
    $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
    $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413,
    $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
    $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412,
    $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A
    );

{Czech (Kamenicky)
}
  CharCP_895: array[128..255] of Word =
  (
    $010C, $00FC, $00E9, $010F, $00E4, $010E, $0164, $010D,
    $011B, $011A, $0139, $00CD, $013E, $013A, $00C4, $00C1,
    $00C9, $017E, $017D, $00F4, $00F6, $00D3, $016F, $00DA,
    $00FD, $00D6, $00DC, $0160, $013D, $00DD, $0158, $0165,
    $00E1, $00ED, $00F3, $00FA, $0148, $0147, $016E, $00D4,
    $0161, $0159, $0155, $0154, $00BC, $00A7, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
    $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
    $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
    $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $03B2, $0393, $03C0, $03A3, $03C3, $03BC, $03C4,
    $03A6, $0398, $03A9, $03B4, $221E, $2205, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
    $2218, $00B7, $2219, $221A, $207F, $00B2, $25A0, $00A0
    );

{Eastern European
}
  CharCP_852: array[128..255] of Word =
  (
    $00C7, $00FC, $00E9, $00E2, $00E4, $016F, $0107, $00E7,
    $0142, $00EB, $0150, $0151, $00EE, $0179, $00C4, $0106,
    $00C9, $0139, $013A, $00F4, $00F6, $013D, $013E, $015A,
    $015B, $00D6, $00DC, $0164, $0165, $0141, $00D7, $010D,
    $00E1, $00ED, $00F3, $00FA, $0104, $0105, $017D, $017E,
    $0118, $0119, $00AC, $017A, $010C, $015F, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $011A,
    $015E, $2563, $2551, $2557, $255D, $017B, $017C, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $0102, $0103,
    $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $0111, $0110, $010E, $00CB, $010F, $0147, $00CD, $00CE,
    $011B, $2518, $250C, $2588, $2584, $0162, $016E, $2580,
    $00D3, $00DF, $00D4, $0143, $0144, $0148, $0160, $0161,
    $0154, $00DA, $0155, $0170, $00FD, $00DD, $0163, $00B4,
    $00AD, $02DD, $02DB, $02C7, $02D8, $00A7, $00F7, $00B8,
    $00B0, $00A8, $02D9, $0171, $0158, $0159, $25A0, $00A0
    );

{==============================================================================}
type
  TIconvChar = record
    Charset: TMimeChar;
    CharName: string;
  end;
  TIconvArr = array [0..112] of TIconvChar;

const
  NotFoundChar = '_';

var
  SetTwo: set of TMimeChar = [UCS_2, UCS_2LE, UTF_7, UTF_7mod];
  SetFour: set of TMimeChar = [UCS_4, UCS_4LE, UTF_8];
  SetLE: set of TMimeChar = [UCS_2LE, UCS_4LE];

  IconvArr: TIconvArr;

{==============================================================================}
function FindIconvID(const Value, Charname: string): Boolean;
var
  s: string;
begin
  Result := True;
  //exact match
  if Value = Charname then
    Exit;
  //Value is on begin of charname
  s := Value + ' ';
  if s = Copy(Charname, 1, Length(s)) then
    Exit;
  //Value is on end of charname
  s := ' ' + Value;
  if s = Copy(Charname, Length(Charname) - Length(s) + 1, Length(s)) then
    Exit;
  //value is somewhere inside charname
  if Pos( s + ' ', Charname) > 0 then
    Exit;
  Result := False;
end;

function GetCPFromIconvID(Value: AnsiString): TMimeChar;
var
  n: integer;
begin
  Result := ISO_8859_1;
  Value := UpperCase(Value);
  for n := 0 to High(IconvArr) do
    if FindIconvID(Value, IconvArr[n].Charname) then
    begin
      Result := IconvArr[n].Charset;
      Break;
    end;
end;

{==============================================================================}
function GetIconvIDFromCP(Value: TMimeChar): AnsiString;
var
  n: integer;
begin
  Result := 'ISO-8859-1';
  for n := 0 to High(IconvArr) do
    if IconvArr[n].Charset = Value then
    begin
      Result := Separateleft(IconvArr[n].Charname, ' ');
      Break;
    end;
end;

{==============================================================================}
function ReplaceUnicode(Value: Word; const TransformTable: array of Word): Word;
var
  n: integer;
begin
  if High(TransformTable) <> 0 then
    for n := 0 to High(TransformTable) do
      if not odd(n) then
        if TransformTable[n] = Value then
          begin
            Value := TransformTable[n+1];
            break;
          end;
  Result := Value;
end;

{==============================================================================}
procedure CopyArray(const SourceTable: array of Word;
  var TargetTable: array of Word);
var
  n: Integer;
begin
  for n := 0 to 127 do
    TargetTable[n] := SourceTable[n];
end;

{==============================================================================}
procedure GetArray(CharSet: TMimeChar; var Result: array of Word);
begin
  case CharSet of
    ISO_8859_2:
      CopyArray(CharISO_8859_2, Result);
    ISO_8859_3:
      CopyArray(CharISO_8859_3, Result);
    ISO_8859_4:
      CopyArray(CharISO_8859_4, Result);
    ISO_8859_5:
      CopyArray(CharISO_8859_5, Result);
    ISO_8859_6:
      CopyArray(CharISO_8859_6, Result);
    ISO_8859_7:
      CopyArray(CharISO_8859_7, Result);
    ISO_8859_8:
      CopyArray(CharISO_8859_8, Result);
    ISO_8859_9:
      CopyArray(CharISO_8859_9, Result);
    ISO_8859_10:
      CopyArray(CharISO_8859_10, Result);
    ISO_8859_13:
      CopyArray(CharISO_8859_13, Result);
    ISO_8859_14:
      CopyArray(CharISO_8859_14, Result);
    ISO_8859_15:
      CopyArray(CharISO_8859_15, Result);
    CP1250:
      CopyArray(CharCP_1250, Result);
    CP1251:
      CopyArray(CharCP_1251, Result);
    CP1252:
      CopyArray(CharCP_1252, Result);
    CP1253:
      CopyArray(CharCP_1253, Result);
    CP1254:
      CopyArray(CharCP_1254, Result);
    CP1255:
      CopyArray(CharCP_1255, Result);
    CP1256:
      CopyArray(CharCP_1256, Result);
    CP1257:
      CopyArray(CharCP_1257, Result);
    CP1258:
      CopyArray(CharCP_1258, Result);
    KOI8_R:
      CopyArray(CharKOI8_R, Result);
    CP895:
      CopyArray(CharCP_895, Result);
    CP852:
      CopyArray(CharCP_852, Result);
  else
      CopyArray(CharISO_8859_1, Result);
  end;
end;

{==============================================================================}
procedure ReadMulti(const Value: AnsiString; var Index: Integer; mb: Byte;
  var b1, b2, b3, b4: Byte; le: boolean);
Begin
  b1 := 0;
  b2 := 0;
  b3 := 0;
  b4 := 0;
  if Index < 0 then
    Index := 1;
  if mb > 4 then
    mb := 1;
  if (Index + mb - 1) <= Length(Value) then
  begin
    if le then
      Case mb Of
        1:
          b1 := Ord(Value[Index]);
        2:
          Begin
            b1 := Ord(Value[Index]);
            b2 := Ord(Value[Index + 1]);
          End;
        3:
          Begin
            b1 := Ord(Value[Index]);
            b2 := Ord(Value[Index + 1]);
            b3 := Ord(Value[Index + 2]);
          End;
        4:
          Begin
            b1 := Ord(Value[Index]);
            b2 := Ord(Value[Index + 1]);
            b3 := Ord(Value[Index + 2]);
            b4 := Ord(Value[Index + 3]);
          End;
      end
    else
      Case mb Of
        1:
          b1 := Ord(Value[Index]);
        2:
          Begin
            b2 := Ord(Value[Index]);
            b1 := Ord(Value[Index + 1]);
          End;
        3:
          Begin
            b3 := Ord(Value[Index]);
            b2 := Ord(Value[Index + 1]);
            b1 := Ord(Value[Index + 2]);
          End;
        4:
          Begin
            b4 := Ord(Value[Index]);
            b3 := Ord(Value[Index + 1]);
            b2 := Ord(Value[Index + 2]);
            b1 := Ord(Value[Index + 3]);
          End;
      end;
  end;
  Inc(Index, mb);
end;

{==============================================================================}
function WriteMulti(b1, b2, b3, b4: Byte; mb: Byte; le: boolean): AnsiString;
begin
  if mb > 4 then
    mb := 1;
  SetLength(Result, mb);
  if le then
    case mb Of
      1:
        Result[1] := AnsiChar(b1);
      2:
        begin
          Result[1] := AnsiChar(b1);
          Result[2] := AnsiChar(b2);
        end;
      3:
        begin
          Result[1] := AnsiChar(b1);
          Result[2] := AnsiChar(b2);
          Result[3] := AnsiChar(b3);
        end;
      4:
        begin
          Result[1] := AnsiChar(b1);
          Result[2] := AnsiChar(b2);
          Result[3] := AnsiChar(b3);
          Result[4] := AnsiChar(b4);
        end;
    end
  else
    case mb Of
      1:
        Result[1] := AnsiChar(b1);
      2:
        begin
          Result[2] := AnsiChar(b1);
          Result[1] := AnsiChar(b2);
        end;
      3:
        begin
          Result[3] := AnsiChar(b1);
          Result[2] := AnsiChar(b2);
          Result[1] := AnsiChar(b3);
        end;
      4:
        begin
          Result[4] := AnsiChar(b1);
          Result[3] := AnsiChar(b2);
          Result[2] := AnsiChar(b3);
          Result[1] := AnsiChar(b4);
        end;
    end;
end;

{==============================================================================}
function UTF8toUCS4(const Value: AnsiString): AnsiString;
var
  n, x, ul, m: Integer;
  s: AnsiString;
  w1, w2: Word;
begin
  Result := '';
  n := 1;
  while Length(Value) >= n do
  begin
    x := Ord(Value[n]);
    Inc(n);
    if x < 128 then
      Result := Result + WriteMulti(x, 0, 0, 0, 4, false)
    else
    begin
      m := 0;
      if (x and $E0) = $C0 then
        m := $1F;
      if (x and $F0) = $E0 then
        m := $0F;
      if (x and $F8) = $F0 then
        m := $07;
      if (x and $FC) = $F8 then
        m := $03;
      if (x and $FE) = $FC then
        m := $01;
      ul := x and m;
      s := IntToBin(ul, 0);
      while Length(Value) >= n do
      begin
        x := Ord(Value[n]);
        Inc(n);
        if (x and $C0) = $80 then
          s := s + IntToBin(x and $3F, 6)
        else
        begin
          Dec(n);
          Break;
        end;
      end;
      ul := BinToInt(s);
      w1 := ul div 65536;
      w2 := ul mod 65536;
      Result := Result + WriteMulti(Lo(w2), Hi(w2), Lo(w1), Hi(w1), 4, false);
    end;
  end;
end;

{==============================================================================}
function UCS4toUTF8(const Value: AnsiString): AnsiString;
var
  s, l, k: AnsiString;
  b1, b2, b3, b4: Byte;
  n, m, x, y: Integer;
  b: Byte;
begin
  Result := '';
  n := 1;
  while Length(Value) >= n do
  begin
    ReadMulti(Value, n, 4, b1, b2, b3, b4, false);
    if (b2 = 0) and (b3 = 0) and (b4 = 0) and (b1 < 128) then
      Result := Result + AnsiChar(b1)
    else
    begin
      x := (b1 + 256 * b2) + (b3 + 256 * b4) * 65536;
      l := IntToBin(x, 0);
      y := Length(l) div 6;
      s := '';
      for m := 1 to y do
      begin
        k := Copy(l, Length(l) - 5, 6);
        l := Copy(l, 1, Length(l) - 6);
        b := BinToInt(k) or $80;
        s := AnsiChar(b) + s;
      end;
      b := BinToInt(l);
      case y of
        5:
          b := b or $FC;
        4:
          b := b or $F8;
        3:
          b := b or $F0;
        2:
          b := b or $E0;
        1:
          b := b or $C0;
      end;
      s := AnsiChar(b) + s;
      Result := Result + s;
    end;
  end;
end;

{==============================================================================}
function UTF7toUCS2(const Value: AnsiString; Modified: Boolean): AnsiString;
var
  n, i: Integer;
  c: AnsiChar;
  s, t: AnsiString;
  shift: AnsiChar;
  table: String;
begin
  Result := '';
  n := 1;
  if modified then
  begin
    shift := '&';
    table := TableBase64mod;
  end
  else
  begin
    shift := '+';
    table := TableBase64;
  end;
  while Length(Value) >= n do
  begin
    c := Value[n];
    Inc(n);
    if c <> shift then
      Result := Result + WriteMulti(Ord(c), 0, 0, 0, 2, false)
    else
    begin
      s := '';
      while Length(Value) >= n do
      begin
        c := Value[n];
        Inc(n);
        if c = '-' then
          Break;
        if (c = '=') or (Pos(c, table) < 1) then
        begin
          Dec(n);
          Break;
        end;
        s := s + c;
      end;
      if s = '' then
        s := WriteMulti(Ord(shift), 0, 0, 0, 2, false)
      else
      begin
        if modified then
          t := DecodeBase64mod(s)
        else
          t := DecodeBase64(s);
        if not odd(length(t)) then
          s := t
        else
        begin //ill-formed sequence
          t := s;
          s := WriteMulti(Ord(shift), 0, 0, 0, 2, false);
          for i := 1 to length(t) do
            s := s + WriteMulti(Ord(t[i]), 0, 0, 0, 2, false);
        end;
      end;
      Result := Result + s;
    end;
  end;
end;

{==============================================================================}
function UCS2toUTF7(const Value: AnsiString; Modified: Boolean): AnsiString;
var
  s: AnsiString;
  b1, b2, b3, b4: Byte;
  n, m: Integer;
  shift: AnsiChar;
begin
  Result := '';
  n := 1;
  if modified then
    shift := '&'
  else
    shift := '+';
  while Length(Value) >= n do
  begin
    ReadMulti(Value, n, 2, b1, b2, b3, b4, false);
    if (b2 = 0) and (b1 < 128) then
      if AnsiChar(b1) = shift then
        Result := Result + shift + '-'
      else
        Result := Result + AnsiChar(b1)
    else
    begin
      s := AnsiChar(b2) + AnsiChar(b1);
      while Length(Value) >= n do
      begin
        ReadMulti(Value, n, 2, b1, b2, b3, b4, false);
        if (b2 = 0) and (b1 < 128) then
        begin
          Dec(n, 2);
          Break;
        end;
        s := s + AnsiChar(b2) + AnsiChar(b1);
      end;
      if modified then
        s := EncodeBase64mod(s)
      else
        s := EncodeBase64(s);
      m := Pos('=', s);
      if m > 0 then
        s := Copy(s, 1, m - 1);
      Result := Result + shift + s + '-';
    end;
  end;
end;

{==============================================================================}
function CharsetConversion(const Value: AnsiString; CharFrom: TMimeChar;
  CharTo: TMimeChar): AnsiString;
begin
  Result := CharsetConversionEx(Value, CharFrom, CharTo, Replace_None);
end;

{==============================================================================}
function CharsetConversionEx(const Value: AnsiString; CharFrom: TMimeChar;
  CharTo: TMimeChar; const TransformTable: array of Word): AnsiString;
begin
  Result := CharsetConversionTrans(Value, CharFrom, CharTo, TransformTable, True);
end;

{==============================================================================}

function InternalToUcs(const Value: AnsiString; Charfrom: TMimeChar): AnsiString;
var
  uni: Word;
  n: Integer;
  b1, b2, b3, b4: Byte;
  SourceTable: array[128..255] of Word;
  mbf: Byte;
  lef: Boolean;
  s: AnsiString;
begin
  if CharFrom = UTF_8 then
    s := UTF8toUCS4(Value)
  else
    if CharFrom = UTF_7 then
      s := UTF7toUCS2(Value, False)
    else
      if CharFrom = UTF_7mod then
        s := UTF7toUCS2(Value, True)
      else
        s := Value;
  GetArray(CharFrom, SourceTable);
  mbf := 1;
  if CharFrom in SetTwo then
    mbf := 2;
  if CharFrom in SetFour then
    mbf := 4;
  lef := CharFrom in SetLe;
  Result := '';
  n := 1;
  while Length(s) >= n do
  begin
    ReadMulti(s, n, mbf, b1, b2, b3, b4, lef);
    //handle BOM
    if (b3 = 0) and (b4 = 0) then
    begin
      if (b1 = $FE) and (b2 = $FF) then
      begin
        lef := not lef;
        continue;
      end;
      if (b1 = $FF) and (b2 = $FE) then
        continue;
    end;
    if mbf = 1 then
      if b1 > 127 then
      begin
        uni := SourceTable[b1];
        b1 := Lo(uni);
        b2 := Hi(uni);
      end;
    Result := Result + WriteMulti(b1, b2, b3, b4, 2, False);
  end;
end;

function CharsetConversionTrans(Value: AnsiString; CharFrom: TMimeChar;
  CharTo: TMimeChar; const TransformTable: array of Word; Translit: Boolean): AnsiString;
var
  uni: Word;
  n, m: Integer;
  b: Byte;
  b1, b2, b3, b4: Byte;
  TargetTable: array[128..255] of Word;
  mbt: Byte;
  let: Boolean;
  ucsstring, s, t: AnsiString;
  cd: iconv_t;
  f: Boolean;
  NotNeedTransform: Boolean;
  FromID, ToID: string;
begin
  NotNeedTransform := (High(TransformTable) = 0);
  if (CharFrom = CharTo) and NotNeedTransform then
  begin
    Result := Value;
    Exit;
  end;
  FromID := GetIDFromCP(CharFrom);
  ToID := GetIDFromCP(CharTo);
  cd := Iconv_t(-1);
  //do two-pass conversion. Transform to UCS-2 first.
  if not DisableIconv then
    cd := SynaIconvOpenIgnore('UCS-2BE', FromID);
  try
    if cd <> iconv_t(-1) then
      SynaIconv(cd, Value, ucsstring)
    else
      ucsstring := InternalToUcs(Value, CharFrom);
  finally
    SynaIconvClose(cd);
  end;
  //here we allways have ucstring with UCS-2 encoding
  //second pass... from UCS-2 to target encoding.
    if not DisableIconv then
      if translit then
        cd := SynaIconvOpenTranslit(ToID, 'UCS-2BE')
      else
        cd := SynaIconvOpenIgnore(ToID, 'UCS-2BE');
  try
    if (cd <> iconv_t(-1)) and NotNeedTransform then
    begin
      if CharTo = UTF_7 then
        ucsstring := ucsstring + #0 + '-';
      //when transformtable is not needed and Iconv know target charset,
      //do it fast by one call.
      SynaIconv(cd, ucsstring, Result);
      if CharTo = UTF_7 then
        Delete(Result, Length(Result), 1);
    end
    else
    begin
      GetArray(CharTo, TargetTable);
      mbt := 1;
      if CharTo in SetTwo then
        mbt := 2;
      if CharTo in SetFour then
        mbt := 4;
      let := CharTo in SetLe;
      b3 := 0;
      b4 := 0;
      Result := '';
      for n:= 0 to (Length(ucsstring) div 2) - 1 do
      begin
        s := Copy(ucsstring, n * 2 + 1, 2);
        b2 := Ord(s[1]);
        b1 := Ord(s[2]);
        uni := b2 * 256 + b1;
        if not NotNeedTransform then
        begin
          uni := ReplaceUnicode(uni, TransformTable);
          b1 := Lo(uni);
          b2 := Hi(uni);
          s[1] := AnsiChar(b2);
          s[2] := AnsiChar(b1);
        end;
        if cd <> iconv_t(-1) then
        begin
          if CharTo = UTF_7 then
            s := s + #0 + '-';
          SynaIconv(cd, s, t);
          if CharTo = UTF_7 then
            Delete(t, Length(t), 1);
          Result := Result + t;
        end
        else
        begin
          f := True;
          if mbt = 1 then
            if uni > 127 then
            begin
              f := False;
              b := 0;
              for m := 128 to 255 do
                if TargetTable[m] = uni then
                begin
                  b := m;
                  f := True;
                  Break;
                end;
              b1 := b;
              b2 := 0;
            end
            else
              b1 := Lo(uni);
          if not f then
            if translit then
            begin
              b1 := Ord(NotFoundChar);
              b2 := 0;
              f := True;
            end;
          if f then
            Result := Result + WriteMulti(b1, b2, b3, b4, mbt, let)
        end;
      end;
      if cd = iconv_t(-1) then
      begin
        if CharTo = UTF_7 then
          Result := UCS2toUTF7(Result, false);
        if CharTo = UTF_7mod then
          Result := UCS2toUTF7(Result, true);
        if CharTo = UTF_8 then
          Result := UCS4toUTF8(Result);
      end;
    end;
  finally
    SynaIconvClose(cd);
  end;
end;

{==============================================================================}
{$IFNDEF MSWINDOWS}

function GetCurCP: TMimeChar;
begin
  {$IFNDEF FPC}
  Result := GetCPFromID(nl_langinfo(_NL_CTYPE_CODESET_NAME));
  {$ELSE}
  //How to get system codepage without LIBC?
  Result := UTF_8;
{ TODO : Waiting for FPC 2.8 solution }
  {$ENDIF}
end;

function GetCurOEMCP: TMimeChar;
begin
  Result := GetCurCP;
end;

{$ELSE}

function CPToMimeChar(Value: Integer): TMimeChar;
begin
  case Value of
    437, 850, 20127:
      Result := ISO_8859_1; //I know, it is not ideal!
    737:
      Result := CP737;
    775:
      Result := CP775;
    852:
      Result := CP852;
    855:
      Result := CP855;
    857:
      Result := CP857;
    858:
      Result := CP858;
    860:
      Result := CP860;
    861:
      Result := CP861;
    862:
      Result := CP862;
    863:
      Result := CP863;
    864:
      Result := CP864;
    865:
      Result := CP865;
    866:
      Result := CP866;
    869:
      Result := CP869;
    874:
      Result := ISO_8859_15;
    895:
      Result := CP895;
    932:
      Result := CP932;
    936:
      Result := CP936;
    949:
      Result := CP949;
    950:
      Result := CP950;
    1200:
      Result := UCS_2LE;
    1201:
      Result := UCS_2;
    1250:
      Result := CP1250;
    1251:
      Result := CP1251;
    1253:
      Result := CP1253;
    1254:
      Result := CP1254;
    1255:
      Result := CP1255;
    1256:
      Result := CP1256;
    1257:
      Result := CP1257;
    1258:
      Result := CP1258;
    1361:
      Result := CP1361;
    10000:
      Result := MAC;
    10004:
      Result := MACAR;
    10005:
      Result := MACHEB;
    10006:
      Result := MACGR;
    10007:
      Result := MACCYR;
    10010:
      Result := MACRO;
    10017:
      Result := MACUK;
    10021:
      Result := MACTH;
    10029:
      Result := MACCE;
    10079:
      Result := MACICE;
    10081:
      Result := MACTU;
    10082:
      Result := MACCRO;
    12000:
      Result := UCS_4LE;
    12001:
      Result := UCS_4;
    20866:
      Result := KOI8_R;
    20932:
      Result := JIS_X0208;
    20936:
      Result := GB2312;
    21866:
      Result := KOI8_U;
    28591:
      Result := ISO_8859_1;
    28592:
      Result := ISO_8859_2;
    28593:
      Result := ISO_8859_3;
    28594:
      Result := ISO_8859_4;
    28595:
      Result := ISO_8859_5;
    28596, 708:
      Result := ISO_8859_6;
    28597:
      Result := ISO_8859_7;
    28598, 38598:
      Result := ISO_8859_8;
    28599:
      Result := ISO_8859_9;
    28605:
      Result := ISO_8859_15;
    50220:
      Result := ISO_2022_JP; //? ISO 2022 Japanese with no halfwidth Katakana
    50221:
      Result := ISO_2022_JP1;//? Japanese with halfwidth Katakana
    50222:
      Result := ISO_2022_JP2;//? Japanese JIS X 0201-1989
    50225:
      Result := ISO_2022_KR;
    50227:
      Result := ISO_2022_CN;//? ISO 2022 Simplified Chinese
    50229:
      Result := ISO_2022_CNE;//? ISO 2022 Traditional Chinese
    51932:
      Result := EUC_JP;
    51936:
      Result := GB2312;
    51949:
      Result := EUC_KR;
    52936:
      Result := HZ;
    54936:
      Result := GB18030;
    65000:
      Result := UTF_7;
    65001:
      Result := UTF_8;
    0:
      Result := UCS_2LE;
  else
    Result := CP1252;
  end;
end;

function GetCurCP: TMimeChar;
begin
  Result := CPToMimeChar(GetACP);
end;

function GetCurOEMCP: TMimeChar;
begin
  Result := CPToMimeChar(GetOEMCP);
end;
{$ENDIF}

{==============================================================================}
function NeedCharsetConversion(const Value: AnsiString): Boolean;
var
  n: Integer;
begin
  Result := False;
  for n := 1 to Length(Value) do
    if (Ord(Value[n]) > 127) or (Ord(Value[n]) = 0) then
    begin
      Result := True;
      Break;
    end;
end;

{==============================================================================}
function IdealCharsetCoding(const Value: AnsiString; CharFrom: TMimeChar;
  CharTo: TMimeSetChar): TMimeChar;
var
  n: Integer;
  max: Integer;
  s, t, u: AnsiString;
  CharSet: TMimeChar;
begin
  Result := ISO_8859_1;
  s := Copy(Value, 1, 1024);  //max first 1KB for next procedure
  max := 0;
  for n := Ord(Low(TMimeChar)) to Ord(High(TMimeChar)) do
  begin
    CharSet := TMimeChar(n);
    if CharSet in CharTo then
    begin
      t := CharsetConversionTrans(s, CharFrom, CharSet, Replace_None, False);
      u := CharsetConversionTrans(t, CharSet, CharFrom, Replace_None, False);
      if s = u then
      begin
        Result := CharSet;
        Exit;
      end;
      if Length(u) > max then
      begin
        Result := CharSet;
        max := Length(u);
      end;
    end;
  end;
end;

{==============================================================================}
function GetBOM(Value: TMimeChar): AnsiString;
begin
  Result := '';
  case Value of
    UCS_2:
      Result := #$fe + #$ff;
    UCS_4:
      Result := #$00 + #$00 + #$fe + #$ff;
    UCS_2LE:
      Result := #$ff + #$fe;
    UCS_4LE:
      Result := #$ff + #$fe + #$00 + #$00;
    UTF_8:
      Result := #$ef + #$bb + #$bf;
  end;
end;

{==============================================================================}
function GetCPFromID(Value: AnsiString): TMimeChar;
begin
  Value := UpperCase(Value);
  if (Pos('KAMENICKY', Value) > 0) or (Pos('895', Value) > 0) then
    Result := CP895
  else
  if Pos('MUTF-7', Value) > 0 then
    Result := UTF_7mod
  else
    Result := GetCPFromIconvID(Value);
end;

{==============================================================================}
function GetIDFromCP(Value: TMimeChar): AnsiString;
begin
  case Value of
    CP895:
      Result := 'CP-895';
    UTF_7mod:
      Result := 'mUTF-7';
  else
    Result := GetIconvIDFromCP(Value);
  end;
end;

{==============================================================================}
function StringToWide(const Value: AnsiString): WideString;
var
  n: integer;
  x, y: integer;
begin
  SetLength(Result, Length(Value) div 2);
  for n := 1 to Length(Value) div 2 do
  begin
    x := Ord(Value[((n-1) * 2) + 1]);
    y := Ord(Value[((n-1) * 2) + 2]);
    Result[n] := WideChar(x * 256 + y);
  end;
end;

{==============================================================================}
function WideToString(const Value: WideString): AnsiString;
var
  n: integer;
  x: integer;
begin
  SetLength(Result, Length(Value) * 2);
  for n := 1 to Length(Value) do
  begin
    x := Ord(Value[n]);
    Result[((n-1) * 2) + 1] := AnsiChar(x div 256);
    Result[((n-1) * 2) + 2] := AnsiChar(x mod 256);
  end;
end;

{==============================================================================}
initialization
begin
  IconvArr[0].Charset := ISO_8859_1;
  IconvArr[0].Charname := 'ISO-8859-1 CP819 IBM819 ISO-IR-100 ISO8859-1 ISO_8859-1 ISO_8859-1:1987 L1 LATIN1 CSISOLATIN1';
  IconvArr[1].Charset := UTF_8;
  IconvArr[1].Charname := 'UTF-8';
  IconvArr[2].Charset := UCS_2;
  IconvArr[2].Charname := 'ISO-10646-UCS-2 UCS-2 CSUNICODE';
  IconvArr[3].Charset := UCS_2;
  IconvArr[3].Charname := 'UCS-2BE UNICODE-1-1 UNICODEBIG CSUNICODE11';
  IconvArr[4].Charset := UCS_2LE;
  IconvArr[4].Charname := 'UCS-2LE UNICODELITTLE';
  IconvArr[5].Charset := UCS_4;
  IconvArr[5].Charname := 'ISO-10646-UCS-4 UCS-4 CSUCS4';
  IconvArr[6].Charset := UCS_4;
  IconvArr[6].Charname := 'UCS-4BE';
  IconvArr[7].Charset := UCS_2LE;
  IconvArr[7].Charname := 'UCS-4LE';
  IconvArr[8].Charset := UTF_16;
  IconvArr[8].Charname := 'UTF-16';
  IconvArr[9].Charset := UTF_16;
  IconvArr[9].Charname := 'UTF-16BE';
  IconvArr[10].Charset := UTF_16LE;
  IconvArr[10].Charname := 'UTF-16LE';
  IconvArr[11].Charset := UTF_32;
  IconvArr[11].Charname := 'UTF-32';
  IconvArr[12].Charset := UTF_32;
  IconvArr[12].Charname := 'UTF-32BE';
  IconvArr[13].Charset := UTF_32;
  IconvArr[13].Charname := 'UTF-32LE';
  IconvArr[14].Charset := UTF_7;
  IconvArr[14].Charname := 'UNICODE-1-1-UTF-7 UTF-7 CSUNICODE11UTF7';
  IconvArr[15].Charset := C99;
  IconvArr[15].Charname := 'C99';
  IconvArr[16].Charset := JAVA;
  IconvArr[16].Charname := 'JAVA';
  IconvArr[17].Charset := ISO_8859_1;
  IconvArr[17].Charname := 'US-ASCII ANSI_X3.4-1968 ANSI_X3.4-1986 ASCII CP367 IBM367 ISO-IR-6 ISO646-US ISO_646.IRV:1991 US CSASCII';
  IconvArr[18].Charset := ISO_8859_2;
  IconvArr[18].Charname := 'ISO-8859-2 ISO-IR-101 ISO8859-2 ISO_8859-2 ISO_8859-2:1987 L2 LATIN2 CSISOLATIN2';
  IconvArr[19].Charset := ISO_8859_3;
  IconvArr[19].Charname := 'ISO-8859-3 ISO-IR-109 ISO8859-3 ISO_8859-3 ISO_8859-3:1988 L3 LATIN3 CSISOLATIN3';
  IconvArr[20].Charset := ISO_8859_4;
  IconvArr[20].Charname := 'ISO-8859-4 ISO-IR-110 ISO8859-4 ISO_8859-4 ISO_8859-4:1988 L4 LATIN4 CSISOLATIN4';
  IconvArr[21].Charset := ISO_8859_5;
  IconvArr[21].Charname := 'ISO-8859-5 CYRILLIC ISO-IR-144 ISO8859-5 ISO_8859-5 ISO_8859-5:1988 CSISOLATINCYRILLIC';
  IconvArr[22].Charset := ISO_8859_6;
  IconvArr[22].Charname := 'ISO-8859-6 ARABIC ASMO-708 ECMA-114 ISO-IR-127 ISO8859-6 ISO_8859-6 ISO_8859-6:1987 CSISOLATINARABIC';
  IconvArr[23].Charset := ISO_8859_7;
  IconvArr[23].Charname := 'ISO-8859-7 ECMA-118 ELOT_928 GREEK GREEK8 ISO-IR-126 ISO8859-7 ISO_8859-7 ISO_8859-7:1987 CSISOLATINGREEK';
  IconvArr[24].Charset := ISO_8859_8;
  IconvArr[24].Charname := 'ISO-8859-8 HEBREW ISO_8859-8 ISO-IR-138 ISO8859-8 ISO_8859-8:1988 CSISOLATINHEBREW ISO-8859-8-I';
  IconvArr[25].Charset := ISO_8859_9;
  IconvArr[25].Charname := 'ISO-8859-9 ISO-IR-148 ISO8859-9 ISO_8859-9 ISO_8859-9:1989 L5 LATIN5 CSISOLATIN5';
  IconvArr[26].Charset := ISO_8859_10;
  IconvArr[26].Charname := 'ISO-8859-10 ISO-IR-157 ISO8859-10 ISO_8859-10 ISO_8859-10:1992 L6 LATIN6 CSISOLATIN6';
  IconvArr[27].Charset := ISO_8859_13;
  IconvArr[27].Charname := 'ISO-8859-13 ISO-IR-179 ISO8859-13 ISO_8859-13 L7 LATIN7';
  IconvArr[28].Charset := ISO_8859_14;
  IconvArr[28].Charname := 'ISO-8859-14 ISO-CELTIC ISO-IR-199 ISO8859-14 ISO_8859-14 ISO_8859-14:1998 L8 LATIN8';
  IconvArr[29].Charset := ISO_8859_15;
  IconvArr[29].Charname := 'ISO-8859-15 ISO-IR-203 ISO8859-15 ISO_8859-15 ISO_8859-15:1998';
  IconvArr[30].Charset := ISO_8859_16;
  IconvArr[30].Charname := 'ISO-8859-16 ISO-IR-226 ISO8859-16 ISO_8859-16 ISO_8859-16:2000';
  IconvArr[31].Charset := KOI8_R;
  IconvArr[31].Charname := 'KOI8-R CSKOI8R';
  IconvArr[32].Charset := KOI8_U;
  IconvArr[32].Charname := 'KOI8-U';
  IconvArr[33].Charset := KOI8_RU;
  IconvArr[33].Charname := 'KOI8-RU';
  IconvArr[34].Charset := CP1250;
  IconvArr[34].Charname := 'WINDOWS-1250 CP1250 MS-EE';
  IconvArr[35].Charset := CP1251;
  IconvArr[35].Charname := 'WINDOWS-1251 CP1251 MS-CYRL';
  IconvArr[36].Charset := CP1252;
  IconvArr[36].Charname := 'WINDOWS-1252 CP1252 MS-ANSI';
  IconvArr[37].Charset := CP1253;
  IconvArr[37].Charname := 'WINDOWS-1253 CP1253 MS-GREEK';
  IconvArr[38].Charset := CP1254;
  IconvArr[38].Charname := 'WINDOWS-1254 CP1254 MS-TURK';
  IconvArr[39].Charset := CP1255;
  IconvArr[39].Charname := 'WINDOWS-1255 CP1255 MS-HEBR';
  IconvArr[40].Charset := CP1256;
  IconvArr[40].Charname := 'WINDOWS-1256 CP1256 MS-ARAB';
  IconvArr[41].Charset := CP1257;
  IconvArr[41].Charname := 'WINDOWS-1257 CP1257 WINBALTRIM';
  IconvArr[42].Charset := CP1258;
  IconvArr[42].Charname := 'WINDOWS-1258 CP1258';
  IconvArr[43].Charset := ISO_8859_1;
  IconvArr[43].Charname := '850 CP850 IBM850 CSPC850MULTILINGUAL';
  IconvArr[44].Charset := CP862;
  IconvArr[44].Charname := '862 CP862 IBM862 CSPC862LATINHEBREW';
  IconvArr[45].Charset := CP866;
  IconvArr[45].Charname := '866 CP866 IBM866 CSIBM866';
  IconvArr[46].Charset := MAC;
  IconvArr[46].Charname := 'MAC MACINTOSH MACROMAN CSMACINTOSH';
  IconvArr[47].Charset := MACCE;
  IconvArr[47].Charname := 'MACCENTRALEUROPE';
  IconvArr[48].Charset := MACICE;
  IconvArr[48].Charname := 'MACICELAND';
  IconvArr[49].Charset := MACCRO;
  IconvArr[49].Charname := 'MACCROATIAN';
  IconvArr[50].Charset := MACRO;
  IconvArr[50].Charname := 'MACROMANIA';
  IconvArr[51].Charset := MACCYR;
  IconvArr[51].Charname := 'MACCYRILLIC';
  IconvArr[52].Charset := MACUK;
  IconvArr[52].Charname := 'MACUKRAINE';
  IconvArr[53].Charset := MACGR;
  IconvArr[53].Charname := 'MACGREEK';
  IconvArr[54].Charset := MACTU;
  IconvArr[54].Charname := 'MACTURKISH';
  IconvArr[55].Charset := MACHEB;
  IconvArr[55].Charname := 'MACHEBREW';
  IconvArr[56].Charset := MACAR;
  IconvArr[56].Charname := 'MACARABIC';
  IconvArr[57].Charset := MACTH;
  IconvArr[57].Charname := 'MACTHAI';
  IconvArr[58].Charset := ROMAN8;
  IconvArr[58].Charname := 'HP-ROMAN8 R8 ROMAN8 CSHPROMAN8';
  IconvArr[59].Charset := NEXTSTEP;
  IconvArr[59].Charname := 'NEXTSTEP';
  IconvArr[60].Charset := ARMASCII;
  IconvArr[60].Charname := 'ARMSCII-8';
  IconvArr[61].Charset := GEORGIAN_AC;
  IconvArr[61].Charname := 'GEORGIAN-ACADEMY';
  IconvArr[62].Charset := GEORGIAN_PS;
  IconvArr[62].Charname := 'GEORGIAN-PS';
  IconvArr[63].Charset := KOI8_T;
  IconvArr[63].Charname := 'KOI8-T';
  IconvArr[64].Charset := MULELAO;
  IconvArr[64].Charname := 'MULELAO-1';
  IconvArr[65].Charset := CP1133;
  IconvArr[65].Charname := 'CP1133 IBM-CP1133';
  IconvArr[66].Charset := TIS620;
  IconvArr[66].Charname := 'TIS-620 ISO-IR-166 TIS620 TIS620-0 TIS620.2529-1 TIS620.2533-0 TIS620.2533-1';
  IconvArr[67].Charset := CP874;
  IconvArr[67].Charname := 'CP874 WINDOWS-874';
  IconvArr[68].Charset := VISCII;
  IconvArr[68].Charname := 'VISCII VISCII1.1-1 CSVISCII';
  IconvArr[69].Charset := TCVN;
  IconvArr[69].Charname := 'TCVN TCVN-5712 TCVN5712-1 TCVN5712-1:1993';
  IconvArr[70].Charset := ISO_IR_14;
  IconvArr[70].Charname := 'ISO-IR-14 ISO646-JP JIS_C6220-1969-RO JP CSISO14JISC6220RO';
  IconvArr[71].Charset := JIS_X0201;
  IconvArr[71].Charname := 'JISX0201-1976 JIS_X0201 X0201 CSHALFWIDTHKATAKANA';
  IconvArr[72].Charset := JIS_X0208;
  IconvArr[72].Charname := 'ISO-IR-87 JIS0208 JIS_C6226-1983 JIS_X0208 JIS_X0208-1983 JIS_X0208-1990 X0208 CSISO87JISX0208';
  IconvArr[73].Charset := JIS_X0212;
  IconvArr[73].Charname := 'ISO-IR-159 JIS_X0212 JIS_X0212-1990 JIS_X0212.1990-0 X0212 CSISO159JISX02121990';
  IconvArr[74].Charset := GB1988_80;
  IconvArr[74].Charname := 'CN GB_1988-80 ISO-IR-57 ISO646-CN CSISO57GB1988';
  IconvArr[75].Charset := GB2312_80;
  IconvArr[75].Charname := 'CHINESE GB_2312-80 ISO-IR-58 CSISO58GB231280';
  IconvArr[76].Charset := ISO_IR_165;
  IconvArr[76].Charname := 'CN-GB-ISOIR165 ISO-IR-165';
  IconvArr[77].Charset := ISO_IR_149;
  IconvArr[77].Charname := 'ISO-IR-149 KOREAN KSC_5601 KS_C_5601-1987 KS_C_5601-1989 CSKSC56011987';
  IconvArr[78].Charset := EUC_JP;
  IconvArr[78].Charname := 'EUC-JP EUCJP EXTENDED_UNIX_CODE_PACKED_FORMAT_FOR_JAPANESE CSEUCPKDFMTJAPANESE';
  IconvArr[79].Charset := SHIFT_JIS;
  IconvArr[79].Charname := 'SHIFT-JIS MS_KANJI SHIFT_JIS SJIS CSSHIFTJIS';
  IconvArr[80].Charset := CP932;
  IconvArr[80].Charname := 'CP932';
  IconvArr[81].Charset := ISO_2022_JP;
  IconvArr[81].Charname := 'ISO-2022-JP CSISO2022JP';
  IconvArr[82].Charset := ISO_2022_JP1;
  IconvArr[82].Charname := 'ISO-2022-JP-1';
  IconvArr[83].Charset := ISO_2022_JP2;
  IconvArr[83].Charname := 'ISO-2022-JP-2 CSISO2022JP2';
  IconvArr[84].Charset := GB2312;
  IconvArr[84].Charname := 'CN-GB EUC-CN EUCCN GB2312 CSGB2312';
  IconvArr[85].Charset := CP936;
  IconvArr[85].Charname := 'CP936 GBK';
  IconvArr[86].Charset := GB18030;
  IconvArr[86].Charname := 'GB18030';
  IconvArr[87].Charset := ISO_2022_CN;
  IconvArr[87].Charname := 'ISO-2022-CN CSISO2022CN';
  IconvArr[88].Charset := ISO_2022_CNE;
  IconvArr[88].Charname := 'ISO-2022-CN-EXT';
  IconvArr[89].Charset := HZ;
  IconvArr[89].Charname := 'HZ HZ-GB-2312';
  IconvArr[90].Charset := EUC_TW;
  IconvArr[90].Charname := 'EUC-TW EUCTW CSEUCTW';
  IconvArr[91].Charset := BIG5;
  IconvArr[91].Charname := 'BIG5 BIG-5 BIG-FIVE BIGFIVE CN-BIG5 CSBIG5';
  IconvArr[92].Charset := CP950;
  IconvArr[92].Charname := 'CP950';
  IconvArr[93].Charset := BIG5_HKSCS;
  IconvArr[93].Charname := 'BIG5-HKSCS BIG5HKSCS';
  IconvArr[94].Charset := EUC_KR;
  IconvArr[94].Charname := 'EUC-KR EUCKR CSEUCKR';
  IconvArr[95].Charset := CP949;
  IconvArr[95].Charname := 'CP949 UHC';
  IconvArr[96].Charset := CP1361;
  IconvArr[96].Charname := 'CP1361 JOHAB';
  IconvArr[97].Charset := ISO_2022_KR;
  IconvArr[97].Charname := 'ISO-2022-KR CSISO2022KR';
  IconvArr[98].Charset := ISO_8859_1;
  IconvArr[98].Charname := '437 CP437 IBM437 CSPC8CODEPAGE437';
  IconvArr[99].Charset := CP737;
  IconvArr[99].Charname := 'CP737';
  IconvArr[100].Charset := CP775;
  IconvArr[100].Charname := 'CP775 IBM775 CSPC775BALTIC';
  IconvArr[101].Charset := CP852;
  IconvArr[101].Charname := '852 CP852 IBM852 CSPCP852';
  IconvArr[102].Charset := CP853;
  IconvArr[102].Charname := 'CP853';
  IconvArr[103].Charset := CP855;
  IconvArr[103].Charname := '855 CP855 IBM855 CSIBM855';
  IconvArr[104].Charset := CP857;
  IconvArr[104].Charname := '857 CP857 IBM857 CSIBM857';
  IconvArr[105].Charset := CP858;
  IconvArr[105].Charname := 'CP858';
  IconvArr[106].Charset := CP860;
  IconvArr[106].Charname := '860 CP860 IBM860 CSIBM860';
  IconvArr[107].Charset := CP861;
  IconvArr[107].Charname := '861 CP-IS CP861 IBM861 CSIBM861';
  IconvArr[108].Charset := CP863;
  IconvArr[108].Charname := '863 CP863 IBM863 CSIBM863';
  IconvArr[109].Charset := CP864;
  IconvArr[109].Charname := 'CP864 IBM864 CSIBM864';
  IconvArr[110].Charset := CP865;
  IconvArr[110].Charname := '865 CP865 IBM865 CSIBM865';
  IconvArr[111].Charset := CP869;
  IconvArr[111].Charname := '869 CP-GR CP869 IBM869 CSIBM869';
  IconvArr[112].Charset := CP1125;
  IconvArr[112].Charname := 'CP1125';
end;

end.
