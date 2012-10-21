{==============================================================================|
| Project : Ararat Synapse                                       | 002.002.002 |
|==============================================================================|
| Content: Coding and decoding support                                         |
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

{:@abstract(Various encoding and decoding support)}
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$R-}
{$H+}
{$TYPEDADDRESS OFF}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$WARN SUSPICIOUS_TYPECAST OFF}
{$ENDIF}

unit synacode;

interface

uses
  SysUtils;

type
  TSpecials = set of AnsiChar;

const

  SpecialChar: TSpecials =
  ['=', '(', ')', '[', ']', '<', '>', ':', ';', ',', '@', '/', '?', '\',
    '"', '_'];
  NonAsciiChar: TSpecials =
  [#0..#31, #127..#255];
  URLFullSpecialChar: TSpecials =
  [';', '/', '?', ':', '@', '=', '&', '#', '+'];
  URLSpecialChar: TSpecials =
  [#$00..#$20, '<', '>', '"', '%', '{', '}', '|', '\', '^', '[', ']', '`', #$7F..#$FF];
  TableBase64 =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
  TableBase64mod =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,=';
  TableUU =
    '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  TableXX =
    '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  ReTablebase64 =
    #$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$3E +#$40
    +#$40 +#$40 +#$3F +#$34 +#$35 +#$36 +#$37 +#$38 +#$39 +#$3A +#$3B +#$3C
    +#$3D +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$00 +#$01 +#$02 +#$03
    +#$04 +#$05 +#$06 +#$07 +#$08 +#$09 +#$0A +#$0B +#$0C +#$0D +#$0E +#$0F
    +#$10 +#$11 +#$12 +#$13 +#$14 +#$15 +#$16 +#$17 +#$18 +#$19 +#$40 +#$40
    +#$40 +#$40 +#$40 +#$40 +#$1A +#$1B +#$1C +#$1D +#$1E +#$1F +#$20 +#$21
    +#$22 +#$23 +#$24 +#$25 +#$26 +#$27 +#$28 +#$29 +#$2A +#$2B +#$2C +#$2D
    +#$2E +#$2F +#$30 +#$31 +#$32 +#$33 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40;
  ReTableUU =
    #$01 +#$02 +#$03 +#$04 +#$05 +#$06 +#$07 +#$08 +#$09 +#$0A +#$0B +#$0C
    +#$0D +#$0E +#$0F +#$10 +#$11 +#$12 +#$13 +#$14 +#$15 +#$16 +#$17 +#$18
    +#$19 +#$1A +#$1B +#$1C +#$1D +#$1E +#$1F +#$20 +#$21 +#$22 +#$23 +#$24
    +#$25 +#$26 +#$27 +#$28 +#$29 +#$2A +#$2B +#$2C +#$2D +#$2E +#$2F +#$30
    +#$31 +#$32 +#$33 +#$34 +#$35 +#$36 +#$37 +#$38 +#$39 +#$3A +#$3B +#$3C
    +#$3D +#$3E +#$3F +#$00 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40
    +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40
    +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40;
  ReTableXX =
    #$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$00 +#$40
    +#$01 +#$40 +#$40 +#$02 +#$03 +#$04 +#$05 +#$06 +#$07 +#$08 +#$09 +#$0A
    +#$0B +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$0C +#$0D +#$0E +#$0F
    +#$10 +#$11 +#$12 +#$13 +#$14 +#$15 +#$16 +#$17 +#$18 +#$19 +#$1A +#$1B
    +#$1C +#$1D +#$1E +#$1F +#$20 +#$21 +#$22 +#$23 +#$24 +#$25 +#$40 +#$40
    +#$40 +#$40 +#$40 +#$40 +#$26 +#$27 +#$28 +#$29 +#$2A +#$2B +#$2C +#$2D
    +#$2E +#$2F +#$30 +#$31 +#$32 +#$33 +#$34 +#$35 +#$36 +#$37 +#$38 +#$39
    +#$3A +#$3B +#$3C +#$3D +#$3E +#$3F +#$40 +#$40 +#$40 +#$40 +#$40 +#$40;

{:Decodes triplet encoding with a given character delimiter. It is used for
 decoding quoted-printable or URL encoding.}
function DecodeTriplet(const Value: AnsiString; Delimiter: AnsiChar): AnsiString;

{:Decodes a string from quoted printable form. (also decodes triplet sequences
 like '=7F')}
function DecodeQuotedPrintable(const Value: AnsiString): AnsiString;

{:Decodes a string of URL encoding. (also decodes triplet sequences like '%7F')}
function DecodeURL(const Value: AnsiString): AnsiString;

{:Performs triplet encoding with a given character delimiter. Used for encoding
 quoted-printable or URL encoding.}
function EncodeTriplet(const Value: AnsiString; Delimiter: AnsiChar;
  Specials: TSpecials): AnsiString;

{:Encodes a string to triplet quoted printable form. All @link(NonAsciiChar)
 are encoded.}
function EncodeQuotedPrintable(const Value: AnsiString): AnsiString;

{:Encodes a string to triplet quoted printable form. All @link(NonAsciiChar) and
 @link(SpecialChar) are encoded.}
function EncodeSafeQuotedPrintable(const Value: AnsiString): AnsiString;

{:Encodes a string to URL format. Used for encoding data from a form field in
 HTTP, etc. (Encodes all critical characters including characters used as URL
 delimiters ('/',':', etc.)}
function EncodeURLElement(const Value: AnsiString): AnsiString;

{:Encodes a string to URL format. Used to encode critical characters in all
 URLs.}
function EncodeURL(const Value: AnsiString): AnsiString;

{:Decode 4to3 encoding with given table. If some element is not found in table,
 first item from table is used. This is good for buggy coded items by Microsoft
 Outlook. This software sometimes using wrong table for UUcode, where is used
 ' ' instead '`'.}
function Decode4to3(const Value, Table: AnsiString): AnsiString;

{:Decode 4to3 encoding with given REVERSE table. Using this function with
reverse table is much faster then @link(Decode4to3). This function is used
internally for Base64, UU or XX decoding.}
function Decode4to3Ex(const Value, Table: AnsiString): AnsiString;

{:Encode by system 3to4 (used by Base64, UU coding, etc) by given table.}
function Encode3to4(const Value, Table: AnsiString): AnsiString;

{:Decode string from base64 format.}
function DecodeBase64(const Value: AnsiString): AnsiString;

{:Encodes a string to base64 format.}
function EncodeBase64(const Value: AnsiString): AnsiString;

{:Decode string from modified base64 format. (used in IMAP, for example.)}
function DecodeBase64mod(const Value: AnsiString): AnsiString;

{:Encodes a string to  modified base64 format. (used in IMAP, for example.)}
function EncodeBase64mod(const Value: AnsiString): AnsiString;

{:Decodes a string from UUcode format.}
function DecodeUU(const Value: AnsiString): AnsiString;

{:encode UUcode. it encode only datas, you must also add header and footer for
 proper encode.}
function EncodeUU(const Value: AnsiString): AnsiString;

{:Decodes a string from XXcode format.}
function DecodeXX(const Value: AnsiString): AnsiString;

{:decode line with Yenc code. This code is sometimes used in newsgroups.}
function DecodeYEnc(const Value: AnsiString): AnsiString;

{:Returns a new CRC32 value after adding a new byte of data.}
function UpdateCrc32(Value: Byte; Crc32: Integer): Integer;

{:return CRC32 from a value string.}
function Crc32(const Value: AnsiString): Integer;

{:Returns a new CRC16 value after adding a new byte of data.}
function UpdateCrc16(Value: Byte; Crc16: Word): Word;

{:return CRC16 from a value string.}
function Crc16(const Value: AnsiString): Word;

{:Returns a binary string with a RSA-MD5 hashing of "Value" string.}
function MD5(const Value: AnsiString): AnsiString;

{:Returns a binary string with HMAC-MD5 hash.}
function HMAC_MD5(Text, Key: AnsiString): AnsiString;

{:Returns a binary string with a RSA-MD5 hashing of string what is constructed
 by repeating "value" until length is "Len".}
function MD5LongHash(const Value: AnsiString; Len: integer): AnsiString;

{:Returns a binary string with a SHA-1 hashing of "Value" string.}
function SHA1(const Value: AnsiString): AnsiString;

{:Returns a binary string with HMAC-SHA1 hash.}
function HMAC_SHA1(Text, Key: AnsiString): AnsiString;

{:Returns a binary string with a SHA-1 hashing of string what is constructed
 by repeating "value" until length is "Len".}
function SHA1LongHash(const Value: AnsiString; Len: integer): AnsiString;

{:Returns a binary string with a RSA-MD4 hashing of "Value" string.}
function MD4(const Value: AnsiString): AnsiString;

implementation

const

  Crc32Tab: array[0..255] of Integer = (
    Integer($00000000), Integer($77073096), Integer($EE0E612C), Integer($990951BA),
    Integer($076DC419), Integer($706AF48F), Integer($E963A535), Integer($9E6495A3),
    Integer($0EDB8832), Integer($79DCB8A4), Integer($E0D5E91E), Integer($97D2D988),
    Integer($09B64C2B), Integer($7EB17CBD), Integer($E7B82D07), Integer($90BF1D91),
    Integer($1DB71064), Integer($6AB020F2), Integer($F3B97148), Integer($84BE41DE),
    Integer($1ADAD47D), Integer($6DDDE4EB), Integer($F4D4B551), Integer($83D385C7),
    Integer($136C9856), Integer($646BA8C0), Integer($FD62F97A), Integer($8A65C9EC),
    Integer($14015C4F), Integer($63066CD9), Integer($FA0F3D63), Integer($8D080DF5),
    Integer($3B6E20C8), Integer($4C69105E), Integer($D56041E4), Integer($A2677172),
    Integer($3C03E4D1), Integer($4B04D447), Integer($D20D85FD), Integer($A50AB56B),
    Integer($35B5A8FA), Integer($42B2986C), Integer($DBBBC9D6), Integer($ACBCF940),
    Integer($32D86CE3), Integer($45DF5C75), Integer($DCD60DCF), Integer($ABD13D59),
    Integer($26D930AC), Integer($51DE003A), Integer($C8D75180), Integer($BFD06116),
    Integer($21B4F4B5), Integer($56B3C423), Integer($CFBA9599), Integer($B8BDA50F),
    Integer($2802B89E), Integer($5F058808), Integer($C60CD9B2), Integer($B10BE924),
    Integer($2F6F7C87), Integer($58684C11), Integer($C1611DAB), Integer($B6662D3D),
    Integer($76DC4190), Integer($01DB7106), Integer($98D220BC), Integer($EFD5102A),
    Integer($71B18589), Integer($06B6B51F), Integer($9FBFE4A5), Integer($E8B8D433),
    Integer($7807C9A2), Integer($0F00F934), Integer($9609A88E), Integer($E10E9818),
    Integer($7F6A0DBB), Integer($086D3D2D), Integer($91646C97), Integer($E6635C01),
    Integer($6B6B51F4), Integer($1C6C6162), Integer($856530D8), Integer($F262004E),
    Integer($6C0695ED), Integer($1B01A57B), Integer($8208F4C1), Integer($F50FC457),
    Integer($65B0D9C6), Integer($12B7E950), Integer($8BBEB8EA), Integer($FCB9887C),
    Integer($62DD1DDF), Integer($15DA2D49), Integer($8CD37CF3), Integer($FBD44C65),
    Integer($4DB26158), Integer($3AB551CE), Integer($A3BC0074), Integer($D4BB30E2),
    Integer($4ADFA541), Integer($3DD895D7), Integer($A4D1C46D), Integer($D3D6F4FB),
    Integer($4369E96A), Integer($346ED9FC), Integer($AD678846), Integer($DA60B8D0),
    Integer($44042D73), Integer($33031DE5), Integer($AA0A4C5F), Integer($DD0D7CC9),
    Integer($5005713C), Integer($270241AA), Integer($BE0B1010), Integer($C90C2086),
    Integer($5768B525), Integer($206F85B3), Integer($B966D409), Integer($CE61E49F),
    Integer($5EDEF90E), Integer($29D9C998), Integer($B0D09822), Integer($C7D7A8B4),
    Integer($59B33D17), Integer($2EB40D81), Integer($B7BD5C3B), Integer($C0BA6CAD),
    Integer($EDB88320), Integer($9ABFB3B6), Integer($03B6E20C), Integer($74B1D29A),
    Integer($EAD54739), Integer($9DD277AF), Integer($04DB2615), Integer($73DC1683),
    Integer($E3630B12), Integer($94643B84), Integer($0D6D6A3E), Integer($7A6A5AA8),
    Integer($E40ECF0B), Integer($9309FF9D), Integer($0A00AE27), Integer($7D079EB1),
    Integer($F00F9344), Integer($8708A3D2), Integer($1E01F268), Integer($6906C2FE),
    Integer($F762575D), Integer($806567CB), Integer($196C3671), Integer($6E6B06E7),
    Integer($FED41B76), Integer($89D32BE0), Integer($10DA7A5A), Integer($67DD4ACC),
    Integer($F9B9DF6F), Integer($8EBEEFF9), Integer($17B7BE43), Integer($60B08ED5),
    Integer($D6D6A3E8), Integer($A1D1937E), Integer($38D8C2C4), Integer($4FDFF252),
    Integer($D1BB67F1), Integer($A6BC5767), Integer($3FB506DD), Integer($48B2364B),
    Integer($D80D2BDA), Integer($AF0A1B4C), Integer($36034AF6), Integer($41047A60),
    Integer($DF60EFC3), Integer($A867DF55), Integer($316E8EEF), Integer($4669BE79),
    Integer($CB61B38C), Integer($BC66831A), Integer($256FD2A0), Integer($5268E236),
    Integer($CC0C7795), Integer($BB0B4703), Integer($220216B9), Integer($5505262F),
    Integer($C5BA3BBE), Integer($B2BD0B28), Integer($2BB45A92), Integer($5CB36A04),
    Integer($C2D7FFA7), Integer($B5D0CF31), Integer($2CD99E8B), Integer($5BDEAE1D),
    Integer($9B64C2B0), Integer($EC63F226), Integer($756AA39C), Integer($026D930A),
    Integer($9C0906A9), Integer($EB0E363F), Integer($72076785), Integer($05005713),
    Integer($95BF4A82), Integer($E2B87A14), Integer($7BB12BAE), Integer($0CB61B38),
    Integer($92D28E9B), Integer($E5D5BE0D), Integer($7CDCEFB7), Integer($0BDBDF21),
    Integer($86D3D2D4), Integer($F1D4E242), Integer($68DDB3F8), Integer($1FDA836E),
    Integer($81BE16CD), Integer($F6B9265B), Integer($6FB077E1), Integer($18B74777),
    Integer($88085AE6), Integer($FF0F6A70), Integer($66063BCA), Integer($11010B5C),
    Integer($8F659EFF), Integer($F862AE69), Integer($616BFFD3), Integer($166CCF45),
    Integer($A00AE278), Integer($D70DD2EE), Integer($4E048354), Integer($3903B3C2),
    Integer($A7672661), Integer($D06016F7), Integer($4969474D), Integer($3E6E77DB),
    Integer($AED16A4A), Integer($D9D65ADC), Integer($40DF0B66), Integer($37D83BF0),
    Integer($A9BCAE53), Integer($DEBB9EC5), Integer($47B2CF7F), Integer($30B5FFE9),
    Integer($BDBDF21C), Integer($CABAC28A), Integer($53B39330), Integer($24B4A3A6),
    Integer($BAD03605), Integer($CDD70693), Integer($54DE5729), Integer($23D967BF),
    Integer($B3667A2E), Integer($C4614AB8), Integer($5D681B02), Integer($2A6F2B94),
    Integer($B40BBE37), Integer($C30C8EA1), Integer($5A05DF1B), Integer($2D02EF8D)
    );

  Crc16Tab: array[0..255] of Word = (
    $0000, $1189, $2312, $329B, $4624, $57AD, $6536, $74BF,
    $8C48, $9DC1, $AF5A, $BED3, $CA6C, $DBE5, $E97E, $F8F7,
    $1081, $0108, $3393, $221A, $56A5, $472C, $75B7, $643E,
    $9CC9, $8D40, $BFDB, $AE52, $DAED, $CB64, $F9FF, $E876,
    $2102, $308B, $0210, $1399, $6726, $76AF, $4434, $55BD,
    $AD4A, $BCC3, $8E58, $9FD1, $EB6E, $FAE7, $C87C, $D9F5,
    $3183, $200A, $1291, $0318, $77A7, $662E, $54B5, $453C,
    $BDCB, $AC42, $9ED9, $8F50, $FBEF, $EA66, $D8FD, $C974,
    $4204, $538D, $6116, $709F, $0420, $15A9, $2732, $36BB,
    $CE4C, $DFC5, $ED5E, $FCD7, $8868, $99E1, $AB7A, $BAF3,
    $5285, $430C, $7197, $601E, $14A1, $0528, $37B3, $263A,
    $DECD, $CF44, $FDDF, $EC56, $98E9, $8960, $BBFB, $AA72,
    $6306, $728F, $4014, $519D, $2522, $34AB, $0630, $17B9,
    $EF4E, $FEC7, $CC5C, $DDD5, $A96A, $B8E3, $8A78, $9BF1,
    $7387, $620E, $5095, $411C, $35A3, $242A, $16B1, $0738,
    $FFCF, $EE46, $DCDD, $CD54, $B9EB, $A862, $9AF9, $8B70,
    $8408, $9581, $A71A, $B693, $C22C, $D3A5, $E13E, $F0B7,
    $0840, $19C9, $2B52, $3ADB, $4E64, $5FED, $6D76, $7CFF,
    $9489, $8500, $B79B, $A612, $D2AD, $C324, $F1BF, $E036,
    $18C1, $0948, $3BD3, $2A5A, $5EE5, $4F6C, $7DF7, $6C7E,
    $A50A, $B483, $8618, $9791, $E32E, $F2A7, $C03C, $D1B5,
    $2942, $38CB, $0A50, $1BD9, $6F66, $7EEF, $4C74, $5DFD,
    $B58B, $A402, $9699, $8710, $F3AF, $E226, $D0BD, $C134,
    $39C3, $284A, $1AD1, $0B58, $7FE7, $6E6E, $5CF5, $4D7C,
    $C60C, $D785, $E51E, $F497, $8028, $91A1, $A33A, $B2B3,
    $4A44, $5BCD, $6956, $78DF, $0C60, $1DE9, $2F72, $3EFB,
    $D68D, $C704, $F59F, $E416, $90A9, $8120, $B3BB, $A232,
    $5AC5, $4B4C, $79D7, $685E, $1CE1, $0D68, $3FF3, $2E7A,
    $E70E, $F687, $C41C, $D595, $A12A, $B0A3, $8238, $93B1,
    $6B46, $7ACF, $4854, $59DD, $2D62, $3CEB, $0E70, $1FF9,
    $F78F, $E606, $D49D, $C514, $B1AB, $A022, $92B9, $8330,
    $7BC7, $6A4E, $58D5, $495C, $3DE3, $2C6A, $1EF1, $0F78
    );

procedure ArrByteToLong(var ArByte: Array of byte; var ArLong: Array of Integer);
{$IFDEF CIL}
var
  n: integer;
{$ENDIF}
begin
  if (High(ArByte) + 1) > ((High(ArLong) + 1) * 4) then
    Exit;
  {$IFDEF CIL}
  for n := 0 to ((high(ArByte) + 1) div 4) - 1 do
    ArLong[n] := ArByte[n * 4 + 0]
      + (ArByte[n * 4 + 1] shl 8)
      + (ArByte[n * 4 + 2] shl 16)
      + (ArByte[n * 4 + 3] shl 24);
  {$ELSE}
  Move(ArByte[0], ArLong[0], High(ArByte) + 1);
  {$ENDIF}
end;

procedure ArrLongToByte(var ArLong: Array of Integer; var ArByte: Array of byte);
{$IFDEF CIL}
var
  n: integer;
{$ENDIF}
begin
  if (High(ArByte) + 1) < ((High(ArLong) + 1) * 4) then
    Exit;
  {$IFDEF CIL}
  for n := 0 to high(ArLong) do
  begin
    ArByte[n * 4 + 0] := ArLong[n] and $000000FF;
    ArByte[n * 4 + 1] := (ArLong[n] shr 8) and $000000FF;
    ArByte[n * 4 + 2] := (ArLong[n] shr 16) and $000000FF;
    ArByte[n * 4 + 3] := (ArLong[n] shr 24) and $000000FF;
  end;
  {$ELSE}
  Move(ArLong[0], ArByte[0], High(ArByte) + 1);
  {$ENDIF}
end;

type
  TMDCtx = record
    State: array[0..3] of Integer;
    Count: array[0..1] of Integer;
    BufAnsiChar: array[0..63] of Byte;
    BufLong: array[0..15] of Integer;
  end;
  TSHA1Ctx= record
    Hi, Lo: integer;
    Buffer: array[0..63] of byte;
    Index: integer;
    Hash: array[0..4] of Integer;
    HashByte: array[0..19] of byte;
  end;

  TMDTransform = procedure(var Buf: array of LongInt; const Data: array of LongInt);

{==============================================================================}

function DecodeTriplet(const Value: AnsiString; Delimiter: AnsiChar): AnsiString;
var
  x, l, lv: Integer;
  c: AnsiChar;
  b: Byte;
  bad: Boolean;
begin
  lv := Length(Value);
  SetLength(Result, lv);
  x := 1;
  l := 1;
  while x <= lv do
  begin
    c := Value[x];
    Inc(x);
    if c <> Delimiter then
    begin
      Result[l] := c;
      Inc(l);
    end
    else
      if x < lv then
      begin
        Case Value[x] Of
          #13:
            if (Value[x + 1] = #10) then
              Inc(x, 2)
            else
              Inc(x);
          #10:
            if (Value[x + 1] = #13) then
              Inc(x, 2)
            else
              Inc(x);
        else
          begin
            bad := False;
            Case Value[x] Of
              '0'..'9': b := (Byte(Value[x]) - 48) Shl 4;
              'a'..'f', 'A'..'F': b := ((Byte(Value[x]) And 7) + 9) shl 4;
            else
              begin
                b := 0;
                bad := True;
              end;
            end;
            Case Value[x + 1] Of
              '0'..'9': b := b Or (Byte(Value[x + 1]) - 48);
              'a'..'f', 'A'..'F': b := b Or ((Byte(Value[x + 1]) And 7) + 9);
            else
              bad := True;
            end;
            if bad then
            begin
              Result[l] := c;
              Inc(l);
            end
            else
            begin
              Inc(x, 2);
              Result[l] := AnsiChar(b);
              Inc(l);
            end;
          end;
        end;
      end
      else
        break;
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}

function DecodeQuotedPrintable(const Value: AnsiString): AnsiString;
begin
  Result := DecodeTriplet(Value, '=');
end;

{==============================================================================}

function DecodeURL(const Value: AnsiString): AnsiString;
begin
  Result := DecodeTriplet(Value, '%');
end;

{==============================================================================}

function EncodeTriplet(const Value: AnsiString; Delimiter: AnsiChar;
  Specials: TSpecials): AnsiString;
var
  n, l: Integer;
  s: AnsiString;
  c: AnsiChar;
begin
  SetLength(Result, Length(Value) * 3);
  l := 1;
  for n := 1 to Length(Value) do
  begin
    c := Value[n];
    if c in Specials then
    begin
      Result[l] := Delimiter;
      Inc(l);
      s := IntToHex(Ord(c), 2);
      Result[l] := s[1];
      Inc(l);
      Result[l] := s[2];
      Inc(l);
    end
    else
    begin
      Result[l] := c;
      Inc(l);
    end;
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}

function EncodeQuotedPrintable(const Value: AnsiString): AnsiString;
begin
  Result := EncodeTriplet(Value, '=',  ['='] + NonAsciiChar);
end;

{==============================================================================}

function EncodeSafeQuotedPrintable(const Value: AnsiString): AnsiString;
begin
  Result := EncodeTriplet(Value, '=', SpecialChar + NonAsciiChar);
end;

{==============================================================================}

function EncodeURLElement(const Value: AnsiString): AnsiString;
begin
  Result := EncodeTriplet(Value, '%', URLSpecialChar + URLFullSpecialChar);
end;

{==============================================================================}

function EncodeURL(const Value: AnsiString): AnsiString;
begin
  Result := EncodeTriplet(Value, '%', URLSpecialChar);
end;

{==============================================================================}

function Decode4to3(const Value, Table: AnsiString): AnsiString;
var
  x, y, n, l: Integer;
  d: array[0..3] of Byte;
begin
  SetLength(Result, Length(Value));
  x := 1;
  l := 1;
  while x <= Length(Value) do
  begin
    for n := 0 to 3 do
    begin
      if x > Length(Value) then
        d[n] := 64
      else
      begin
        y := Pos(Value[x], Table);
        if y < 1 then
          y := 1;
        d[n] := y - 1;
      end;
      Inc(x);
    end;
    Result[l] := AnsiChar((D[0] and $3F) shl 2 + (D[1] and $30) shr 4);
    Inc(l);
    if d[2] <> 64 then
    begin
      Result[l] := AnsiChar((D[1] and $0F) shl 4 + (D[2] and $3C) shr 2);
      Inc(l);
      if d[3] <> 64 then
      begin
        Result[l] := AnsiChar((D[2] and $03) shl 6 + (D[3] and $3F));
        Inc(l);
      end;
    end;
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}
function Decode4to3Ex(const Value, Table: AnsiString): AnsiString;
var
  x, y, lv: Integer;
  d: integer;
  dl: integer;
  c: byte;
  p: integer;
begin
  lv := Length(Value);
  SetLength(Result, lv);
  x := 1;
  dl := 4;
  d := 0;
  p := 1;
  while x <= lv do
  begin
    y := Ord(Value[x]);
    if y in [33..127] then
      c := Ord(Table[y - 32])
    else
      c := 64;
    Inc(x);
    if c > 63 then
      continue;
    d := (d shl 6) or c;
    dec(dl);
    if dl <> 0 then
      continue;
    Result[p] := AnsiChar((d shr 16) and $ff);
    inc(p);
    Result[p] := AnsiChar((d shr 8) and $ff);
    inc(p);
    Result[p] := AnsiChar(d and $ff);
    inc(p);
    d := 0;
    dl := 4;
  end;
  case dl of
    1:
      begin
        d := d shr 2;
        Result[p] := AnsiChar((d shr 8) and $ff);
        inc(p);
        Result[p] := AnsiChar(d and $ff);
        inc(p);
      end;
    2:
      begin
        d := d shr 4;
        Result[p] := AnsiChar(d and $ff);
        inc(p);
      end;
  end;
  SetLength(Result, p - 1);
end;

{==============================================================================}

function Encode3to4(const Value, Table: AnsiString): AnsiString;
var
  c: Byte;
  n, l: Integer;
  Count: Integer;
  DOut: array[0..3] of Byte;
begin
  setlength(Result, ((Length(Value) + 2) div 3) * 4);
  l := 1;
  Count := 1;
  while Count <= Length(Value) do
  begin
    c := Ord(Value[Count]);
    Inc(Count);
    DOut[0] := (c and $FC) shr 2;
    DOut[1] := (c and $03) shl 4;
    if Count <= Length(Value) then
    begin
      c := Ord(Value[Count]);
      Inc(Count);
      DOut[1] := DOut[1] + (c and $F0) shr 4;
      DOut[2] := (c and $0F) shl 2;
      if Count <= Length(Value) then
      begin
        c := Ord(Value[Count]);
        Inc(Count);
        DOut[2] := DOut[2] + (c and $C0) shr 6;
        DOut[3] := (c and $3F);
      end
      else
      begin
        DOut[3] := $40;
      end;
    end
    else
    begin
      DOut[2] := $40;
      DOut[3] := $40;
    end;
    for n := 0 to 3 do
    begin
      if (DOut[n] + 1) <= Length(Table) then
      begin
        Result[l] := Table[DOut[n] + 1];
        Inc(l);
      end;
    end;
  end;
  SetLength(Result, l - 1);
end;

{==============================================================================}

function DecodeBase64(const Value: AnsiString): AnsiString;
begin
  Result := Decode4to3Ex(Value, ReTableBase64);
end;

{==============================================================================}

function EncodeBase64(const Value: AnsiString): AnsiString;
begin
  Result := Encode3to4(Value, TableBase64);
end;

{==============================================================================}

function DecodeBase64mod(const Value: AnsiString): AnsiString;
begin
  Result := Decode4to3(Value, TableBase64mod);
end;

{==============================================================================}

function EncodeBase64mod(const Value: AnsiString): AnsiString;
begin
  Result := Encode3to4(Value, TableBase64mod);
end;

{==============================================================================}

function DecodeUU(const Value: AnsiString): AnsiString;
var
  s: AnsiString;
  uut: AnsiString;
  x: Integer;
begin
  Result := '';
  uut := TableUU;
  s := trim(UpperCase(Value));
  if s = '' then Exit;
  if Pos('BEGIN', s) = 1 then
    Exit;
  if Pos('END', s) = 1 then
    Exit;
  if Pos('TABLE', s) = 1 then
    Exit; //ignore Table yet (set custom UUT)
  //begin decoding
  x := Pos(Value[1], uut) - 1;
  case (x mod 3) of
    0: x :=(x div 3)* 4;
    1: x :=((x div 3) * 4) + 2;
    2: x :=((x  div 3) * 4) + 3;
  end;
  //x - lenght UU line
  s := Copy(Value, 2, x);
  if s = '' then
    Exit;
  s := s + StringOfChar(' ', x - length(s));
  Result := Decode4to3(s, uut);
end;

{==============================================================================}

function EncodeUU(const Value: AnsiString): AnsiString;
begin
  Result := '';
  if Length(Value) < Length(TableUU) then
    Result := TableUU[Length(Value) + 1] + Encode3to4(Value, TableUU);
end;

{==============================================================================}

function DecodeXX(const Value: AnsiString): AnsiString;
var
  s: AnsiString;
  x: Integer;
begin
  Result := '';
  s := trim(UpperCase(Value));
  if s = '' then
    Exit;
  if Pos('BEGIN', s) = 1 then
    Exit;
  if Pos('END', s) = 1 then
    Exit;
  //begin decoding
  x := Pos(Value[1], TableXX) - 1;
  case (x mod 3) of
    0: x :=(x div 3)* 4;
    1: x :=((x div 3) * 4) + 2;
    2: x :=((x  div 3) * 4) + 3;
  end;
  //x - lenght XX line
  s := Copy(Value, 2, x);
  if s = '' then
    Exit;
  s := s + StringOfChar(' ', x - length(s));
  Result := Decode4to3(s, TableXX);
end;

{==============================================================================}

function DecodeYEnc(const Value: AnsiString): AnsiString;
var
  C : Byte;
  i: integer;
begin
  Result := '';
  i := 1;
  while i <= Length(Value) do
  begin
    c := Ord(Value[i]);
    Inc(i);
    if c = Ord('=') then
    begin
      c := Ord(Value[i]);
      Inc(i);
      Dec(c, 64);
    end;
    Dec(C, 42);
    Result := Result + AnsiChar(C);
  end;
end;

{==============================================================================}

function UpdateCrc32(Value: Byte; Crc32: Integer): Integer;
begin
  Result := (Crc32 shr 8)
    xor crc32tab[Byte(Value xor (Crc32 and Integer($000000FF)))];
end;

{==============================================================================}

function Crc32(const Value: AnsiString): Integer;
var
  n: Integer;
begin
  Result := Integer($FFFFFFFF);
  for n := 1 to Length(Value) do
    Result := UpdateCrc32(Ord(Value[n]), Result);
  Result := not Result;
end;

{==============================================================================}

function UpdateCrc16(Value: Byte; Crc16: Word): Word;
begin
  Result := ((Crc16 shr 8) and $00FF) xor
    crc16tab[Byte(Crc16 xor (Word(Value)) and $00FF)];
end;

{==============================================================================}

function Crc16(const Value: AnsiString): Word;
var
  n: Integer;
begin
  Result := $FFFF;
  for n := 1 to Length(Value) do
    Result := UpdateCrc16(Ord(Value[n]), Result);
end;

{==============================================================================}

procedure MDInit(var MDContext: TMDCtx);
var
  n: integer;
begin
  MDContext.Count[0] := 0;
  MDContext.Count[1] := 0;
  for n := 0 to high(MDContext.BufAnsiChar) do
    MDContext.BufAnsiChar[n] := 0;
  for n := 0 to high(MDContext.BufLong) do
    MDContext.BufLong[n] := 0;
  MDContext.State[0] := Integer($67452301);
  MDContext.State[1] := Integer($EFCDAB89);
  MDContext.State[2] := Integer($98BADCFE);
  MDContext.State[3] := Integer($10325476);
end;

procedure MD5Transform(var Buf: array of LongInt; const Data: array of LongInt);
var
  A, B, C, D: LongInt;

  procedure Round1(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Z xor (X and (Y xor Z))) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X);
  end;

  procedure Round2(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Y xor (Z and (X xor Y))) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X);
  end;

  procedure Round3(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (X xor Y xor Z) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X);
  end;

  procedure Round4(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Y xor (X or not Z)) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X);
  end;
begin
  A := Buf[0];
  B := Buf[1];
  C := Buf[2];
  D := Buf[3];

  Round1(A, B, C, D, Data[0] + Longint($D76AA478), 7);
  Round1(D, A, B, C, Data[1] + Longint($E8C7B756), 12);
  Round1(C, D, A, B, Data[2] + Longint($242070DB), 17);
  Round1(B, C, D, A, Data[3] + Longint($C1BDCEEE), 22);
  Round1(A, B, C, D, Data[4] + Longint($F57C0FAF), 7);
  Round1(D, A, B, C, Data[5] + Longint($4787C62A), 12);
  Round1(C, D, A, B, Data[6] + Longint($A8304613), 17);
  Round1(B, C, D, A, Data[7] + Longint($FD469501), 22);
  Round1(A, B, C, D, Data[8] + Longint($698098D8), 7);
  Round1(D, A, B, C, Data[9] + Longint($8B44F7AF), 12);
  Round1(C, D, A, B, Data[10] + Longint($FFFF5BB1), 17);
  Round1(B, C, D, A, Data[11] + Longint($895CD7BE), 22);
  Round1(A, B, C, D, Data[12] + Longint($6B901122), 7);
  Round1(D, A, B, C, Data[13] + Longint($FD987193), 12);
  Round1(C, D, A, B, Data[14] + Longint($A679438E), 17);
  Round1(B, C, D, A, Data[15] + Longint($49B40821), 22);

  Round2(A, B, C, D, Data[1] + Longint($F61E2562), 5);
  Round2(D, A, B, C, Data[6] + Longint($C040B340), 9);
  Round2(C, D, A, B, Data[11] + Longint($265E5A51), 14);
  Round2(B, C, D, A, Data[0] + Longint($E9B6C7AA), 20);
  Round2(A, B, C, D, Data[5] + Longint($D62F105D), 5);
  Round2(D, A, B, C, Data[10] + Longint($02441453), 9);
  Round2(C, D, A, B, Data[15] + Longint($D8A1E681), 14);
  Round2(B, C, D, A, Data[4] + Longint($E7D3FBC8), 20);
  Round2(A, B, C, D, Data[9] + Longint($21E1CDE6), 5);
  Round2(D, A, B, C, Data[14] + Longint($C33707D6), 9);
  Round2(C, D, A, B, Data[3] + Longint($F4D50D87), 14);
  Round2(B, C, D, A, Data[8] + Longint($455A14ED), 20);
  Round2(A, B, C, D, Data[13] + Longint($A9E3E905), 5);
  Round2(D, A, B, C, Data[2] + Longint($FCEFA3F8), 9);
  Round2(C, D, A, B, Data[7] + Longint($676F02D9), 14);
  Round2(B, C, D, A, Data[12] + Longint($8D2A4C8A), 20);

  Round3(A, B, C, D, Data[5] + Longint($FFFA3942), 4);
  Round3(D, A, B, C, Data[8] + Longint($8771F681), 11);
  Round3(C, D, A, B, Data[11] + Longint($6D9D6122), 16);
  Round3(B, C, D, A, Data[14] + Longint($FDE5380C), 23);
  Round3(A, B, C, D, Data[1] + Longint($A4BEEA44), 4);
  Round3(D, A, B, C, Data[4] + Longint($4BDECFA9), 11);
  Round3(C, D, A, B, Data[7] + Longint($F6BB4B60), 16);
  Round3(B, C, D, A, Data[10] + Longint($BEBFBC70), 23);
  Round3(A, B, C, D, Data[13] + Longint($289B7EC6), 4);
  Round3(D, A, B, C, Data[0] + Longint($EAA127FA), 11);
  Round3(C, D, A, B, Data[3] + Longint($D4EF3085), 16);
  Round3(B, C, D, A, Data[6] + Longint($04881D05), 23);
  Round3(A, B, C, D, Data[9] + Longint($D9D4D039), 4);
  Round3(D, A, B, C, Data[12] + Longint($E6DB99E5), 11);
  Round3(C, D, A, B, Data[15] + Longint($1FA27CF8), 16);
  Round3(B, C, D, A, Data[2] + Longint($C4AC5665), 23);

  Round4(A, B, C, D, Data[0] + Longint($F4292244), 6);
  Round4(D, A, B, C, Data[7] + Longint($432AFF97), 10);
  Round4(C, D, A, B, Data[14] + Longint($AB9423A7), 15);
  Round4(B, C, D, A, Data[5] + Longint($FC93A039), 21);
  Round4(A, B, C, D, Data[12] + Longint($655B59C3), 6);
  Round4(D, A, B, C, Data[3] + Longint($8F0CCC92), 10);
  Round4(C, D, A, B, Data[10] + Longint($FFEFF47D), 15);
  Round4(B, C, D, A, Data[1] + Longint($85845DD1), 21);
  Round4(A, B, C, D, Data[8] + Longint($6FA87E4F), 6);
  Round4(D, A, B, C, Data[15] + Longint($FE2CE6E0), 10);
  Round4(C, D, A, B, Data[6] + Longint($A3014314), 15);
  Round4(B, C, D, A, Data[13] + Longint($4E0811A1), 21);
  Round4(A, B, C, D, Data[4] + Longint($F7537E82), 6);
  Round4(D, A, B, C, Data[11] + Longint($BD3AF235), 10);
  Round4(C, D, A, B, Data[2] + Longint($2AD7D2BB), 15);
  Round4(B, C, D, A, Data[9] + Longint($EB86D391), 21);

  Inc(Buf[0], A);
  Inc(Buf[1], B);
  Inc(Buf[2], C);
  Inc(Buf[3], D);
end;

//fixed by James McAdams
procedure MDUpdate(var MDContext: TMDCtx; const Data: AnsiString; transform: TMDTransform);
var
  Index, partLen, InputLen, I: integer;
{$IFDEF CIL}
  n: integer;
{$ENDIF}
begin
  InputLen := Length(Data);
  with MDContext do
  begin
    Index := (Count[0] shr 3) and $3F;
    Inc(Count[0], InputLen shl 3);
    if Count[0] < (InputLen shl 3) then
      Inc(Count[1]);
    Inc(Count[1], InputLen shr 29);
    partLen := 64 - Index;
    if InputLen >= partLen then
    begin
      ArrLongToByte(BufLong, BufAnsiChar);
      {$IFDEF CIL}
      for n := 1 to partLen do
        BufAnsiChar[index - 1 + n] := Ord(Data[n]);
      {$ELSE}
      Move(Data[1], BufAnsiChar[Index], partLen);
      {$ENDIF}
      ArrByteToLong(BufAnsiChar, BufLong);
      Transform(State, Buflong);
      I := partLen;
  		while I + 63 < InputLen do
      begin
        ArrLongToByte(BufLong, BufAnsiChar);
        {$IFDEF CIL}
        for n := 1 to 64 do
          BufAnsiChar[n - 1] := Ord(Data[i + n]);
        {$ELSE}
        Move(Data[I+1], BufAnsiChar, 64);
        {$ENDIF}
        ArrByteToLong(BufAnsiChar, BufLong);
        Transform(State, Buflong);
	  	  inc(I, 64);
		  end;
      Index := 0;
    end
    else
      I := 0;
    ArrLongToByte(BufLong, BufAnsiChar);
    {$IFDEF CIL}
    for n := 1 to InputLen-I do
      BufAnsiChar[Index + n - 1] := Ord(Data[i + n]);
    {$ELSE}
    Move(Data[I+1], BufAnsiChar[Index], InputLen-I);
    {$ENDIF}
    ArrByteToLong(BufAnsiChar, BufLong);
  end
end;

function MDFinal(var MDContext: TMDCtx; transform: TMDTransform): AnsiString;
var
  Cnt: Word;
  P: Byte;
  digest: array[0..15] of Byte;
  i: Integer;
  n: integer;
begin
  for I := 0 to 15 do
    Digest[I] := I + 1;
  with MDContext do
  begin
    Cnt := (Count[0] shr 3) and $3F;
    P := Cnt;
    BufAnsiChar[P] := $80;
    Inc(P);
    Cnt := 64 - 1 - Cnt;
    if Cnt < 8 then
    begin
      for n := 0 to cnt - 1 do
        BufAnsiChar[P + n] := 0;
      ArrByteToLong(BufAnsiChar, BufLong);
//      FillChar(BufAnsiChar[P], Cnt, #0);
      Transform(State, BufLong);
      ArrLongToByte(BufLong, BufAnsiChar);
      for n := 0 to 55 do
        BufAnsiChar[n] := 0;
      ArrByteToLong(BufAnsiChar, BufLong);
//      FillChar(BufAnsiChar, 56, #0);
    end
    else
    begin
      for n := 0 to Cnt - 8 - 1 do
        BufAnsiChar[p + n] := 0;
      ArrByteToLong(BufAnsiChar, BufLong);
//      FillChar(BufAnsiChar[P], Cnt - 8, #0);
    end;
    BufLong[14] := Count[0];
    BufLong[15] := Count[1];
    Transform(State, BufLong);
    ArrLongToByte(State, Digest);
//    Move(State, Digest, 16);
    Result := '';
    for i := 0 to 15 do
      Result := Result + AnsiChar(digest[i]);
  end;
//  FillChar(MD5Context, SizeOf(TMD5Ctx), #0)
end;

{==============================================================================}

function MD5(const Value: AnsiString): AnsiString;
var
  MDContext: TMDCtx;
begin
  MDInit(MDContext);
  MDUpdate(MDContext, Value, @MD5Transform);
  Result := MDFinal(MDContext, @MD5Transform);
end;

{==============================================================================}

function HMAC_MD5(Text, Key: AnsiString): AnsiString;
var
  ipad, opad, s: AnsiString;
  n: Integer;
  MDContext: TMDCtx;
begin
  if Length(Key) > 64 then
    Key := md5(Key);
  ipad := StringOfChar(#$36, 64);
  opad := StringOfChar(#$5C, 64);
  for n := 1 to Length(Key) do
  begin
    ipad[n] := AnsiChar(Byte(ipad[n]) xor Byte(Key[n]));
    opad[n] := AnsiChar(Byte(opad[n]) xor Byte(Key[n]));
  end;
  MDInit(MDContext);
  MDUpdate(MDContext, ipad, @MD5Transform);
  MDUpdate(MDContext, Text, @MD5Transform);
  s := MDFinal(MDContext, @MD5Transform);
  MDInit(MDContext);
  MDUpdate(MDContext, opad, @MD5Transform);
  MDUpdate(MDContext, s, @MD5Transform);
  Result := MDFinal(MDContext, @MD5Transform);
end;

{==============================================================================}

function MD5LongHash(const Value: AnsiString; Len: integer): AnsiString;
var
  cnt, rest: integer;
  l: integer;
  n: integer;
  MDContext: TMDCtx;
begin
  l := length(Value);
  cnt := Len div l;
  rest := Len mod l;
  MDInit(MDContext);
  for n := 1 to cnt do
    MDUpdate(MDContext, Value, @MD5Transform);
  if rest > 0 then
    MDUpdate(MDContext, Copy(Value, 1, rest), @MD5Transform);
  Result := MDFinal(MDContext, @MD5Transform);
end;

{==============================================================================}
// SHA1 is based on sources by Dave Barton (davebarton@bigfoot.com)

procedure SHA1init( var SHA1Context: TSHA1Ctx );
var
  n: integer;
begin
  SHA1Context.Hi := 0;
  SHA1Context.Lo := 0;
  SHA1Context.Index := 0;
  for n := 0 to High(SHA1Context.Buffer) do
    SHA1Context.Buffer[n] := 0;
  for n := 0 to High(SHA1Context.HashByte) do
    SHA1Context.HashByte[n] := 0;
//  FillChar(SHA1Context, SizeOf(TSHA1Ctx), #0);
  SHA1Context.Hash[0] := integer($67452301);
  SHA1Context.Hash[1] := integer($EFCDAB89);
  SHA1Context.Hash[2] := integer($98BADCFE);
  SHA1Context.Hash[3] := integer($10325476);
  SHA1Context.Hash[4] := integer($C3D2E1F0);
end;

//******************************************************************************
function RB(A: integer): integer;
begin
  Result := (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

procedure SHA1Compress(var Data: TSHA1Ctx);
var
  A, B, C, D, E, T: integer;
  W: array[0..79] of integer;
  i: integer;
  n: integer;

  function F1(x, y, z: integer): integer;
  begin
    Result := z xor (x and (y xor z));
  end;
  function F2(x, y, z: integer): integer;
  begin
    Result := x xor y xor z;
  end;
  function F3(x, y, z: integer): integer;
  begin
    Result := (x and y) or (z and (x or y));
  end;
  function LRot32(X: integer; c: integer): integer;
  begin
    result := (x shl c) or (x shr (32 - c));
  end;
begin
  ArrByteToLong(Data.Buffer, W);
//  Move(Data.Buffer, W, Sizeof(Data.Buffer));
  for i := 0 to 15 do
    W[i] := RB(W[i]);
  for i := 16 to 79 do
    W[i] := LRot32(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16], 1);
  A := Data.Hash[0];
  B := Data.Hash[1];
  C := Data.Hash[2];
  D := Data.Hash[3];
  E := Data.Hash[4];
  for i := 0 to 19 do
  begin
    T := LRot32(A, 5) + F1(B, C, D) + E + W[i] + integer($5A827999);
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for i := 20 to 39 do
  begin
    T := LRot32(A, 5) + F2(B, C, D) + E + W[i] + integer($6ED9EBA1);
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for i := 40 to 59 do
  begin
    T := LRot32(A, 5) + F3(B, C, D) + E + W[i] + integer($8F1BBCDC);
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  for i := 60 to 79 do
  begin
    T := LRot32(A, 5) + F2(B, C, D) + E + W[i] + integer($CA62C1D6);
    E := D;
    D := C;
    C := LRot32(B, 30);
    B := A;
    A := T;
  end;
  Data.Hash[0] := Data.Hash[0] + A;
  Data.Hash[1] := Data.Hash[1] + B;
  Data.Hash[2] := Data.Hash[2] + C;
  Data.Hash[3] := Data.Hash[3] + D;
  Data.Hash[4] := Data.Hash[4] + E;
  for n := 0 to high(w) do
    w[n] := 0;
//  FillChar(W, Sizeof(W), 0);
  for n := 0 to high(Data.Buffer) do
    Data.Buffer[n] := 0;
//  FillChar(Data.Buffer, Sizeof(Data.Buffer), 0);
end;

//******************************************************************************
procedure SHA1Update(var Context: TSHA1Ctx; const Data: AnsiString);
var
  Len: integer;
  n: integer;
  i, k: integer;
begin
  Len := Length(data);
  for k := 0 to 7 do
  begin
    i := Context.Lo;
    Inc(Context.Lo, Len);
    if Context.Lo < i then
      Inc(Context.Hi);
  end;
  for n := 1 to len do
  begin
    Context.Buffer[Context.Index] := byte(Data[n]);
    Inc(Context.Index);
    if Context.Index = 64 then
    begin
      Context.Index := 0;
      SHA1Compress(Context);
    end;
  end;
end;

//******************************************************************************
function SHA1Final(var Context: TSHA1Ctx): AnsiString;
type
  Pinteger = ^integer;
var
  i: integer;
  procedure ItoArr(var Ar: Array of byte; I, value: Integer);
  begin
    Ar[i + 0] := Value and $000000FF;
    Ar[i + 1] := (Value shr 8) and $000000FF;
    Ar[i + 2] := (Value shr 16) and $000000FF;
    Ar[i + 3] := (Value shr 24) and $000000FF;
  end;
begin
  Context.Buffer[Context.Index] := $80;
  if Context.Index >= 56 then
    SHA1Compress(Context);
  ItoArr(Context.Buffer, 56, RB(Context.Hi));
  ItoArr(Context.Buffer, 60, RB(Context.Lo));
//  Pinteger(@Context.Buffer[56])^ := RB(Context.Hi);
//  Pinteger(@Context.Buffer[60])^ := RB(Context.Lo);
  SHA1Compress(Context);
  Context.Hash[0] := RB(Context.Hash[0]);
  Context.Hash[1] := RB(Context.Hash[1]);
  Context.Hash[2] := RB(Context.Hash[2]);
  Context.Hash[3] := RB(Context.Hash[3]);
  Context.Hash[4] := RB(Context.Hash[4]);
  ArrLongToByte(Context.Hash, Context.HashByte);
  Result := '';
  for i := 0 to 19 do
    Result := Result + AnsiChar(Context.HashByte[i]);
end;

function SHA1(const Value: AnsiString): AnsiString;
var
  SHA1Context: TSHA1Ctx;
begin
  SHA1Init(SHA1Context);
  SHA1Update(SHA1Context, Value);
  Result := SHA1Final(SHA1Context);
end;

{==============================================================================}

function HMAC_SHA1(Text, Key: AnsiString): AnsiString;
var
  ipad, opad, s: AnsiString;
  n: Integer;
  SHA1Context: TSHA1Ctx;
begin
  if Length(Key) > 64 then
    Key := SHA1(Key);
  ipad := StringOfChar(#$36, 64);
  opad := StringOfChar(#$5C, 64);
  for n := 1 to Length(Key) do
  begin
    ipad[n] := AnsiChar(Byte(ipad[n]) xor Byte(Key[n]));
    opad[n] := AnsiChar(Byte(opad[n]) xor Byte(Key[n]));
  end;
  SHA1Init(SHA1Context);
  SHA1Update(SHA1Context, ipad);
  SHA1Update(SHA1Context, Text);
  s := SHA1Final(SHA1Context);
  SHA1Init(SHA1Context);
  SHA1Update(SHA1Context, opad);
  SHA1Update(SHA1Context, s);
  Result := SHA1Final(SHA1Context);
end;

{==============================================================================}

function SHA1LongHash(const Value: AnsiString; Len: integer): AnsiString;
var
  cnt, rest: integer;
  l: integer;
  n: integer;
  SHA1Context: TSHA1Ctx;
begin
  l := length(Value);
  cnt := Len div l;
  rest := Len mod l;
  SHA1Init(SHA1Context);
  for n := 1 to cnt do
    SHA1Update(SHA1Context, Value);
  if rest > 0 then
    SHA1Update(SHA1Context, Copy(Value, 1, rest));
  Result := SHA1Final(SHA1Context);
end;

{==============================================================================}

procedure MD4Transform(var Buf: array of LongInt; const Data: array of LongInt);
var
  A, B, C, D: LongInt;
  function LRot32(a, b: longint): longint;
  begin
    Result:= (a shl b) or (a shr (32 - b));
  end;
begin
  A := Buf[0];
  B := Buf[1];
  C := Buf[2];
  D := Buf[3];

  A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 0], 3);
  D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 1], 7);
  C:= LRot32(C + (B xor (D and (A xor B))) + Data[ 2], 11);
  B:= LRot32(B + (A xor (C and (D xor A))) + Data[ 3], 19);
  A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 4], 3);
  D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 5], 7);
  C:= LRot32(C + (B xor (D and (A xor B))) + Data[ 6], 11);
  B:= LRot32(B + (A xor (C and (D xor A))) + Data[ 7], 19);
  A:= LRot32(A + (D xor (B and (C xor D))) + Data[ 8], 3);
  D:= LRot32(D + (C xor (A and (B xor C))) + Data[ 9], 7);
  C:= LRot32(C + (B xor (D and (A xor B))) + Data[10], 11);
  B:= LRot32(B + (A xor (C and (D xor A))) + Data[11], 19);
  A:= LRot32(A + (D xor (B and (C xor D))) + Data[12], 3);
  D:= LRot32(D + (C xor (A and (B xor C))) + Data[13], 7);
  C:= LRot32(C + (B xor (D and (A xor B))) + Data[14], 11);
  B:= LRot32(B + (A xor (C and (D xor A))) + Data[15], 19);

  A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 0] + longint($5a827999), 3);
  D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 4] + longint($5a827999), 5);
  C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[ 8] + longint($5a827999), 9);
  B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[12] + longint($5a827999), 13);
  A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 1] + longint($5a827999), 3);
  D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 5] + longint($5a827999), 5);
  C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[ 9] + longint($5a827999), 9);
  B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[13] + longint($5a827999), 13);
  A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 2] + longint($5a827999), 3);
  D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 6] + longint($5a827999), 5);
  C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[10] + longint($5a827999), 9);
  B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[14] + longint($5a827999), 13);
  A:= LRot32(A + ((B and C) or (B and D) or (C and D)) + Data[ 3] + longint($5a827999), 3);
  D:= LRot32(D + ((A and B) or (A and C) or (B and C)) + Data[ 7] + longint($5a827999), 5);
  C:= LRot32(C + ((D and A) or (D and B) or (A and B)) + Data[11] + longint($5a827999), 9);
  B:= LRot32(B + ((C and D) or (C and A) or (D and A)) + Data[15] + longint($5a827999), 13);

  A:= LRot32(A + (B xor C xor D) + Data[ 0] + longint($6ed9eba1), 3);
  D:= LRot32(D + (A xor B xor C) + Data[ 8] + longint($6ed9eba1), 9);
  C:= LRot32(C + (D xor A xor B) + Data[ 4] + longint($6ed9eba1), 11);
  B:= LRot32(B + (C xor D xor A) + Data[12] + longint($6ed9eba1), 15);
  A:= LRot32(A + (B xor C xor D) + Data[ 2] + longint($6ed9eba1), 3);
  D:= LRot32(D + (A xor B xor C) + Data[10] + longint($6ed9eba1), 9);
  C:= LRot32(C + (D xor A xor B) + Data[ 6] + longint($6ed9eba1), 11);
  B:= LRot32(B + (C xor D xor A) + Data[14] + longint($6ed9eba1), 15);
  A:= LRot32(A + (B xor C xor D) + Data[ 1] + longint($6ed9eba1), 3);
  D:= LRot32(D + (A xor B xor C) + Data[ 9] + longint($6ed9eba1), 9);
  C:= LRot32(C + (D xor A xor B) + Data[ 5] + longint($6ed9eba1), 11);
  B:= LRot32(B + (C xor D xor A) + Data[13] + longint($6ed9eba1), 15);
  A:= LRot32(A + (B xor C xor D) + Data[ 3] + longint($6ed9eba1), 3);
  D:= LRot32(D + (A xor B xor C) + Data[11] + longint($6ed9eba1), 9);
  C:= LRot32(C + (D xor A xor B) + Data[ 7] + longint($6ed9eba1), 11);
  B:= LRot32(B + (C xor D xor A) + Data[15] + longint($6ed9eba1), 15);

  Inc(Buf[0], A);
  Inc(Buf[1], B);
  Inc(Buf[2], C);
  Inc(Buf[3], D);
end;

{==============================================================================}

function MD4(const Value: AnsiString): AnsiString;
var
  MDContext: TMDCtx;
begin
  MDInit(MDContext);
  MDUpdate(MDContext, Value, @MD4Transform);
  Result := MDFinal(MDContext, @MD4Transform);
end;

{==============================================================================}


end.
