{==============================================================================|
| Project : Ararat Synapse                                       | 001.001.000 |
|==============================================================================|
| Content: Encryption support                                                  |
|==============================================================================|
| Copyright (c)2007-2011, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2007-2011.                |
| All Rights Reserved.                                                         |
| Based on work of David Barton and Eric Young                                 |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(Encryption support)

Implemented are DES and 3DES encryption/decryption by ECB, CBC, CFB-8bit,
 CFB-block, OFB and CTR methods.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$R-}
{$H+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit synacrypt;

interface

uses
  SysUtils, Classes, synautil, synafpc;

type
  {:@abstract(Implementation of common routines block ciphers (dafault size is 64-bits))

   Do not use this class directly, use descendants only!}
  TSynaBlockCipher= class(TObject)
  protected
    procedure InitKey(Key: AnsiString); virtual;
    function GetSize: byte; virtual;
  private
    IV, CV: AnsiString;
    procedure IncCounter;
  public
    {:Sets the IV to Value and performs a reset}
    procedure SetIV(const Value: AnsiString); virtual;
    {:Returns the current chaining information, not the actual IV}
    function GetIV: AnsiString; virtual;
    {:Reset any stored chaining information}
    procedure Reset; virtual;
    {:Encrypt a 64-bit block of data using the ECB method of encryption}
    function EncryptECB(const InData: AnsiString): AnsiString; virtual;
    {:Decrypt a 64-bit block of data using the ECB method of decryption}
    function DecryptECB(const InData: AnsiString): AnsiString; virtual;
    {:Encrypt data using the CBC method of encryption}
    function EncryptCBC(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the CBC method of decryption}
    function DecryptCBC(const Indata: AnsiString): AnsiString; virtual;
    {:Encrypt data using the CFB (8 bit) method of encryption}
    function EncryptCFB8bit(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the CFB (8 bit) method of decryption}
    function DecryptCFB8bit(const Indata: AnsiString): AnsiString; virtual;
    {:Encrypt data using the CFB (block) method of encryption}
    function EncryptCFBblock(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the CFB (block) method of decryption}
    function DecryptCFBblock(const Indata: AnsiString): AnsiString; virtual;
    {:Encrypt data using the OFB method of encryption}
    function EncryptOFB(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the OFB method of decryption}
    function DecryptOFB(const Indata: AnsiString): AnsiString; virtual;
    {:Encrypt data using the CTR method of encryption}
    function EncryptCTR(const Indata: AnsiString): AnsiString; virtual;
    {:Decrypt data using the CTR method of decryption}
    function DecryptCTR(const Indata: AnsiString): AnsiString; virtual;
    {:Create a encryptor/decryptor instance and initialize it by the Key.}
    constructor Create(Key: AnsiString);
  end;

  {:@abstract(Datatype for holding one DES key data)

    This data type is used internally.}
  TDesKeyData = array[0..31] of integer;

  {:@abstract(Implementation of common routines for DES encryption)

   Do not use this class directly, use descendants only!}
  TSynaCustomDes = class(TSynaBlockcipher)
  protected
    procedure DoInit(KeyB: AnsiString; var KeyData: TDesKeyData);
    function EncryptBlock(const InData: AnsiString; var KeyData: TDesKeyData): AnsiString;
    function DecryptBlock(const InData: AnsiString; var KeyData: TDesKeyData): AnsiString;
  end;

  {:@abstract(Implementation of DES encryption)}
  TSynaDes= class(TSynaCustomDes)
  protected
    KeyData: TDesKeyData;
    procedure InitKey(Key: AnsiString); override;
  public
    {:Encrypt a 64-bit block of data using the ECB method of encryption}
    function EncryptECB(const InData: AnsiString): AnsiString; override;
    {:Decrypt a 64-bit block of data using the ECB method of decryption}
    function DecryptECB(const InData: AnsiString): AnsiString; override;
  end;

  {:@abstract(Implementation of 3DES encryption)}
  TSyna3Des= class(TSynaCustomDes)
  protected
    KeyData: array[0..2] of TDesKeyData;
    procedure InitKey(Key: AnsiString); override;
  public
    {:Encrypt a 64-bit block of data using the ECB method of encryption}
    function EncryptECB(const InData: AnsiString): AnsiString; override;
    {:Decrypt a 64-bit block of data using the ECB method of decryption}
    function DecryptECB(const InData: AnsiString): AnsiString; override;
  end;

const
  BC = 4;
  MAXROUNDS = 14;
type
  {:@abstract(Implementation of AES encryption)}
  TSynaAes= class(TSynaBlockcipher)
  protected
    numrounds: longword;
    rk, drk: array[0..MAXROUNDS,0..7] of longword;
    procedure InitKey(Key: AnsiString); override;
    function GetSize: byte; override;
  public
    {:Encrypt a 128-bit block of data using the ECB method of encryption}
    function EncryptECB(const InData: AnsiString): AnsiString; override;
    {:Decrypt a 128-bit block of data using the ECB method of decryption}
    function DecryptECB(const InData: AnsiString): AnsiString; override;
  end;

{:Call internal test of all DES encryptions. Returns @true if all is OK.}
function TestDes: boolean;
{:Call internal test of all 3DES encryptions. Returns @true if all is OK.}
function Test3Des: boolean;
{:Call internal test of all AES encryptions. Returns @true if all is OK.}
function TestAes: boolean;

{==============================================================================}
implementation

//DES consts
const
  shifts2: array[0..15]of byte=
    (0,0,1,1,1,1,1,1,0,1,1,1,1,1,1,0);

  des_skb: array[0..7,0..63]of integer=(
    (
    (* for C bits (numbered as per FIPS 46) 1 2 3 4 5 6 *)
    integer($00000000),integer($00000010),integer($20000000),integer($20000010),
    integer($00010000),integer($00010010),integer($20010000),integer($20010010),
    integer($00000800),integer($00000810),integer($20000800),integer($20000810),
    integer($00010800),integer($00010810),integer($20010800),integer($20010810),
    integer($00000020),integer($00000030),integer($20000020),integer($20000030),
    integer($00010020),integer($00010030),integer($20010020),integer($20010030),
    integer($00000820),integer($00000830),integer($20000820),integer($20000830),
    integer($00010820),integer($00010830),integer($20010820),integer($20010830),
    integer($00080000),integer($00080010),integer($20080000),integer($20080010),
    integer($00090000),integer($00090010),integer($20090000),integer($20090010),
    integer($00080800),integer($00080810),integer($20080800),integer($20080810),
    integer($00090800),integer($00090810),integer($20090800),integer($20090810),
    integer($00080020),integer($00080030),integer($20080020),integer($20080030),
    integer($00090020),integer($00090030),integer($20090020),integer($20090030),
    integer($00080820),integer($00080830),integer($20080820),integer($20080830),
    integer($00090820),integer($00090830),integer($20090820),integer($20090830)
    ),(
    (* for C bits (numbered as per FIPS 46) 7 8 10 11 12 13 *)
    integer($00000000),integer($02000000),integer($00002000),integer($02002000),
    integer($00200000),integer($02200000),integer($00202000),integer($02202000),
    integer($00000004),integer($02000004),integer($00002004),integer($02002004),
    integer($00200004),integer($02200004),integer($00202004),integer($02202004),
    integer($00000400),integer($02000400),integer($00002400),integer($02002400),
    integer($00200400),integer($02200400),integer($00202400),integer($02202400),
    integer($00000404),integer($02000404),integer($00002404),integer($02002404),
    integer($00200404),integer($02200404),integer($00202404),integer($02202404),
    integer($10000000),integer($12000000),integer($10002000),integer($12002000),
    integer($10200000),integer($12200000),integer($10202000),integer($12202000),
    integer($10000004),integer($12000004),integer($10002004),integer($12002004),
    integer($10200004),integer($12200004),integer($10202004),integer($12202004),
    integer($10000400),integer($12000400),integer($10002400),integer($12002400),
    integer($10200400),integer($12200400),integer($10202400),integer($12202400),
    integer($10000404),integer($12000404),integer($10002404),integer($12002404),
    integer($10200404),integer($12200404),integer($10202404),integer($12202404)
    ),(
    (* for C bits (numbered as per FIPS 46) 14 15 16 17 19 20 *)
    integer($00000000),integer($00000001),integer($00040000),integer($00040001),
    integer($01000000),integer($01000001),integer($01040000),integer($01040001),
    integer($00000002),integer($00000003),integer($00040002),integer($00040003),
    integer($01000002),integer($01000003),integer($01040002),integer($01040003),
    integer($00000200),integer($00000201),integer($00040200),integer($00040201),
    integer($01000200),integer($01000201),integer($01040200),integer($01040201),
    integer($00000202),integer($00000203),integer($00040202),integer($00040203),
    integer($01000202),integer($01000203),integer($01040202),integer($01040203),
    integer($08000000),integer($08000001),integer($08040000),integer($08040001),
    integer($09000000),integer($09000001),integer($09040000),integer($09040001),
    integer($08000002),integer($08000003),integer($08040002),integer($08040003),
    integer($09000002),integer($09000003),integer($09040002),integer($09040003),
    integer($08000200),integer($08000201),integer($08040200),integer($08040201),
    integer($09000200),integer($09000201),integer($09040200),integer($09040201),
    integer($08000202),integer($08000203),integer($08040202),integer($08040203),
    integer($09000202),integer($09000203),integer($09040202),integer($09040203)
    ),(
    (* for C bits (numbered as per FIPS 46) 21 23 24 26 27 28 *)
    integer($00000000),integer($00100000),integer($00000100),integer($00100100),
    integer($00000008),integer($00100008),integer($00000108),integer($00100108),
    integer($00001000),integer($00101000),integer($00001100),integer($00101100),
    integer($00001008),integer($00101008),integer($00001108),integer($00101108),
    integer($04000000),integer($04100000),integer($04000100),integer($04100100),
    integer($04000008),integer($04100008),integer($04000108),integer($04100108),
    integer($04001000),integer($04101000),integer($04001100),integer($04101100),
    integer($04001008),integer($04101008),integer($04001108),integer($04101108),
    integer($00020000),integer($00120000),integer($00020100),integer($00120100),
    integer($00020008),integer($00120008),integer($00020108),integer($00120108),
    integer($00021000),integer($00121000),integer($00021100),integer($00121100),
    integer($00021008),integer($00121008),integer($00021108),integer($00121108),
    integer($04020000),integer($04120000),integer($04020100),integer($04120100),
    integer($04020008),integer($04120008),integer($04020108),integer($04120108),
    integer($04021000),integer($04121000),integer($04021100),integer($04121100),
    integer($04021008),integer($04121008),integer($04021108),integer($04121108)
    ),(
    (* for D bits (numbered as per FIPS 46) 1 2 3 4 5 6 *)
    integer($00000000),integer($10000000),integer($00010000),integer($10010000),
    integer($00000004),integer($10000004),integer($00010004),integer($10010004),
    integer($20000000),integer($30000000),integer($20010000),integer($30010000),
    integer($20000004),integer($30000004),integer($20010004),integer($30010004),
    integer($00100000),integer($10100000),integer($00110000),integer($10110000),
    integer($00100004),integer($10100004),integer($00110004),integer($10110004),
    integer($20100000),integer($30100000),integer($20110000),integer($30110000),
    integer($20100004),integer($30100004),integer($20110004),integer($30110004),
    integer($00001000),integer($10001000),integer($00011000),integer($10011000),
    integer($00001004),integer($10001004),integer($00011004),integer($10011004),
    integer($20001000),integer($30001000),integer($20011000),integer($30011000),
    integer($20001004),integer($30001004),integer($20011004),integer($30011004),
    integer($00101000),integer($10101000),integer($00111000),integer($10111000),
    integer($00101004),integer($10101004),integer($00111004),integer($10111004),
    integer($20101000),integer($30101000),integer($20111000),integer($30111000),
    integer($20101004),integer($30101004),integer($20111004),integer($30111004)
    ),(
    (* for D bits (numbered as per FIPS 46) 8 9 11 12 13 14 *)
    integer($00000000),integer($08000000),integer($00000008),integer($08000008),
    integer($00000400),integer($08000400),integer($00000408),integer($08000408),
    integer($00020000),integer($08020000),integer($00020008),integer($08020008),
    integer($00020400),integer($08020400),integer($00020408),integer($08020408),
    integer($00000001),integer($08000001),integer($00000009),integer($08000009),
    integer($00000401),integer($08000401),integer($00000409),integer($08000409),
    integer($00020001),integer($08020001),integer($00020009),integer($08020009),
    integer($00020401),integer($08020401),integer($00020409),integer($08020409),
    integer($02000000),integer($0A000000),integer($02000008),integer($0A000008),
    integer($02000400),integer($0A000400),integer($02000408),integer($0A000408),
    integer($02020000),integer($0A020000),integer($02020008),integer($0A020008),
    integer($02020400),integer($0A020400),integer($02020408),integer($0A020408),
    integer($02000001),integer($0A000001),integer($02000009),integer($0A000009),
    integer($02000401),integer($0A000401),integer($02000409),integer($0A000409),
    integer($02020001),integer($0A020001),integer($02020009),integer($0A020009),
    integer($02020401),integer($0A020401),integer($02020409),integer($0A020409)
    ),(
    (* for D bits (numbered as per FIPS 46) 16 17 18 19 20 21 *)
    integer($00000000),integer($00000100),integer($00080000),integer($00080100),
    integer($01000000),integer($01000100),integer($01080000),integer($01080100),
    integer($00000010),integer($00000110),integer($00080010),integer($00080110),
    integer($01000010),integer($01000110),integer($01080010),integer($01080110),
    integer($00200000),integer($00200100),integer($00280000),integer($00280100),
    integer($01200000),integer($01200100),integer($01280000),integer($01280100),
    integer($00200010),integer($00200110),integer($00280010),integer($00280110),
    integer($01200010),integer($01200110),integer($01280010),integer($01280110),
    integer($00000200),integer($00000300),integer($00080200),integer($00080300),
    integer($01000200),integer($01000300),integer($01080200),integer($01080300),
    integer($00000210),integer($00000310),integer($00080210),integer($00080310),
    integer($01000210),integer($01000310),integer($01080210),integer($01080310),
    integer($00200200),integer($00200300),integer($00280200),integer($00280300),
    integer($01200200),integer($01200300),integer($01280200),integer($01280300),
    integer($00200210),integer($00200310),integer($00280210),integer($00280310),
    integer($01200210),integer($01200310),integer($01280210),integer($01280310)
    ),(
    (* for D bits (numbered as per FIPS 46) 22 23 24 25 27 28 *)
    integer($00000000),integer($04000000),integer($00040000),integer($04040000),
    integer($00000002),integer($04000002),integer($00040002),integer($04040002),
    integer($00002000),integer($04002000),integer($00042000),integer($04042000),
    integer($00002002),integer($04002002),integer($00042002),integer($04042002),
    integer($00000020),integer($04000020),integer($00040020),integer($04040020),
    integer($00000022),integer($04000022),integer($00040022),integer($04040022),
    integer($00002020),integer($04002020),integer($00042020),integer($04042020),
    integer($00002022),integer($04002022),integer($00042022),integer($04042022),
    integer($00000800),integer($04000800),integer($00040800),integer($04040800),
    integer($00000802),integer($04000802),integer($00040802),integer($04040802),
    integer($00002800),integer($04002800),integer($00042800),integer($04042800),
    integer($00002802),integer($04002802),integer($00042802),integer($04042802),
    integer($00000820),integer($04000820),integer($00040820),integer($04040820),
    integer($00000822),integer($04000822),integer($00040822),integer($04040822),
    integer($00002820),integer($04002820),integer($00042820),integer($04042820),
    integer($00002822),integer($04002822),integer($00042822),integer($04042822)
    ));

  des_sptrans: array[0..7,0..63] of integer=(
    (
    (* nibble 0 *)
    integer($02080800), integer($00080000), integer($02000002), integer($02080802),
    integer($02000000), integer($00080802), integer($00080002), integer($02000002),
    integer($00080802), integer($02080800), integer($02080000), integer($00000802),
    integer($02000802), integer($02000000), integer($00000000), integer($00080002),
    integer($00080000), integer($00000002), integer($02000800), integer($00080800),
    integer($02080802), integer($02080000), integer($00000802), integer($02000800),
    integer($00000002), integer($00000800), integer($00080800), integer($02080002),
    integer($00000800), integer($02000802), integer($02080002), integer($00000000),
    integer($00000000), integer($02080802), integer($02000800), integer($00080002),
    integer($02080800), integer($00080000), integer($00000802), integer($02000800),
    integer($02080002), integer($00000800), integer($00080800), integer($02000002),
    integer($00080802), integer($00000002), integer($02000002), integer($02080000),
    integer($02080802), integer($00080800), integer($02080000), integer($02000802),
    integer($02000000), integer($00000802), integer($00080002), integer($00000000),
    integer($00080000), integer($02000000), integer($02000802), integer($02080800),
    integer($00000002), integer($02080002), integer($00000800), integer($00080802)
    ),(
    (* nibble 1 *)
    integer($40108010), integer($00000000), integer($00108000), integer($40100000),
    integer($40000010), integer($00008010), integer($40008000), integer($00108000),
    integer($00008000), integer($40100010), integer($00000010), integer($40008000),
    integer($00100010), integer($40108000), integer($40100000), integer($00000010),
    integer($00100000), integer($40008010), integer($40100010), integer($00008000),
    integer($00108010), integer($40000000), integer($00000000), integer($00100010),
    integer($40008010), integer($00108010), integer($40108000), integer($40000010),
    integer($40000000), integer($00100000), integer($00008010), integer($40108010),
    integer($00100010), integer($40108000), integer($40008000), integer($00108010),
    integer($40108010), integer($00100010), integer($40000010), integer($00000000),
    integer($40000000), integer($00008010), integer($00100000), integer($40100010),
    integer($00008000), integer($40000000), integer($00108010), integer($40008010),
    integer($40108000), integer($00008000), integer($00000000), integer($40000010),
    integer($00000010), integer($40108010), integer($00108000), integer($40100000),
    integer($40100010), integer($00100000), integer($00008010), integer($40008000),
    integer($40008010), integer($00000010), integer($40100000), integer($00108000)
    ),(
    (* nibble 2 *)
    integer($04000001), integer($04040100), integer($00000100), integer($04000101),
    integer($00040001), integer($04000000), integer($04000101), integer($00040100),
    integer($04000100), integer($00040000), integer($04040000), integer($00000001),
    integer($04040101), integer($00000101), integer($00000001), integer($04040001),
    integer($00000000), integer($00040001), integer($04040100), integer($00000100),
    integer($00000101), integer($04040101), integer($00040000), integer($04000001),
    integer($04040001), integer($04000100), integer($00040101), integer($04040000),
    integer($00040100), integer($00000000), integer($04000000), integer($00040101),
    integer($04040100), integer($00000100), integer($00000001), integer($00040000),
    integer($00000101), integer($00040001), integer($04040000), integer($04000101),
    integer($00000000), integer($04040100), integer($00040100), integer($04040001),
    integer($00040001), integer($04000000), integer($04040101), integer($00000001),
    integer($00040101), integer($04000001), integer($04000000), integer($04040101),
    integer($00040000), integer($04000100), integer($04000101), integer($00040100),
    integer($04000100), integer($00000000), integer($04040001), integer($00000101),
    integer($04000001), integer($00040101), integer($00000100), integer($04040000)
    ),(
    (* nibble 3 *)
    integer($00401008), integer($10001000), integer($00000008), integer($10401008),
    integer($00000000), integer($10400000), integer($10001008), integer($00400008),
    integer($10401000), integer($10000008), integer($10000000), integer($00001008),
    integer($10000008), integer($00401008), integer($00400000), integer($10000000),
    integer($10400008), integer($00401000), integer($00001000), integer($00000008),
    integer($00401000), integer($10001008), integer($10400000), integer($00001000),
    integer($00001008), integer($00000000), integer($00400008), integer($10401000),
    integer($10001000), integer($10400008), integer($10401008), integer($00400000),
    integer($10400008), integer($00001008), integer($00400000), integer($10000008),
    integer($00401000), integer($10001000), integer($00000008), integer($10400000),
    integer($10001008), integer($00000000), integer($00001000), integer($00400008),
    integer($00000000), integer($10400008), integer($10401000), integer($00001000),
    integer($10000000), integer($10401008), integer($00401008), integer($00400000),
    integer($10401008), integer($00000008), integer($10001000), integer($00401008),
    integer($00400008), integer($00401000), integer($10400000), integer($10001008),
    integer($00001008), integer($10000000), integer($10000008), integer($10401000)
    ),(
    (* nibble 4 *)
    integer($08000000), integer($00010000), integer($00000400), integer($08010420),
    integer($08010020), integer($08000400), integer($00010420), integer($08010000),
    integer($00010000), integer($00000020), integer($08000020), integer($00010400),
    integer($08000420), integer($08010020), integer($08010400), integer($00000000),
    integer($00010400), integer($08000000), integer($00010020), integer($00000420),
    integer($08000400), integer($00010420), integer($00000000), integer($08000020),
    integer($00000020), integer($08000420), integer($08010420), integer($00010020),
    integer($08010000), integer($00000400), integer($00000420), integer($08010400),
    integer($08010400), integer($08000420), integer($00010020), integer($08010000),
    integer($00010000), integer($00000020), integer($08000020), integer($08000400),
    integer($08000000), integer($00010400), integer($08010420), integer($00000000),
    integer($00010420), integer($08000000), integer($00000400), integer($00010020),
    integer($08000420), integer($00000400), integer($00000000), integer($08010420),
    integer($08010020), integer($08010400), integer($00000420), integer($00010000),
    integer($00010400), integer($08010020), integer($08000400), integer($00000420),
    integer($00000020), integer($00010420), integer($08010000), integer($08000020)
    ),(
    (* nibble 5 *)
    integer($80000040), integer($00200040), integer($00000000), integer($80202000),
    integer($00200040), integer($00002000), integer($80002040), integer($00200000),
    integer($00002040), integer($80202040), integer($00202000), integer($80000000),
    integer($80002000), integer($80000040), integer($80200000), integer($00202040),
    integer($00200000), integer($80002040), integer($80200040), integer($00000000),
    integer($00002000), integer($00000040), integer($80202000), integer($80200040),
    integer($80202040), integer($80200000), integer($80000000), integer($00002040),
    integer($00000040), integer($00202000), integer($00202040), integer($80002000),
    integer($00002040), integer($80000000), integer($80002000), integer($00202040),
    integer($80202000), integer($00200040), integer($00000000), integer($80002000),
    integer($80000000), integer($00002000), integer($80200040), integer($00200000),
    integer($00200040), integer($80202040), integer($00202000), integer($00000040),
    integer($80202040), integer($00202000), integer($00200000), integer($80002040),
    integer($80000040), integer($80200000), integer($00202040), integer($00000000),
    integer($00002000), integer($80000040), integer($80002040), integer($80202000),
    integer($80200000), integer($00002040), integer($00000040), integer($80200040)
    ),(
    (* nibble 6 *)
    integer($00004000), integer($00000200), integer($01000200), integer($01000004),
    integer($01004204), integer($00004004), integer($00004200), integer($00000000),
    integer($01000000), integer($01000204), integer($00000204), integer($01004000),
    integer($00000004), integer($01004200), integer($01004000), integer($00000204),
    integer($01000204), integer($00004000), integer($00004004), integer($01004204),
    integer($00000000), integer($01000200), integer($01000004), integer($00004200),
    integer($01004004), integer($00004204), integer($01004200), integer($00000004),
    integer($00004204), integer($01004004), integer($00000200), integer($01000000),
    integer($00004204), integer($01004000), integer($01004004), integer($00000204),
    integer($00004000), integer($00000200), integer($01000000), integer($01004004),
    integer($01000204), integer($00004204), integer($00004200), integer($00000000),
    integer($00000200), integer($01000004), integer($00000004), integer($01000200),
    integer($00000000), integer($01000204), integer($01000200), integer($00004200),
    integer($00000204), integer($00004000), integer($01004204), integer($01000000),
    integer($01004200), integer($00000004), integer($00004004), integer($01004204),
    integer($01000004), integer($01004200), integer($01004000), integer($00004004)
    ),(
    (* nibble 7 *)
    integer($20800080), integer($20820000), integer($00020080), integer($00000000),
    integer($20020000), integer($00800080), integer($20800000), integer($20820080),
    integer($00000080), integer($20000000), integer($00820000), integer($00020080),
    integer($00820080), integer($20020080), integer($20000080), integer($20800000),
    integer($00020000), integer($00820080), integer($00800080), integer($20020000),
    integer($20820080), integer($20000080), integer($00000000), integer($00820000),
    integer($20000000), integer($00800000), integer($20020080), integer($20800080),
    integer($00800000), integer($00020000), integer($20820000), integer($00000080),
    integer($00800000), integer($00020000), integer($20000080), integer($20820080),
    integer($00020080), integer($20000000), integer($00000000), integer($00820000),
    integer($20800080), integer($20020080), integer($20020000), integer($00800080),
    integer($20820000), integer($00000080), integer($00800080), integer($20020000),
    integer($20820080), integer($00800000), integer($20800000), integer($20000080),
    integer($00820000), integer($00020080), integer($20020080), integer($20800000),
    integer($00000080), integer($20820000), integer($00820080), integer($00000000),
    integer($20000000), integer($20800080), integer($00020000), integer($00820080)
    ));

//AES consts
const
  MAXBC= 8;
  MAXKC= 8;

  S: array[0..255] of byte= (
     99, 124, 119, 123, 242, 107, 111, 197,  48,   1, 103,  43, 254, 215, 171, 118,
    202, 130, 201, 125, 250,  89,  71, 240, 173, 212, 162, 175, 156, 164, 114, 192,
    183, 253, 147,  38,  54,  63, 247, 204,  52, 165, 229, 241, 113, 216,  49,  21,
      4, 199,  35, 195,  24, 150,   5, 154,   7,  18, 128, 226, 235,  39, 178, 117,
      9, 131,  44,  26,  27, 110,  90, 160,  82,  59, 214, 179,  41, 227,  47, 132,
     83, 209,   0, 237,  32, 252, 177,  91, 106, 203, 190,  57,  74,  76,  88, 207, 
    208, 239, 170, 251,  67,  77,  51, 133,  69, 249,   2, 127,  80,  60, 159, 168, 
     81, 163,  64, 143, 146, 157,  56, 245, 188, 182, 218,  33,  16, 255, 243, 210, 
    205,  12,  19, 236,  95, 151,  68,  23, 196, 167, 126,  61, 100,  93,  25, 115, 
     96, 129,  79, 220,  34,  42, 144, 136,  70, 238, 184,  20, 222,  94,  11, 219, 
    224,  50,  58,  10,  73,   6,  36,  92, 194, 211, 172,  98, 145, 149, 228, 121, 
    231, 200,  55, 109, 141, 213,  78, 169, 108,  86, 244, 234, 101, 122, 174,   8, 
    186, 120,  37,  46,  28, 166, 180, 198, 232, 221, 116,  31,  75, 189, 139, 138,
    112,  62, 181, 102,  72,   3, 246,  14,  97,  53,  87, 185, 134, 193,  29, 158,
    225, 248, 152,  17, 105, 217, 142, 148, 155,  30, 135, 233, 206,  85,  40, 223,
    140, 161, 137,  13, 191, 230,  66, 104,  65, 153,  45,  15, 176,  84, 187,  22);
  T1: array[0..255,0..3] of byte= (
    ($c6,$63,$63,$a5), ($f8,$7c,$7c,$84), ($ee,$77,$77,$99), ($f6,$7b,$7b,$8d), 
    ($ff,$f2,$f2,$0d), ($d6,$6b,$6b,$bd), ($de,$6f,$6f,$b1), ($91,$c5,$c5,$54),
    ($60,$30,$30,$50), ($02,$01,$01,$03), ($ce,$67,$67,$a9), ($56,$2b,$2b,$7d),
    ($e7,$fe,$fe,$19), ($b5,$d7,$d7,$62), ($4d,$ab,$ab,$e6), ($ec,$76,$76,$9a),
    ($8f,$ca,$ca,$45), ($1f,$82,$82,$9d), ($89,$c9,$c9,$40), ($fa,$7d,$7d,$87),
    ($ef,$fa,$fa,$15), ($b2,$59,$59,$eb), ($8e,$47,$47,$c9), ($fb,$f0,$f0,$0b), 
    ($41,$ad,$ad,$ec), ($b3,$d4,$d4,$67), ($5f,$a2,$a2,$fd), ($45,$af,$af,$ea), 
    ($23,$9c,$9c,$bf), ($53,$a4,$a4,$f7), ($e4,$72,$72,$96), ($9b,$c0,$c0,$5b), 
    ($75,$b7,$b7,$c2), ($e1,$fd,$fd,$1c), ($3d,$93,$93,$ae), ($4c,$26,$26,$6a),
    ($6c,$36,$36,$5a), ($7e,$3f,$3f,$41), ($f5,$f7,$f7,$02), ($83,$cc,$cc,$4f), 
    ($68,$34,$34,$5c), ($51,$a5,$a5,$f4), ($d1,$e5,$e5,$34), ($f9,$f1,$f1,$08), 
    ($e2,$71,$71,$93), ($ab,$d8,$d8,$73), ($62,$31,$31,$53), ($2a,$15,$15,$3f),
    ($08,$04,$04,$0c), ($95,$c7,$c7,$52), ($46,$23,$23,$65), ($9d,$c3,$c3,$5e), 
    ($30,$18,$18,$28), ($37,$96,$96,$a1), ($0a,$05,$05,$0f), ($2f,$9a,$9a,$b5),
    ($0e,$07,$07,$09), ($24,$12,$12,$36), ($1b,$80,$80,$9b), ($df,$e2,$e2,$3d), 
    ($cd,$eb,$eb,$26), ($4e,$27,$27,$69), ($7f,$b2,$b2,$cd), ($ea,$75,$75,$9f),
    ($12,$09,$09,$1b), ($1d,$83,$83,$9e), ($58,$2c,$2c,$74), ($34,$1a,$1a,$2e), 
    ($36,$1b,$1b,$2d), ($dc,$6e,$6e,$b2), ($b4,$5a,$5a,$ee), ($5b,$a0,$a0,$fb), 
    ($a4,$52,$52,$f6), ($76,$3b,$3b,$4d), ($b7,$d6,$d6,$61), ($7d,$b3,$b3,$ce),
    ($52,$29,$29,$7b), ($dd,$e3,$e3,$3e), ($5e,$2f,$2f,$71), ($13,$84,$84,$97), 
    ($a6,$53,$53,$f5), ($b9,$d1,$d1,$68), ($00,$00,$00,$00), ($c1,$ed,$ed,$2c), 
    ($40,$20,$20,$60), ($e3,$fc,$fc,$1f), ($79,$b1,$b1,$c8), ($b6,$5b,$5b,$ed), 
    ($d4,$6a,$6a,$be), ($8d,$cb,$cb,$46), ($67,$be,$be,$d9), ($72,$39,$39,$4b), 
    ($94,$4a,$4a,$de), ($98,$4c,$4c,$d4), ($b0,$58,$58,$e8), ($85,$cf,$cf,$4a), 
    ($bb,$d0,$d0,$6b), ($c5,$ef,$ef,$2a), ($4f,$aa,$aa,$e5), ($ed,$fb,$fb,$16),
    ($86,$43,$43,$c5), ($9a,$4d,$4d,$d7), ($66,$33,$33,$55), ($11,$85,$85,$94), 
    ($8a,$45,$45,$cf), ($e9,$f9,$f9,$10), ($04,$02,$02,$06), ($fe,$7f,$7f,$81), 
    ($a0,$50,$50,$f0), ($78,$3c,$3c,$44), ($25,$9f,$9f,$ba), ($4b,$a8,$a8,$e3), 
    ($a2,$51,$51,$f3), ($5d,$a3,$a3,$fe), ($80,$40,$40,$c0), ($05,$8f,$8f,$8a),
    ($3f,$92,$92,$ad), ($21,$9d,$9d,$bc), ($70,$38,$38,$48), ($f1,$f5,$f5,$04), 
    ($63,$bc,$bc,$df), ($77,$b6,$b6,$c1), ($af,$da,$da,$75), ($42,$21,$21,$63), 
    ($20,$10,$10,$30), ($e5,$ff,$ff,$1a), ($fd,$f3,$f3,$0e), ($bf,$d2,$d2,$6d), 
    ($81,$cd,$cd,$4c), ($18,$0c,$0c,$14), ($26,$13,$13,$35), ($c3,$ec,$ec,$2f), 
    ($be,$5f,$5f,$e1), ($35,$97,$97,$a2), ($88,$44,$44,$cc), ($2e,$17,$17,$39), 
    ($93,$c4,$c4,$57), ($55,$a7,$a7,$f2), ($fc,$7e,$7e,$82), ($7a,$3d,$3d,$47), 
    ($c8,$64,$64,$ac), ($ba,$5d,$5d,$e7), ($32,$19,$19,$2b), ($e6,$73,$73,$95), 
    ($c0,$60,$60,$a0), ($19,$81,$81,$98), ($9e,$4f,$4f,$d1), ($a3,$dc,$dc,$7f),
    ($44,$22,$22,$66), ($54,$2a,$2a,$7e), ($3b,$90,$90,$ab), ($0b,$88,$88,$83),
    ($8c,$46,$46,$ca), ($c7,$ee,$ee,$29), ($6b,$b8,$b8,$d3), ($28,$14,$14,$3c), 
    ($a7,$de,$de,$79), ($bc,$5e,$5e,$e2), ($16,$0b,$0b,$1d), ($ad,$db,$db,$76), 
    ($db,$e0,$e0,$3b), ($64,$32,$32,$56), ($74,$3a,$3a,$4e), ($14,$0a,$0a,$1e), 
    ($92,$49,$49,$db), ($0c,$06,$06,$0a), ($48,$24,$24,$6c), ($b8,$5c,$5c,$e4),
    ($9f,$c2,$c2,$5d), ($bd,$d3,$d3,$6e), ($43,$ac,$ac,$ef), ($c4,$62,$62,$a6), 
    ($39,$91,$91,$a8), ($31,$95,$95,$a4), ($d3,$e4,$e4,$37), ($f2,$79,$79,$8b), 
    ($d5,$e7,$e7,$32), ($8b,$c8,$c8,$43), ($6e,$37,$37,$59), ($da,$6d,$6d,$b7),
    ($01,$8d,$8d,$8c), ($b1,$d5,$d5,$64), ($9c,$4e,$4e,$d2), ($49,$a9,$a9,$e0), 
    ($d8,$6c,$6c,$b4), ($ac,$56,$56,$fa), ($f3,$f4,$f4,$07), ($cf,$ea,$ea,$25), 
    ($ca,$65,$65,$af), ($f4,$7a,$7a,$8e), ($47,$ae,$ae,$e9), ($10,$08,$08,$18), 
    ($6f,$ba,$ba,$d5), ($f0,$78,$78,$88), ($4a,$25,$25,$6f), ($5c,$2e,$2e,$72),
    ($38,$1c,$1c,$24), ($57,$a6,$a6,$f1), ($73,$b4,$b4,$c7), ($97,$c6,$c6,$51), 
    ($cb,$e8,$e8,$23), ($a1,$dd,$dd,$7c), ($e8,$74,$74,$9c), ($3e,$1f,$1f,$21), 
    ($96,$4b,$4b,$dd), ($61,$bd,$bd,$dc), ($0d,$8b,$8b,$86), ($0f,$8a,$8a,$85), 
    ($e0,$70,$70,$90), ($7c,$3e,$3e,$42), ($71,$b5,$b5,$c4), ($cc,$66,$66,$aa), 
    ($90,$48,$48,$d8), ($06,$03,$03,$05), ($f7,$f6,$f6,$01), ($1c,$0e,$0e,$12), 
    ($c2,$61,$61,$a3), ($6a,$35,$35,$5f), ($ae,$57,$57,$f9), ($69,$b9,$b9,$d0), 
    ($17,$86,$86,$91), ($99,$c1,$c1,$58), ($3a,$1d,$1d,$27), ($27,$9e,$9e,$b9), 
    ($d9,$e1,$e1,$38), ($eb,$f8,$f8,$13), ($2b,$98,$98,$b3), ($22,$11,$11,$33), 
    ($d2,$69,$69,$bb), ($a9,$d9,$d9,$70), ($07,$8e,$8e,$89), ($33,$94,$94,$a7), 
    ($2d,$9b,$9b,$b6), ($3c,$1e,$1e,$22), ($15,$87,$87,$92), ($c9,$e9,$e9,$20), 
    ($87,$ce,$ce,$49), ($aa,$55,$55,$ff), ($50,$28,$28,$78), ($a5,$df,$df,$7a),
    ($03,$8c,$8c,$8f), ($59,$a1,$a1,$f8), ($09,$89,$89,$80), ($1a,$0d,$0d,$17), 
    ($65,$bf,$bf,$da), ($d7,$e6,$e6,$31), ($84,$42,$42,$c6), ($d0,$68,$68,$b8),
    ($82,$41,$41,$c3), ($29,$99,$99,$b0), ($5a,$2d,$2d,$77), ($1e,$0f,$0f,$11), 
    ($7b,$b0,$b0,$cb), ($a8,$54,$54,$fc), ($6d,$bb,$bb,$d6), ($2c,$16,$16,$3a));
 T2: array[0..255,0..3] of byte= (
    ($a5,$c6,$63,$63), ($84,$f8,$7c,$7c), ($99,$ee,$77,$77), ($8d,$f6,$7b,$7b),
    ($0d,$ff,$f2,$f2), ($bd,$d6,$6b,$6b), ($b1,$de,$6f,$6f), ($54,$91,$c5,$c5),
    ($50,$60,$30,$30), ($03,$02,$01,$01), ($a9,$ce,$67,$67), ($7d,$56,$2b,$2b),
    ($19,$e7,$fe,$fe), ($62,$b5,$d7,$d7), ($e6,$4d,$ab,$ab), ($9a,$ec,$76,$76),
    ($45,$8f,$ca,$ca), ($9d,$1f,$82,$82), ($40,$89,$c9,$c9), ($87,$fa,$7d,$7d),
    ($15,$ef,$fa,$fa), ($eb,$b2,$59,$59), ($c9,$8e,$47,$47), ($0b,$fb,$f0,$f0),
    ($ec,$41,$ad,$ad), ($67,$b3,$d4,$d4), ($fd,$5f,$a2,$a2), ($ea,$45,$af,$af),
    ($bf,$23,$9c,$9c), ($f7,$53,$a4,$a4), ($96,$e4,$72,$72), ($5b,$9b,$c0,$c0),
    ($c2,$75,$b7,$b7), ($1c,$e1,$fd,$fd), ($ae,$3d,$93,$93), ($6a,$4c,$26,$26),
    ($5a,$6c,$36,$36), ($41,$7e,$3f,$3f), ($02,$f5,$f7,$f7), ($4f,$83,$cc,$cc),
    ($5c,$68,$34,$34), ($f4,$51,$a5,$a5), ($34,$d1,$e5,$e5), ($08,$f9,$f1,$f1),
    ($93,$e2,$71,$71), ($73,$ab,$d8,$d8), ($53,$62,$31,$31), ($3f,$2a,$15,$15), 
    ($0c,$08,$04,$04), ($52,$95,$c7,$c7), ($65,$46,$23,$23), ($5e,$9d,$c3,$c3),
    ($28,$30,$18,$18), ($a1,$37,$96,$96), ($0f,$0a,$05,$05), ($b5,$2f,$9a,$9a),
    ($09,$0e,$07,$07), ($36,$24,$12,$12), ($9b,$1b,$80,$80), ($3d,$df,$e2,$e2), 
    ($26,$cd,$eb,$eb), ($69,$4e,$27,$27), ($cd,$7f,$b2,$b2), ($9f,$ea,$75,$75),
    ($1b,$12,$09,$09), ($9e,$1d,$83,$83), ($74,$58,$2c,$2c), ($2e,$34,$1a,$1a),
    ($2d,$36,$1b,$1b), ($b2,$dc,$6e,$6e), ($ee,$b4,$5a,$5a), ($fb,$5b,$a0,$a0),
    ($f6,$a4,$52,$52), ($4d,$76,$3b,$3b), ($61,$b7,$d6,$d6), ($ce,$7d,$b3,$b3),
    ($7b,$52,$29,$29), ($3e,$dd,$e3,$e3), ($71,$5e,$2f,$2f), ($97,$13,$84,$84), 
    ($f5,$a6,$53,$53), ($68,$b9,$d1,$d1), ($00,$00,$00,$00), ($2c,$c1,$ed,$ed), 
    ($60,$40,$20,$20), ($1f,$e3,$fc,$fc), ($c8,$79,$b1,$b1), ($ed,$b6,$5b,$5b),
    ($be,$d4,$6a,$6a), ($46,$8d,$cb,$cb), ($d9,$67,$be,$be), ($4b,$72,$39,$39), 
    ($de,$94,$4a,$4a), ($d4,$98,$4c,$4c), ($e8,$b0,$58,$58), ($4a,$85,$cf,$cf),
    ($6b,$bb,$d0,$d0), ($2a,$c5,$ef,$ef), ($e5,$4f,$aa,$aa), ($16,$ed,$fb,$fb),
    ($c5,$86,$43,$43), ($d7,$9a,$4d,$4d), ($55,$66,$33,$33), ($94,$11,$85,$85), 
    ($cf,$8a,$45,$45), ($10,$e9,$f9,$f9), ($06,$04,$02,$02), ($81,$fe,$7f,$7f), 
    ($f0,$a0,$50,$50), ($44,$78,$3c,$3c), ($ba,$25,$9f,$9f), ($e3,$4b,$a8,$a8),
    ($f3,$a2,$51,$51), ($fe,$5d,$a3,$a3), ($c0,$80,$40,$40), ($8a,$05,$8f,$8f),
    ($ad,$3f,$92,$92), ($bc,$21,$9d,$9d), ($48,$70,$38,$38), ($04,$f1,$f5,$f5),
    ($df,$63,$bc,$bc), ($c1,$77,$b6,$b6), ($75,$af,$da,$da), ($63,$42,$21,$21),
    ($30,$20,$10,$10), ($1a,$e5,$ff,$ff), ($0e,$fd,$f3,$f3), ($6d,$bf,$d2,$d2), 
    ($4c,$81,$cd,$cd), ($14,$18,$0c,$0c), ($35,$26,$13,$13), ($2f,$c3,$ec,$ec),
    ($e1,$be,$5f,$5f), ($a2,$35,$97,$97), ($cc,$88,$44,$44), ($39,$2e,$17,$17),
    ($57,$93,$c4,$c4), ($f2,$55,$a7,$a7), ($82,$fc,$7e,$7e), ($47,$7a,$3d,$3d), 
    ($ac,$c8,$64,$64), ($e7,$ba,$5d,$5d), ($2b,$32,$19,$19), ($95,$e6,$73,$73), 
    ($a0,$c0,$60,$60), ($98,$19,$81,$81), ($d1,$9e,$4f,$4f), ($7f,$a3,$dc,$dc),
    ($66,$44,$22,$22), ($7e,$54,$2a,$2a), ($ab,$3b,$90,$90), ($83,$0b,$88,$88), 
    ($ca,$8c,$46,$46), ($29,$c7,$ee,$ee), ($d3,$6b,$b8,$b8), ($3c,$28,$14,$14),
    ($79,$a7,$de,$de), ($e2,$bc,$5e,$5e), ($1d,$16,$0b,$0b), ($76,$ad,$db,$db),
    ($3b,$db,$e0,$e0), ($56,$64,$32,$32), ($4e,$74,$3a,$3a), ($1e,$14,$0a,$0a),
    ($db,$92,$49,$49), ($0a,$0c,$06,$06), ($6c,$48,$24,$24), ($e4,$b8,$5c,$5c),
    ($5d,$9f,$c2,$c2), ($6e,$bd,$d3,$d3), ($ef,$43,$ac,$ac), ($a6,$c4,$62,$62),
    ($a8,$39,$91,$91), ($a4,$31,$95,$95), ($37,$d3,$e4,$e4), ($8b,$f2,$79,$79),
    ($32,$d5,$e7,$e7), ($43,$8b,$c8,$c8), ($59,$6e,$37,$37), ($b7,$da,$6d,$6d), 
    ($8c,$01,$8d,$8d), ($64,$b1,$d5,$d5), ($d2,$9c,$4e,$4e), ($e0,$49,$a9,$a9),
    ($b4,$d8,$6c,$6c), ($fa,$ac,$56,$56), ($07,$f3,$f4,$f4), ($25,$cf,$ea,$ea),
    ($af,$ca,$65,$65), ($8e,$f4,$7a,$7a), ($e9,$47,$ae,$ae), ($18,$10,$08,$08),
    ($d5,$6f,$ba,$ba), ($88,$f0,$78,$78), ($6f,$4a,$25,$25), ($72,$5c,$2e,$2e),
    ($24,$38,$1c,$1c), ($f1,$57,$a6,$a6), ($c7,$73,$b4,$b4), ($51,$97,$c6,$c6),
    ($23,$cb,$e8,$e8), ($7c,$a1,$dd,$dd), ($9c,$e8,$74,$74), ($21,$3e,$1f,$1f),
    ($dd,$96,$4b,$4b), ($dc,$61,$bd,$bd), ($86,$0d,$8b,$8b), ($85,$0f,$8a,$8a),
    ($90,$e0,$70,$70), ($42,$7c,$3e,$3e), ($c4,$71,$b5,$b5), ($aa,$cc,$66,$66),
    ($d8,$90,$48,$48), ($05,$06,$03,$03), ($01,$f7,$f6,$f6), ($12,$1c,$0e,$0e),
    ($a3,$c2,$61,$61), ($5f,$6a,$35,$35), ($f9,$ae,$57,$57), ($d0,$69,$b9,$b9),
    ($91,$17,$86,$86), ($58,$99,$c1,$c1), ($27,$3a,$1d,$1d), ($b9,$27,$9e,$9e),
    ($38,$d9,$e1,$e1), ($13,$eb,$f8,$f8), ($b3,$2b,$98,$98), ($33,$22,$11,$11),
    ($bb,$d2,$69,$69), ($70,$a9,$d9,$d9), ($89,$07,$8e,$8e), ($a7,$33,$94,$94),
    ($b6,$2d,$9b,$9b), ($22,$3c,$1e,$1e), ($92,$15,$87,$87), ($20,$c9,$e9,$e9),
    ($49,$87,$ce,$ce), ($ff,$aa,$55,$55), ($78,$50,$28,$28), ($7a,$a5,$df,$df),
    ($8f,$03,$8c,$8c), ($f8,$59,$a1,$a1), ($80,$09,$89,$89), ($17,$1a,$0d,$0d),
    ($da,$65,$bf,$bf), ($31,$d7,$e6,$e6), ($c6,$84,$42,$42), ($b8,$d0,$68,$68),
    ($c3,$82,$41,$41), ($b0,$29,$99,$99), ($77,$5a,$2d,$2d), ($11,$1e,$0f,$0f),
    ($cb,$7b,$b0,$b0), ($fc,$a8,$54,$54), ($d6,$6d,$bb,$bb), ($3a,$2c,$16,$16));
  T3: array[0..255,0..3] of byte= (
    ($63,$a5,$c6,$63), ($7c,$84,$f8,$7c), ($77,$99,$ee,$77), ($7b,$8d,$f6,$7b),
    ($f2,$0d,$ff,$f2), ($6b,$bd,$d6,$6b), ($6f,$b1,$de,$6f), ($c5,$54,$91,$c5),
    ($30,$50,$60,$30), ($01,$03,$02,$01), ($67,$a9,$ce,$67), ($2b,$7d,$56,$2b),
    ($fe,$19,$e7,$fe), ($d7,$62,$b5,$d7), ($ab,$e6,$4d,$ab), ($76,$9a,$ec,$76),
    ($ca,$45,$8f,$ca), ($82,$9d,$1f,$82), ($c9,$40,$89,$c9), ($7d,$87,$fa,$7d),
    ($fa,$15,$ef,$fa), ($59,$eb,$b2,$59), ($47,$c9,$8e,$47), ($f0,$0b,$fb,$f0),
    ($ad,$ec,$41,$ad), ($d4,$67,$b3,$d4), ($a2,$fd,$5f,$a2), ($af,$ea,$45,$af),
    ($9c,$bf,$23,$9c), ($a4,$f7,$53,$a4), ($72,$96,$e4,$72), ($c0,$5b,$9b,$c0),
    ($b7,$c2,$75,$b7), ($fd,$1c,$e1,$fd), ($93,$ae,$3d,$93), ($26,$6a,$4c,$26), 
    ($36,$5a,$6c,$36), ($3f,$41,$7e,$3f), ($f7,$02,$f5,$f7), ($cc,$4f,$83,$cc),
    ($34,$5c,$68,$34), ($a5,$f4,$51,$a5), ($e5,$34,$d1,$e5), ($f1,$08,$f9,$f1), 
    ($71,$93,$e2,$71), ($d8,$73,$ab,$d8), ($31,$53,$62,$31), ($15,$3f,$2a,$15),
    ($04,$0c,$08,$04), ($c7,$52,$95,$c7), ($23,$65,$46,$23), ($c3,$5e,$9d,$c3), 
    ($18,$28,$30,$18), ($96,$a1,$37,$96), ($05,$0f,$0a,$05), ($9a,$b5,$2f,$9a), 
    ($07,$09,$0e,$07), ($12,$36,$24,$12), ($80,$9b,$1b,$80), ($e2,$3d,$df,$e2), 
    ($eb,$26,$cd,$eb), ($27,$69,$4e,$27), ($b2,$cd,$7f,$b2), ($75,$9f,$ea,$75),
    ($09,$1b,$12,$09), ($83,$9e,$1d,$83), ($2c,$74,$58,$2c), ($1a,$2e,$34,$1a), 
    ($1b,$2d,$36,$1b), ($6e,$b2,$dc,$6e), ($5a,$ee,$b4,$5a), ($a0,$fb,$5b,$a0), 
    ($52,$f6,$a4,$52), ($3b,$4d,$76,$3b), ($d6,$61,$b7,$d6), ($b3,$ce,$7d,$b3),
    ($29,$7b,$52,$29), ($e3,$3e,$dd,$e3), ($2f,$71,$5e,$2f), ($84,$97,$13,$84), 
    ($53,$f5,$a6,$53), ($d1,$68,$b9,$d1), ($00,$00,$00,$00), ($ed,$2c,$c1,$ed),
    ($20,$60,$40,$20), ($fc,$1f,$e3,$fc), ($b1,$c8,$79,$b1), ($5b,$ed,$b6,$5b), 
    ($6a,$be,$d4,$6a), ($cb,$46,$8d,$cb), ($be,$d9,$67,$be), ($39,$4b,$72,$39), 
    ($4a,$de,$94,$4a), ($4c,$d4,$98,$4c), ($58,$e8,$b0,$58), ($cf,$4a,$85,$cf),
    ($d0,$6b,$bb,$d0), ($ef,$2a,$c5,$ef), ($aa,$e5,$4f,$aa), ($fb,$16,$ed,$fb), 
    ($43,$c5,$86,$43), ($4d,$d7,$9a,$4d), ($33,$55,$66,$33), ($85,$94,$11,$85), 
    ($45,$cf,$8a,$45), ($f9,$10,$e9,$f9), ($02,$06,$04,$02), ($7f,$81,$fe,$7f), 
    ($50,$f0,$a0,$50), ($3c,$44,$78,$3c), ($9f,$ba,$25,$9f), ($a8,$e3,$4b,$a8),
    ($51,$f3,$a2,$51), ($a3,$fe,$5d,$a3), ($40,$c0,$80,$40), ($8f,$8a,$05,$8f), 
    ($92,$ad,$3f,$92), ($9d,$bc,$21,$9d), ($38,$48,$70,$38), ($f5,$04,$f1,$f5), 
    ($bc,$df,$63,$bc), ($b6,$c1,$77,$b6), ($da,$75,$af,$da), ($21,$63,$42,$21),
    ($10,$30,$20,$10), ($ff,$1a,$e5,$ff), ($f3,$0e,$fd,$f3), ($d2,$6d,$bf,$d2), 
    ($cd,$4c,$81,$cd), ($0c,$14,$18,$0c), ($13,$35,$26,$13), ($ec,$2f,$c3,$ec), 
    ($5f,$e1,$be,$5f), ($97,$a2,$35,$97), ($44,$cc,$88,$44), ($17,$39,$2e,$17), 
    ($c4,$57,$93,$c4), ($a7,$f2,$55,$a7), ($7e,$82,$fc,$7e), ($3d,$47,$7a,$3d),
    ($64,$ac,$c8,$64), ($5d,$e7,$ba,$5d), ($19,$2b,$32,$19), ($73,$95,$e6,$73), 
    ($60,$a0,$c0,$60), ($81,$98,$19,$81), ($4f,$d1,$9e,$4f), ($dc,$7f,$a3,$dc), 
    ($22,$66,$44,$22), ($2a,$7e,$54,$2a), ($90,$ab,$3b,$90), ($88,$83,$0b,$88),
    ($46,$ca,$8c,$46), ($ee,$29,$c7,$ee), ($b8,$d3,$6b,$b8), ($14,$3c,$28,$14), 
    ($de,$79,$a7,$de), ($5e,$e2,$bc,$5e), ($0b,$1d,$16,$0b), ($db,$76,$ad,$db), 
    ($e0,$3b,$db,$e0), ($32,$56,$64,$32), ($3a,$4e,$74,$3a), ($0a,$1e,$14,$0a), 
    ($49,$db,$92,$49), ($06,$0a,$0c,$06), ($24,$6c,$48,$24), ($5c,$e4,$b8,$5c),
    ($c2,$5d,$9f,$c2), ($d3,$6e,$bd,$d3), ($ac,$ef,$43,$ac), ($62,$a6,$c4,$62), 
    ($91,$a8,$39,$91), ($95,$a4,$31,$95), ($e4,$37,$d3,$e4), ($79,$8b,$f2,$79),
    ($e7,$32,$d5,$e7), ($c8,$43,$8b,$c8), ($37,$59,$6e,$37), ($6d,$b7,$da,$6d), 
    ($8d,$8c,$01,$8d), ($d5,$64,$b1,$d5), ($4e,$d2,$9c,$4e), ($a9,$e0,$49,$a9), 
    ($6c,$b4,$d8,$6c), ($56,$fa,$ac,$56), ($f4,$07,$f3,$f4), ($ea,$25,$cf,$ea),
    ($65,$af,$ca,$65), ($7a,$8e,$f4,$7a), ($ae,$e9,$47,$ae), ($08,$18,$10,$08), 
    ($ba,$d5,$6f,$ba), ($78,$88,$f0,$78), ($25,$6f,$4a,$25), ($2e,$72,$5c,$2e), 
    ($1c,$24,$38,$1c), ($a6,$f1,$57,$a6), ($b4,$c7,$73,$b4), ($c6,$51,$97,$c6), 
    ($e8,$23,$cb,$e8), ($dd,$7c,$a1,$dd), ($74,$9c,$e8,$74), ($1f,$21,$3e,$1f),
    ($4b,$dd,$96,$4b), ($bd,$dc,$61,$bd), ($8b,$86,$0d,$8b), ($8a,$85,$0f,$8a), 
    ($70,$90,$e0,$70), ($3e,$42,$7c,$3e), ($b5,$c4,$71,$b5), ($66,$aa,$cc,$66), 
    ($48,$d8,$90,$48), ($03,$05,$06,$03), ($f6,$01,$f7,$f6), ($0e,$12,$1c,$0e), 
    ($61,$a3,$c2,$61), ($35,$5f,$6a,$35), ($57,$f9,$ae,$57), ($b9,$d0,$69,$b9), 
    ($86,$91,$17,$86), ($c1,$58,$99,$c1), ($1d,$27,$3a,$1d), ($9e,$b9,$27,$9e), 
    ($e1,$38,$d9,$e1), ($f8,$13,$eb,$f8), ($98,$b3,$2b,$98), ($11,$33,$22,$11), 
    ($69,$bb,$d2,$69), ($d9,$70,$a9,$d9), ($8e,$89,$07,$8e), ($94,$a7,$33,$94),
    ($9b,$b6,$2d,$9b), ($1e,$22,$3c,$1e), ($87,$92,$15,$87), ($e9,$20,$c9,$e9), 
    ($ce,$49,$87,$ce), ($55,$ff,$aa,$55), ($28,$78,$50,$28), ($df,$7a,$a5,$df), 
    ($8c,$8f,$03,$8c), ($a1,$f8,$59,$a1), ($89,$80,$09,$89), ($0d,$17,$1a,$0d), 
    ($bf,$da,$65,$bf), ($e6,$31,$d7,$e6), ($42,$c6,$84,$42), ($68,$b8,$d0,$68), 
    ($41,$c3,$82,$41), ($99,$b0,$29,$99), ($2d,$77,$5a,$2d), ($0f,$11,$1e,$0f), 
    ($b0,$cb,$7b,$b0), ($54,$fc,$a8,$54), ($bb,$d6,$6d,$bb), ($16,$3a,$2c,$16));
  T4: array[0..255,0..3] of byte= (
    ($63,$63,$a5,$c6), ($7c,$7c,$84,$f8), ($77,$77,$99,$ee), ($7b,$7b,$8d,$f6),
    ($f2,$f2,$0d,$ff), ($6b,$6b,$bd,$d6), ($6f,$6f,$b1,$de), ($c5,$c5,$54,$91),
    ($30,$30,$50,$60), ($01,$01,$03,$02), ($67,$67,$a9,$ce), ($2b,$2b,$7d,$56),
    ($fe,$fe,$19,$e7), ($d7,$d7,$62,$b5), ($ab,$ab,$e6,$4d), ($76,$76,$9a,$ec), 
    ($ca,$ca,$45,$8f), ($82,$82,$9d,$1f), ($c9,$c9,$40,$89), ($7d,$7d,$87,$fa), 
    ($fa,$fa,$15,$ef), ($59,$59,$eb,$b2), ($47,$47,$c9,$8e), ($f0,$f0,$0b,$fb), 
    ($ad,$ad,$ec,$41), ($d4,$d4,$67,$b3), ($a2,$a2,$fd,$5f), ($af,$af,$ea,$45), 
    ($9c,$9c,$bf,$23), ($a4,$a4,$f7,$53), ($72,$72,$96,$e4), ($c0,$c0,$5b,$9b), 
    ($b7,$b7,$c2,$75), ($fd,$fd,$1c,$e1), ($93,$93,$ae,$3d), ($26,$26,$6a,$4c),
    ($36,$36,$5a,$6c), ($3f,$3f,$41,$7e), ($f7,$f7,$02,$f5), ($cc,$cc,$4f,$83), 
    ($34,$34,$5c,$68), ($a5,$a5,$f4,$51), ($e5,$e5,$34,$d1), ($f1,$f1,$08,$f9), 
    ($71,$71,$93,$e2), ($d8,$d8,$73,$ab), ($31,$31,$53,$62), ($15,$15,$3f,$2a), 
    ($04,$04,$0c,$08), ($c7,$c7,$52,$95), ($23,$23,$65,$46), ($c3,$c3,$5e,$9d), 
    ($18,$18,$28,$30), ($96,$96,$a1,$37), ($05,$05,$0f,$0a), ($9a,$9a,$b5,$2f),
    ($07,$07,$09,$0e), ($12,$12,$36,$24), ($80,$80,$9b,$1b), ($e2,$e2,$3d,$df), 
    ($eb,$eb,$26,$cd), ($27,$27,$69,$4e), ($b2,$b2,$cd,$7f), ($75,$75,$9f,$ea), 
    ($09,$09,$1b,$12), ($83,$83,$9e,$1d), ($2c,$2c,$74,$58), ($1a,$1a,$2e,$34), 
    ($1b,$1b,$2d,$36), ($6e,$6e,$b2,$dc), ($5a,$5a,$ee,$b4), ($a0,$a0,$fb,$5b), 
    ($52,$52,$f6,$a4), ($3b,$3b,$4d,$76), ($d6,$d6,$61,$b7), ($b3,$b3,$ce,$7d),
    ($29,$29,$7b,$52), ($e3,$e3,$3e,$dd), ($2f,$2f,$71,$5e), ($84,$84,$97,$13), 
    ($53,$53,$f5,$a6), ($d1,$d1,$68,$b9), ($00,$00,$00,$00), ($ed,$ed,$2c,$c1), 
    ($20,$20,$60,$40), ($fc,$fc,$1f,$e3), ($b1,$b1,$c8,$79), ($5b,$5b,$ed,$b6), 
    ($6a,$6a,$be,$d4), ($cb,$cb,$46,$8d), ($be,$be,$d9,$67), ($39,$39,$4b,$72), 
    ($4a,$4a,$de,$94), ($4c,$4c,$d4,$98), ($58,$58,$e8,$b0), ($cf,$cf,$4a,$85),
    ($d0,$d0,$6b,$bb), ($ef,$ef,$2a,$c5), ($aa,$aa,$e5,$4f), ($fb,$fb,$16,$ed),
    ($43,$43,$c5,$86), ($4d,$4d,$d7,$9a), ($33,$33,$55,$66), ($85,$85,$94,$11), 
    ($45,$45,$cf,$8a), ($f9,$f9,$10,$e9), ($02,$02,$06,$04), ($7f,$7f,$81,$fe), 
    ($50,$50,$f0,$a0), ($3c,$3c,$44,$78), ($9f,$9f,$ba,$25), ($a8,$a8,$e3,$4b), 
    ($51,$51,$f3,$a2), ($a3,$a3,$fe,$5d), ($40,$40,$c0,$80), ($8f,$8f,$8a,$05),
    ($92,$92,$ad,$3f), ($9d,$9d,$bc,$21), ($38,$38,$48,$70), ($f5,$f5,$04,$f1), 
    ($bc,$bc,$df,$63), ($b6,$b6,$c1,$77), ($da,$da,$75,$af), ($21,$21,$63,$42), 
    ($10,$10,$30,$20), ($ff,$ff,$1a,$e5), ($f3,$f3,$0e,$fd), ($d2,$d2,$6d,$bf),
    ($cd,$cd,$4c,$81), ($0c,$0c,$14,$18), ($13,$13,$35,$26), ($ec,$ec,$2f,$c3), 
    ($5f,$5f,$e1,$be), ($97,$97,$a2,$35), ($44,$44,$cc,$88), ($17,$17,$39,$2e), 
    ($c4,$c4,$57,$93), ($a7,$a7,$f2,$55), ($7e,$7e,$82,$fc), ($3d,$3d,$47,$7a), 
    ($64,$64,$ac,$c8), ($5d,$5d,$e7,$ba), ($19,$19,$2b,$32), ($73,$73,$95,$e6), 
    ($60,$60,$a0,$c0), ($81,$81,$98,$19), ($4f,$4f,$d1,$9e), ($dc,$dc,$7f,$a3),
    ($22,$22,$66,$44), ($2a,$2a,$7e,$54), ($90,$90,$ab,$3b), ($88,$88,$83,$0b), 
    ($46,$46,$ca,$8c), ($ee,$ee,$29,$c7), ($b8,$b8,$d3,$6b), ($14,$14,$3c,$28), 
    ($de,$de,$79,$a7), ($5e,$5e,$e2,$bc), ($0b,$0b,$1d,$16), ($db,$db,$76,$ad), 
    ($e0,$e0,$3b,$db), ($32,$32,$56,$64), ($3a,$3a,$4e,$74), ($0a,$0a,$1e,$14), 
    ($49,$49,$db,$92), ($06,$06,$0a,$0c), ($24,$24,$6c,$48), ($5c,$5c,$e4,$b8),
    ($c2,$c2,$5d,$9f), ($d3,$d3,$6e,$bd), ($ac,$ac,$ef,$43), ($62,$62,$a6,$c4), 
    ($91,$91,$a8,$39), ($95,$95,$a4,$31), ($e4,$e4,$37,$d3), ($79,$79,$8b,$f2), 
    ($e7,$e7,$32,$d5), ($c8,$c8,$43,$8b), ($37,$37,$59,$6e), ($6d,$6d,$b7,$da), 
    ($8d,$8d,$8c,$01), ($d5,$d5,$64,$b1), ($4e,$4e,$d2,$9c), ($a9,$a9,$e0,$49), 
    ($6c,$6c,$b4,$d8), ($56,$56,$fa,$ac), ($f4,$f4,$07,$f3), ($ea,$ea,$25,$cf),
    ($65,$65,$af,$ca), ($7a,$7a,$8e,$f4), ($ae,$ae,$e9,$47), ($08,$08,$18,$10),
    ($ba,$ba,$d5,$6f), ($78,$78,$88,$f0), ($25,$25,$6f,$4a), ($2e,$2e,$72,$5c), 
    ($1c,$1c,$24,$38), ($a6,$a6,$f1,$57), ($b4,$b4,$c7,$73), ($c6,$c6,$51,$97), 
    ($e8,$e8,$23,$cb), ($dd,$dd,$7c,$a1), ($74,$74,$9c,$e8), ($1f,$1f,$21,$3e),
    ($4b,$4b,$dd,$96), ($bd,$bd,$dc,$61), ($8b,$8b,$86,$0d), ($8a,$8a,$85,$0f), 
    ($70,$70,$90,$e0), ($3e,$3e,$42,$7c), ($b5,$b5,$c4,$71), ($66,$66,$aa,$cc), 
    ($48,$48,$d8,$90), ($03,$03,$05,$06), ($f6,$f6,$01,$f7), ($0e,$0e,$12,$1c), 
    ($61,$61,$a3,$c2), ($35,$35,$5f,$6a), ($57,$57,$f9,$ae), ($b9,$b9,$d0,$69),
    ($86,$86,$91,$17), ($c1,$c1,$58,$99), ($1d,$1d,$27,$3a), ($9e,$9e,$b9,$27), 
    ($e1,$e1,$38,$d9), ($f8,$f8,$13,$eb), ($98,$98,$b3,$2b), ($11,$11,$33,$22), 
    ($69,$69,$bb,$d2), ($d9,$d9,$70,$a9), ($8e,$8e,$89,$07), ($94,$94,$a7,$33), 
    ($9b,$9b,$b6,$2d), ($1e,$1e,$22,$3c), ($87,$87,$92,$15), ($e9,$e9,$20,$c9), 
    ($ce,$ce,$49,$87), ($55,$55,$ff,$aa), ($28,$28,$78,$50), ($df,$df,$7a,$a5),
    ($8c,$8c,$8f,$03), ($a1,$a1,$f8,$59), ($89,$89,$80,$09), ($0d,$0d,$17,$1a),
    ($bf,$bf,$da,$65), ($e6,$e6,$31,$d7), ($42,$42,$c6,$84), ($68,$68,$b8,$d0), 
    ($41,$41,$c3,$82), ($99,$99,$b0,$29), ($2d,$2d,$77,$5a), ($0f,$0f,$11,$1e), 
    ($b0,$b0,$cb,$7b), ($54,$54,$fc,$a8), ($bb,$bb,$d6,$6d), ($16,$16,$3a,$2c));
  T5: array[0..255,0..3] of byte= (
    ($51,$f4,$a7,$50), ($7e,$41,$65,$53), ($1a,$17,$a4,$c3), ($3a,$27,$5e,$96),
    ($3b,$ab,$6b,$cb), ($1f,$9d,$45,$f1), ($ac,$fa,$58,$ab), ($4b,$e3,$03,$93),
    ($20,$30,$fa,$55), ($ad,$76,$6d,$f6), ($88,$cc,$76,$91), ($f5,$02,$4c,$25),
    ($4f,$e5,$d7,$fc), ($c5,$2a,$cb,$d7), ($26,$35,$44,$80), ($b5,$62,$a3,$8f), 
    ($de,$b1,$5a,$49), ($25,$ba,$1b,$67), ($45,$ea,$0e,$98), ($5d,$fe,$c0,$e1), 
    ($c3,$2f,$75,$02), ($81,$4c,$f0,$12), ($8d,$46,$97,$a3), ($6b,$d3,$f9,$c6),
    ($03,$8f,$5f,$e7), ($15,$92,$9c,$95), ($bf,$6d,$7a,$eb), ($95,$52,$59,$da), 
    ($d4,$be,$83,$2d), ($58,$74,$21,$d3), ($49,$e0,$69,$29), ($8e,$c9,$c8,$44), 
    ($75,$c2,$89,$6a), ($f4,$8e,$79,$78), ($99,$58,$3e,$6b), ($27,$b9,$71,$dd), 
    ($be,$e1,$4f,$b6), ($f0,$88,$ad,$17), ($c9,$20,$ac,$66), ($7d,$ce,$3a,$b4), 
    ($63,$df,$4a,$18), ($e5,$1a,$31,$82), ($97,$51,$33,$60), ($62,$53,$7f,$45), 
    ($b1,$64,$77,$e0), ($bb,$6b,$ae,$84), ($fe,$81,$a0,$1c), ($f9,$08,$2b,$94),
    ($70,$48,$68,$58), ($8f,$45,$fd,$19), ($94,$de,$6c,$87), ($52,$7b,$f8,$b7),
    ($ab,$73,$d3,$23), ($72,$4b,$02,$e2), ($e3,$1f,$8f,$57), ($66,$55,$ab,$2a), 
    ($b2,$eb,$28,$07), ($2f,$b5,$c2,$03), ($86,$c5,$7b,$9a), ($d3,$37,$08,$a5), 
    ($30,$28,$87,$f2), ($23,$bf,$a5,$b2), ($02,$03,$6a,$ba), ($ed,$16,$82,$5c),
    ($8a,$cf,$1c,$2b), ($a7,$79,$b4,$92), ($f3,$07,$f2,$f0), ($4e,$69,$e2,$a1), 
    ($65,$da,$f4,$cd), ($06,$05,$be,$d5), ($d1,$34,$62,$1f), ($c4,$a6,$fe,$8a),
    ($34,$2e,$53,$9d), ($a2,$f3,$55,$a0), ($05,$8a,$e1,$32), ($a4,$f6,$eb,$75),
    ($0b,$83,$ec,$39), ($40,$60,$ef,$aa), ($5e,$71,$9f,$06), ($bd,$6e,$10,$51), 
    ($3e,$21,$8a,$f9), ($96,$dd,$06,$3d), ($dd,$3e,$05,$ae), ($4d,$e6,$bd,$46), 
    ($91,$54,$8d,$b5), ($71,$c4,$5d,$05), ($04,$06,$d4,$6f), ($60,$50,$15,$ff), 
    ($19,$98,$fb,$24), ($d6,$bd,$e9,$97), ($89,$40,$43,$cc), ($67,$d9,$9e,$77),
    ($b0,$e8,$42,$bd), ($07,$89,$8b,$88), ($e7,$19,$5b,$38), ($79,$c8,$ee,$db),
    ($a1,$7c,$0a,$47), ($7c,$42,$0f,$e9), ($f8,$84,$1e,$c9), ($00,$00,$00,$00), 
    ($09,$80,$86,$83), ($32,$2b,$ed,$48), ($1e,$11,$70,$ac), ($6c,$5a,$72,$4e), 
    ($fd,$0e,$ff,$fb), ($0f,$85,$38,$56), ($3d,$ae,$d5,$1e), ($36,$2d,$39,$27), 
    ($0a,$0f,$d9,$64), ($68,$5c,$a6,$21), ($9b,$5b,$54,$d1), ($24,$36,$2e,$3a), 
    ($0c,$0a,$67,$b1), ($93,$57,$e7,$0f), ($b4,$ee,$96,$d2), ($1b,$9b,$91,$9e),
    ($80,$c0,$c5,$4f), ($61,$dc,$20,$a2), ($5a,$77,$4b,$69), ($1c,$12,$1a,$16), 
    ($e2,$93,$ba,$0a), ($c0,$a0,$2a,$e5), ($3c,$22,$e0,$43), ($12,$1b,$17,$1d), 
    ($0e,$09,$0d,$0b), ($f2,$8b,$c7,$ad), ($2d,$b6,$a8,$b9), ($14,$1e,$a9,$c8), 
    ($57,$f1,$19,$85), ($af,$75,$07,$4c), ($ee,$99,$dd,$bb), ($a3,$7f,$60,$fd),
    ($f7,$01,$26,$9f), ($5c,$72,$f5,$bc), ($44,$66,$3b,$c5), ($5b,$fb,$7e,$34), 
    ($8b,$43,$29,$76), ($cb,$23,$c6,$dc), ($b6,$ed,$fc,$68), ($b8,$e4,$f1,$63), 
    ($d7,$31,$dc,$ca), ($42,$63,$85,$10), ($13,$97,$22,$40), ($84,$c6,$11,$20),
    ($85,$4a,$24,$7d), ($d2,$bb,$3d,$f8), ($ae,$f9,$32,$11), ($c7,$29,$a1,$6d), 
    ($1d,$9e,$2f,$4b), ($dc,$b2,$30,$f3), ($0d,$86,$52,$ec), ($77,$c1,$e3,$d0),
    ($2b,$b3,$16,$6c), ($a9,$70,$b9,$99), ($11,$94,$48,$fa), ($47,$e9,$64,$22),
    ($a8,$fc,$8c,$c4), ($a0,$f0,$3f,$1a), ($56,$7d,$2c,$d8), ($22,$33,$90,$ef), 
    ($87,$49,$4e,$c7), ($d9,$38,$d1,$c1), ($8c,$ca,$a2,$fe), ($98,$d4,$0b,$36),
    ($a6,$f5,$81,$cf), ($a5,$7a,$de,$28), ($da,$b7,$8e,$26), ($3f,$ad,$bf,$a4),
    ($2c,$3a,$9d,$e4), ($50,$78,$92,$0d), ($6a,$5f,$cc,$9b), ($54,$7e,$46,$62), 
    ($f6,$8d,$13,$c2), ($90,$d8,$b8,$e8), ($2e,$39,$f7,$5e), ($82,$c3,$af,$f5), 
    ($9f,$5d,$80,$be), ($69,$d0,$93,$7c), ($6f,$d5,$2d,$a9), ($cf,$25,$12,$b3), 
    ($c8,$ac,$99,$3b), ($10,$18,$7d,$a7), ($e8,$9c,$63,$6e), ($db,$3b,$bb,$7b),
    ($cd,$26,$78,$09), ($6e,$59,$18,$f4), ($ec,$9a,$b7,$01), ($83,$4f,$9a,$a8),
    ($e6,$95,$6e,$65), ($aa,$ff,$e6,$7e), ($21,$bc,$cf,$08), ($ef,$15,$e8,$e6), 
    ($ba,$e7,$9b,$d9), ($4a,$6f,$36,$ce), ($ea,$9f,$09,$d4), ($29,$b0,$7c,$d6), 
    ($31,$a4,$b2,$af), ($2a,$3f,$23,$31), ($c6,$a5,$94,$30), ($35,$a2,$66,$c0), 
    ($74,$4e,$bc,$37), ($fc,$82,$ca,$a6), ($e0,$90,$d0,$b0), ($33,$a7,$d8,$15), 
    ($f1,$04,$98,$4a), ($41,$ec,$da,$f7), ($7f,$cd,$50,$0e), ($17,$91,$f6,$2f),
    ($76,$4d,$d6,$8d), ($43,$ef,$b0,$4d), ($cc,$aa,$4d,$54), ($e4,$96,$04,$df), 
    ($9e,$d1,$b5,$e3), ($4c,$6a,$88,$1b), ($c1,$2c,$1f,$b8), ($46,$65,$51,$7f), 
    ($9d,$5e,$ea,$04), ($01,$8c,$35,$5d), ($fa,$87,$74,$73), ($fb,$0b,$41,$2e), 
    ($b3,$67,$1d,$5a), ($92,$db,$d2,$52), ($e9,$10,$56,$33), ($6d,$d6,$47,$13), 
    ($9a,$d7,$61,$8c), ($37,$a1,$0c,$7a), ($59,$f8,$14,$8e), ($eb,$13,$3c,$89), 
    ($ce,$a9,$27,$ee), ($b7,$61,$c9,$35), ($e1,$1c,$e5,$ed), ($7a,$47,$b1,$3c), 
    ($9c,$d2,$df,$59), ($55,$f2,$73,$3f), ($18,$14,$ce,$79), ($73,$c7,$37,$bf),
    ($53,$f7,$cd,$ea), ($5f,$fd,$aa,$5b), ($df,$3d,$6f,$14), ($78,$44,$db,$86), 
    ($ca,$af,$f3,$81), ($b9,$68,$c4,$3e), ($38,$24,$34,$2c), ($c2,$a3,$40,$5f),
    ($16,$1d,$c3,$72), ($bc,$e2,$25,$0c), ($28,$3c,$49,$8b), ($ff,$0d,$95,$41),
    ($39,$a8,$01,$71), ($08,$0c,$b3,$de), ($d8,$b4,$e4,$9c), ($64,$56,$c1,$90),
    ($7b,$cb,$84,$61), ($d5,$32,$b6,$70), ($48,$6c,$5c,$74), ($d0,$b8,$57,$42));
  T6: array[0..255,0..3] of byte= (
    ($50,$51,$f4,$a7), ($53,$7e,$41,$65), ($c3,$1a,$17,$a4), ($96,$3a,$27,$5e),
    ($cb,$3b,$ab,$6b), ($f1,$1f,$9d,$45), ($ab,$ac,$fa,$58), ($93,$4b,$e3,$03),
    ($55,$20,$30,$fa), ($f6,$ad,$76,$6d), ($91,$88,$cc,$76), ($25,$f5,$02,$4c),
    ($fc,$4f,$e5,$d7), ($d7,$c5,$2a,$cb), ($80,$26,$35,$44), ($8f,$b5,$62,$a3), 
    ($49,$de,$b1,$5a), ($67,$25,$ba,$1b), ($98,$45,$ea,$0e), ($e1,$5d,$fe,$c0), 
    ($02,$c3,$2f,$75), ($12,$81,$4c,$f0), ($a3,$8d,$46,$97), ($c6,$6b,$d3,$f9), 
    ($e7,$03,$8f,$5f), ($95,$15,$92,$9c), ($eb,$bf,$6d,$7a), ($da,$95,$52,$59), 
    ($2d,$d4,$be,$83), ($d3,$58,$74,$21), ($29,$49,$e0,$69), ($44,$8e,$c9,$c8), 
    ($6a,$75,$c2,$89), ($78,$f4,$8e,$79), ($6b,$99,$58,$3e), ($dd,$27,$b9,$71), 
    ($b6,$be,$e1,$4f), ($17,$f0,$88,$ad), ($66,$c9,$20,$ac), ($b4,$7d,$ce,$3a),
    ($18,$63,$df,$4a), ($82,$e5,$1a,$31), ($60,$97,$51,$33), ($45,$62,$53,$7f), 
    ($e0,$b1,$64,$77), ($84,$bb,$6b,$ae), ($1c,$fe,$81,$a0), ($94,$f9,$08,$2b), 
    ($58,$70,$48,$68), ($19,$8f,$45,$fd), ($87,$94,$de,$6c), ($b7,$52,$7b,$f8), 
    ($23,$ab,$73,$d3), ($e2,$72,$4b,$02), ($57,$e3,$1f,$8f), ($2a,$66,$55,$ab), 
    ($07,$b2,$eb,$28), ($03,$2f,$b5,$c2), ($9a,$86,$c5,$7b), ($a5,$d3,$37,$08), 
    ($f2,$30,$28,$87), ($b2,$23,$bf,$a5), ($ba,$02,$03,$6a), ($5c,$ed,$16,$82), 
    ($2b,$8a,$cf,$1c), ($92,$a7,$79,$b4), ($f0,$f3,$07,$f2), ($a1,$4e,$69,$e2),
    ($cd,$65,$da,$f4), ($d5,$06,$05,$be), ($1f,$d1,$34,$62), ($8a,$c4,$a6,$fe),
    ($9d,$34,$2e,$53), ($a0,$a2,$f3,$55), ($32,$05,$8a,$e1), ($75,$a4,$f6,$eb),
    ($39,$0b,$83,$ec), ($aa,$40,$60,$ef), ($06,$5e,$71,$9f), ($51,$bd,$6e,$10), 
    ($f9,$3e,$21,$8a), ($3d,$96,$dd,$06), ($ae,$dd,$3e,$05), ($46,$4d,$e6,$bd), 
    ($b5,$91,$54,$8d), ($05,$71,$c4,$5d), ($6f,$04,$06,$d4), ($ff,$60,$50,$15), 
    ($24,$19,$98,$fb), ($97,$d6,$bd,$e9), ($cc,$89,$40,$43), ($77,$67,$d9,$9e),
    ($bd,$b0,$e8,$42), ($88,$07,$89,$8b), ($38,$e7,$19,$5b), ($db,$79,$c8,$ee),
    ($47,$a1,$7c,$0a), ($e9,$7c,$42,$0f), ($c9,$f8,$84,$1e), ($00,$00,$00,$00),
    ($83,$09,$80,$86), ($48,$32,$2b,$ed), ($ac,$1e,$11,$70), ($4e,$6c,$5a,$72), 
    ($fb,$fd,$0e,$ff), ($56,$0f,$85,$38), ($1e,$3d,$ae,$d5), ($27,$36,$2d,$39), 
    ($64,$0a,$0f,$d9), ($21,$68,$5c,$a6), ($d1,$9b,$5b,$54), ($3a,$24,$36,$2e), 
    ($b1,$0c,$0a,$67), ($0f,$93,$57,$e7), ($d2,$b4,$ee,$96), ($9e,$1b,$9b,$91),
    ($4f,$80,$c0,$c5), ($a2,$61,$dc,$20), ($69,$5a,$77,$4b), ($16,$1c,$12,$1a), 
    ($0a,$e2,$93,$ba), ($e5,$c0,$a0,$2a), ($43,$3c,$22,$e0), ($1d,$12,$1b,$17), 
    ($0b,$0e,$09,$0d), ($ad,$f2,$8b,$c7), ($b9,$2d,$b6,$a8), ($c8,$14,$1e,$a9), 
    ($85,$57,$f1,$19), ($4c,$af,$75,$07), ($bb,$ee,$99,$dd), ($fd,$a3,$7f,$60),
    ($9f,$f7,$01,$26), ($bc,$5c,$72,$f5), ($c5,$44,$66,$3b), ($34,$5b,$fb,$7e),
    ($76,$8b,$43,$29), ($dc,$cb,$23,$c6), ($68,$b6,$ed,$fc), ($63,$b8,$e4,$f1), 
    ($ca,$d7,$31,$dc), ($10,$42,$63,$85), ($40,$13,$97,$22), ($20,$84,$c6,$11), 
    ($7d,$85,$4a,$24), ($f8,$d2,$bb,$3d), ($11,$ae,$f9,$32), ($6d,$c7,$29,$a1), 
    ($4b,$1d,$9e,$2f), ($f3,$dc,$b2,$30), ($ec,$0d,$86,$52), ($d0,$77,$c1,$e3), 
    ($6c,$2b,$b3,$16), ($99,$a9,$70,$b9), ($fa,$11,$94,$48), ($22,$47,$e9,$64), 
    ($c4,$a8,$fc,$8c), ($1a,$a0,$f0,$3f), ($d8,$56,$7d,$2c), ($ef,$22,$33,$90),
    ($c7,$87,$49,$4e), ($c1,$d9,$38,$d1), ($fe,$8c,$ca,$a2), ($36,$98,$d4,$0b),
    ($cf,$a6,$f5,$81), ($28,$a5,$7a,$de), ($26,$da,$b7,$8e), ($a4,$3f,$ad,$bf),
    ($e4,$2c,$3a,$9d), ($0d,$50,$78,$92), ($9b,$6a,$5f,$cc), ($62,$54,$7e,$46), 
    ($c2,$f6,$8d,$13), ($e8,$90,$d8,$b8), ($5e,$2e,$39,$f7), ($f5,$82,$c3,$af), 
    ($be,$9f,$5d,$80), ($7c,$69,$d0,$93), ($a9,$6f,$d5,$2d), ($b3,$cf,$25,$12), 
    ($3b,$c8,$ac,$99), ($a7,$10,$18,$7d), ($6e,$e8,$9c,$63), ($7b,$db,$3b,$bb),
    ($09,$cd,$26,$78), ($f4,$6e,$59,$18), ($01,$ec,$9a,$b7), ($a8,$83,$4f,$9a),
    ($65,$e6,$95,$6e), ($7e,$aa,$ff,$e6), ($08,$21,$bc,$cf), ($e6,$ef,$15,$e8),
    ($d9,$ba,$e7,$9b), ($ce,$4a,$6f,$36), ($d4,$ea,$9f,$09), ($d6,$29,$b0,$7c),
    ($af,$31,$a4,$b2), ($31,$2a,$3f,$23), ($30,$c6,$a5,$94), ($c0,$35,$a2,$66),
    ($37,$74,$4e,$bc), ($a6,$fc,$82,$ca), ($b0,$e0,$90,$d0), ($15,$33,$a7,$d8),
    ($4a,$f1,$04,$98), ($f7,$41,$ec,$da), ($0e,$7f,$cd,$50), ($2f,$17,$91,$f6),
    ($8d,$76,$4d,$d6), ($4d,$43,$ef,$b0), ($54,$cc,$aa,$4d), ($df,$e4,$96,$04),
    ($e3,$9e,$d1,$b5), ($1b,$4c,$6a,$88), ($b8,$c1,$2c,$1f), ($7f,$46,$65,$51),
    ($04,$9d,$5e,$ea), ($5d,$01,$8c,$35), ($73,$fa,$87,$74), ($2e,$fb,$0b,$41),
    ($5a,$b3,$67,$1d), ($52,$92,$db,$d2), ($33,$e9,$10,$56), ($13,$6d,$d6,$47),
    ($8c,$9a,$d7,$61), ($7a,$37,$a1,$0c), ($8e,$59,$f8,$14), ($89,$eb,$13,$3c),
    ($ee,$ce,$a9,$27), ($35,$b7,$61,$c9), ($ed,$e1,$1c,$e5), ($3c,$7a,$47,$b1),
    ($59,$9c,$d2,$df), ($3f,$55,$f2,$73), ($79,$18,$14,$ce), ($bf,$73,$c7,$37),
    ($ea,$53,$f7,$cd), ($5b,$5f,$fd,$aa), ($14,$df,$3d,$6f), ($86,$78,$44,$db),
    ($81,$ca,$af,$f3), ($3e,$b9,$68,$c4), ($2c,$38,$24,$34), ($5f,$c2,$a3,$40),
    ($72,$16,$1d,$c3), ($0c,$bc,$e2,$25), ($8b,$28,$3c,$49), ($41,$ff,$0d,$95),
    ($71,$39,$a8,$01), ($de,$08,$0c,$b3), ($9c,$d8,$b4,$e4), ($90,$64,$56,$c1),
    ($61,$7b,$cb,$84), ($70,$d5,$32,$b6), ($74,$48,$6c,$5c), ($42,$d0,$b8,$57));
  T7: array[0..255,0..3] of byte= (
    ($a7,$50,$51,$f4), ($65,$53,$7e,$41), ($a4,$c3,$1a,$17), ($5e,$96,$3a,$27),
    ($6b,$cb,$3b,$ab), ($45,$f1,$1f,$9d), ($58,$ab,$ac,$fa), ($03,$93,$4b,$e3),
    ($fa,$55,$20,$30), ($6d,$f6,$ad,$76), ($76,$91,$88,$cc), ($4c,$25,$f5,$02),
    ($d7,$fc,$4f,$e5), ($cb,$d7,$c5,$2a), ($44,$80,$26,$35), ($a3,$8f,$b5,$62),
    ($5a,$49,$de,$b1), ($1b,$67,$25,$ba), ($0e,$98,$45,$ea), ($c0,$e1,$5d,$fe),
    ($75,$02,$c3,$2f), ($f0,$12,$81,$4c), ($97,$a3,$8d,$46), ($f9,$c6,$6b,$d3),
    ($5f,$e7,$03,$8f), ($9c,$95,$15,$92), ($7a,$eb,$bf,$6d), ($59,$da,$95,$52),
    ($83,$2d,$d4,$be), ($21,$d3,$58,$74), ($69,$29,$49,$e0), ($c8,$44,$8e,$c9),
    ($89,$6a,$75,$c2), ($79,$78,$f4,$8e), ($3e,$6b,$99,$58), ($71,$dd,$27,$b9), 
    ($4f,$b6,$be,$e1), ($ad,$17,$f0,$88), ($ac,$66,$c9,$20), ($3a,$b4,$7d,$ce), 
    ($4a,$18,$63,$df), ($31,$82,$e5,$1a), ($33,$60,$97,$51), ($7f,$45,$62,$53), 
    ($77,$e0,$b1,$64), ($ae,$84,$bb,$6b), ($a0,$1c,$fe,$81), ($2b,$94,$f9,$08),
    ($68,$58,$70,$48), ($fd,$19,$8f,$45), ($6c,$87,$94,$de), ($f8,$b7,$52,$7b), 
    ($d3,$23,$ab,$73), ($02,$e2,$72,$4b), ($8f,$57,$e3,$1f), ($ab,$2a,$66,$55),
    ($28,$07,$b2,$eb), ($c2,$03,$2f,$b5), ($7b,$9a,$86,$c5), ($08,$a5,$d3,$37), 
    ($87,$f2,$30,$28), ($a5,$b2,$23,$bf), ($6a,$ba,$02,$03), ($82,$5c,$ed,$16),
    ($1c,$2b,$8a,$cf), ($b4,$92,$a7,$79), ($f2,$f0,$f3,$07), ($e2,$a1,$4e,$69), 
    ($f4,$cd,$65,$da), ($be,$d5,$06,$05), ($62,$1f,$d1,$34), ($fe,$8a,$c4,$a6), 
    ($53,$9d,$34,$2e), ($55,$a0,$a2,$f3), ($e1,$32,$05,$8a), ($eb,$75,$a4,$f6),
    ($ec,$39,$0b,$83), ($ef,$aa,$40,$60), ($9f,$06,$5e,$71), ($10,$51,$bd,$6e),
    ($8a,$f9,$3e,$21), ($06,$3d,$96,$dd), ($05,$ae,$dd,$3e), ($bd,$46,$4d,$e6),
    ($8d,$b5,$91,$54), ($5d,$05,$71,$c4), ($d4,$6f,$04,$06), ($15,$ff,$60,$50), 
    ($fb,$24,$19,$98), ($e9,$97,$d6,$bd), ($43,$cc,$89,$40), ($9e,$77,$67,$d9), 
    ($42,$bd,$b0,$e8), ($8b,$88,$07,$89), ($5b,$38,$e7,$19), ($ee,$db,$79,$c8),
    ($0a,$47,$a1,$7c), ($0f,$e9,$7c,$42), ($1e,$c9,$f8,$84), ($00,$00,$00,$00), 
    ($86,$83,$09,$80), ($ed,$48,$32,$2b), ($70,$ac,$1e,$11), ($72,$4e,$6c,$5a), 
    ($ff,$fb,$fd,$0e), ($38,$56,$0f,$85), ($d5,$1e,$3d,$ae), ($39,$27,$36,$2d), 
    ($d9,$64,$0a,$0f), ($a6,$21,$68,$5c), ($54,$d1,$9b,$5b), ($2e,$3a,$24,$36), 
    ($67,$b1,$0c,$0a), ($e7,$0f,$93,$57), ($96,$d2,$b4,$ee), ($91,$9e,$1b,$9b), 
    ($c5,$4f,$80,$c0), ($20,$a2,$61,$dc), ($4b,$69,$5a,$77), ($1a,$16,$1c,$12), 
    ($ba,$0a,$e2,$93), ($2a,$e5,$c0,$a0), ($e0,$43,$3c,$22), ($17,$1d,$12,$1b), 
    ($0d,$0b,$0e,$09), ($c7,$ad,$f2,$8b), ($a8,$b9,$2d,$b6), ($a9,$c8,$14,$1e), 
    ($19,$85,$57,$f1), ($07,$4c,$af,$75), ($dd,$bb,$ee,$99), ($60,$fd,$a3,$7f), 
    ($26,$9f,$f7,$01), ($f5,$bc,$5c,$72), ($3b,$c5,$44,$66), ($7e,$34,$5b,$fb), 
    ($29,$76,$8b,$43), ($c6,$dc,$cb,$23), ($fc,$68,$b6,$ed), ($f1,$63,$b8,$e4),
    ($dc,$ca,$d7,$31), ($85,$10,$42,$63), ($22,$40,$13,$97), ($11,$20,$84,$c6), 
    ($24,$7d,$85,$4a), ($3d,$f8,$d2,$bb), ($32,$11,$ae,$f9), ($a1,$6d,$c7,$29),
    ($2f,$4b,$1d,$9e), ($30,$f3,$dc,$b2), ($52,$ec,$0d,$86), ($e3,$d0,$77,$c1),
    ($16,$6c,$2b,$b3), ($b9,$99,$a9,$70), ($48,$fa,$11,$94), ($64,$22,$47,$e9), 
    ($8c,$c4,$a8,$fc), ($3f,$1a,$a0,$f0), ($2c,$d8,$56,$7d), ($90,$ef,$22,$33), 
    ($4e,$c7,$87,$49), ($d1,$c1,$d9,$38), ($a2,$fe,$8c,$ca), ($0b,$36,$98,$d4), 
    ($81,$cf,$a6,$f5), ($de,$28,$a5,$7a), ($8e,$26,$da,$b7), ($bf,$a4,$3f,$ad),
    ($9d,$e4,$2c,$3a), ($92,$0d,$50,$78), ($cc,$9b,$6a,$5f), ($46,$62,$54,$7e),
    ($13,$c2,$f6,$8d), ($b8,$e8,$90,$d8), ($f7,$5e,$2e,$39), ($af,$f5,$82,$c3),
    ($80,$be,$9f,$5d), ($93,$7c,$69,$d0), ($2d,$a9,$6f,$d5), ($12,$b3,$cf,$25),
    ($99,$3b,$c8,$ac), ($7d,$a7,$10,$18), ($63,$6e,$e8,$9c), ($bb,$7b,$db,$3b),
    ($78,$09,$cd,$26), ($18,$f4,$6e,$59), ($b7,$01,$ec,$9a), ($9a,$a8,$83,$4f),
    ($6e,$65,$e6,$95), ($e6,$7e,$aa,$ff), ($cf,$08,$21,$bc), ($e8,$e6,$ef,$15), 
    ($9b,$d9,$ba,$e7), ($36,$ce,$4a,$6f), ($09,$d4,$ea,$9f), ($7c,$d6,$29,$b0), 
    ($b2,$af,$31,$a4), ($23,$31,$2a,$3f), ($94,$30,$c6,$a5), ($66,$c0,$35,$a2),
    ($bc,$37,$74,$4e), ($ca,$a6,$fc,$82), ($d0,$b0,$e0,$90), ($d8,$15,$33,$a7), 
    ($98,$4a,$f1,$04), ($da,$f7,$41,$ec), ($50,$0e,$7f,$cd), ($f6,$2f,$17,$91),
    ($d6,$8d,$76,$4d), ($b0,$4d,$43,$ef), ($4d,$54,$cc,$aa), ($04,$df,$e4,$96),
    ($b5,$e3,$9e,$d1), ($88,$1b,$4c,$6a), ($1f,$b8,$c1,$2c), ($51,$7f,$46,$65), 
    ($ea,$04,$9d,$5e), ($35,$5d,$01,$8c), ($74,$73,$fa,$87), ($41,$2e,$fb,$0b), 
    ($1d,$5a,$b3,$67), ($d2,$52,$92,$db), ($56,$33,$e9,$10), ($47,$13,$6d,$d6),
    ($61,$8c,$9a,$d7), ($0c,$7a,$37,$a1), ($14,$8e,$59,$f8), ($3c,$89,$eb,$13), 
    ($27,$ee,$ce,$a9), ($c9,$35,$b7,$61), ($e5,$ed,$e1,$1c), ($b1,$3c,$7a,$47),
    ($df,$59,$9c,$d2), ($73,$3f,$55,$f2), ($ce,$79,$18,$14), ($37,$bf,$73,$c7),
    ($cd,$ea,$53,$f7), ($aa,$5b,$5f,$fd), ($6f,$14,$df,$3d), ($db,$86,$78,$44),
    ($f3,$81,$ca,$af), ($c4,$3e,$b9,$68), ($34,$2c,$38,$24), ($40,$5f,$c2,$a3),
    ($c3,$72,$16,$1d), ($25,$0c,$bc,$e2), ($49,$8b,$28,$3c), ($95,$41,$ff,$0d),
    ($01,$71,$39,$a8), ($b3,$de,$08,$0c), ($e4,$9c,$d8,$b4), ($c1,$90,$64,$56),
    ($84,$61,$7b,$cb), ($b6,$70,$d5,$32), ($5c,$74,$48,$6c), ($57,$42,$d0,$b8));
  T8: array[0..255,0..3] of byte= (
    ($f4,$a7,$50,$51), ($41,$65,$53,$7e), ($17,$a4,$c3,$1a), ($27,$5e,$96,$3a),
    ($ab,$6b,$cb,$3b), ($9d,$45,$f1,$1f), ($fa,$58,$ab,$ac), ($e3,$03,$93,$4b),
    ($30,$fa,$55,$20), ($76,$6d,$f6,$ad), ($cc,$76,$91,$88), ($02,$4c,$25,$f5),
    ($e5,$d7,$fc,$4f), ($2a,$cb,$d7,$c5), ($35,$44,$80,$26), ($62,$a3,$8f,$b5), 
    ($b1,$5a,$49,$de), ($ba,$1b,$67,$25), ($ea,$0e,$98,$45), ($fe,$c0,$e1,$5d), 
    ($2f,$75,$02,$c3), ($4c,$f0,$12,$81), ($46,$97,$a3,$8d), ($d3,$f9,$c6,$6b), 
    ($8f,$5f,$e7,$03), ($92,$9c,$95,$15), ($6d,$7a,$eb,$bf), ($52,$59,$da,$95), 
    ($be,$83,$2d,$d4), ($74,$21,$d3,$58), ($e0,$69,$29,$49), ($c9,$c8,$44,$8e),
    ($c2,$89,$6a,$75), ($8e,$79,$78,$f4), ($58,$3e,$6b,$99), ($b9,$71,$dd,$27), 
    ($e1,$4f,$b6,$be), ($88,$ad,$17,$f0), ($20,$ac,$66,$c9), ($ce,$3a,$b4,$7d), 
    ($df,$4a,$18,$63), ($1a,$31,$82,$e5), ($51,$33,$60,$97), ($53,$7f,$45,$62), 
    ($64,$77,$e0,$b1), ($6b,$ae,$84,$bb), ($81,$a0,$1c,$fe), ($08,$2b,$94,$f9), 
    ($48,$68,$58,$70), ($45,$fd,$19,$8f), ($de,$6c,$87,$94), ($7b,$f8,$b7,$52), 
    ($73,$d3,$23,$ab), ($4b,$02,$e2,$72), ($1f,$8f,$57,$e3), ($55,$ab,$2a,$66), 
    ($eb,$28,$07,$b2), ($b5,$c2,$03,$2f), ($c5,$7b,$9a,$86), ($37,$08,$a5,$d3),
    ($28,$87,$f2,$30), ($bf,$a5,$b2,$23), ($03,$6a,$ba,$02), ($16,$82,$5c,$ed), 
    ($cf,$1c,$2b,$8a), ($79,$b4,$92,$a7), ($07,$f2,$f0,$f3), ($69,$e2,$a1,$4e), 
    ($da,$f4,$cd,$65), ($05,$be,$d5,$06), ($34,$62,$1f,$d1), ($a6,$fe,$8a,$c4),
    ($2e,$53,$9d,$34), ($f3,$55,$a0,$a2), ($8a,$e1,$32,$05), ($f6,$eb,$75,$a4),
    ($83,$ec,$39,$0b), ($60,$ef,$aa,$40), ($71,$9f,$06,$5e), ($6e,$10,$51,$bd), 
    ($21,$8a,$f9,$3e), ($dd,$06,$3d,$96), ($3e,$05,$ae,$dd), ($e6,$bd,$46,$4d), 
    ($54,$8d,$b5,$91), ($c4,$5d,$05,$71), ($06,$d4,$6f,$04), ($50,$15,$ff,$60),
    ($98,$fb,$24,$19), ($bd,$e9,$97,$d6), ($40,$43,$cc,$89), ($d9,$9e,$77,$67), 
    ($e8,$42,$bd,$b0), ($89,$8b,$88,$07), ($19,$5b,$38,$e7), ($c8,$ee,$db,$79),
    ($7c,$0a,$47,$a1), ($42,$0f,$e9,$7c), ($84,$1e,$c9,$f8), ($00,$00,$00,$00),
    ($80,$86,$83,$09), ($2b,$ed,$48,$32), ($11,$70,$ac,$1e), ($5a,$72,$4e,$6c), 
    ($0e,$ff,$fb,$fd), ($85,$38,$56,$0f), ($ae,$d5,$1e,$3d), ($2d,$39,$27,$36), 
    ($0f,$d9,$64,$0a), ($5c,$a6,$21,$68), ($5b,$54,$d1,$9b), ($36,$2e,$3a,$24), 
    ($0a,$67,$b1,$0c), ($57,$e7,$0f,$93), ($ee,$96,$d2,$b4), ($9b,$91,$9e,$1b),
    ($c0,$c5,$4f,$80), ($dc,$20,$a2,$61), ($77,$4b,$69,$5a), ($12,$1a,$16,$1c), 
    ($93,$ba,$0a,$e2), ($a0,$2a,$e5,$c0), ($22,$e0,$43,$3c), ($1b,$17,$1d,$12), 
    ($09,$0d,$0b,$0e), ($8b,$c7,$ad,$f2), ($b6,$a8,$b9,$2d), ($1e,$a9,$c8,$14), 
    ($f1,$19,$85,$57), ($75,$07,$4c,$af), ($99,$dd,$bb,$ee), ($7f,$60,$fd,$a3), 
    ($01,$26,$9f,$f7), ($72,$f5,$bc,$5c), ($66,$3b,$c5,$44), ($fb,$7e,$34,$5b), 
    ($43,$29,$76,$8b), ($23,$c6,$dc,$cb), ($ed,$fc,$68,$b6), ($e4,$f1,$63,$b8), 
    ($31,$dc,$ca,$d7), ($63,$85,$10,$42), ($97,$22,$40,$13), ($c6,$11,$20,$84),
    ($4a,$24,$7d,$85), ($bb,$3d,$f8,$d2), ($f9,$32,$11,$ae), ($29,$a1,$6d,$c7), 
    ($9e,$2f,$4b,$1d), ($b2,$30,$f3,$dc), ($86,$52,$ec,$0d), ($c1,$e3,$d0,$77), 
    ($b3,$16,$6c,$2b), ($70,$b9,$99,$a9), ($94,$48,$fa,$11), ($e9,$64,$22,$47), 
    ($fc,$8c,$c4,$a8), ($f0,$3f,$1a,$a0), ($7d,$2c,$d8,$56), ($33,$90,$ef,$22), 
    ($49,$4e,$c7,$87), ($38,$d1,$c1,$d9), ($ca,$a2,$fe,$8c), ($d4,$0b,$36,$98),
    ($f5,$81,$cf,$a6), ($7a,$de,$28,$a5), ($b7,$8e,$26,$da), ($ad,$bf,$a4,$3f),
    ($3a,$9d,$e4,$2c), ($78,$92,$0d,$50), ($5f,$cc,$9b,$6a), ($7e,$46,$62,$54),
    ($8d,$13,$c2,$f6), ($d8,$b8,$e8,$90), ($39,$f7,$5e,$2e), ($c3,$af,$f5,$82), 
    ($5d,$80,$be,$9f), ($d0,$93,$7c,$69), ($d5,$2d,$a9,$6f), ($25,$12,$b3,$cf),
    ($ac,$99,$3b,$c8), ($18,$7d,$a7,$10), ($9c,$63,$6e,$e8), ($3b,$bb,$7b,$db), 
    ($26,$78,$09,$cd), ($59,$18,$f4,$6e), ($9a,$b7,$01,$ec), ($4f,$9a,$a8,$83),
    ($95,$6e,$65,$e6), ($ff,$e6,$7e,$aa), ($bc,$cf,$08,$21), ($15,$e8,$e6,$ef),
    ($e7,$9b,$d9,$ba), ($6f,$36,$ce,$4a), ($9f,$09,$d4,$ea), ($b0,$7c,$d6,$29), 
    ($a4,$b2,$af,$31), ($3f,$23,$31,$2a), ($a5,$94,$30,$c6), ($a2,$66,$c0,$35),
    ($4e,$bc,$37,$74), ($82,$ca,$a6,$fc), ($90,$d0,$b0,$e0), ($a7,$d8,$15,$33),
    ($04,$98,$4a,$f1), ($ec,$da,$f7,$41), ($cd,$50,$0e,$7f), ($91,$f6,$2f,$17),
    ($4d,$d6,$8d,$76), ($ef,$b0,$4d,$43), ($aa,$4d,$54,$cc), ($96,$04,$df,$e4),
    ($d1,$b5,$e3,$9e), ($6a,$88,$1b,$4c), ($2c,$1f,$b8,$c1), ($65,$51,$7f,$46), 
    ($5e,$ea,$04,$9d), ($8c,$35,$5d,$01), ($87,$74,$73,$fa), ($0b,$41,$2e,$fb),
    ($67,$1d,$5a,$b3), ($db,$d2,$52,$92), ($10,$56,$33,$e9), ($d6,$47,$13,$6d),
    ($d7,$61,$8c,$9a), ($a1,$0c,$7a,$37), ($f8,$14,$8e,$59), ($13,$3c,$89,$eb), 
    ($a9,$27,$ee,$ce), ($61,$c9,$35,$b7), ($1c,$e5,$ed,$e1), ($47,$b1,$3c,$7a),
    ($d2,$df,$59,$9c), ($f2,$73,$3f,$55), ($14,$ce,$79,$18), ($c7,$37,$bf,$73),
    ($f7,$cd,$ea,$53), ($fd,$aa,$5b,$5f), ($3d,$6f,$14,$df), ($44,$db,$86,$78),
    ($af,$f3,$81,$ca), ($68,$c4,$3e,$b9), ($24,$34,$2c,$38), ($a3,$40,$5f,$c2),
    ($1d,$c3,$72,$16), ($e2,$25,$0c,$bc), ($3c,$49,$8b,$28), ($0d,$95,$41,$ff),
    ($a8,$01,$71,$39), ($0c,$b3,$de,$08), ($b4,$e4,$9c,$d8), ($56,$c1,$90,$64),
    ($cb,$84,$61,$7b), ($32,$b6,$70,$d5), ($6c,$5c,$74,$48), ($b8,$57,$42,$d0));
  S5: array[0..255] of byte= (
    $52,$09,$6a,$d5,
    $30,$36,$a5,$38,
    $bf,$40,$a3,$9e,
    $81,$f3,$d7,$fb,
    $7c,$e3,$39,$82,
    $9b,$2f,$ff,$87,
    $34,$8e,$43,$44,
    $c4,$de,$e9,$cb,
    $54,$7b,$94,$32,
    $a6,$c2,$23,$3d,
    $ee,$4c,$95,$0b,
    $42,$fa,$c3,$4e,
    $08,$2e,$a1,$66,
    $28,$d9,$24,$b2,
    $76,$5b,$a2,$49,
    $6d,$8b,$d1,$25,
    $72,$f8,$f6,$64,
    $86,$68,$98,$16,
    $d4,$a4,$5c,$cc,
    $5d,$65,$b6,$92,
    $6c,$70,$48,$50,
    $fd,$ed,$b9,$da,
    $5e,$15,$46,$57,
    $a7,$8d,$9d,$84,
    $90,$d8,$ab,$00,
    $8c,$bc,$d3,$0a,
    $f7,$e4,$58,$05,
    $b8,$b3,$45,$06,
    $d0,$2c,$1e,$8f,
    $ca,$3f,$0f,$02,
    $c1,$af,$bd,$03,
    $01,$13,$8a,$6b,
    $3a,$91,$11,$41,
    $4f,$67,$dc,$ea,
    $97,$f2,$cf,$ce,
    $f0,$b4,$e6,$73,
    $96,$ac,$74,$22,
    $e7,$ad,$35,$85,
    $e2,$f9,$37,$e8,
    $1c,$75,$df,$6e,
    $47,$f1,$1a,$71,
    $1d,$29,$c5,$89,
    $6f,$b7,$62,$0e,
    $aa,$18,$be,$1b,
    $fc,$56,$3e,$4b,
    $c6,$d2,$79,$20,
    $9a,$db,$c0,$fe,
    $78,$cd,$5a,$f4,
    $1f,$dd,$a8,$33,
    $88,$07,$c7,$31,
    $b1,$12,$10,$59,
    $27,$80,$ec,$5f,
    $60,$51,$7f,$a9,
    $19,$b5,$4a,$0d,
    $2d,$e5,$7a,$9f,
    $93,$c9,$9c,$ef,
    $a0,$e0,$3b,$4d,
    $ae,$2a,$f5,$b0,
    $c8,$eb,$bb,$3c,
    $83,$53,$99,$61,
    $17,$2b,$04,$7e,
    $ba,$77,$d6,$26,
    $e1,$69,$14,$63,
    $55,$21,$0c,$7d);
  U1: array[0..255,0..3] of byte= (
    ($00,$00,$00,$00), ($0e,$09,$0d,$0b), ($1c,$12,$1a,$16), ($12,$1b,$17,$1d),
    ($38,$24,$34,$2c), ($36,$2d,$39,$27), ($24,$36,$2e,$3a), ($2a,$3f,$23,$31),
    ($70,$48,$68,$58), ($7e,$41,$65,$53), ($6c,$5a,$72,$4e), ($62,$53,$7f,$45),
    ($48,$6c,$5c,$74), ($46,$65,$51,$7f), ($54,$7e,$46,$62), ($5a,$77,$4b,$69),
    ($e0,$90,$d0,$b0), ($ee,$99,$dd,$bb), ($fc,$82,$ca,$a6), ($f2,$8b,$c7,$ad),
    ($d8,$b4,$e4,$9c), ($d6,$bd,$e9,$97), ($c4,$a6,$fe,$8a), ($ca,$af,$f3,$81), 
    ($90,$d8,$b8,$e8), ($9e,$d1,$b5,$e3), ($8c,$ca,$a2,$fe), ($82,$c3,$af,$f5),
    ($a8,$fc,$8c,$c4), ($a6,$f5,$81,$cf), ($b4,$ee,$96,$d2), ($ba,$e7,$9b,$d9),
    ($db,$3b,$bb,$7b), ($d5,$32,$b6,$70), ($c7,$29,$a1,$6d), ($c9,$20,$ac,$66),
    ($e3,$1f,$8f,$57), ($ed,$16,$82,$5c), ($ff,$0d,$95,$41), ($f1,$04,$98,$4a),
    ($ab,$73,$d3,$23), ($a5,$7a,$de,$28), ($b7,$61,$c9,$35), ($b9,$68,$c4,$3e),
    ($93,$57,$e7,$0f), ($9d,$5e,$ea,$04), ($8f,$45,$fd,$19), ($81,$4c,$f0,$12),
    ($3b,$ab,$6b,$cb), ($35,$a2,$66,$c0), ($27,$b9,$71,$dd), ($29,$b0,$7c,$d6),
    ($03,$8f,$5f,$e7), ($0d,$86,$52,$ec), ($1f,$9d,$45,$f1), ($11,$94,$48,$fa),
    ($4b,$e3,$03,$93), ($45,$ea,$0e,$98), ($57,$f1,$19,$85), ($59,$f8,$14,$8e),
    ($73,$c7,$37,$bf), ($7d,$ce,$3a,$b4), ($6f,$d5,$2d,$a9), ($61,$dc,$20,$a2),
    ($ad,$76,$6d,$f6), ($a3,$7f,$60,$fd), ($b1,$64,$77,$e0), ($bf,$6d,$7a,$eb),
    ($95,$52,$59,$da), ($9b,$5b,$54,$d1), ($89,$40,$43,$cc), ($87,$49,$4e,$c7),
    ($dd,$3e,$05,$ae), ($d3,$37,$08,$a5), ($c1,$2c,$1f,$b8), ($cf,$25,$12,$b3),
    ($e5,$1a,$31,$82), ($eb,$13,$3c,$89), ($f9,$08,$2b,$94), ($f7,$01,$26,$9f),
    ($4d,$e6,$bd,$46), ($43,$ef,$b0,$4d), ($51,$f4,$a7,$50), ($5f,$fd,$aa,$5b),
    ($75,$c2,$89,$6a), ($7b,$cb,$84,$61), ($69,$d0,$93,$7c), ($67,$d9,$9e,$77), 
    ($3d,$ae,$d5,$1e), ($33,$a7,$d8,$15), ($21,$bc,$cf,$08), ($2f,$b5,$c2,$03),
    ($05,$8a,$e1,$32), ($0b,$83,$ec,$39), ($19,$98,$fb,$24), ($17,$91,$f6,$2f),
    ($76,$4d,$d6,$8d), ($78,$44,$db,$86), ($6a,$5f,$cc,$9b), ($64,$56,$c1,$90),
    ($4e,$69,$e2,$a1), ($40,$60,$ef,$aa), ($52,$7b,$f8,$b7), ($5c,$72,$f5,$bc),
    ($06,$05,$be,$d5), ($08,$0c,$b3,$de), ($1a,$17,$a4,$c3), ($14,$1e,$a9,$c8), 
    ($3e,$21,$8a,$f9), ($30,$28,$87,$f2), ($22,$33,$90,$ef), ($2c,$3a,$9d,$e4), 
    ($96,$dd,$06,$3d), ($98,$d4,$0b,$36), ($8a,$cf,$1c,$2b), ($84,$c6,$11,$20),
    ($ae,$f9,$32,$11), ($a0,$f0,$3f,$1a), ($b2,$eb,$28,$07), ($bc,$e2,$25,$0c),
    ($e6,$95,$6e,$65), ($e8,$9c,$63,$6e), ($fa,$87,$74,$73), ($f4,$8e,$79,$78), 
    ($de,$b1,$5a,$49), ($d0,$b8,$57,$42), ($c2,$a3,$40,$5f), ($cc,$aa,$4d,$54),
    ($41,$ec,$da,$f7), ($4f,$e5,$d7,$fc), ($5d,$fe,$c0,$e1), ($53,$f7,$cd,$ea),
    ($79,$c8,$ee,$db), ($77,$c1,$e3,$d0), ($65,$da,$f4,$cd), ($6b,$d3,$f9,$c6), 
    ($31,$a4,$b2,$af), ($3f,$ad,$bf,$a4), ($2d,$b6,$a8,$b9), ($23,$bf,$a5,$b2), 
    ($09,$80,$86,$83), ($07,$89,$8b,$88), ($15,$92,$9c,$95), ($1b,$9b,$91,$9e), 
    ($a1,$7c,$0a,$47), ($af,$75,$07,$4c), ($bd,$6e,$10,$51), ($b3,$67,$1d,$5a), 
    ($99,$58,$3e,$6b), ($97,$51,$33,$60), ($85,$4a,$24,$7d), ($8b,$43,$29,$76),
    ($d1,$34,$62,$1f), ($df,$3d,$6f,$14), ($cd,$26,$78,$09), ($c3,$2f,$75,$02), 
    ($e9,$10,$56,$33), ($e7,$19,$5b,$38), ($f5,$02,$4c,$25), ($fb,$0b,$41,$2e), 
    ($9a,$d7,$61,$8c), ($94,$de,$6c,$87), ($86,$c5,$7b,$9a), ($88,$cc,$76,$91),
    ($a2,$f3,$55,$a0), ($ac,$fa,$58,$ab), ($be,$e1,$4f,$b6), ($b0,$e8,$42,$bd),
    ($ea,$9f,$09,$d4), ($e4,$96,$04,$df), ($f6,$8d,$13,$c2), ($f8,$84,$1e,$c9), 
    ($d2,$bb,$3d,$f8), ($dc,$b2,$30,$f3), ($ce,$a9,$27,$ee), ($c0,$a0,$2a,$e5), 
    ($7a,$47,$b1,$3c), ($74,$4e,$bc,$37), ($66,$55,$ab,$2a), ($68,$5c,$a6,$21), 
    ($42,$63,$85,$10), ($4c,$6a,$88,$1b), ($5e,$71,$9f,$06), ($50,$78,$92,$0d),
    ($0a,$0f,$d9,$64), ($04,$06,$d4,$6f), ($16,$1d,$c3,$72), ($18,$14,$ce,$79),
    ($32,$2b,$ed,$48), ($3c,$22,$e0,$43), ($2e,$39,$f7,$5e), ($20,$30,$fa,$55),
    ($ec,$9a,$b7,$01), ($e2,$93,$ba,$0a), ($f0,$88,$ad,$17), ($fe,$81,$a0,$1c),
    ($d4,$be,$83,$2d), ($da,$b7,$8e,$26), ($c8,$ac,$99,$3b), ($c6,$a5,$94,$30), 
    ($9c,$d2,$df,$59), ($92,$db,$d2,$52), ($80,$c0,$c5,$4f), ($8e,$c9,$c8,$44),
    ($a4,$f6,$eb,$75), ($aa,$ff,$e6,$7e), ($b8,$e4,$f1,$63), ($b6,$ed,$fc,$68), 
    ($0c,$0a,$67,$b1), ($02,$03,$6a,$ba), ($10,$18,$7d,$a7), ($1e,$11,$70,$ac),
    ($34,$2e,$53,$9d), ($3a,$27,$5e,$96), ($28,$3c,$49,$8b), ($26,$35,$44,$80), 
    ($7c,$42,$0f,$e9), ($72,$4b,$02,$e2), ($60,$50,$15,$ff), ($6e,$59,$18,$f4),
    ($44,$66,$3b,$c5), ($4a,$6f,$36,$ce), ($58,$74,$21,$d3), ($56,$7d,$2c,$d8),
    ($37,$a1,$0c,$7a), ($39,$a8,$01,$71), ($2b,$b3,$16,$6c), ($25,$ba,$1b,$67), 
    ($0f,$85,$38,$56), ($01,$8c,$35,$5d), ($13,$97,$22,$40), ($1d,$9e,$2f,$4b),
    ($47,$e9,$64,$22), ($49,$e0,$69,$29), ($5b,$fb,$7e,$34), ($55,$f2,$73,$3f),
    ($7f,$cd,$50,$0e), ($71,$c4,$5d,$05), ($63,$df,$4a,$18), ($6d,$d6,$47,$13),
    ($d7,$31,$dc,$ca), ($d9,$38,$d1,$c1), ($cb,$23,$c6,$dc), ($c5,$2a,$cb,$d7),
    ($ef,$15,$e8,$e6), ($e1,$1c,$e5,$ed), ($f3,$07,$f2,$f0), ($fd,$0e,$ff,$fb),
    ($a7,$79,$b4,$92), ($a9,$70,$b9,$99), ($bb,$6b,$ae,$84), ($b5,$62,$a3,$8f),
    ($9f,$5d,$80,$be), ($91,$54,$8d,$b5), ($83,$4f,$9a,$a8), ($8d,$46,$97,$a3));
  U2: array[0..255,0..3] of byte= (
    ($00,$00,$00,$00), ($0b,$0e,$09,$0d), ($16,$1c,$12,$1a), ($1d,$12,$1b,$17),
    ($2c,$38,$24,$34), ($27,$36,$2d,$39), ($3a,$24,$36,$2e), ($31,$2a,$3f,$23),
    ($58,$70,$48,$68), ($53,$7e,$41,$65), ($4e,$6c,$5a,$72), ($45,$62,$53,$7f),
    ($74,$48,$6c,$5c), ($7f,$46,$65,$51), ($62,$54,$7e,$46), ($69,$5a,$77,$4b),
    ($b0,$e0,$90,$d0), ($bb,$ee,$99,$dd), ($a6,$fc,$82,$ca), ($ad,$f2,$8b,$c7),
    ($9c,$d8,$b4,$e4), ($97,$d6,$bd,$e9), ($8a,$c4,$a6,$fe), ($81,$ca,$af,$f3),
    ($e8,$90,$d8,$b8), ($e3,$9e,$d1,$b5), ($fe,$8c,$ca,$a2), ($f5,$82,$c3,$af), 
    ($c4,$a8,$fc,$8c), ($cf,$a6,$f5,$81), ($d2,$b4,$ee,$96), ($d9,$ba,$e7,$9b), 
    ($7b,$db,$3b,$bb), ($70,$d5,$32,$b6), ($6d,$c7,$29,$a1), ($66,$c9,$20,$ac),
    ($57,$e3,$1f,$8f), ($5c,$ed,$16,$82), ($41,$ff,$0d,$95), ($4a,$f1,$04,$98), 
    ($23,$ab,$73,$d3), ($28,$a5,$7a,$de), ($35,$b7,$61,$c9), ($3e,$b9,$68,$c4), 
    ($0f,$93,$57,$e7), ($04,$9d,$5e,$ea), ($19,$8f,$45,$fd), ($12,$81,$4c,$f0),
    ($cb,$3b,$ab,$6b), ($c0,$35,$a2,$66), ($dd,$27,$b9,$71), ($d6,$29,$b0,$7c),
    ($e7,$03,$8f,$5f), ($ec,$0d,$86,$52), ($f1,$1f,$9d,$45), ($fa,$11,$94,$48),
    ($93,$4b,$e3,$03), ($98,$45,$ea,$0e), ($85,$57,$f1,$19), ($8e,$59,$f8,$14), 
    ($bf,$73,$c7,$37), ($b4,$7d,$ce,$3a), ($a9,$6f,$d5,$2d), ($a2,$61,$dc,$20),
    ($f6,$ad,$76,$6d), ($fd,$a3,$7f,$60), ($e0,$b1,$64,$77), ($eb,$bf,$6d,$7a),
    ($da,$95,$52,$59), ($d1,$9b,$5b,$54), ($cc,$89,$40,$43), ($c7,$87,$49,$4e), 
    ($ae,$dd,$3e,$05), ($a5,$d3,$37,$08), ($b8,$c1,$2c,$1f), ($b3,$cf,$25,$12),
    ($82,$e5,$1a,$31), ($89,$eb,$13,$3c), ($94,$f9,$08,$2b), ($9f,$f7,$01,$26), 
    ($46,$4d,$e6,$bd), ($4d,$43,$ef,$b0), ($50,$51,$f4,$a7), ($5b,$5f,$fd,$aa), 
    ($6a,$75,$c2,$89), ($61,$7b,$cb,$84), ($7c,$69,$d0,$93), ($77,$67,$d9,$9e), 
    ($1e,$3d,$ae,$d5), ($15,$33,$a7,$d8), ($08,$21,$bc,$cf), ($03,$2f,$b5,$c2), 
    ($32,$05,$8a,$e1), ($39,$0b,$83,$ec), ($24,$19,$98,$fb), ($2f,$17,$91,$f6),
    ($8d,$76,$4d,$d6), ($86,$78,$44,$db), ($9b,$6a,$5f,$cc), ($90,$64,$56,$c1),
    ($a1,$4e,$69,$e2), ($aa,$40,$60,$ef), ($b7,$52,$7b,$f8), ($bc,$5c,$72,$f5), 
    ($d5,$06,$05,$be), ($de,$08,$0c,$b3), ($c3,$1a,$17,$a4), ($c8,$14,$1e,$a9), 
    ($f9,$3e,$21,$8a), ($f2,$30,$28,$87), ($ef,$22,$33,$90), ($e4,$2c,$3a,$9d),
    ($3d,$96,$dd,$06), ($36,$98,$d4,$0b), ($2b,$8a,$cf,$1c), ($20,$84,$c6,$11), 
    ($11,$ae,$f9,$32), ($1a,$a0,$f0,$3f), ($07,$b2,$eb,$28), ($0c,$bc,$e2,$25), 
    ($65,$e6,$95,$6e), ($6e,$e8,$9c,$63), ($73,$fa,$87,$74), ($78,$f4,$8e,$79), 
    ($49,$de,$b1,$5a), ($42,$d0,$b8,$57), ($5f,$c2,$a3,$40), ($54,$cc,$aa,$4d),
    ($f7,$41,$ec,$da), ($fc,$4f,$e5,$d7), ($e1,$5d,$fe,$c0), ($ea,$53,$f7,$cd), 
    ($db,$79,$c8,$ee), ($d0,$77,$c1,$e3), ($cd,$65,$da,$f4), ($c6,$6b,$d3,$f9), 
    ($af,$31,$a4,$b2), ($a4,$3f,$ad,$bf), ($b9,$2d,$b6,$a8), ($b2,$23,$bf,$a5), 
    ($83,$09,$80,$86), ($88,$07,$89,$8b), ($95,$15,$92,$9c), ($9e,$1b,$9b,$91),
    ($47,$a1,$7c,$0a), ($4c,$af,$75,$07), ($51,$bd,$6e,$10), ($5a,$b3,$67,$1d),
    ($6b,$99,$58,$3e), ($60,$97,$51,$33), ($7d,$85,$4a,$24), ($76,$8b,$43,$29),
    ($1f,$d1,$34,$62), ($14,$df,$3d,$6f), ($09,$cd,$26,$78), ($02,$c3,$2f,$75),
    ($33,$e9,$10,$56), ($38,$e7,$19,$5b), ($25,$f5,$02,$4c), ($2e,$fb,$0b,$41),
    ($8c,$9a,$d7,$61), ($87,$94,$de,$6c), ($9a,$86,$c5,$7b), ($91,$88,$cc,$76), 
    ($a0,$a2,$f3,$55), ($ab,$ac,$fa,$58), ($b6,$be,$e1,$4f), ($bd,$b0,$e8,$42),
    ($d4,$ea,$9f,$09), ($df,$e4,$96,$04), ($c2,$f6,$8d,$13), ($c9,$f8,$84,$1e), 
    ($f8,$d2,$bb,$3d), ($f3,$dc,$b2,$30), ($ee,$ce,$a9,$27), ($e5,$c0,$a0,$2a), 
    ($3c,$7a,$47,$b1), ($37,$74,$4e,$bc), ($2a,$66,$55,$ab), ($21,$68,$5c,$a6),
    ($10,$42,$63,$85), ($1b,$4c,$6a,$88), ($06,$5e,$71,$9f), ($0d,$50,$78,$92), 
    ($64,$0a,$0f,$d9), ($6f,$04,$06,$d4), ($72,$16,$1d,$c3), ($79,$18,$14,$ce),
    ($48,$32,$2b,$ed), ($43,$3c,$22,$e0), ($5e,$2e,$39,$f7), ($55,$20,$30,$fa),
    ($01,$ec,$9a,$b7), ($0a,$e2,$93,$ba), ($17,$f0,$88,$ad), ($1c,$fe,$81,$a0), 
    ($2d,$d4,$be,$83), ($26,$da,$b7,$8e), ($3b,$c8,$ac,$99), ($30,$c6,$a5,$94), 
    ($59,$9c,$d2,$df), ($52,$92,$db,$d2), ($4f,$80,$c0,$c5), ($44,$8e,$c9,$c8),
    ($75,$a4,$f6,$eb), ($7e,$aa,$ff,$e6), ($63,$b8,$e4,$f1), ($68,$b6,$ed,$fc), 
    ($b1,$0c,$0a,$67), ($ba,$02,$03,$6a), ($a7,$10,$18,$7d), ($ac,$1e,$11,$70), 
    ($9d,$34,$2e,$53), ($96,$3a,$27,$5e), ($8b,$28,$3c,$49), ($80,$26,$35,$44), 
    ($e9,$7c,$42,$0f), ($e2,$72,$4b,$02), ($ff,$60,$50,$15), ($f4,$6e,$59,$18),
    ($c5,$44,$66,$3b), ($ce,$4a,$6f,$36), ($d3,$58,$74,$21), ($d8,$56,$7d,$2c), 
    ($7a,$37,$a1,$0c), ($71,$39,$a8,$01), ($6c,$2b,$b3,$16), ($67,$25,$ba,$1b), 
    ($56,$0f,$85,$38), ($5d,$01,$8c,$35), ($40,$13,$97,$22), ($4b,$1d,$9e,$2f), 
    ($22,$47,$e9,$64), ($29,$49,$e0,$69), ($34,$5b,$fb,$7e), ($3f,$55,$f2,$73),
    ($0e,$7f,$cd,$50), ($05,$71,$c4,$5d), ($18,$63,$df,$4a), ($13,$6d,$d6,$47),
    ($ca,$d7,$31,$dc), ($c1,$d9,$38,$d1), ($dc,$cb,$23,$c6), ($d7,$c5,$2a,$cb), 
    ($e6,$ef,$15,$e8), ($ed,$e1,$1c,$e5), ($f0,$f3,$07,$f2), ($fb,$fd,$0e,$ff), 
    ($92,$a7,$79,$b4), ($99,$a9,$70,$b9), ($84,$bb,$6b,$ae), ($8f,$b5,$62,$a3),
    ($be,$9f,$5d,$80), ($b5,$91,$54,$8d), ($a8,$83,$4f,$9a), ($a3,$8d,$46,$97));
  U3: array[0..255,0..3] of byte= (
    ($00,$00,$00,$00), ($0d,$0b,$0e,$09), ($1a,$16,$1c,$12), ($17,$1d,$12,$1b),
    ($34,$2c,$38,$24), ($39,$27,$36,$2d), ($2e,$3a,$24,$36), ($23,$31,$2a,$3f),
    ($68,$58,$70,$48), ($65,$53,$7e,$41), ($72,$4e,$6c,$5a), ($7f,$45,$62,$53),
    ($5c,$74,$48,$6c), ($51,$7f,$46,$65), ($46,$62,$54,$7e), ($4b,$69,$5a,$77),
    ($d0,$b0,$e0,$90), ($dd,$bb,$ee,$99), ($ca,$a6,$fc,$82), ($c7,$ad,$f2,$8b), 
    ($e4,$9c,$d8,$b4), ($e9,$97,$d6,$bd), ($fe,$8a,$c4,$a6), ($f3,$81,$ca,$af),
    ($b8,$e8,$90,$d8), ($b5,$e3,$9e,$d1), ($a2,$fe,$8c,$ca), ($af,$f5,$82,$c3),
    ($8c,$c4,$a8,$fc), ($81,$cf,$a6,$f5), ($96,$d2,$b4,$ee), ($9b,$d9,$ba,$e7), 
    ($bb,$7b,$db,$3b), ($b6,$70,$d5,$32), ($a1,$6d,$c7,$29), ($ac,$66,$c9,$20), 
    ($8f,$57,$e3,$1f), ($82,$5c,$ed,$16), ($95,$41,$ff,$0d), ($98,$4a,$f1,$04), 
    ($d3,$23,$ab,$73), ($de,$28,$a5,$7a), ($c9,$35,$b7,$61), ($c4,$3e,$b9,$68),
    ($e7,$0f,$93,$57), ($ea,$04,$9d,$5e), ($fd,$19,$8f,$45), ($f0,$12,$81,$4c), 
    ($6b,$cb,$3b,$ab), ($66,$c0,$35,$a2), ($71,$dd,$27,$b9), ($7c,$d6,$29,$b0), 
    ($5f,$e7,$03,$8f), ($52,$ec,$0d,$86), ($45,$f1,$1f,$9d), ($48,$fa,$11,$94), 
    ($03,$93,$4b,$e3), ($0e,$98,$45,$ea), ($19,$85,$57,$f1), ($14,$8e,$59,$f8), 
    ($37,$bf,$73,$c7), ($3a,$b4,$7d,$ce), ($2d,$a9,$6f,$d5), ($20,$a2,$61,$dc), 
    ($6d,$f6,$ad,$76), ($60,$fd,$a3,$7f), ($77,$e0,$b1,$64), ($7a,$eb,$bf,$6d),
    ($59,$da,$95,$52), ($54,$d1,$9b,$5b), ($43,$cc,$89,$40), ($4e,$c7,$87,$49),
    ($05,$ae,$dd,$3e), ($08,$a5,$d3,$37), ($1f,$b8,$c1,$2c), ($12,$b3,$cf,$25),
    ($31,$82,$e5,$1a), ($3c,$89,$eb,$13), ($2b,$94,$f9,$08), ($26,$9f,$f7,$01),
    ($bd,$46,$4d,$e6), ($b0,$4d,$43,$ef), ($a7,$50,$51,$f4), ($aa,$5b,$5f,$fd), 
    ($89,$6a,$75,$c2), ($84,$61,$7b,$cb), ($93,$7c,$69,$d0), ($9e,$77,$67,$d9), 
    ($d5,$1e,$3d,$ae), ($d8,$15,$33,$a7), ($cf,$08,$21,$bc), ($c2,$03,$2f,$b5),
    ($e1,$32,$05,$8a), ($ec,$39,$0b,$83), ($fb,$24,$19,$98), ($f6,$2f,$17,$91),
    ($d6,$8d,$76,$4d), ($db,$86,$78,$44), ($cc,$9b,$6a,$5f), ($c1,$90,$64,$56),
    ($e2,$a1,$4e,$69), ($ef,$aa,$40,$60), ($f8,$b7,$52,$7b), ($f5,$bc,$5c,$72),
    ($be,$d5,$06,$05), ($b3,$de,$08,$0c), ($a4,$c3,$1a,$17), ($a9,$c8,$14,$1e),
    ($8a,$f9,$3e,$21), ($87,$f2,$30,$28), ($90,$ef,$22,$33), ($9d,$e4,$2c,$3a), 
    ($06,$3d,$96,$dd), ($0b,$36,$98,$d4), ($1c,$2b,$8a,$cf), ($11,$20,$84,$c6),
    ($32,$11,$ae,$f9), ($3f,$1a,$a0,$f0), ($28,$07,$b2,$eb), ($25,$0c,$bc,$e2),
    ($6e,$65,$e6,$95), ($63,$6e,$e8,$9c), ($74,$73,$fa,$87), ($79,$78,$f4,$8e), 
    ($5a,$49,$de,$b1), ($57,$42,$d0,$b8), ($40,$5f,$c2,$a3), ($4d,$54,$cc,$aa),
    ($da,$f7,$41,$ec), ($d7,$fc,$4f,$e5), ($c0,$e1,$5d,$fe), ($cd,$ea,$53,$f7), 
    ($ee,$db,$79,$c8), ($e3,$d0,$77,$c1), ($f4,$cd,$65,$da), ($f9,$c6,$6b,$d3),
    ($b2,$af,$31,$a4), ($bf,$a4,$3f,$ad), ($a8,$b9,$2d,$b6), ($a5,$b2,$23,$bf), 
    ($86,$83,$09,$80), ($8b,$88,$07,$89), ($9c,$95,$15,$92), ($91,$9e,$1b,$9b), 
    ($0a,$47,$a1,$7c), ($07,$4c,$af,$75), ($10,$51,$bd,$6e), ($1d,$5a,$b3,$67), 
    ($3e,$6b,$99,$58), ($33,$60,$97,$51), ($24,$7d,$85,$4a), ($29,$76,$8b,$43), 
    ($62,$1f,$d1,$34), ($6f,$14,$df,$3d), ($78,$09,$cd,$26), ($75,$02,$c3,$2f), 
    ($56,$33,$e9,$10), ($5b,$38,$e7,$19), ($4c,$25,$f5,$02), ($41,$2e,$fb,$0b),
    ($61,$8c,$9a,$d7), ($6c,$87,$94,$de), ($7b,$9a,$86,$c5), ($76,$91,$88,$cc),
    ($55,$a0,$a2,$f3), ($58,$ab,$ac,$fa), ($4f,$b6,$be,$e1), ($42,$bd,$b0,$e8),
    ($09,$d4,$ea,$9f), ($04,$df,$e4,$96), ($13,$c2,$f6,$8d), ($1e,$c9,$f8,$84),
    ($3d,$f8,$d2,$bb), ($30,$f3,$dc,$b2), ($27,$ee,$ce,$a9), ($2a,$e5,$c0,$a0), 
    ($b1,$3c,$7a,$47), ($bc,$37,$74,$4e), ($ab,$2a,$66,$55), ($a6,$21,$68,$5c), 
    ($85,$10,$42,$63), ($88,$1b,$4c,$6a), ($9f,$06,$5e,$71), ($92,$0d,$50,$78),
    ($d9,$64,$0a,$0f), ($d4,$6f,$04,$06), ($c3,$72,$16,$1d), ($ce,$79,$18,$14),
    ($ed,$48,$32,$2b), ($e0,$43,$3c,$22), ($f7,$5e,$2e,$39), ($fa,$55,$20,$30),
    ($b7,$01,$ec,$9a), ($ba,$0a,$e2,$93), ($ad,$17,$f0,$88), ($a0,$1c,$fe,$81),
    ($83,$2d,$d4,$be), ($8e,$26,$da,$b7), ($99,$3b,$c8,$ac), ($94,$30,$c6,$a5),
    ($df,$59,$9c,$d2), ($d2,$52,$92,$db), ($c5,$4f,$80,$c0), ($c8,$44,$8e,$c9),
    ($eb,$75,$a4,$f6), ($e6,$7e,$aa,$ff), ($f1,$63,$b8,$e4), ($fc,$68,$b6,$ed),
    ($67,$b1,$0c,$0a), ($6a,$ba,$02,$03), ($7d,$a7,$10,$18), ($70,$ac,$1e,$11),
    ($53,$9d,$34,$2e), ($5e,$96,$3a,$27), ($49,$8b,$28,$3c), ($44,$80,$26,$35),
    ($0f,$e9,$7c,$42), ($02,$e2,$72,$4b), ($15,$ff,$60,$50), ($18,$f4,$6e,$59),
    ($3b,$c5,$44,$66), ($36,$ce,$4a,$6f), ($21,$d3,$58,$74), ($2c,$d8,$56,$7d),
    ($0c,$7a,$37,$a1), ($01,$71,$39,$a8), ($16,$6c,$2b,$b3), ($1b,$67,$25,$ba),
    ($38,$56,$0f,$85), ($35,$5d,$01,$8c), ($22,$40,$13,$97), ($2f,$4b,$1d,$9e),
    ($64,$22,$47,$e9), ($69,$29,$49,$e0), ($7e,$34,$5b,$fb), ($73,$3f,$55,$f2),
    ($50,$0e,$7f,$cd), ($5d,$05,$71,$c4), ($4a,$18,$63,$df), ($47,$13,$6d,$d6),
    ($dc,$ca,$d7,$31), ($d1,$c1,$d9,$38), ($c6,$dc,$cb,$23), ($cb,$d7,$c5,$2a),
    ($e8,$e6,$ef,$15), ($e5,$ed,$e1,$1c), ($f2,$f0,$f3,$07), ($ff,$fb,$fd,$0e),
    ($b4,$92,$a7,$79), ($b9,$99,$a9,$70), ($ae,$84,$bb,$6b), ($a3,$8f,$b5,$62),
    ($80,$be,$9f,$5d), ($8d,$b5,$91,$54), ($9a,$a8,$83,$4f), ($97,$a3,$8d,$46));
  U4: array[0..255,0..3] of byte= (
    ($00,$00,$00,$00), ($09,$0d,$0b,$0e), ($12,$1a,$16,$1c), ($1b,$17,$1d,$12),
    ($24,$34,$2c,$38), ($2d,$39,$27,$36), ($36,$2e,$3a,$24), ($3f,$23,$31,$2a),
    ($48,$68,$58,$70), ($41,$65,$53,$7e), ($5a,$72,$4e,$6c), ($53,$7f,$45,$62),
    ($6c,$5c,$74,$48), ($65,$51,$7f,$46), ($7e,$46,$62,$54), ($77,$4b,$69,$5a),
    ($90,$d0,$b0,$e0), ($99,$dd,$bb,$ee), ($82,$ca,$a6,$fc), ($8b,$c7,$ad,$f2),
    ($b4,$e4,$9c,$d8), ($bd,$e9,$97,$d6), ($a6,$fe,$8a,$c4), ($af,$f3,$81,$ca),
    ($d8,$b8,$e8,$90), ($d1,$b5,$e3,$9e), ($ca,$a2,$fe,$8c), ($c3,$af,$f5,$82),
    ($fc,$8c,$c4,$a8), ($f5,$81,$cf,$a6), ($ee,$96,$d2,$b4), ($e7,$9b,$d9,$ba),
    ($3b,$bb,$7b,$db), ($32,$b6,$70,$d5), ($29,$a1,$6d,$c7), ($20,$ac,$66,$c9),
    ($1f,$8f,$57,$e3), ($16,$82,$5c,$ed), ($0d,$95,$41,$ff), ($04,$98,$4a,$f1), 
    ($73,$d3,$23,$ab), ($7a,$de,$28,$a5), ($61,$c9,$35,$b7), ($68,$c4,$3e,$b9), 
    ($57,$e7,$0f,$93), ($5e,$ea,$04,$9d), ($45,$fd,$19,$8f), ($4c,$f0,$12,$81),
    ($ab,$6b,$cb,$3b), ($a2,$66,$c0,$35), ($b9,$71,$dd,$27), ($b0,$7c,$d6,$29),
    ($8f,$5f,$e7,$03), ($86,$52,$ec,$0d), ($9d,$45,$f1,$1f), ($94,$48,$fa,$11), 
    ($e3,$03,$93,$4b), ($ea,$0e,$98,$45), ($f1,$19,$85,$57), ($f8,$14,$8e,$59), 
    ($c7,$37,$bf,$73), ($ce,$3a,$b4,$7d), ($d5,$2d,$a9,$6f), ($dc,$20,$a2,$61),
    ($76,$6d,$f6,$ad), ($7f,$60,$fd,$a3), ($64,$77,$e0,$b1), ($6d,$7a,$eb,$bf), 
    ($52,$59,$da,$95), ($5b,$54,$d1,$9b), ($40,$43,$cc,$89), ($49,$4e,$c7,$87),
    ($3e,$05,$ae,$dd), ($37,$08,$a5,$d3), ($2c,$1f,$b8,$c1), ($25,$12,$b3,$cf),
    ($1a,$31,$82,$e5), ($13,$3c,$89,$eb), ($08,$2b,$94,$f9), ($01,$26,$9f,$f7),
    ($e6,$bd,$46,$4d), ($ef,$b0,$4d,$43), ($f4,$a7,$50,$51), ($fd,$aa,$5b,$5f),
    ($c2,$89,$6a,$75), ($cb,$84,$61,$7b), ($d0,$93,$7c,$69), ($d9,$9e,$77,$67),
    ($ae,$d5,$1e,$3d), ($a7,$d8,$15,$33), ($bc,$cf,$08,$21), ($b5,$c2,$03,$2f), 
    ($8a,$e1,$32,$05), ($83,$ec,$39,$0b), ($98,$fb,$24,$19), ($91,$f6,$2f,$17),
    ($4d,$d6,$8d,$76), ($44,$db,$86,$78), ($5f,$cc,$9b,$6a), ($56,$c1,$90,$64), 
    ($69,$e2,$a1,$4e), ($60,$ef,$aa,$40), ($7b,$f8,$b7,$52), ($72,$f5,$bc,$5c), 
    ($05,$be,$d5,$06), ($0c,$b3,$de,$08), ($17,$a4,$c3,$1a), ($1e,$a9,$c8,$14), 
    ($21,$8a,$f9,$3e), ($28,$87,$f2,$30), ($33,$90,$ef,$22), ($3a,$9d,$e4,$2c), 
    ($dd,$06,$3d,$96), ($d4,$0b,$36,$98), ($cf,$1c,$2b,$8a), ($c6,$11,$20,$84),
    ($f9,$32,$11,$ae), ($f0,$3f,$1a,$a0), ($eb,$28,$07,$b2), ($e2,$25,$0c,$bc), 
    ($95,$6e,$65,$e6), ($9c,$63,$6e,$e8), ($87,$74,$73,$fa), ($8e,$79,$78,$f4), 
    ($b1,$5a,$49,$de), ($b8,$57,$42,$d0), ($a3,$40,$5f,$c2), ($aa,$4d,$54,$cc),
    ($ec,$da,$f7,$41), ($e5,$d7,$fc,$4f), ($fe,$c0,$e1,$5d), ($f7,$cd,$ea,$53), 
    ($c8,$ee,$db,$79), ($c1,$e3,$d0,$77), ($da,$f4,$cd,$65), ($d3,$f9,$c6,$6b), 
    ($a4,$b2,$af,$31), ($ad,$bf,$a4,$3f), ($b6,$a8,$b9,$2d), ($bf,$a5,$b2,$23), 
    ($80,$86,$83,$09), ($89,$8b,$88,$07), ($92,$9c,$95,$15), ($9b,$91,$9e,$1b),
    ($7c,$0a,$47,$a1), ($75,$07,$4c,$af), ($6e,$10,$51,$bd), ($67,$1d,$5a,$b3), 
    ($58,$3e,$6b,$99), ($51,$33,$60,$97), ($4a,$24,$7d,$85), ($43,$29,$76,$8b),
    ($34,$62,$1f,$d1), ($3d,$6f,$14,$df), ($26,$78,$09,$cd), ($2f,$75,$02,$c3), 
    ($10,$56,$33,$e9), ($19,$5b,$38,$e7), ($02,$4c,$25,$f5), ($0b,$41,$2e,$fb), 
    ($d7,$61,$8c,$9a), ($de,$6c,$87,$94), ($c5,$7b,$9a,$86), ($cc,$76,$91,$88),
    ($f3,$55,$a0,$a2), ($fa,$58,$ab,$ac), ($e1,$4f,$b6,$be), ($e8,$42,$bd,$b0),
    ($9f,$09,$d4,$ea), ($96,$04,$df,$e4), ($8d,$13,$c2,$f6), ($84,$1e,$c9,$f8),
    ($bb,$3d,$f8,$d2), ($b2,$30,$f3,$dc), ($a9,$27,$ee,$ce), ($a0,$2a,$e5,$c0),
    ($47,$b1,$3c,$7a), ($4e,$bc,$37,$74), ($55,$ab,$2a,$66), ($5c,$a6,$21,$68),
    ($63,$85,$10,$42), ($6a,$88,$1b,$4c), ($71,$9f,$06,$5e), ($78,$92,$0d,$50), 
    ($0f,$d9,$64,$0a), ($06,$d4,$6f,$04), ($1d,$c3,$72,$16), ($14,$ce,$79,$18),
    ($2b,$ed,$48,$32), ($22,$e0,$43,$3c), ($39,$f7,$5e,$2e), ($30,$fa,$55,$20),
    ($9a,$b7,$01,$ec), ($93,$ba,$0a,$e2), ($88,$ad,$17,$f0), ($81,$a0,$1c,$fe),
    ($be,$83,$2d,$d4), ($b7,$8e,$26,$da), ($ac,$99,$3b,$c8), ($a5,$94,$30,$c6),
    ($d2,$df,$59,$9c), ($db,$d2,$52,$92), ($c0,$c5,$4f,$80), ($c9,$c8,$44,$8e),
    ($f6,$eb,$75,$a4), ($ff,$e6,$7e,$aa), ($e4,$f1,$63,$b8), ($ed,$fc,$68,$b6),
    ($0a,$67,$b1,$0c), ($03,$6a,$ba,$02), ($18,$7d,$a7,$10), ($11,$70,$ac,$1e),
    ($2e,$53,$9d,$34), ($27,$5e,$96,$3a), ($3c,$49,$8b,$28), ($35,$44,$80,$26),
    ($42,$0f,$e9,$7c), ($4b,$02,$e2,$72), ($50,$15,$ff,$60), ($59,$18,$f4,$6e),
    ($66,$3b,$c5,$44), ($6f,$36,$ce,$4a), ($74,$21,$d3,$58), ($7d,$2c,$d8,$56),
    ($a1,$0c,$7a,$37), ($a8,$01,$71,$39), ($b3,$16,$6c,$2b), ($ba,$1b,$67,$25),
    ($85,$38,$56,$0f), ($8c,$35,$5d,$01), ($97,$22,$40,$13), ($9e,$2f,$4b,$1d),
    ($e9,$64,$22,$47), ($e0,$69,$29,$49), ($fb,$7e,$34,$5b), ($f2,$73,$3f,$55),
    ($cd,$50,$0e,$7f), ($c4,$5d,$05,$71), ($df,$4a,$18,$63), ($d6,$47,$13,$6d),
    ($31,$dc,$ca,$d7), ($38,$d1,$c1,$d9), ($23,$c6,$dc,$cb), ($2a,$cb,$d7,$c5),
    ($15,$e8,$e6,$ef), ($1c,$e5,$ed,$e1), ($07,$f2,$f0,$f3), ($0e,$ff,$fb,$fd),
    ($79,$b4,$92,$a7), ($70,$b9,$99,$a9), ($6b,$ae,$84,$bb), ($62,$a3,$8f,$b5),
    ($5d,$80,$be,$9f), ($54,$8d,$b5,$91), ($4f,$9a,$a8,$83), ($46,$97,$a3,$8d));

  rcon: array[0..29] of cardinal= (
    $01, $02, $04, $08, $10, $20, $40, $80, $1b, $36, $6c, $d8, $ab, $4d, $9a,
    $2f, $5e, $bc, $63, $c6, $97, $35, $6a, $d4, $b3, $7d, $fa, $ef, $c5, $91);

{==============================================================================}
type
  PDWord = ^LongWord;

procedure hperm_op(var a, t: integer; n, m: integer);
begin
  t:= ((a shl (16 - n)) xor a) and m;
  a:= a xor t xor (t shr (16 - n));
end;

procedure perm_op(var a, b, t: integer; n, m: integer);
begin
  t:= ((a shr n) xor b) and m;
  b:= b xor t;
  a:= a xor (t shl n);
end;

{==============================================================================}
function TSynaBlockCipher.GetSize: byte;
begin
  Result := 8;
end;

procedure TSynaBlockCipher.IncCounter;
var
  i: integer;
begin
  Inc(CV[GetSize]);
  i:= GetSize -1;
  while (i> 0) and (CV[i + 1] = #0) do
  begin
    Inc(CV[i]);
    Dec(i);
  end;
end;

procedure TSynaBlockCipher.Reset;
begin
  CV := IV;
end;

procedure TSynaBlockCipher.InitKey(Key: AnsiString);
begin
end;

procedure TSynaBlockCipher.SetIV(const Value: AnsiString);
begin
  IV := PadString(Value, GetSize, #0);
  Reset;
end;

function TSynaBlockCipher.GetIV: AnsiString;
begin
  Result := CV;
end;

function TSynaBlockCipher.EncryptECB(const InData: AnsiString): AnsiString;
begin
  Result := InData;
end;

function TSynaBlockCipher.DecryptECB(const InData: AnsiString): AnsiString;
begin
  Result := InData;
end;

function TSynaBlockCipher.EncryptCBC(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s: ansistring;
  l: integer;
  bs: byte;
begin
  Result := '';
  l := Length(InData);
  bs := GetSize;
  for i:= 1 to (l div bs) do
  begin
    s := copy(Indata, (i - 1) * bs + 1, bs);
    s := XorString(s, CV);
    s := EncryptECB(s);
    CV := s;
    Result := Result + s;
  end;
  if (l mod bs)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div bs) * bs + 1, l mod bs);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.DecryptCBC(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s, temp: ansistring;
  l: integer;
  bs: byte;
begin
  Result := '';
  l := Length(InData);
  bs := GetSize;
  for i:= 1 to (l div bs) do
  begin
    s := copy(Indata, (i - 1) * bs + 1, bs);
    temp := s;
    s := DecryptECB(s);
    s := XorString(s, CV);
    Result := Result + s;
    CV := Temp;
  end;
  if (l mod bs)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div bs) * bs + 1, l mod bs);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.EncryptCFB8bit(const Indata: AnsiString): AnsiString;
var
  i: integer;
  Temp: AnsiString;
  c: AnsiChar;
begin
  Result := '';
  for i:= 1 to Length(Indata) do
  begin
    Temp := EncryptECB(CV);
    c := AnsiChar(ord(InData[i]) xor ord(temp[1]));
    Result := Result + c;
    Delete(CV, 1, 1);
    CV := CV + c;
  end;
end;

function TSynaBlockCipher.DecryptCFB8bit(const Indata: AnsiString): AnsiString;
var
  i: integer;
  Temp: AnsiString;
  c: AnsiChar;
begin
  Result := '';
  for i:= 1 to length(Indata) do
  begin
    c:= Indata[i];
    Temp := EncryptECB(CV);
    Result := Result + AnsiChar(ord(InData[i]) xor ord(temp[1]));
    Delete(CV, 1, 1);
    CV := CV + c;
  end;
end;

function TSynaBlockCipher.EncryptCFBblock(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s: AnsiString;
  l: integer;
  bs: byte;
begin
  Result := '';
  l := Length(InData);
  bs := GetSize;
  for i:= 1 to (l div bs) do
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (i - 1) * bs + 1, bs);
    s := XorString(s, CV);
    Result := Result + s;
    CV := s;
  end;
  if (l mod bs)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div bs) * bs + 1, l mod bs);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.DecryptCFBblock(const Indata: AnsiString): AnsiString;
var
  i: integer;
  S, Temp: AnsiString;
  l: integer;
  bs: byte;
begin
  Result := '';
  l := Length(InData);
  bs := GetSize;
  for i:= 1 to (l div bs) do
  begin
    s := copy(Indata, (i - 1) * bs + 1, bs);
    Temp := s;
    CV := EncryptECB(CV);
    s := XorString(s, CV);
    Result := result + s;
    CV := temp;
  end;
  if (l mod bs)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div bs) * bs + 1, l mod bs);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.EncryptOFB(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s: AnsiString;
  l: integer;
  bs: byte;
begin
  Result := '';
  l := Length(InData);
  bs := GetSize;
  for i:= 1 to (l div bs) do
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (i - 1) * bs + 1, bs);
    s := XorString(s, CV);
    Result := Result + s;
  end;
  if (l mod bs)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div bs) * bs + 1, l mod bs);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.DecryptOFB(const Indata: AnsiString): AnsiString;
var
  i: integer;
  s: AnsiString;
  l: integer;
  bs: byte;
begin
  Result := '';
  l := Length(InData);
  bs := GetSize;
  for i:= 1 to (l div bs) do
  begin
    Cv := EncryptECB(CV);
    s := copy(Indata, (i - 1) * bs + 1, bs);
    s := XorString(s, CV);
    Result := Result + s;
  end;
  if (l mod bs)<> 0 then
  begin
    CV := EncryptECB(CV);
    s := copy(Indata, (l div bs) * bs + 1, l mod bs);
    s := XorString(s, CV);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.EncryptCTR(const Indata: AnsiString): AnsiString;
var
  temp: AnsiString;
  i: integer;
  s: AnsiString;
  l: integer;
  bs: byte;
begin
  Result := '';
  l := Length(InData);
  bs := GetSize;
  for i:= 1 to (l div bs) do
  begin
    temp := EncryptECB(CV);
    IncCounter;
    s := copy(Indata, (i - 1) * bs + 1, bs);
    s := XorString(s, temp);
    Result := Result + s;
  end;
  if (l mod bs)<> 0 then
  begin
    temp := EncryptECB(CV);
    IncCounter;
    s := copy(Indata, (l div bs) * bs + 1, l mod bs);
    s := XorString(s, temp);
    Result := Result + s;
  end;
end;

function TSynaBlockCipher.DecryptCTR(const Indata: AnsiString): AnsiString;
var
  temp: AnsiString;
  s: AnsiString;
  i: integer;
  l: integer;
  bs: byte;
begin
  Result := '';
  l := Length(InData);
  bs := GetSize;
  for i:= 1 to (l div bs) do
  begin
    temp := EncryptECB(CV);
    IncCounter;
    s := copy(Indata, (i - 1) * bs + 1, bs);
    s := XorString(s, temp);
    Result := Result + s;
  end;
  if (l mod bs)<> 0 then
  begin
    temp := EncryptECB(CV);
    IncCounter;
    s := copy(Indata, (l div bs) * bs + 1, l mod bs);
    s := XorString(s, temp);
    Result := Result + s;
  end;
end;

constructor TSynaBlockCipher.Create(Key: AnsiString);
begin
  inherited Create;
  InitKey(Key);
  IV := StringOfChar(#0, GetSize);
  IV := EncryptECB(IV);
  Reset;
end;

{==============================================================================}

procedure TSynaCustomDes.DoInit(KeyB: AnsiString; var KeyData: TDesKeyData);
var
  c, d, t, s, t2, i: integer;
begin
  KeyB := PadString(KeyB, 8, #0);
  c:= ord(KeyB[1]) or (ord(KeyB[2]) shl 8) or (ord(KeyB[3]) shl 16) or (ord(KeyB[4]) shl 24);
  d:= ord(KeyB[5]) or (ord(KeyB[6]) shl 8) or (ord(KeyB[7]) shl 16) or (ord(KeyB[8]) shl 24);
  perm_op(d,c,t,4,integer($0f0f0f0f));
  hperm_op(c,t,integer(-2),integer($cccc0000));
  hperm_op(d,t,integer(-2),integer($cccc0000));
  perm_op(d,c,t,1,integer($55555555));
  perm_op(c,d,t,8,integer($00ff00ff));
  perm_op(d,c,t,1,integer($55555555));
  d:= ((d and $ff) shl 16) or (d and $ff00) or ((d and $ff0000) shr 16) or
        ((c and integer($f0000000)) shr 4);
  c:= c and $fffffff;
  for i:= 0 to 15 do
  begin
    if shifts2[i]<> 0 then
    begin
      c:= ((c shr 2) or (c shl 26));
      d:= ((d shr 2) or (d shl 26));
    end
    else
    begin
      c:= ((c shr 1) or (c shl 27));
      d:= ((d shr 1) or (d shl 27));
    end;
    c:= c and $fffffff;
    d:= d and $fffffff;
    s:= des_skb[0,c and $3f] or
        des_skb[1,((c shr  6) and $03) or ((c shr  7) and $3c)] or
        des_skb[2,((c shr 13) and $0f) or ((c shr 14) and $30)] or
        des_skb[3,((c shr 20) and $01) or ((c shr 21) and $06) or ((c shr 22) and $38)];
    t:= des_skb[4,d and $3f] or
        des_skb[5,((d shr  7) and $03) or ((d shr  8) and $3c)] or
        des_skb[6, (d shr 15) and $3f                         ] or
        des_skb[7,((d shr 21) and $0f) or ((d shr 22) and $30)];
    t2:= ((t shl 16) or (s and $ffff));
    KeyData[(i shl 1)]:= ((t2 shl 2) or (t2 shr 30));
    t2:= ((s shr 16) or (t and integer($ffff0000)));
    KeyData[(i shl 1)+1]:= ((t2 shl 6) or (t2 shr 26));
  end;
end;

function TSynaCustomDes.EncryptBlock(const InData: AnsiString; var KeyData: TDesKeyData): AnsiString;
var
  l, r, t, u: integer;
  i: longint;
begin
  r := Swapbytes(DecodeLongint(Indata, 1));
  l := swapbytes(DecodeLongint(Indata, 5));
  t:= ((l shr 4) xor r) and $0f0f0f0f;
  r:= r xor t;
  l:= l xor (t shl 4);
  t:= ((r shr 16) xor l) and $0000ffff;
  l:= l xor t;
  r:= r xor (t shl 16);
  t:= ((l shr 2) xor r) and $33333333;
  r:= r xor t;
  l:= l xor (t shl 2);
  t:= ((r shr 8) xor l) and $00ff00ff;
  l:= l xor t;
  r:= r xor (t shl 8);
  t:= ((l shr 1) xor r) and $55555555;
  r:= r xor t;
  l:= l xor (t shl 1);
  r:= (r shr 29) or (r shl 3);
  l:= (l shr 29) or (l shl 3);
  i:= 0;
  while i< 32 do
  begin
    u:= r xor KeyData[i  ];
    t:= r xor KeyData[i+1];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData[i+2];
    t:= l xor KeyData[i+3];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= r xor KeyData[i+4];
    t:= r xor KeyData[i+5];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData[i+6];
    t:= l xor KeyData[i+7];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    Inc(i,8);
  end;
  r:= (r shr 3) or (r shl 29);
  l:= (l shr 3) or (l shl 29);
  t:= ((r shr 1) xor l) and $55555555;
  l:= l xor t;
  r:= r xor (t shl 1);
  t:= ((l shr 8) xor r) and $00ff00ff;
  r:= r xor t;
  l:= l xor (t shl 8);
  t:= ((r shr 2) xor l) and $33333333;
  l:= l xor t;
  r:= r xor (t shl 2);
  t:= ((l shr 16) xor r) and $0000ffff;
  r:= r xor t;
  l:= l xor (t shl 16);
  t:= ((r shr 4) xor l) and $0f0f0f0f;
  l:= l xor t;
  r:= r xor (t shl 4);
  Result := CodeLongInt(Swapbytes(l)) + CodeLongInt(Swapbytes(r));
end;

function TSynaCustomDes.DecryptBlock(const InData: AnsiString; var KeyData: TDesKeyData): AnsiString;
var
  l, r, t, u: integer;
  i: longint;
begin
  r := Swapbytes(DecodeLongint(Indata, 1));
  l := Swapbytes(DecodeLongint(Indata, 5));
  t:= ((l shr 4) xor r) and $0f0f0f0f;
  r:= r xor t;
  l:= l xor (t shl 4);
  t:= ((r shr 16) xor l) and $0000ffff;
  l:= l xor t;
  r:= r xor (t shl 16);
  t:= ((l shr 2) xor r) and $33333333;
  r:= r xor t;
  l:= l xor (t shl 2);
  t:= ((r shr 8) xor l) and $00ff00ff;
  l:= l xor t;
  r:= r xor (t shl 8);
  t:= ((l shr 1) xor r) and $55555555;
  r:= r xor t;
  l:= l xor (t shl 1);
  r:= (r shr 29) or (r shl 3);
  l:= (l shr 29) or (l shl 3);
  i:= 30;
  while i> 0 do
  begin
    u:= r xor KeyData[i  ];
    t:= r xor KeyData[i+1];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData[i-2];
    t:= l xor KeyData[i-1];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= r xor KeyData[i-4];
    t:= r xor KeyData[i-3];
    t:= (t shr 4) or (t shl 28);
    l:= l xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    u:= l xor KeyData[i-6];
    t:= l xor KeyData[i-5];
    t:= (t shr 4) or (t shl 28);
    r:= r xor des_SPtrans[0,(u shr  2) and $3f] xor
              des_SPtrans[2,(u shr 10) and $3f] xor
              des_SPtrans[4,(u shr 18) and $3f] xor
              des_SPtrans[6,(u shr 26) and $3f] xor
              des_SPtrans[1,(t shr  2) and $3f] xor
              des_SPtrans[3,(t shr 10) and $3f] xor
              des_SPtrans[5,(t shr 18) and $3f] xor
              des_SPtrans[7,(t shr 26) and $3f];
    Dec(i,8);
  end;
  r:= (r shr 3) or (r shl 29);
  l:= (l shr 3) or (l shl 29);
  t:= ((r shr 1) xor l) and $55555555;
  l:= l xor t;
  r:= r xor (t shl 1);
  t:= ((l shr 8) xor r) and $00ff00ff;
  r:= r xor t;
  l:= l xor (t shl 8);
  t:= ((r shr 2) xor l) and $33333333;
  l:= l xor t;
  r:= r xor (t shl 2);
  t:= ((l shr 16) xor r) and $0000ffff;
  r:= r xor t;
  l:= l xor (t shl 16);
  t:= ((r shr 4) xor l) and $0f0f0f0f;
  l:= l xor t;
  r:= r xor (t shl 4);
  Result := CodeLongInt(Swapbytes(l)) + CodeLongInt(Swapbytes(r));
end;

{==============================================================================}

procedure TSynaDes.InitKey(Key: AnsiString);
begin
  Key := PadString(Key, 8, #0);
  DoInit(Key,KeyData);
end;

function TSynaDes.EncryptECB(const InData: AnsiString): AnsiString;
begin
  Result := EncryptBlock(InData,KeyData);
end;

function TSynaDes.DecryptECB(const InData: AnsiString): AnsiString;
begin
  Result := DecryptBlock(Indata,KeyData);
end;

{==============================================================================}

procedure TSyna3Des.InitKey(Key: AnsiString);
var
  Size: integer;
  n: integer;
begin
  Size := length(Key);
  key := PadString(key, 3 * 8, #0);
  DoInit(Copy(key, 1, 8),KeyData[0]);
  DoInit(Copy(key, 9, 8),KeyData[1]);
  if Size > 16 then
    DoInit(Copy(key, 17, 8),KeyData[2])
  else
    for n := 0 to high(KeyData[0]) do
      KeyData[2][n] := Keydata[0][n];
end;

function TSyna3Des.EncryptECB(const InData: AnsiString): AnsiString;
begin
  Result := EncryptBlock(Indata,KeyData[0]);
  Result := DecryptBlock(Result,KeyData[1]);
  Result := EncryptBlock(Result,KeyData[2]);
end;

function TSyna3Des.DecryptECB(const InData: AnsiString): AnsiString;
begin
  Result := DecryptBlock(InData,KeyData[2]);
  Result := EncryptBlock(Result,KeyData[1]);
  Result := DecryptBlock(Result,KeyData[0]);
end;

{==============================================================================}

procedure InvMixColumn(a: PByteArray; BC: byte);
var
  j: longword;
begin
  for j:= 0 to (BC-1) do
    PDWord(@(a^[j*4]))^:= PDWord(@U1[a^[j*4+0]])^
      xor PDWord(@U2[a^[j*4+1]])^
      xor PDWord(@U3[a^[j*4+2]])^
      xor PDWord(@U4[a^[j*4+3]])^;
end;

{==============================================================================}

function TSynaAes.GetSize: byte;
begin
  Result := 16;
end;

procedure TSynaAes.InitKey(Key: AnsiString);
var
  Size: integer;
  KC, ROUNDS, j, r, t, rconpointer: longword;
  tk: array[0..MAXKC-1,0..3] of byte;
  n: integer;
begin
  FillChar(tk,Sizeof(tk),0);
  //key must have at least 128 bits and max 256 bits
  if length(key) < 16 then
    key := PadString(key, 16, #0);
  if length(key) > 32 then
    delete(key, 33, maxint);
  Size := length(Key);
  Move(PAnsiChar(Key)^, tk, Size);
  if Size<= 16 then
  begin
    KC:= 4;
    Rounds:= 10;
  end
  else if Size<= 24 then
  begin
    KC:= 6;
    Rounds:= 12;
  end
  else
  begin
    KC:= 8;
    Rounds:= 14;
  end;
  numrounds:= rounds;
  r:= 0;
  t:= 0;
  j:= 0;
  while (j< KC) and (r< (rounds+1)) do
  begin
    while (j< KC) and (t< BC) do
    begin
      rk[r,t]:= PDWord(@tk[j])^;
      Inc(j);
      Inc(t);
    end;
    if t= BC then
    begin
      t:= 0;
      Inc(r);
    end;
  end;
  rconpointer:= 0;
  while (r< (rounds+1)) do
  begin
    tk[0,0]:= tk[0,0] xor S[tk[KC-1,1]];
    tk[0,1]:= tk[0,1] xor S[tk[KC-1,2]];
    tk[0,2]:= tk[0,2] xor S[tk[KC-1,3]];
    tk[0,3]:= tk[0,3] xor S[tk[KC-1,0]];
    tk[0,0]:= tk[0,0] xor rcon[rconpointer];
    Inc(rconpointer);
    if KC<> 8 then
    begin
      for j:= 1 to (KC-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end
    else
    begin
      for j:= 1 to ((KC div 2)-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
      tk[KC div 2,0]:= tk[KC div 2,0] xor S[tk[KC div 2 - 1,0]];
      tk[KC div 2,1]:= tk[KC div 2,1] xor S[tk[KC div 2 - 1,1]];
      tk[KC div 2,2]:= tk[KC div 2,2] xor S[tk[KC div 2 - 1,2]];
      tk[KC div 2,3]:= tk[KC div 2,3] xor S[tk[KC div 2 - 1,3]];
      for j:= ((KC div 2) + 1) to (KC-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end;
    j:= 0;
    while (j< KC) and (r< (rounds+1)) do
    begin
      while (j< KC) and (t< BC) do
      begin
        rk[r,t]:= PDWord(@tk[j])^;
        Inc(j);
        Inc(t);
      end;
      if t= BC then
      begin
        Inc(r);
        t:= 0;
      end;
    end;
  end;
  Move(rk,drk,Sizeof(rk));
  for r:= 1 to (numrounds-1) do
    InvMixColumn(@drk[r],BC);
end;

function TSynaAes.EncryptECB(const InData: AnsiString): AnsiString;
var
  r: longword;
  tempb: array[0..MAXBC-1,0..3] of byte;
  a: array[0..MAXBC,0..3] of byte;
  p: pointer;
begin
  p := @a[0,0];
  move(pointer(InData)^, p^, 16);
  for r:= 0 to (numrounds-2) do
  begin
    PDWord(@tempb[0])^:= PDWord(@a[0])^ xor rk[r,0];
    PDWord(@tempb[1])^:= PDWord(@a[1])^ xor rk[r,1];
    PDWord(@tempb[2])^:= PDWord(@a[2])^ xor rk[r,2];
    PDWord(@tempb[3])^:= PDWord(@a[3])^ xor rk[r,3];
    PDWord(@a[0])^:= PDWord(@T1[tempb[0,0]])^ xor
                     PDWord(@T2[tempb[1,1]])^ xor
                     PDWord(@T3[tempb[2,2]])^ xor
                     PDWord(@T4[tempb[3,3]])^;
    PDWord(@a[1])^:= PDWord(@T1[tempb[1,0]])^ xor
                     PDWord(@T2[tempb[2,1]])^ xor
                     PDWord(@T3[tempb[3,2]])^ xor
                     PDWord(@T4[tempb[0,3]])^;
    PDWord(@a[2])^:= PDWord(@T1[tempb[2,0]])^ xor
                     PDWord(@T2[tempb[3,1]])^ xor
                     PDWord(@T3[tempb[0,2]])^ xor
                     PDWord(@T4[tempb[1,3]])^;
    PDWord(@a[3])^:= PDWord(@T1[tempb[3,0]])^ xor
                     PDWord(@T2[tempb[0,1]])^ xor
                     PDWord(@T3[tempb[1,2]])^ xor
                     PDWord(@T4[tempb[2,3]])^;
  end;
  PDWord(@tempb[0])^:= PDWord(@a[0])^ xor rk[numrounds-1,0];
  PDWord(@tempb[1])^:= PDWord(@a[1])^ xor rk[numrounds-1,1];
  PDWord(@tempb[2])^:= PDWord(@a[2])^ xor rk[numrounds-1,2];
  PDWord(@tempb[3])^:= PDWord(@a[3])^ xor rk[numrounds-1,3];
  a[0,0]:= T1[tempb[0,0],1];
  a[0,1]:= T1[tempb[1,1],1];
  a[0,2]:= T1[tempb[2,2],1];
  a[0,3]:= T1[tempb[3,3],1];
  a[1,0]:= T1[tempb[1,0],1];
  a[1,1]:= T1[tempb[2,1],1];
  a[1,2]:= T1[tempb[3,2],1];
  a[1,3]:= T1[tempb[0,3],1];
  a[2,0]:= T1[tempb[2,0],1];
  a[2,1]:= T1[tempb[3,1],1];
  a[2,2]:= T1[tempb[0,2],1];
  a[2,3]:= T1[tempb[1,3],1];
  a[3,0]:= T1[tempb[3,0],1];
  a[3,1]:= T1[tempb[0,1],1];
  a[3,2]:= T1[tempb[1,2],1];
  a[3,3]:= T1[tempb[2,3],1];
  PDWord(@a[0])^:= PDWord(@a[0])^ xor rk[numrounds,0];
  PDWord(@a[1])^:= PDWord(@a[1])^ xor rk[numrounds,1];
  PDWord(@a[2])^:= PDWord(@a[2])^ xor rk[numrounds,2];
  PDWord(@a[3])^:= PDWord(@a[3])^ xor rk[numrounds,3];

  Result := StringOfChar(#0, 16);
  move(p^, pointer(Result)^, 16);
end;

function TSynaAes.DecryptECB(const InData: AnsiString): AnsiString;
var
  r: longword;
  tempb: array[0..MAXBC-1,0..3] of byte;
  a: array[0..MAXBC,0..3] of byte;
  p: pointer;
begin
  p := @a[0,0];
  move(pointer(InData)^, p^, 16);
  for r:= NumRounds downto 2 do
  begin
    PDWord(@tempb[0])^:= PDWord(@a[0])^ xor drk[r,0];
    PDWord(@tempb[1])^:= PDWord(@a[1])^ xor drk[r,1];
    PDWord(@tempb[2])^:= PDWord(@a[2])^ xor drk[r,2];
    PDWord(@tempb[3])^:= PDWord(@a[3])^ xor drk[r,3];
    PDWord(@a[0])^:= PDWord(@T5[tempb[0,0]])^ xor
                     PDWord(@T6[tempb[3,1]])^ xor
                     PDWord(@T7[tempb[2,2]])^ xor
                     PDWord(@T8[tempb[1,3]])^;
    PDWord(@a[1])^:= PDWord(@T5[tempb[1,0]])^ xor
                     PDWord(@T6[tempb[0,1]])^ xor
                     PDWord(@T7[tempb[3,2]])^ xor
                     PDWord(@T8[tempb[2,3]])^;
    PDWord(@a[2])^:= PDWord(@T5[tempb[2,0]])^ xor
                     PDWord(@T6[tempb[1,1]])^ xor
                     PDWord(@T7[tempb[0,2]])^ xor
                     PDWord(@T8[tempb[3,3]])^;
    PDWord(@a[3])^:= PDWord(@T5[tempb[3,0]])^ xor
                     PDWord(@T6[tempb[2,1]])^ xor
                     PDWord(@T7[tempb[1,2]])^ xor
                     PDWord(@T8[tempb[0,3]])^;
  end;
  PDWord(@tempb[0])^:= PDWord(@a[0])^ xor drk[1,0];
  PDWord(@tempb[1])^:= PDWord(@a[1])^ xor drk[1,1];
  PDWord(@tempb[2])^:= PDWord(@a[2])^ xor drk[1,2];
  PDWord(@tempb[3])^:= PDWord(@a[3])^ xor drk[1,3];
  a[0,0]:= S5[tempb[0,0]];
  a[0,1]:= S5[tempb[3,1]];
  a[0,2]:= S5[tempb[2,2]];
  a[0,3]:= S5[tempb[1,3]];
  a[1,0]:= S5[tempb[1,0]];
  a[1,1]:= S5[tempb[0,1]];
  a[1,2]:= S5[tempb[3,2]];
  a[1,3]:= S5[tempb[2,3]];
  a[2,0]:= S5[tempb[2,0]];
  a[2,1]:= S5[tempb[1,1]];
  a[2,2]:= S5[tempb[0,2]];
  a[2,3]:= S5[tempb[3,3]];
  a[3,0]:= S5[tempb[3,0]];
  a[3,1]:= S5[tempb[2,1]];
  a[3,2]:= S5[tempb[1,2]];
  a[3,3]:= S5[tempb[0,3]];
  PDWord(@a[0])^:= PDWord(@a[0])^ xor drk[0,0];
  PDWord(@a[1])^:= PDWord(@a[1])^ xor drk[0,1];
  PDWord(@a[2])^:= PDWord(@a[2])^ xor drk[0,2];
  PDWord(@a[3])^:= PDWord(@a[3])^ xor drk[0,3];
  Result := StringOfChar(#0, 16);
  move(p^, pointer(Result)^, 16);
end;

{==============================================================================}

function TestDes: boolean;
var
  des: TSynaDes;
  s, t: string;
const
  key = '01234567';
  data1= '01234567';
  data2= '0123456789abcdefghij';
begin
  //ECB
  des := TSynaDes.Create(key);
  try
    s := des.EncryptECB(data1);
    t := strtohex(s);
    result := t = 'c50ad028c6da9800';
    s := des.DecryptECB(s);
    result := result and (data1 = s);
  finally
    des.free;
  end;
  //CBC
  des := TSynaDes.Create(key);
  try
    s := des.EncryptCBC(data2);
    t := strtohex(s);
    result := result and (t = 'eec50f6353115ad6dee90a22ed1b6a88a0926e35');
    des.Reset;
    s := des.DecryptCBC(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CFB-8bit
  des := TSynaDes.Create(key);
  try
    s := des.EncryptCFB8bit(data2);
    t := strtohex(s);
    result := result and (t = 'eb6aa12c2f0ff634b4dfb6da6cb2af8f9c5c1452');
    des.Reset;
    s := des.DecryptCFB8bit(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CFB-block
  des := TSynaDes.Create(key);
  try
    s := des.EncryptCFBblock(data2);
    t := strtohex(s);
    result := result and (t = 'ebdbbaa7f9286cdec28605e07f9b7f3be1053257');
    des.Reset;
    s := des.DecryptCFBblock(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //OFB
  des := TSynaDes.Create(key);
  try
    s := des.EncryptOFB(data2);
    t := strtohex(s);
    result := result and (t = 'ebdbbaa7f9286cdee0b8b3798c4c34baac87dbdc');
    des.Reset;
    s := des.DecryptOFB(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CTR
  des := TSynaDes.Create(key);
  try
    s := des.EncryptCTR(data2);
    t := strtohex(s);
    result := result and (t = 'ebdbbaa7f9286cde0dd20b45f3afd9aa1b91b87e');
    des.Reset;
    s := des.DecryptCTR(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
end;

function Test3Des: boolean;
var
  des: TSyna3Des;
  s, t: string;
const
  key = '0123456789abcdefghijklmn';
  data1= '01234567';
  data2= '0123456789abcdefghij';
begin
  //ECB
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptECB(data1);
    t := strtohex(s);
    result := t = 'e0dee91008dc460c';
    s := des.DecryptECB(s);
    result := result and (data1 = s);
  finally
    des.free;
  end;
  //CBC
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptCBC(data2);
    t := strtohex(s);
    result := result and (t = 'ee844a2a4f49c01b91a1599b8eba29128c1ad87a');
    des.Reset;
    s := des.DecryptCBC(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CFB-8bit
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptCFB8bit(data2);
    t := strtohex(s);
    result := result and (t = '935bbf5210c32cfa1faf61f91e8dc02dfa0ff1e8');
    des.Reset;
    s := des.DecryptCFB8bit(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CFB-block
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptCFBblock(data2);
    t := strtohex(s);
    result := result and (t = '93754e3d54828fbf4bd81f1739419e8d2cfe1671');
    des.Reset;
    s := des.DecryptCFBblock(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //OFB
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptOFB(data2);
    t := strtohex(s);
    result := result and (t = '93754e3d54828fbf04ef0a5efc926ebdf2d95f20');
    des.Reset;
    s := des.DecryptOFB(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
  //CTR
  des := TSyna3Des.Create(key);
  try
    s := des.EncryptCTR(data2);
    t := strtohex(s);
    result := result and (t = '93754e3d54828fbf1c51a121d2c93f989e70b3ad');
    des.Reset;
    s := des.DecryptCTR(s);
    result := result and (data2 = s);
  finally
    des.free;
  end;
end;

function TestAes: boolean;
var
  aes: TSynaAes;
  s, t: string;
const
  key1 = #$00#$01#$02#$03#$05#$06#$07#$08#$0A#$0B#$0C#$0D#$0F#$10#$11#$12;
  data1= #$50#$68#$12#$A4#$5F#$08#$C8#$89#$B9#$7F#$59#$80#$03#$8B#$83#$59;
  key2 = #$A0#$A1#$A2#$A3#$A5#$A6#$A7#$A8#$AA#$AB#$AC#$AD#$AF#$B0#$B1#$B2#$B4#$B5#$B6#$B7#$B9#$BA#$BB#$BC;
  data2= #$4F#$1C#$76#$9D#$1E#$5B#$05#$52#$C7#$EC#$A8#$4D#$EA#$26#$A5#$49;
  key3 = #$00#$01#$02#$03#$05#$06#$07#$08#$0A#$0B#$0C#$0D#$0F#$10#$11#$12#$14#$15#$16#$17#$19#$1A#$1B#$1C#$1E#$1F#$20#$21#$23#$24#$25#$26;
  data3= #$5E#$25#$CA#$78#$F0#$DE#$55#$80#$25#$24#$D3#$8D#$A3#$FE#$44#$56;
begin
  //ECB
  aes := TSynaAes.Create(key1);
  try
    t := aes.EncryptECB(data1);
    result := t = #$D8#$F5#$32#$53#$82#$89#$EF#$7D#$06#$B5#$06#$A4#$FD#$5B#$E9#$C9;
    s := aes.DecryptECB(t);
    result := result and (data1 = s);
  finally
    aes.free;
  end;
  aes := TSynaAes.Create(key2);
  try
    t := aes.EncryptECB(data2);
    result := result and (t = #$F3#$84#$72#$10#$D5#$39#$1E#$23#$60#$60#$8E#$5A#$CB#$56#$05#$81);
    s := aes.DecryptECB(t);
    result := result and (data2 = s);
  finally
    aes.free;
  end;
  aes := TSynaAes.Create(key3);
  try
    t := aes.EncryptECB(data3);
    result := result and (t = #$E8#$B7#$2B#$4E#$8B#$E2#$43#$43#$8C#$9F#$FF#$1F#$0E#$20#$58#$72);
    s := aes.DecryptECB(t);
    result := result and (data3 = s);
  finally
    aes.free;
  end;
end;

{==============================================================================}

end.
