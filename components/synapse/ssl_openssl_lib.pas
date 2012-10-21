{==============================================================================|
| Project : Ararat Synapse                                       | 003.007.001 |
|==============================================================================|
| Content: SSL support by OpenSSL                                              |
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
| Portions created by Lukas Gebauer are Copyright (c)2002-2012.                |
| Portions created by Petr Fejfar are Copyright (c)2011-2012.                  |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{
Special thanks to Gregor Ibic <gregor.ibic@intelicom.si>
 (Intelicom d.o.o., http://www.intelicom.si)
 for good inspiration about begin with SSL programming.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
  (*$HPPEMIT 'namespace ssl_openssl_lib { using System::Shortint; }' *)
{$ENDIF}

//old Delphi does not have MSWINDOWS define.
{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{:@abstract(OpenSSL support)

This unit is Pascal interface to OpenSSL library (used by @link(ssl_openssl) unit).
OpenSSL is loaded dynamicly on-demand. If this library is not found in system,
requested OpenSSL function just return errorcode.
}
unit ssl_openssl_lib;

interface

uses
{$IFDEF CIL}
  System.Runtime.InteropServices,
  System.Text,
{$ENDIF}
  Classes,
  synafpc,
{$IFNDEF MSWINDOWS}
  {$IFDEF FPC}
  BaseUnix, SysUtils;
  {$ELSE}
   Libc, SysUtils;
  {$ENDIF}
{$ELSE}
  Windows;
{$ENDIF}


{$IFDEF CIL}
const
  {$IFDEF LINUX}
  DLLSSLName = 'libssl.so';
  DLLUtilName = 'libcrypto.so';
  {$ELSE}
  DLLSSLName = 'ssleay32.dll';
  DLLUtilName = 'libeay32.dll';
  {$ENDIF}
{$ELSE}
var
  {$IFNDEF MSWINDOWS}
    {$IFDEF DARWIN}
    DLLSSLName: string = 'libssl.dylib';
    DLLUtilName: string = 'libcrypto.dylib';
    {$ELSE}
    DLLSSLName: string = 'libssl.so';
    DLLUtilName: string = 'libcrypto.so';
    {$ENDIF}
  {$ELSE}
  DLLSSLName: string = 'ssleay32.dll';
  DLLSSLName2: string = 'libssl32.dll';
  DLLUtilName: string = 'libeay32.dll';
  {$ENDIF}
{$ENDIF}

type
{$IFDEF CIL}
  SslPtr = IntPtr;
{$ELSE}
  SslPtr = Pointer;
{$ENDIF}
  PSslPtr = ^SslPtr;
  PSSL_CTX = SslPtr;
  PSSL = SslPtr;
  PSSL_METHOD = SslPtr;
  PX509 = SslPtr;
  PX509_NAME = SslPtr;
  PEVP_MD	= SslPtr;
  PInteger = ^Integer;
  PBIO_METHOD = SslPtr;
  PBIO = SslPtr;
  EVP_PKEY = SslPtr;
  PRSA = SslPtr;
  PASN1_UTCTIME = SslPtr;
  PASN1_INTEGER = SslPtr;
  PPasswdCb = SslPtr;
  PFunction = procedure;
  PSTACK = SslPtr; {pf}
  TSkPopFreeFunc = procedure(p:SslPtr); cdecl; {pf}
  TX509Free = procedure(x: PX509); cdecl; {pf}

  DES_cblock = array[0..7] of Byte;
  PDES_cblock = ^DES_cblock;
  des_ks_struct = packed record
    ks: DES_cblock;
    weak_key: Integer;
  end;
  des_key_schedule = array[1..16] of des_ks_struct;

const
  EVP_MAX_MD_SIZE = 16 + 20;

  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5; //look at error stack/return value/errno
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;

  SSL_OP_NO_SSLv2 = $01000000;
  SSL_OP_NO_SSLv3 = $02000000;
  SSL_OP_NO_TLSv1 = $04000000;
  SSL_OP_ALL = $000FFFFF;
  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;

  OPENSSL_DES_DECRYPT = 0;
  OPENSSL_DES_ENCRYPT = 1;

  X509_V_OK =	0;
  X509_V_ILLEGAL = 1;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT = 2;
  X509_V_ERR_UNABLE_TO_GET_CRL = 3;
  X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE = 4;
  X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE = 5;
  X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY = 6;
  X509_V_ERR_CERT_SIGNATURE_FAILURE = 7;
  X509_V_ERR_CRL_SIGNATURE_FAILURE = 8;
  X509_V_ERR_CERT_NOT_YET_VALID = 9;
  X509_V_ERR_CERT_HAS_EXPIRED = 10;
  X509_V_ERR_CRL_NOT_YET_VALID = 11;
  X509_V_ERR_CRL_HAS_EXPIRED = 12;
  X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD = 13;
  X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD = 14;
  X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD = 15;
  X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD = 16;
  X509_V_ERR_OUT_OF_MEM = 17;
  X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT = 18;
  X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN = 19;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY = 20;
  X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE = 21;
  X509_V_ERR_CERT_CHAIN_TOO_LONG = 22;
  X509_V_ERR_CERT_REVOKED = 23;
  X509_V_ERR_INVALID_CA = 24;
  X509_V_ERR_PATH_LENGTH_EXCEEDED = 25;
  X509_V_ERR_INVALID_PURPOSE = 26;
  X509_V_ERR_CERT_UNTRUSTED = 27;
  X509_V_ERR_CERT_REJECTED = 28;
  //These are 'informational' when looking for issuer cert
  X509_V_ERR_SUBJECT_ISSUER_MISMATCH = 29;
  X509_V_ERR_AKID_SKID_MISMATCH = 30;
  X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH = 31;
  X509_V_ERR_KEYUSAGE_NO_CERTSIGN = 32;
  X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER = 33;
  X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION = 34;
  //The application is not happy
  X509_V_ERR_APPLICATION_VERIFICATION = 50;

  SSL_FILETYPE_ASN1	= 2;
  SSL_FILETYPE_PEM = 1;
  EVP_PKEY_RSA = 6;

  SSL_CTRL_SET_TLSEXT_HOSTNAME = 55;
  TLSEXT_NAMETYPE_host_name = 0;

var
  SSLLibHandle: TLibHandle = 0;
  SSLUtilHandle: TLibHandle = 0;
  SSLLibFile: string = '';
  SSLUtilFile: string = '';

{$IFDEF CIL}
  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_get_error')]
    function SslGetError(s: PSSL; ret_code: Integer): Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_library_init')]
    function SslLibraryInit: Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_load_error_strings')]
    procedure SslLoadErrorStrings; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_set_cipher_list')]
    function SslCtxSetCipherList(arg0: PSSL_CTX; var str: String): Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_new')]
    function SslCtxNew(meth: PSSL_METHOD):PSSL_CTX;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_free')]
    procedure SslCtxFree (arg0: PSSL_CTX);   external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_set_fd')]
    function SslSetFd(s: PSSL; fd: Integer):Integer;    external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSLv2_method')]
    function SslMethodV2 : PSSL_METHOD; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSLv3_method')]
    function SslMethodV3 : PSSL_METHOD;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'TLSv1_method')]
    function SslMethodTLSV1:PSSL_METHOD;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSLv23_method')]
    function SslMethodV23 : PSSL_METHOD; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_use_PrivateKey')]
    function SslCtxUsePrivateKey(ctx: PSSL_CTX; pkey: SslPtr):Integer;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_use_PrivateKey_ASN1')]
    function SslCtxUsePrivateKeyASN1(pk: integer; ctx: PSSL_CTX; d: String; len: integer):Integer;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_use_RSAPrivateKey_file')]
    function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: String; _type: Integer):Integer;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_use_certificate')]
    function SslCtxUseCertificate(ctx: PSSL_CTX; x: SslPtr):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_use_certificate_ASN1')]
    function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: integer; d: String):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_use_certificate_file')]
    function SslCtxUseCertificateFile(ctx: PSSL_CTX; const _file: String; _type: Integer):Integer;external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_use_certificate_chain_file')]
    function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: String):Integer;external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_check_private_key')]
    function SslCtxCheckPrivateKeyFile(ctx: PSSL_CTX):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_set_default_passwd_cb')]
    procedure SslCtxSetDefaultPasswdCb(ctx: PSSL_CTX; cb: PPasswdCb); external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_set_default_passwd_cb_userdata')]
    procedure SslCtxSetDefaultPasswdCbUserdata(ctx: PSSL_CTX; u: IntPtr); external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_load_verify_locations')]
    function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; CAfile: string; CApath: String):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_ctrl')]
    function SslCtxCtrl(ctx: PSSL_CTX; cmd: integer; larg: integer; parg: IntPtr): integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_new')]
    function SslNew(ctx: PSSL_CTX):PSSL;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_free')]
    procedure SslFree(ssl: PSSL); external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_accept')]
    function SslAccept(ssl: PSSL):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_connect')]
    function SslConnect(ssl: PSSL):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_shutdown')]
    function SslShutdown(s: PSSL):Integer;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_read')]
    function SslRead(ssl: PSSL; buf: StringBuilder; num: Integer):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_peek')]
    function SslPeek(ssl: PSSL; buf: StringBuilder; num: Integer):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_write')]
    function SslWrite(ssl: PSSL; buf: String; num: Integer):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_pending')]
    function SslPending(ssl: PSSL):Integer; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_get_version')]
    function SslGetVersion(ssl: PSSL):String; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_get_peer_certificate')]
    function SslGetPeerCertificate(s: PSSL):PX509; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CTX_set_verify')]
    procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: Integer; arg2: PFunction); external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_get_current_cipher')]
    function SSLGetCurrentCipher(s: PSSL): SslPtr;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CIPHER_get_name')]
    function SSLCipherGetName(c: SslPtr):String; external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_CIPHER_get_bits')]
    function SSLCipherGetBits(c: SslPtr; var alg_bits: Integer):Integer;  external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_get_verify_result')]
    function SSLGetVerifyResult(ssl: PSSL):Integer;external;

  [DllImport(DLLSSLName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'SSL_ctrl')]
    function SslCtrl(ssl: PSSL; cmd: integer; larg: integer; parg: IntPtr): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'X509_new')]
    function X509New: PX509; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'X509_free')]
    procedure X509Free(x: PX509); external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'X509_NAME_oneline')]
    function X509NameOneline(a: PX509_NAME; buf: StringBuilder; size: Integer): String; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'X509_get_subject_name')]
    function X509GetSubjectName(a: PX509):PX509_NAME; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'X509_get_issuer_name')]
    function X509GetIssuerName(a: PX509):PX509_NAME;  external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'X509_NAME_hash')]
    function X509NameHash(x: PX509_NAME):Cardinal;   external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'X509_digest')]
    function X509Digest (data: PX509; _type: PEVP_MD; md: StringBuilder; var len: Integer):Integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_set_version')]
    function X509SetVersion(x: PX509; version: integer): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_set_pubkey')]
    function X509SetPubkey(x: PX509; pkey: EVP_PKEY): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_set_issuer_name')]
    function X509SetIssuerName(x: PX509; name: PX509_NAME): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_NAME_add_entry_by_txt')]
    function X509NameAddEntryByTxt(name: PX509_NAME; field: string; _type: integer;
      bytes: string; len, loc, _set: integer): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_sign')]
    function X509Sign(x: PX509; pkey: EVP_PKEY; const md: PEVP_MD): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_print')]
    function X509print(b: PBIO; a: PX509): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_gmtime_adj')]
    function X509GmtimeAdj(s: PASN1_UTCTIME; adj: integer): PASN1_UTCTIME; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_set_notBefore')]
    function X509SetNotBefore(x: PX509; tm: PASN1_UTCTIME): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_set_notAfter')]
    function X509SetNotAfter(x: PX509; tm: PASN1_UTCTIME): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'X509_get_serialNumber')]
    function X509GetSerialNumber(x: PX509): PASN1_INTEGER; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'EVP_PKEY_new')]
    function EvpPkeyNew: EVP_PKEY; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'EVP_PKEY_free')]
    procedure EvpPkeyFree(pk: EVP_PKEY); external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'EVP_PKEY_assign')]
    function EvpPkeyAssign(pkey: EVP_PKEY; _type: integer; key: Prsa): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'EVP_get_digestbyname')]
    function EvpGetDigestByName(Name: String): PEVP_MD; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'EVP_cleanup')]
    procedure EVPcleanup; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'SSLeay_version')]
    function SSLeayversion(t: integer): String; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'ERR_error_string_n')]
    procedure ErrErrorString(e: integer; buf: StringBuilder; len: integer); external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'ERR_get_error')]
    function ErrGetError: integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'ERR_clear_error')]
    procedure ErrClearError; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'ERR_free_strings')]
    procedure ErrFreeStrings; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'ERR_remove_state')]
    procedure ErrRemoveState(pid: integer); external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'OPENSSL_add_all_algorithms_noconf')]
    procedure OPENSSLaddallalgorithms; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'CRYPTO_cleanup_all_ex_data')]
    procedure CRYPTOcleanupAllExData; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'RAND_screen')]
    procedure RandScreen; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'BIO_new')]
    function BioNew(b: PBIO_METHOD): PBIO; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'BIO_free_all')]
    procedure BioFreeAll(b: PBIO); external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'BIO_s_mem')]
    function BioSMem: PBIO_METHOD; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'BIO_ctrl_pending')]
    function BioCtrlPending(b: PBIO): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'BIO_read')]
    function BioRead(b: PBIO; Buf: StringBuilder; Len: integer): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'BIO_write')]
    function BioWrite(b: PBIO; var Buf: String; Len: integer): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'd2i_PKCS12_bio')]
    function d2iPKCS12bio(b:PBIO; Pkcs12: SslPtr): SslPtr; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'PKCS12_parse')]
    function PKCS12parse(p12: SslPtr; pass: string; var pkey, cert, ca: SslPtr): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'PKCS12_free')]
    procedure PKCS12free(p12: SslPtr); external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'RSA_generate_key')]
    function RsaGenerateKey(bits, e: integer; callback: PFunction; cb_arg: SslPtr): PRSA; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'ASN1_UTCTIME_new')]
    function Asn1UtctimeNew: PASN1_UTCTIME; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'ASN1_UTCTIME_free')]
    procedure Asn1UtctimeFree(a: PASN1_UTCTIME); external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'ASN1_INTEGER_set')]
    function Asn1IntegerSet(a: PASN1_INTEGER; v: integer): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'i2d_X509_bio')]
    function i2dX509bio(b: PBIO; x: PX509): integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint =  'i2d_PrivateKey_bio')]
    function i2dPrivateKeyBio(b: PBIO; pkey: EVP_PKEY): integer; external;

  // 3DES functions
  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'DES_set_odd_parity')]
    procedure DESsetoddparity(Key: des_cblock); external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'DES_set_key_checked')]
    function DESsetkeychecked(key: des_cblock; schedule: des_key_schedule): Integer; external;

  [DllImport(DLLUtilName, CharSet = CharSet.Ansi,
    SetLastError = False, CallingConvention= CallingConvention.cdecl,
    EntryPoint = 'DES_ecb_encrypt')]
    procedure DESecbencrypt(Input: des_cblock; output: des_cblock; ks: des_key_schedule; enc: Integer); external;

{$ELSE}
// libssl.dll
  function SslGetError(s: PSSL; ret_code: Integer):Integer;
  function SslLibraryInit:Integer;
  procedure SslLoadErrorStrings;
//  function SslCtxSetCipherList(arg0: PSSL_CTX; str: PChar):Integer;
  function SslCtxSetCipherList(arg0: PSSL_CTX; var str: AnsiString):Integer;
  function SslCtxNew(meth: PSSL_METHOD):PSSL_CTX;
  procedure SslCtxFree(arg0: PSSL_CTX);
  function SslSetFd(s: PSSL; fd: Integer):Integer;
  function SslMethodV2:PSSL_METHOD;
  function SslMethodV3:PSSL_METHOD;
  function SslMethodTLSV1:PSSL_METHOD;
  function SslMethodV23:PSSL_METHOD;
  function SslCtxUsePrivateKey(ctx: PSSL_CTX; pkey: SslPtr):Integer;
  function SslCtxUsePrivateKeyASN1(pk: integer; ctx: PSSL_CTX; d: AnsiString; len: integer):Integer;
//  function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: PChar; _type: Integer):Integer;
  function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: AnsiString; _type: Integer):Integer;
  function SslCtxUseCertificate(ctx: PSSL_CTX; x: SslPtr):Integer;
  function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: integer; d: AnsiString):Integer;
  function SslCtxUseCertificateFile(ctx: PSSL_CTX; const _file: AnsiString; _type: Integer):Integer;
//  function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: PChar):Integer;
  function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: AnsiString):Integer;
  function SslCtxCheckPrivateKeyFile(ctx: PSSL_CTX):Integer;
  procedure SslCtxSetDefaultPasswdCb(ctx: PSSL_CTX; cb: PPasswdCb);
  procedure SslCtxSetDefaultPasswdCbUserdata(ctx: PSSL_CTX; u: SslPtr);
//  function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):Integer;
  function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: AnsiString; const CApath: AnsiString):Integer;
  function SslCtxCtrl(ctx: PSSL_CTX; cmd: integer; larg: integer; parg: SslPtr): integer;
  function SslNew(ctx: PSSL_CTX):PSSL;
  procedure SslFree(ssl: PSSL);
  function SslAccept(ssl: PSSL):Integer;
  function SslConnect(ssl: PSSL):Integer;
  function SslShutdown(ssl: PSSL):Integer;
  function SslRead(ssl: PSSL; buf: SslPtr; num: Integer):Integer;
  function SslPeek(ssl: PSSL; buf: SslPtr; num: Integer):Integer;
  function SslWrite(ssl: PSSL; buf: SslPtr; num: Integer):Integer;
  function SslPending(ssl: PSSL):Integer;
  function SslGetVersion(ssl: PSSL):AnsiString;
  function SslGetPeerCertificate(ssl: PSSL):PX509;
  procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: Integer; arg2: PFunction);
  function SSLGetCurrentCipher(s: PSSL):SslPtr;
  function SSLCipherGetName(c: SslPtr): AnsiString;
  function SSLCipherGetBits(c: SslPtr; var alg_bits: Integer):Integer;
  function SSLGetVerifyResult(ssl: PSSL):Integer;
  function SSLCtrl(ssl: PSSL; cmd: integer; larg: integer; parg: SslPtr):Integer;

// libeay.dll
  function X509New: PX509;
  procedure X509Free(x: PX509);
  function X509NameOneline(a: PX509_NAME; var buf: AnsiString; size: Integer):AnsiString;
  function X509GetSubjectName(a: PX509):PX509_NAME;
  function X509GetIssuerName(a: PX509):PX509_NAME;
  function X509NameHash(x: PX509_NAME):Cardinal;
//  function SslX509Digest(data: PX509; _type: PEVP_MD; md: PChar; len: PInteger):Integer;
  function X509Digest(data: PX509; _type: PEVP_MD; md: AnsiString; var len: Integer):Integer;
  function X509print(b: PBIO; a: PX509): integer;
  function X509SetVersion(x: PX509; version: integer): integer;
  function X509SetPubkey(x: PX509; pkey: EVP_PKEY): integer;
  function X509SetIssuerName(x: PX509; name: PX509_NAME): integer;
  function X509NameAddEntryByTxt(name: PX509_NAME; field: Ansistring; _type: integer;
    bytes: Ansistring; len, loc, _set: integer): integer;
  function X509Sign(x: PX509; pkey: EVP_PKEY; const md: PEVP_MD): integer;
  function X509GmtimeAdj(s: PASN1_UTCTIME; adj: integer): PASN1_UTCTIME;
  function X509SetNotBefore(x: PX509; tm: PASN1_UTCTIME): integer;
  function X509SetNotAfter(x: PX509; tm: PASN1_UTCTIME): integer;
  function X509GetSerialNumber(x: PX509): PASN1_INTEGER;
  function EvpPkeyNew: EVP_PKEY;
  procedure EvpPkeyFree(pk: EVP_PKEY);
  function EvpPkeyAssign(pkey: EVP_PKEY; _type: integer; key: Prsa): integer;
  function EvpGetDigestByName(Name: AnsiString): PEVP_MD;
  procedure EVPcleanup;
//  function ErrErrorString(e: integer; buf: PChar): PChar;
  function SSLeayversion(t: integer): Ansistring;
  procedure ErrErrorString(e: integer; var buf: Ansistring; len: integer);
  function ErrGetError: integer;
  procedure ErrClearError;
  procedure ErrFreeStrings;
  procedure ErrRemoveState(pid: integer);
  procedure OPENSSLaddallalgorithms;
  procedure CRYPTOcleanupAllExData;
  procedure RandScreen;
  function BioNew(b: PBIO_METHOD): PBIO;
  procedure BioFreeAll(b: PBIO);
  function BioSMem: PBIO_METHOD;
  function BioCtrlPending(b: PBIO): integer;
  function BioRead(b: PBIO; var Buf: AnsiString; Len: integer): integer;
  function BioWrite(b: PBIO; Buf: AnsiString; Len: integer): integer;
  function d2iPKCS12bio(b:PBIO; Pkcs12: SslPtr): SslPtr;
  function PKCS12parse(p12: SslPtr; pass: Ansistring; var pkey, cert, ca: SslPtr): integer;
  procedure PKCS12free(p12: SslPtr);
  function RsaGenerateKey(bits, e: integer; callback: PFunction; cb_arg: SslPtr): PRSA;
  function Asn1UtctimeNew: PASN1_UTCTIME;
  procedure Asn1UtctimeFree(a: PASN1_UTCTIME);
  function Asn1IntegerSet(a: PASN1_INTEGER; v: integer): integer;
  function Asn1IntegerGet(a: PASN1_INTEGER): integer; {pf}
  function i2dX509bio(b: PBIO; x: PX509): integer;
  function d2iX509bio(b:PBIO; x:PX509):  PX509;    {pf}
  function PEMReadBioX509(b:PBIO; {var x:PX509;}x:PSslPtr; callback:PFunction; cb_arg: SslPtr):  PX509;    {pf}
  procedure SkX509PopFree(st: PSTACK; func: TSkPopFreeFunc); {pf}


  function i2dPrivateKeyBio(b: PBIO; pkey: EVP_PKEY): integer;

  // 3DES functions
  procedure DESsetoddparity(Key: des_cblock);
  function DESsetkeychecked(key: des_cblock; schedule: des_key_schedule): Integer;
  procedure DESecbencrypt(Input: des_cblock; output: des_cblock; ks: des_key_schedule; enc: Integer);

{$ENDIF}

function IsSSLloaded: Boolean;
function InitSSLInterface: Boolean;
function DestroySSLInterface: Boolean;

var
  _X509Free: TX509Free = nil; {pf}

implementation

uses SyncObjs;

{$IFNDEF CIL}
type
// libssl.dll
  TSslGetError = function(s: PSSL; ret_code: Integer):Integer; cdecl;
  TSslLibraryInit = function:Integer; cdecl;
  TSslLoadErrorStrings = procedure; cdecl;
  TSslCtxSetCipherList = function(arg0: PSSL_CTX; str: PAnsiChar):Integer; cdecl;
  TSslCtxNew = function(meth: PSSL_METHOD):PSSL_CTX; cdecl;
  TSslCtxFree = procedure(arg0: PSSL_CTX); cdecl;
  TSslSetFd = function(s: PSSL; fd: Integer):Integer; cdecl;
  TSslMethodV2 = function:PSSL_METHOD; cdecl;
  TSslMethodV3 = function:PSSL_METHOD; cdecl;
  TSslMethodTLSV1 = function:PSSL_METHOD; cdecl;
  TSslMethodV23 = function:PSSL_METHOD; cdecl;
  TSslCtxUsePrivateKey = function(ctx: PSSL_CTX; pkey: sslptr):Integer; cdecl;
  TSslCtxUsePrivateKeyASN1 = function(pk: integer; ctx: PSSL_CTX; d: sslptr; len: integer):Integer; cdecl;
  TSslCtxUsePrivateKeyFile = function(ctx: PSSL_CTX; const _file: PAnsiChar; _type: Integer):Integer; cdecl;
  TSslCtxUseCertificate = function(ctx: PSSL_CTX; x: SslPtr):Integer; cdecl;
  TSslCtxUseCertificateASN1 = function(ctx: PSSL_CTX; len: Integer; d: SslPtr):Integer; cdecl;
  TSslCtxUseCertificateFile = function(ctx: PSSL_CTX; const _file: PAnsiChar; _type: Integer):Integer; cdecl;
  TSslCtxUseCertificateChainFile = function(ctx: PSSL_CTX; const _file: PAnsiChar):Integer; cdecl;
  TSslCtxCheckPrivateKeyFile = function(ctx: PSSL_CTX):Integer; cdecl;
  TSslCtxSetDefaultPasswdCb = procedure(ctx: PSSL_CTX; cb: SslPtr); cdecl;
  TSslCtxSetDefaultPasswdCbUserdata = procedure(ctx: PSSL_CTX; u: SslPtr); cdecl;
  TSslCtxLoadVerifyLocations = function(ctx: PSSL_CTX; const CAfile: PAnsiChar; const CApath: PAnsiChar):Integer; cdecl;
  TSslCtxCtrl = function(ctx: PSSL_CTX; cmd: integer; larg: integer; parg: SslPtr): integer; cdecl;
  TSslNew = function(ctx: PSSL_CTX):PSSL; cdecl;
  TSslFree = procedure(ssl: PSSL); cdecl;
  TSslAccept = function(ssl: PSSL):Integer; cdecl;
  TSslConnect = function(ssl: PSSL):Integer; cdecl;
  TSslShutdown = function(ssl: PSSL):Integer; cdecl;
  TSslRead = function(ssl: PSSL; buf: PAnsiChar; num: Integer):Integer; cdecl;
  TSslPeek = function(ssl: PSSL; buf: PAnsiChar; num: Integer):Integer; cdecl;
  TSslWrite = function(ssl: PSSL; const buf: PAnsiChar; num: Integer):Integer; cdecl;
  TSslPending = function(ssl: PSSL):Integer; cdecl;
  TSslGetVersion = function(ssl: PSSL):PAnsiChar; cdecl;
  TSslGetPeerCertificate = function(ssl: PSSL):PX509; cdecl;
  TSslCtxSetVerify = procedure(ctx: PSSL_CTX; mode: Integer; arg2: SslPtr); cdecl;
  TSSLGetCurrentCipher = function(s: PSSL):SslPtr; cdecl;
  TSSLCipherGetName = function(c: Sslptr):PAnsiChar; cdecl;
  TSSLCipherGetBits = function(c: SslPtr; alg_bits: PInteger):Integer; cdecl;
  TSSLGetVerifyResult = function(ssl: PSSL):Integer; cdecl;
  TSSLCtrl = function(ssl: PSSL; cmd: integer; larg: integer; parg: SslPtr):Integer; cdecl;

  TSSLSetTlsextHostName = function(ssl: PSSL; buf: PAnsiChar):Integer; cdecl;

// libeay.dll
  TX509New = function: PX509; cdecl;
  TX509NameOneline = function(a: PX509_NAME; buf: PAnsiChar; size: Integer):PAnsiChar; cdecl;
  TX509GetSubjectName = function(a: PX509):PX509_NAME; cdecl;
  TX509GetIssuerName = function(a: PX509):PX509_NAME; cdecl;
  TX509NameHash = function(x: PX509_NAME):Cardinal; cdecl;
  TX509Digest = function(data: PX509; _type: PEVP_MD; md: PAnsiChar; len: PInteger):Integer; cdecl;
  TX509print = function(b: PBIO; a: PX509): integer; cdecl;
  TX509SetVersion = function(x: PX509; version: integer): integer; cdecl;
  TX509SetPubkey = function(x: PX509; pkey: EVP_PKEY): integer; cdecl;
  TX509SetIssuerName = function(x: PX509; name: PX509_NAME): integer; cdecl;
  TX509NameAddEntryByTxt = function(name: PX509_NAME; field: PAnsiChar; _type: integer;
    bytes: PAnsiChar; len, loc, _set: integer): integer; cdecl;
  TX509Sign = function(x: PX509; pkey: EVP_PKEY; const md: PEVP_MD): integer; cdecl;
  TX509GmtimeAdj = function(s: PASN1_UTCTIME; adj: integer): PASN1_UTCTIME; cdecl;
  TX509SetNotBefore = function(x: PX509; tm: PASN1_UTCTIME): integer; cdecl;
  TX509SetNotAfter = function(x: PX509; tm: PASN1_UTCTIME): integer; cdecl;
  TX509GetSerialNumber = function(x: PX509): PASN1_INTEGER; cdecl;
  TEvpPkeyNew = function: EVP_PKEY; cdecl;
  TEvpPkeyFree = procedure(pk: EVP_PKEY); cdecl;
  TEvpPkeyAssign = function(pkey: EVP_PKEY; _type: integer; key: Prsa): integer; cdecl;
  TEvpGetDigestByName = function(Name: PAnsiChar): PEVP_MD; cdecl;
  TEVPcleanup = procedure; cdecl;
  TSSLeayversion = function(t: integer): PAnsiChar; cdecl;
  TErrErrorString = procedure(e: integer; buf: PAnsiChar; len: integer); cdecl;
  TErrGetError = function: integer; cdecl;
  TErrClearError = procedure; cdecl;
  TErrFreeStrings = procedure; cdecl;
  TErrRemoveState = procedure(pid: integer); cdecl;
  TOPENSSLaddallalgorithms = procedure; cdecl;
  TCRYPTOcleanupAllExData = procedure; cdecl;
  TRandScreen = procedure; cdecl;
  TBioNew = function(b: PBIO_METHOD): PBIO; cdecl;
  TBioFreeAll = procedure(b: PBIO); cdecl;
  TBioSMem = function: PBIO_METHOD; cdecl;
  TBioCtrlPending = function(b: PBIO): integer; cdecl;
  TBioRead = function(b: PBIO; Buf: PAnsiChar; Len: integer): integer; cdecl;
  TBioWrite = function(b: PBIO; Buf: PAnsiChar; Len: integer): integer; cdecl;
  Td2iPKCS12bio = function(b:PBIO; Pkcs12: SslPtr): SslPtr; cdecl;
  TPKCS12parse = function(p12: SslPtr; pass: PAnsiChar; var pkey, cert, ca: SslPtr): integer; cdecl;
  TPKCS12free = procedure(p12: SslPtr); cdecl;
  TRsaGenerateKey = function(bits, e: integer; callback: PFunction; cb_arg: SslPtr): PRSA; cdecl;
  TAsn1UtctimeNew = function: PASN1_UTCTIME; cdecl;
  TAsn1UtctimeFree = procedure(a: PASN1_UTCTIME); cdecl;
  TAsn1IntegerSet = function(a: PASN1_INTEGER; v: integer): integer; cdecl;
  TAsn1IntegerGet = function(a: PASN1_INTEGER): integer; cdecl; {pf}
  Ti2dX509bio = function(b: PBIO; x: PX509): integer; cdecl;
  Td2iX509bio = function(b:PBIO;  x:PX509):   PX509;   cdecl; {pf}
  TPEMReadBioX509 = function(b:PBIO;  {var x:PX509;}x:PSslPtr; callback:PFunction; cb_arg:SslPtr): PX509;   cdecl; {pf}
  TSkX509PopFree = procedure(st: PSTACK; func: TSkPopFreeFunc); cdecl; {pf}
  Ti2dPrivateKeyBio= function(b: PBIO; pkey: EVP_PKEY): integer; cdecl;

  // 3DES functions
  TDESsetoddparity = procedure(Key: des_cblock); cdecl;
  TDESsetkeychecked = function(key: des_cblock; schedule: des_key_schedule): Integer; cdecl;
  TDESecbencrypt = procedure(Input: des_cblock; output: des_cblock; ks: des_key_schedule; enc: Integer); cdecl;
  //thread lock functions
  TCRYPTOnumlocks = function: integer; cdecl;
  TCRYPTOSetLockingCallback = procedure(cb: Sslptr); cdecl;

var
// libssl.dll
  _SslGetError: TSslGetError = nil;
  _SslLibraryInit: TSslLibraryInit = nil;
  _SslLoadErrorStrings: TSslLoadErrorStrings = nil;
  _SslCtxSetCipherList: TSslCtxSetCipherList = nil;
  _SslCtxNew: TSslCtxNew = nil;
  _SslCtxFree: TSslCtxFree = nil;
  _SslSetFd: TSslSetFd = nil;
  _SslMethodV2: TSslMethodV2 = nil;
  _SslMethodV3: TSslMethodV3 = nil;
  _SslMethodTLSV1: TSslMethodTLSV1 = nil;
  _SslMethodV23: TSslMethodV23 = nil;
  _SslCtxUsePrivateKey: TSslCtxUsePrivateKey = nil;
  _SslCtxUsePrivateKeyASN1: TSslCtxUsePrivateKeyASN1 = nil;
  _SslCtxUsePrivateKeyFile: TSslCtxUsePrivateKeyFile = nil;
  _SslCtxUseCertificate: TSslCtxUseCertificate = nil;
  _SslCtxUseCertificateASN1: TSslCtxUseCertificateASN1 = nil;
  _SslCtxUseCertificateFile: TSslCtxUseCertificateFile = nil;
  _SslCtxUseCertificateChainFile: TSslCtxUseCertificateChainFile = nil;
  _SslCtxCheckPrivateKeyFile: TSslCtxCheckPrivateKeyFile = nil;
  _SslCtxSetDefaultPasswdCb: TSslCtxSetDefaultPasswdCb = nil;
  _SslCtxSetDefaultPasswdCbUserdata: TSslCtxSetDefaultPasswdCbUserdata = nil;
  _SslCtxLoadVerifyLocations: TSslCtxLoadVerifyLocations = nil;
  _SslCtxCtrl: TSslCtxCtrl = nil;
  _SslNew: TSslNew = nil;
  _SslFree: TSslFree = nil;
  _SslAccept: TSslAccept = nil;
  _SslConnect: TSslConnect = nil;
  _SslShutdown: TSslShutdown = nil;
  _SslRead: TSslRead = nil;
  _SslPeek: TSslPeek = nil;
  _SslWrite: TSslWrite = nil;
  _SslPending: TSslPending = nil;
  _SslGetVersion: TSslGetVersion = nil;
  _SslGetPeerCertificate: TSslGetPeerCertificate = nil;
  _SslCtxSetVerify: TSslCtxSetVerify = nil;
  _SSLGetCurrentCipher: TSSLGetCurrentCipher = nil;
  _SSLCipherGetName: TSSLCipherGetName = nil;
  _SSLCipherGetBits: TSSLCipherGetBits = nil;
  _SSLGetVerifyResult: TSSLGetVerifyResult = nil;
  _SSLCtrl: TSSLCtrl = nil;

// libeay.dll
  _X509New: TX509New = nil;
  _X509NameOneline: TX509NameOneline = nil;
  _X509GetSubjectName: TX509GetSubjectName = nil;
  _X509GetIssuerName: TX509GetIssuerName = nil;
  _X509NameHash: TX509NameHash = nil;
  _X509Digest: TX509Digest = nil;
  _X509print: TX509print = nil;
  _X509SetVersion: TX509SetVersion = nil;
  _X509SetPubkey: TX509SetPubkey = nil;
  _X509SetIssuerName: TX509SetIssuerName = nil;
  _X509NameAddEntryByTxt: TX509NameAddEntryByTxt = nil;
  _X509Sign: TX509Sign = nil;
  _X509GmtimeAdj: TX509GmtimeAdj = nil;
  _X509SetNotBefore: TX509SetNotBefore = nil;
  _X509SetNotAfter: TX509SetNotAfter = nil;
  _X509GetSerialNumber: TX509GetSerialNumber = nil;
  _EvpPkeyNew: TEvpPkeyNew = nil;
  _EvpPkeyFree: TEvpPkeyFree = nil;
  _EvpPkeyAssign: TEvpPkeyAssign = nil;
  _EvpGetDigestByName: TEvpGetDigestByName = nil;
  _EVPcleanup: TEVPcleanup = nil;
  _SSLeayversion: TSSLeayversion = nil;
  _ErrErrorString: TErrErrorString = nil;
  _ErrGetError: TErrGetError = nil;
  _ErrClearError: TErrClearError = nil;
  _ErrFreeStrings: TErrFreeStrings = nil;
  _ErrRemoveState: TErrRemoveState = nil;
  _OPENSSLaddallalgorithms: TOPENSSLaddallalgorithms = nil;
  _CRYPTOcleanupAllExData: TCRYPTOcleanupAllExData = nil;
  _RandScreen: TRandScreen = nil;
  _BioNew: TBioNew = nil;
  _BioFreeAll: TBioFreeAll = nil;
  _BioSMem: TBioSMem = nil;
  _BioCtrlPending: TBioCtrlPending = nil;
  _BioRead: TBioRead = nil;
  _BioWrite: TBioWrite = nil;
  _d2iPKCS12bio: Td2iPKCS12bio = nil;
  _PKCS12parse: TPKCS12parse = nil;
  _PKCS12free: TPKCS12free = nil;
  _RsaGenerateKey: TRsaGenerateKey = nil;
  _Asn1UtctimeNew: TAsn1UtctimeNew = nil;
  _Asn1UtctimeFree: TAsn1UtctimeFree = nil;
  _Asn1IntegerSet: TAsn1IntegerSet = nil;
  _Asn1IntegerGet: TAsn1IntegerGet = nil; {pf}
  _i2dX509bio: Ti2dX509bio = nil;
  _d2iX509bio: Td2iX509bio = nil; {pf}
  _PEMReadBioX509: TPEMReadBioX509 = nil; {pf}
  _SkX509PopFree: TSkX509PopFree = nil; {pf}
  _i2dPrivateKeyBio: Ti2dPrivateKeyBio = nil;

  // 3DES functions
  _DESsetoddparity: TDESsetoddparity = nil;
  _DESsetkeychecked: TDESsetkeychecked = nil;
  _DESecbencrypt: TDESecbencrypt = nil;
  //thread lock functions
  _CRYPTOnumlocks: TCRYPTOnumlocks = nil;
  _CRYPTOSetLockingCallback: TCRYPTOSetLockingCallback = nil;
{$ENDIF}

var
  SSLCS: TCriticalSection;
  SSLloaded: boolean = false;
{$IFNDEF CIL}
  Locks: TList;
{$ENDIF}

{$IFNDEF CIL}
// libssl.dll
function SslGetError(s: PSSL; ret_code: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslGetError) then
    Result := _SslGetError(s, ret_code)
  else
    Result := SSL_ERROR_SSL;
end;

function SslLibraryInit:Integer;
begin
  if InitSSLInterface and Assigned(_SslLibraryInit) then
    Result := _SslLibraryInit
  else
    Result := 1;
end;

procedure SslLoadErrorStrings;
begin
  if InitSSLInterface and Assigned(_SslLoadErrorStrings) then
    _SslLoadErrorStrings;
end;

//function SslCtxSetCipherList(arg0: PSSL_CTX; str: PChar):Integer;
function SslCtxSetCipherList(arg0: PSSL_CTX; var str: AnsiString):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxSetCipherList) then
    Result := _SslCtxSetCipherList(arg0, PAnsiChar(str))
  else
    Result := 0;
end;

function SslCtxNew(meth: PSSL_METHOD):PSSL_CTX;
begin
  if InitSSLInterface and Assigned(_SslCtxNew) then
    Result := _SslCtxNew(meth)
  else
    Result := nil;
end;

procedure SslCtxFree(arg0: PSSL_CTX);
begin
  if InitSSLInterface and Assigned(_SslCtxFree) then
    _SslCtxFree(arg0);
end;

function SslSetFd(s: PSSL; fd: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslSetFd) then
    Result := _SslSetFd(s, fd)
  else
    Result := 0;
end;

function SslMethodV2:PSSL_METHOD;
begin
  if InitSSLInterface and Assigned(_SslMethodV2) then
    Result := _SslMethodV2
  else
    Result := nil;
end;

function SslMethodV3:PSSL_METHOD;
begin
  if InitSSLInterface and Assigned(_SslMethodV3) then
    Result := _SslMethodV3
  else
    Result := nil;
end;

function SslMethodTLSV1:PSSL_METHOD;
begin
  if InitSSLInterface and Assigned(_SslMethodTLSV1) then
    Result := _SslMethodTLSV1
  else
    Result := nil;
end;

function SslMethodV23:PSSL_METHOD;
begin
  if InitSSLInterface and Assigned(_SslMethodV23) then
    Result := _SslMethodV23
  else
    Result := nil;
end;

function SslCtxUsePrivateKey(ctx: PSSL_CTX; pkey: SslPtr):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUsePrivateKey) then
    Result := _SslCtxUsePrivateKey(ctx, pkey)
  else
    Result := 0;
end;

function SslCtxUsePrivateKeyASN1(pk: integer; ctx: PSSL_CTX; d: AnsiString; len: integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUsePrivateKeyASN1) then
    Result := _SslCtxUsePrivateKeyASN1(pk, ctx, Sslptr(d), len)
  else
    Result := 0;
end;

//function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: PChar; _type: Integer):Integer;
function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: AnsiString; _type: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUsePrivateKeyFile) then
    Result := _SslCtxUsePrivateKeyFile(ctx, PAnsiChar(_file), _type)
  else
    Result := 0;
end;

function SslCtxUseCertificate(ctx: PSSL_CTX; x: SslPtr):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUseCertificate) then
    Result := _SslCtxUseCertificate(ctx, x)
  else
    Result := 0;
end;

function SslCtxUseCertificateASN1(ctx: PSSL_CTX; len: integer; d: AnsiString):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUseCertificateASN1) then
    Result := _SslCtxUseCertificateASN1(ctx, len, SslPtr(d))
  else
    Result := 0;
end;

function SslCtxUseCertificateFile(ctx: PSSL_CTX; const _file: AnsiString; _type: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUseCertificateFile) then
    Result := _SslCtxUseCertificateFile(ctx, PAnsiChar(_file), _type)
  else
    Result := 0;
end;

//function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: PChar):Integer;
function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: AnsiString):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUseCertificateChainFile) then
    Result := _SslCtxUseCertificateChainFile(ctx, PAnsiChar(_file))
  else
    Result := 0;
end;

function SslCtxCheckPrivateKeyFile(ctx: PSSL_CTX):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxCheckPrivateKeyFile) then
    Result := _SslCtxCheckPrivateKeyFile(ctx)
  else
    Result := 0;
end;

procedure SslCtxSetDefaultPasswdCb(ctx: PSSL_CTX; cb: PPasswdCb);
begin
  if InitSSLInterface and Assigned(_SslCtxSetDefaultPasswdCb) then
    _SslCtxSetDefaultPasswdCb(ctx, cb);
end;

procedure SslCtxSetDefaultPasswdCbUserdata(ctx: PSSL_CTX; u: SslPtr);
begin
  if InitSSLInterface and Assigned(_SslCtxSetDefaultPasswdCbUserdata) then
    _SslCtxSetDefaultPasswdCbUserdata(ctx, u);
end;

//function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):Integer;
function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: AnsiString; const CApath: AnsiString):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxLoadVerifyLocations) then
    Result := _SslCtxLoadVerifyLocations(ctx, SslPtr(CAfile), SslPtr(CApath))
  else
    Result := 0;
end;

function SslCtxCtrl(ctx: PSSL_CTX; cmd: integer; larg: integer; parg: SslPtr): integer;
begin
  if InitSSLInterface and Assigned(_SslCtxCtrl) then
    Result := _SslCtxCtrl(ctx, cmd, larg, parg)
  else
    Result := 0;
end;

function SslNew(ctx: PSSL_CTX):PSSL;
begin
  if InitSSLInterface and Assigned(_SslNew) then
    Result := _SslNew(ctx)
  else
    Result := nil;
end;

procedure SslFree(ssl: PSSL);
begin
  if InitSSLInterface and Assigned(_SslFree) then
    _SslFree(ssl);
end;

function SslAccept(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SslAccept) then
    Result := _SslAccept(ssl)
  else
    Result := -1;
end;

function SslConnect(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SslConnect) then
    Result := _SslConnect(ssl)
  else
    Result := -1;
end;

function SslShutdown(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SslShutdown) then
    Result := _SslShutdown(ssl)
  else
    Result := -1;
end;

//function SslRead(ssl: PSSL; buf: PChar; num: Integer):Integer;
function SslRead(ssl: PSSL; buf: SslPtr; num: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslRead) then
    Result := _SslRead(ssl, PAnsiChar(buf), num)
  else
    Result := -1;
end;

//function SslPeek(ssl: PSSL; buf: PChar; num: Integer):Integer;
function SslPeek(ssl: PSSL; buf: SslPtr; num: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslPeek) then
    Result := _SslPeek(ssl, PAnsiChar(buf), num)
  else
    Result := -1;
end;

//function SslWrite(ssl: PSSL; const buf: PChar; num: Integer):Integer;
function SslWrite(ssl: PSSL; buf: SslPtr; num: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslWrite) then
    Result := _SslWrite(ssl, PAnsiChar(buf), num)
  else
    Result := -1;
end;

function SslPending(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SslPending) then
    Result := _SslPending(ssl)
  else
    Result := 0;
end;

//function SslGetVersion(ssl: PSSL):PChar;
function SslGetVersion(ssl: PSSL):AnsiString;
begin
  if InitSSLInterface and Assigned(_SslGetVersion) then
    Result := _SslGetVersion(ssl)
  else
    Result := '';
end;

function SslGetPeerCertificate(ssl: PSSL):PX509;
begin
  if InitSSLInterface and Assigned(_SslGetPeerCertificate) then
    Result := _SslGetPeerCertificate(ssl)
  else
    Result := nil;
end;

//procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: Integer; arg2: SslPtr);
procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: Integer; arg2: PFunction);
begin
  if InitSSLInterface and Assigned(_SslCtxSetVerify) then
    _SslCtxSetVerify(ctx, mode, @arg2);
end;

function SSLGetCurrentCipher(s: PSSL):SslPtr;
begin
  if InitSSLInterface and Assigned(_SSLGetCurrentCipher) then
{$IFDEF CIL}
{$ELSE}
    Result := _SSLGetCurrentCipher(s)
{$ENDIF}
  else
    Result := nil;
end;

//function SSLCipherGetName(c: SslPtr):PChar;
function SSLCipherGetName(c: SslPtr):AnsiString;
begin
  if InitSSLInterface and Assigned(_SSLCipherGetName) then
    Result := _SSLCipherGetName(c)
  else
    Result := '';
end;

//function SSLCipherGetBits(c: SslPtr; alg_bits: PInteger):Integer;
function SSLCipherGetBits(c: SslPtr; var alg_bits: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SSLCipherGetBits) then
    Result := _SSLCipherGetBits(c, @alg_bits)
  else
    Result := 0;
end;

function SSLGetVerifyResult(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SSLGetVerifyResult) then
    Result := _SSLGetVerifyResult(ssl)
  else
    Result := X509_V_ERR_APPLICATION_VERIFICATION;
end;


function SSLCtrl(ssl: PSSL; cmd: integer; larg: integer; parg: SslPtr):Integer;
begin
  if InitSSLInterface and Assigned(_SSLCtrl) then
    Result := _SSLCtrl(ssl, cmd, larg, parg)
  else
    Result := X509_V_ERR_APPLICATION_VERIFICATION;
end;

// libeay.dll
function X509New: PX509;
begin
  if InitSSLInterface and Assigned(_X509New) then
    Result := _X509New
  else
    Result := nil;
end;

procedure X509Free(x: PX509);
begin
  if InitSSLInterface and Assigned(_X509Free) then
    _X509Free(x);
end;

//function SslX509NameOneline(a: PX509_NAME; buf: PChar; size: Integer):PChar;
function X509NameOneline(a: PX509_NAME; var buf: AnsiString; size: Integer):AnsiString;
begin
  if InitSSLInterface and Assigned(_X509NameOneline) then
    Result := _X509NameOneline(a, PAnsiChar(buf),size)
  else
    Result := '';
end;

function X509GetSubjectName(a: PX509):PX509_NAME;
begin
  if InitSSLInterface and Assigned(_X509GetSubjectName) then
    Result := _X509GetSubjectName(a)
  else
    Result := nil;
end;

function X509GetIssuerName(a: PX509):PX509_NAME;
begin
  if InitSSLInterface and Assigned(_X509GetIssuerName) then
    Result := _X509GetIssuerName(a)
  else
    Result := nil;
end;

function X509NameHash(x: PX509_NAME):Cardinal;
begin
  if InitSSLInterface and Assigned(_X509NameHash) then
    Result := _X509NameHash(x)
  else
    Result := 0;
end;

//function SslX509Digest(data: PX509; _type: PEVP_MD; md: PChar; len: PInteger):Integer;
function X509Digest(data: PX509; _type: PEVP_MD; md: AnsiString; var len: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_X509Digest) then
    Result := _X509Digest(data, _type, PAnsiChar(md), @len)
  else
    Result := 0;
end;

function EvpPkeyNew: EVP_PKEY;
begin
  if InitSSLInterface and Assigned(_EvpPkeyNew) then
    Result := _EvpPkeyNew
  else
    Result := nil;
end;

procedure EvpPkeyFree(pk: EVP_PKEY);
begin
  if InitSSLInterface and Assigned(_EvpPkeyFree) then
    _EvpPkeyFree(pk);
end;

function SSLeayversion(t: integer): Ansistring;
begin
  if InitSSLInterface and Assigned(_SSLeayversion) then
    Result := PAnsiChar(_SSLeayversion(t))
  else
    Result := '';
end;

procedure ErrErrorString(e: integer; var buf: Ansistring; len: integer);
begin
  if InitSSLInterface and Assigned(_ErrErrorString) then
    _ErrErrorString(e, Pointer(buf), len);
  buf := PAnsiChar(Buf);
end;

function ErrGetError: integer;
begin
  if InitSSLInterface and Assigned(_ErrGetError) then
    Result := _ErrGetError
  else
    Result := SSL_ERROR_SSL;
end;

procedure ErrClearError;
begin
  if InitSSLInterface and Assigned(_ErrClearError) then
    _ErrClearError;
end;

procedure ErrFreeStrings;
begin
  if InitSSLInterface and Assigned(_ErrFreeStrings) then
    _ErrFreeStrings;
end;

procedure ErrRemoveState(pid: integer);
begin
  if InitSSLInterface and Assigned(_ErrRemoveState) then
    _ErrRemoveState(pid);
end;

procedure OPENSSLaddallalgorithms;
begin
  if InitSSLInterface and Assigned(_OPENSSLaddallalgorithms) then
    _OPENSSLaddallalgorithms;
end;

procedure EVPcleanup;
begin
  if InitSSLInterface and Assigned(_EVPcleanup) then
    _EVPcleanup;
end;

procedure CRYPTOcleanupAllExData;
begin
  if InitSSLInterface and Assigned(_CRYPTOcleanupAllExData) then
    _CRYPTOcleanupAllExData;
end;

procedure RandScreen;
begin
  if InitSSLInterface and Assigned(_RandScreen) then
    _RandScreen;
end;

function BioNew(b: PBIO_METHOD): PBIO;
begin
  if InitSSLInterface and Assigned(_BioNew) then
    Result := _BioNew(b)
  else
    Result := nil;
end;

procedure BioFreeAll(b: PBIO);
begin
  if InitSSLInterface and Assigned(_BioFreeAll) then
    _BioFreeAll(b);
end;

function BioSMem: PBIO_METHOD;
begin
  if InitSSLInterface and Assigned(_BioSMem) then
    Result := _BioSMem
  else
    Result := nil;
end;

function BioCtrlPending(b: PBIO): integer;
begin
  if InitSSLInterface and Assigned(_BioCtrlPending) then
    Result := _BioCtrlPending(b)
  else
    Result := 0;
end;

//function BioRead(b: PBIO; Buf: PChar; Len: integer): integer;
function BioRead(b: PBIO; var Buf: AnsiString; Len: integer): integer;
begin
  if InitSSLInterface and Assigned(_BioRead) then
    Result := _BioRead(b, PAnsiChar(Buf), Len)
  else
    Result := -2;
end;

//function BioWrite(b: PBIO; Buf: PChar; Len: integer): integer;
function BioWrite(b: PBIO; Buf: AnsiString; Len: integer): integer;
begin
  if InitSSLInterface and Assigned(_BioWrite) then
    Result := _BioWrite(b, PAnsiChar(Buf), Len)
  else
    Result := -2;
end;

function X509print(b: PBIO; a: PX509): integer;
begin
  if InitSSLInterface and Assigned(_X509print) then
    Result := _X509print(b, a)
  else
    Result := 0;
end;

function d2iPKCS12bio(b:PBIO; Pkcs12: SslPtr): SslPtr;
begin
  if InitSSLInterface and Assigned(_d2iPKCS12bio) then
    Result := _d2iPKCS12bio(b, Pkcs12)
  else
    Result := nil;
end;

function PKCS12parse(p12: SslPtr; pass: Ansistring; var pkey, cert, ca: SslPtr): integer;
begin
  if InitSSLInterface and Assigned(_PKCS12parse) then
    Result := _PKCS12parse(p12, SslPtr(pass), pkey, cert, ca)
  else
    Result := 0;
end;

procedure PKCS12free(p12: SslPtr);
begin
  if InitSSLInterface and Assigned(_PKCS12free) then
    _PKCS12free(p12);
end;

function RsaGenerateKey(bits, e: integer; callback: PFunction; cb_arg: SslPtr): PRSA;
begin
  if InitSSLInterface and Assigned(_RsaGenerateKey) then
    Result := _RsaGenerateKey(bits, e, callback, cb_arg)
  else
    Result := nil;
end;

function EvpPkeyAssign(pkey: EVP_PKEY; _type: integer; key: Prsa): integer;
begin
  if InitSSLInterface and Assigned(_EvpPkeyAssign) then
    Result := _EvpPkeyAssign(pkey, _type, key)
  else
    Result := 0;
end;

function X509SetVersion(x: PX509; version: integer): integer;
begin
  if InitSSLInterface and Assigned(_X509SetVersion) then
    Result := _X509SetVersion(x, version)
  else
    Result := 0;
end;

function X509SetPubkey(x: PX509; pkey: EVP_PKEY): integer;
begin
  if InitSSLInterface and Assigned(_X509SetPubkey) then
    Result := _X509SetPubkey(x, pkey)
  else
    Result := 0;
end;

function X509SetIssuerName(x: PX509; name: PX509_NAME): integer;
begin
  if InitSSLInterface and Assigned(_X509SetIssuerName) then
    Result := _X509SetIssuerName(x, name)
  else
    Result := 0;
end;

function X509NameAddEntryByTxt(name: PX509_NAME; field: Ansistring; _type: integer;
  bytes: Ansistring; len, loc, _set: integer): integer;
begin
  if InitSSLInterface and Assigned(_X509NameAddEntryByTxt) then
    Result := _X509NameAddEntryByTxt(name, PAnsiChar(field), _type, PAnsiChar(Bytes), len, loc, _set)
  else
    Result := 0;
end;

function X509Sign(x: PX509; pkey: EVP_PKEY; const md: PEVP_MD): integer;
begin
  if InitSSLInterface and Assigned(_X509Sign) then
    Result := _X509Sign(x, pkey, md)
  else
    Result := 0;
end;

function Asn1UtctimeNew: PASN1_UTCTIME;
begin
  if InitSSLInterface and Assigned(_Asn1UtctimeNew) then
    Result := _Asn1UtctimeNew
  else
    Result := nil;
end;

procedure Asn1UtctimeFree(a: PASN1_UTCTIME);
begin
  if InitSSLInterface and Assigned(_Asn1UtctimeFree) then
    _Asn1UtctimeFree(a);
end;

function X509GmtimeAdj(s: PASN1_UTCTIME; adj: integer): PASN1_UTCTIME;
begin
  if InitSSLInterface and Assigned(_X509GmtimeAdj) then
    Result := _X509GmtimeAdj(s, adj)
  else
    Result := nil;
end;

function X509SetNotBefore(x: PX509; tm: PASN1_UTCTIME): integer;
begin
  if InitSSLInterface and Assigned(_X509SetNotBefore) then
    Result := _X509SetNotBefore(x, tm)
  else
    Result := 0;
end;

function X509SetNotAfter(x: PX509; tm: PASN1_UTCTIME): integer;
begin
  if InitSSLInterface and Assigned(_X509SetNotAfter) then
    Result := _X509SetNotAfter(x, tm)
  else
    Result := 0;
end;

function i2dX509bio(b: PBIO; x: PX509): integer;
begin
  if InitSSLInterface and Assigned(_i2dX509bio) then
    Result := _i2dX509bio(b, x)
  else
    Result := 0;
end;

function d2iX509bio(b: PBIO; x: PX509): PX509; {pf}
begin
  if InitSSLInterface and Assigned(_d2iX509bio) then
    Result := _d2iX509bio(x,b)
  else
    Result := nil;
end;

function PEMReadBioX509(b:PBIO; {var x:PX509;}x:PSslPtr; callback:PFunction; cb_arg: SslPtr):  PX509;    {pf}
begin
  if InitSSLInterface and Assigned(_PEMReadBioX509) then
    Result := _PEMReadBioX509(b,x,callback,cb_arg)
  else
    Result := nil;
end;

procedure SkX509PopFree(st: PSTACK; func:TSkPopFreeFunc); {pf}
begin
  if InitSSLInterface and Assigned(_SkX509PopFree) then
    _SkX509PopFree(st,func);
end;

function i2dPrivateKeyBio(b: PBIO; pkey: EVP_PKEY): integer;
begin
  if InitSSLInterface and Assigned(_i2dPrivateKeyBio) then
    Result := _i2dPrivateKeyBio(b, pkey)
  else
    Result := 0;
end;

function EvpGetDigestByName(Name: AnsiString): PEVP_MD;
begin
  if InitSSLInterface and Assigned(_EvpGetDigestByName) then
    Result := _EvpGetDigestByName(PAnsiChar(Name))
  else
    Result := nil;
end;

function Asn1IntegerSet(a: PASN1_INTEGER; v: integer): integer;
begin
  if InitSSLInterface and Assigned(_Asn1IntegerSet) then
    Result := _Asn1IntegerSet(a, v)
  else
    Result := 0;
end;

function Asn1IntegerGet(a: PASN1_INTEGER): integer; {pf}
begin
  if InitSSLInterface and Assigned(_Asn1IntegerGet) then
    Result := _Asn1IntegerGet(a)
  else
    Result := 0;
end;

function X509GetSerialNumber(x: PX509): PASN1_INTEGER;
begin
  if InitSSLInterface and Assigned(_X509GetSerialNumber) then
    Result := _X509GetSerialNumber(x)
  else
    Result := nil;
end;

// 3DES functions
procedure DESsetoddparity(Key: des_cblock);
begin
  if InitSSLInterface and Assigned(_DESsetoddparity) then
    _DESsetoddparity(Key);
end;

function DESsetkeychecked(key: des_cblock; schedule: des_key_schedule): Integer;
begin
  if InitSSLInterface and Assigned(_DESsetkeychecked) then
    Result := _DESsetkeychecked(key, schedule)
  else
    Result := -1;
end;

procedure DESecbencrypt(Input: des_cblock; output: des_cblock; ks: des_key_schedule; enc: Integer);
begin
  if InitSSLInterface and Assigned(_DESecbencrypt) then
    _DESecbencrypt(Input, output, ks, enc);
end;

procedure locking_callback(mode, ltype: integer; lfile: PChar; line: integer); cdecl;
begin
  if (mode and 1) > 0 then
    TCriticalSection(Locks[ltype]).Enter
  else
    TCriticalSection(Locks[ltype]).Leave;
end;

procedure InitLocks;
var
  n: integer;
  max: integer;
begin
  Locks := TList.Create;
  max := _CRYPTOnumlocks;
  for n := 1 to max do
    Locks.Add(TCriticalSection.Create);
  _CRYPTOsetlockingcallback(@locking_callback);
end;

procedure FreeLocks;
var
  n: integer;
begin
  _CRYPTOsetlockingcallback(nil);
  for n := 0 to Locks.Count - 1 do
    TCriticalSection(Locks[n]).Free;
  Locks.Free;
end;

{$ENDIF}

function LoadLib(const Value: String): HModule;
begin
{$IFDEF CIL}
  Result := LoadLibrary(Value);
{$ELSE}
  Result := LoadLibrary(PChar(Value));
{$ENDIF}
end;

function GetProcAddr(module: HModule; const ProcName: string): SslPtr;
begin
{$IFDEF CIL}
  Result := GetProcAddress(module, ProcName);
{$ELSE}
  Result := GetProcAddress(module, PChar(ProcName));
{$ENDIF}
end;

function InitSSLInterface: Boolean;
var
  s: string;
  x: integer;
begin
  {pf}
  if SSLLoaded then
    begin
      Result := TRUE;
      exit;
    end;
  {/pf}  
  SSLCS.Enter;
  try
    if not IsSSLloaded then
    begin
{$IFDEF CIL}
      SSLLibHandle := 1;
      SSLUtilHandle := 1;
{$ELSE}
      SSLUtilHandle := LoadLib(DLLUtilName);
      SSLLibHandle := LoadLib(DLLSSLName);
  {$IFDEF MSWINDOWS}
      if (SSLLibHandle = 0) then
        SSLLibHandle := LoadLib(DLLSSLName2);
  {$ENDIF}
{$ENDIF}
      if (SSLLibHandle <> 0) and (SSLUtilHandle <> 0) then
      begin
{$IFNDEF CIL}
        _SslGetError := GetProcAddr(SSLLibHandle, 'SSL_get_error');
        _SslLibraryInit := GetProcAddr(SSLLibHandle, 'SSL_library_init');
        _SslLoadErrorStrings := GetProcAddr(SSLLibHandle, 'SSL_load_error_strings');
        _SslCtxSetCipherList := GetProcAddr(SSLLibHandle, 'SSL_CTX_set_cipher_list');
        _SslCtxNew := GetProcAddr(SSLLibHandle, 'SSL_CTX_new');
        _SslCtxFree := GetProcAddr(SSLLibHandle, 'SSL_CTX_free');
        _SslSetFd := GetProcAddr(SSLLibHandle, 'SSL_set_fd');
        _SslMethodV2 := GetProcAddr(SSLLibHandle, 'SSLv2_method');
        _SslMethodV3 := GetProcAddr(SSLLibHandle, 'SSLv3_method');
        _SslMethodTLSV1 := GetProcAddr(SSLLibHandle, 'TLSv1_method');
        _SslMethodV23 := GetProcAddr(SSLLibHandle, 'SSLv23_method');
        _SslCtxUsePrivateKey := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_PrivateKey');
        _SslCtxUsePrivateKeyASN1 := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_PrivateKey_ASN1');
        //use SSL_CTX_use_RSAPrivateKey_file instead SSL_CTX_use_PrivateKey_file,
        //because SSL_CTX_use_PrivateKey_file not support DER format. :-O
        _SslCtxUsePrivateKeyFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_RSAPrivateKey_file');
        _SslCtxUseCertificate := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_certificate');
        _SslCtxUseCertificateASN1 := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_certificate_ASN1');
        _SslCtxUseCertificateFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_certificate_file');
        _SslCtxUseCertificateChainFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_certificate_chain_file');
        _SslCtxCheckPrivateKeyFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_check_private_key');
        _SslCtxSetDefaultPasswdCb := GetProcAddr(SSLLibHandle, 'SSL_CTX_set_default_passwd_cb');
        _SslCtxSetDefaultPasswdCbUserdata := GetProcAddr(SSLLibHandle, 'SSL_CTX_set_default_passwd_cb_userdata');
        _SslCtxLoadVerifyLocations := GetProcAddr(SSLLibHandle, 'SSL_CTX_load_verify_locations');
        _SslCtxCtrl := GetProcAddr(SSLLibHandle, 'SSL_CTX_ctrl');
        _SslNew := GetProcAddr(SSLLibHandle, 'SSL_new');
        _SslFree := GetProcAddr(SSLLibHandle, 'SSL_free');
        _SslAccept := GetProcAddr(SSLLibHandle, 'SSL_accept');
        _SslConnect := GetProcAddr(SSLLibHandle, 'SSL_connect');
        _SslShutdown := GetProcAddr(SSLLibHandle, 'SSL_shutdown');
        _SslRead := GetProcAddr(SSLLibHandle, 'SSL_read');
        _SslPeek := GetProcAddr(SSLLibHandle, 'SSL_peek');
        _SslWrite := GetProcAddr(SSLLibHandle, 'SSL_write');
        _SslPending := GetProcAddr(SSLLibHandle, 'SSL_pending');
        _SslGetPeerCertificate := GetProcAddr(SSLLibHandle, 'SSL_get_peer_certificate');
        _SslGetVersion := GetProcAddr(SSLLibHandle, 'SSL_get_version');
        _SslCtxSetVerify := GetProcAddr(SSLLibHandle, 'SSL_CTX_set_verify');
        _SslGetCurrentCipher := GetProcAddr(SSLLibHandle, 'SSL_get_current_cipher');
        _SslCipherGetName := GetProcAddr(SSLLibHandle, 'SSL_CIPHER_get_name');
        _SslCipherGetBits := GetProcAddr(SSLLibHandle, 'SSL_CIPHER_get_bits');
        _SslGetVerifyResult := GetProcAddr(SSLLibHandle, 'SSL_get_verify_result');
        _SslCtrl := GetProcAddr(SSLLibHandle, 'SSL_ctrl');

        _X509New := GetProcAddr(SSLUtilHandle, 'X509_new');
        _X509Free := GetProcAddr(SSLUtilHandle, 'X509_free');
        _X509NameOneline := GetProcAddr(SSLUtilHandle, 'X509_NAME_oneline');
        _X509GetSubjectName := GetProcAddr(SSLUtilHandle, 'X509_get_subject_name');
        _X509GetIssuerName := GetProcAddr(SSLUtilHandle, 'X509_get_issuer_name');
        _X509NameHash := GetProcAddr(SSLUtilHandle, 'X509_NAME_hash');
        _X509Digest := GetProcAddr(SSLUtilHandle, 'X509_digest');
        _X509print := GetProcAddr(SSLUtilHandle, 'X509_print');
        _X509SetVersion := GetProcAddr(SSLUtilHandle, 'X509_set_version');
        _X509SetPubkey := GetProcAddr(SSLUtilHandle, 'X509_set_pubkey');
        _X509SetIssuerName := GetProcAddr(SSLUtilHandle, 'X509_set_issuer_name');
        _X509NameAddEntryByTxt := GetProcAddr(SSLUtilHandle, 'X509_NAME_add_entry_by_txt');
        _X509Sign := GetProcAddr(SSLUtilHandle, 'X509_sign');
        _X509GmtimeAdj := GetProcAddr(SSLUtilHandle, 'X509_gmtime_adj');
        _X509SetNotBefore := GetProcAddr(SSLUtilHandle, 'X509_set_notBefore');
        _X509SetNotAfter := GetProcAddr(SSLUtilHandle, 'X509_set_notAfter');
        _X509GetSerialNumber := GetProcAddr(SSLUtilHandle, 'X509_get_serialNumber');
        _EvpPkeyNew := GetProcAddr(SSLUtilHandle, 'EVP_PKEY_new');
        _EvpPkeyFree := GetProcAddr(SSLUtilHandle, 'EVP_PKEY_free');
        _EvpPkeyAssign := GetProcAddr(SSLUtilHandle, 'EVP_PKEY_assign');
        _EVPCleanup := GetProcAddr(SSLUtilHandle, 'EVP_cleanup');
        _EvpGetDigestByName := GetProcAddr(SSLUtilHandle, 'EVP_get_digestbyname');
        _SSLeayversion := GetProcAddr(SSLUtilHandle, 'SSLeay_version');
        _ErrErrorString := GetProcAddr(SSLUtilHandle, 'ERR_error_string_n');
        _ErrGetError := GetProcAddr(SSLUtilHandle, 'ERR_get_error');
        _ErrClearError := GetProcAddr(SSLUtilHandle, 'ERR_clear_error');
        _ErrFreeStrings := GetProcAddr(SSLUtilHandle, 'ERR_free_strings');
        _ErrRemoveState := GetProcAddr(SSLUtilHandle, 'ERR_remove_state');
        _OPENSSLaddallalgorithms := GetProcAddr(SSLUtilHandle, 'OPENSSL_add_all_algorithms_noconf');
        _CRYPTOcleanupAllExData := GetProcAddr(SSLUtilHandle, 'CRYPTO_cleanup_all_ex_data');
        _RandScreen := GetProcAddr(SSLUtilHandle, 'RAND_screen');
        _BioNew := GetProcAddr(SSLUtilHandle, 'BIO_new');
        _BioFreeAll := GetProcAddr(SSLUtilHandle, 'BIO_free_all');
        _BioSMem := GetProcAddr(SSLUtilHandle, 'BIO_s_mem');
        _BioCtrlPending := GetProcAddr(SSLUtilHandle, 'BIO_ctrl_pending');
        _BioRead := GetProcAddr(SSLUtilHandle, 'BIO_read');
        _BioWrite := GetProcAddr(SSLUtilHandle, 'BIO_write');
        _d2iPKCS12bio := GetProcAddr(SSLUtilHandle, 'd2i_PKCS12_bio');
        _PKCS12parse := GetProcAddr(SSLUtilHandle, 'PKCS12_parse');
        _PKCS12free := GetProcAddr(SSLUtilHandle, 'PKCS12_free');
        _RsaGenerateKey := GetProcAddr(SSLUtilHandle, 'RSA_generate_key');
        _Asn1UtctimeNew := GetProcAddr(SSLUtilHandle, 'ASN1_UTCTIME_new');
        _Asn1UtctimeFree := GetProcAddr(SSLUtilHandle, 'ASN1_UTCTIME_free');
        _Asn1IntegerSet := GetProcAddr(SSLUtilHandle, 'ASN1_INTEGER_set');
        _Asn1IntegerGet := GetProcAddr(SSLUtilHandle, 'ASN1_INTEGER_get'); {pf}
        _i2dX509bio := GetProcAddr(SSLUtilHandle, 'i2d_X509_bio');
        _d2iX509bio := GetProcAddr(SSLUtilHandle, 'd2i_X509_bio'); {pf}
        _PEMReadBioX509 := GetProcAddr(SSLUtilHandle, 'PEM_read_bio_X509'); {pf}
        _SkX509PopFree := GetProcAddr(SSLUtilHandle, 'SK_X509_POP_FREE'); {pf}
        _i2dPrivateKeyBio := GetProcAddr(SSLUtilHandle, 'i2d_PrivateKey_bio');

        // 3DES functions
        _DESsetoddparity := GetProcAddr(SSLUtilHandle, 'DES_set_odd_parity');
        _DESsetkeychecked := GetProcAddr(SSLUtilHandle, 'DES_set_key_checked');
        _DESecbencrypt := GetProcAddr(SSLUtilHandle, 'DES_ecb_encrypt');
        //
        _CRYPTOnumlocks := GetProcAddr(SSLUtilHandle, 'CRYPTO_num_locks');
        _CRYPTOsetlockingcallback := GetProcAddr(SSLUtilHandle, 'CRYPTO_set_locking_callback');
{$ENDIF}
{$IFDEF CIL}
        SslLibraryInit;
        SslLoadErrorStrings;
        OPENSSLaddallalgorithms;
        RandScreen;
{$ELSE}
        SetLength(s, 1024);
        x := GetModuleFilename(SSLLibHandle,PChar(s),Length(s));
        SetLength(s, x);
        SSLLibFile := s;
        SetLength(s, 1024);
        x := GetModuleFilename(SSLUtilHandle,PChar(s),Length(s));
        SetLength(s, x);
        SSLUtilFile := s;
        //init library
        if assigned(_SslLibraryInit) then
          _SslLibraryInit;
        if assigned(_SslLoadErrorStrings) then
          _SslLoadErrorStrings;
        if assigned(_OPENSSLaddallalgorithms) then
          _OPENSSLaddallalgorithms;
        if assigned(_RandScreen) then
          _RandScreen;
        if assigned(_CRYPTOnumlocks) and assigned(_CRYPTOsetlockingcallback) then
          InitLocks;
{$ENDIF}
        Result := True;
        SSLloaded := True;
      end
      else
      begin
        //load failed!
        if SSLLibHandle <> 0 then
        begin
{$IFNDEF CIL}
          FreeLibrary(SSLLibHandle);
{$ENDIF}
          SSLLibHandle := 0;
        end;
        if SSLUtilHandle <> 0 then
        begin
{$IFNDEF CIL}
          FreeLibrary(SSLUtilHandle);
{$ENDIF}
          SSLLibHandle := 0;
        end;
        Result := False;
      end;
    end
    else
      //loaded before...
      Result := true;
  finally
    SSLCS.Leave;
  end;
end;

function DestroySSLInterface: Boolean;
begin
  SSLCS.Enter;
  try
    if IsSSLLoaded then
    begin
      //deinit library
{$IFNDEF CIL}
      if assigned(_CRYPTOnumlocks) and assigned(_CRYPTOsetlockingcallback) then
        FreeLocks;
{$ENDIF}
      EVPCleanup;
      CRYPTOcleanupAllExData;
      ErrRemoveState(0);
    end;
    SSLloaded := false;
    if SSLLibHandle <> 0 then
    begin
{$IFNDEF CIL}
      FreeLibrary(SSLLibHandle);
{$ENDIF}
      SSLLibHandle := 0;
    end;
    if SSLUtilHandle <> 0 then
    begin
{$IFNDEF CIL}
      FreeLibrary(SSLUtilHandle);
{$ENDIF}
      SSLLibHandle := 0;
    end;

{$IFNDEF CIL}
    _SslGetError := nil;
    _SslLibraryInit := nil;
    _SslLoadErrorStrings := nil;
    _SslCtxSetCipherList := nil;
    _SslCtxNew := nil;
    _SslCtxFree := nil;
    _SslSetFd := nil;
    _SslMethodV2 := nil;
    _SslMethodV3 := nil;
    _SslMethodTLSV1 := nil;
    _SslMethodV23 := nil;
    _SslCtxUsePrivateKey := nil;
    _SslCtxUsePrivateKeyASN1 := nil;
    _SslCtxUsePrivateKeyFile := nil;
    _SslCtxUseCertificate := nil;
    _SslCtxUseCertificateASN1 := nil;
    _SslCtxUseCertificateFile := nil;
    _SslCtxUseCertificateChainFile := nil;
    _SslCtxCheckPrivateKeyFile := nil;
    _SslCtxSetDefaultPasswdCb := nil;
    _SslCtxSetDefaultPasswdCbUserdata := nil;
    _SslCtxLoadVerifyLocations := nil;
    _SslCtxCtrl := nil;
    _SslNew := nil;
    _SslFree := nil;
    _SslAccept := nil;
    _SslConnect := nil;
    _SslShutdown := nil;
    _SslRead := nil;
    _SslPeek := nil;
    _SslWrite := nil;
    _SslPending := nil;
    _SslGetPeerCertificate := nil;
    _SslGetVersion := nil;
    _SslCtxSetVerify := nil;
    _SslGetCurrentCipher := nil;
    _SslCipherGetName := nil;
    _SslCipherGetBits := nil;
    _SslGetVerifyResult := nil;
    _SslCtrl := nil;

    _X509New := nil;
    _X509Free := nil;
    _X509NameOneline := nil;
    _X509GetSubjectName := nil;
    _X509GetIssuerName := nil;
    _X509NameHash := nil;
    _X509Digest := nil;
    _X509print := nil;
    _X509SetVersion := nil;
    _X509SetPubkey := nil;
    _X509SetIssuerName := nil;
    _X509NameAddEntryByTxt := nil;
    _X509Sign := nil;
    _X509GmtimeAdj := nil;
    _X509SetNotBefore := nil;
    _X509SetNotAfter := nil;
    _X509GetSerialNumber := nil;
    _EvpPkeyNew := nil;
    _EvpPkeyFree := nil;
    _EvpPkeyAssign := nil;
    _EVPCleanup := nil;
    _EvpGetDigestByName := nil;
    _SSLeayversion := nil;
    _ErrErrorString := nil;
    _ErrGetError := nil;
    _ErrClearError := nil;
    _ErrFreeStrings := nil;
    _ErrRemoveState := nil;
    _OPENSSLaddallalgorithms := nil;
    _CRYPTOcleanupAllExData := nil;
    _RandScreen := nil;
    _BioNew := nil;
    _BioFreeAll := nil;
    _BioSMem := nil;
    _BioCtrlPending := nil;
    _BioRead := nil;
    _BioWrite := nil;
    _d2iPKCS12bio := nil;
    _PKCS12parse := nil;
    _PKCS12free := nil;
    _RsaGenerateKey := nil;
    _Asn1UtctimeNew := nil;
    _Asn1UtctimeFree := nil;
    _Asn1IntegerSet := nil;
    _Asn1IntegerGet := nil; {pf}
    _SkX509PopFree := nil; {pf}
    _i2dX509bio := nil;
    _i2dPrivateKeyBio := nil;

    // 3DES functions
    _DESsetoddparity := nil;
    _DESsetkeychecked := nil;
    _DESecbencrypt := nil;
    //
    _CRYPTOnumlocks := nil;
    _CRYPTOsetlockingcallback := nil;
{$ENDIF}
  finally
    SSLCS.Leave;
  end;
  Result := True;
end;

function IsSSLloaded: Boolean;
begin
  Result := SSLLoaded;
end;

initialization
begin
  SSLCS:= TCriticalSection.Create;
end;

finalization
begin
{$IFNDEF CIL}
  DestroySSLInterface;
{$ENDIF}
  SSLCS.Free;
end;

end.
