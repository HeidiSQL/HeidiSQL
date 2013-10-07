/// automated tests for common units of the Synopse Framework
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynSelfTests;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.8
  - first public release, corresponding to SQLite3 Framework 1.8
  - includes Unitary Testing class and functions

  Version 1.9
  - test multi-threaded AES encryption/decryption of 4 MB blocks
  - added crc32 tests

  Version 1.11
  - added some more regression tests in TTestCompression.ZipFormat

  Version 1.12
  - handle producer version change in TTestSynopsePDF

  Version 1.13
  - code modifications to compile with Delphi 5 compiler
  - enhanced compression tests

  Version 1.15
  - unit now tested with Delphi XE2 (32 Bit)
  - new TTestSQLite3ExternalDB class to test TSQLRecordExternal records,
    i.e. external DB access from the mORMot framework (use an in-memory SQLite3
    database as an external SynDB engine for fast and reliable testing)
  - added test of TModTime published property (i.e. latest update time)
  - added test of TCreateTime published property (i.e. record creation time)

  Version 1.16
  - added interface-based remote service implementation tests
  - added test about per-database encryption in TTestExternalDatabase.CryptedDatabase
  - added TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes tests (+ PKCS7)
  - enhanced SynLZ tests (comparing asm and pas versions of the implementation)

  Version 1.17
  - added test for TInterfaceCollection kind of parameter
  - added multi-thread testing of ExecuteInMainThread() method
  - removed TSQLRecordExternal class type, to allow any TSQLRecord (e.g.
    TSQLRecordMany) to be used with VirtualTableExternalRegister()
  - added DBMS full test coverage in TTestExternalDatabase.AutoAdaptSQL
}

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64

uses
  Windows,
  Classes,
  SysUtils,
{$ifndef FPC}
{$ifndef LVCL}
  Contnrs,
  SynDB,
{$endif}
{$ifndef DELPHI5OROLDER}
{$ifndef CPU64}
  SynSQLite3,
{$ifndef LVCL}
  SynDBSQLite3,
  SQLite3DB,
{$endif}
  SQLite3Commons,
  SQLite3,
  SQLite3HttpServer,
  SQLite3HttpClient,
{$endif}
{$endif}
{$endif}
  SynCommons;

type
  /// this test case will test most functions, classes and types defined and
  // implemented in the SynZip unit
  TTestCompression = class(TSynTestCase)
  protected
    Data: RawByteString;
    M: THeapMemoryStream;
    crc: cardinal;
  public
    destructor Destroy; override;
  published
    /// direct LZ77 deflate/inflate functions
    procedure InMemoryCompression;
    /// .gzip archive handling
    procedure GZipFormat;
    /// .zip archive handling
    procedure ZipFormat;
    /// SynLZO internal format
    procedure _SynLZO;
    /// SynLZ internal format
    procedure _SynLZ;
  end;

  /// this test case will test most functions, classes and types defined and
  // implemented in the SynCrypto unit
  TTestCryptographicRoutines = class(TSynTestCase)
  published
    /// Adler32 hashing functions
    procedure Adler32;
    /// MD5 hashing functions
    procedure _MD5;
    /// SHA-1 hashing functions
    procedure _SHA1;
    /// SHA-256 hashing functions
    procedure _SHA256;
    /// AES encryption/decryption functions
    procedure _AES256;
    /// Base-64 encoding/decoding functions
    procedure Base64;
  end;

{$ifndef FPC}
{$ifndef LVCL}
  /// this test case will test most functions, classes and types defined and
  // implemented in the SynPDF unit
  TTestSynopsePDF = class(TSynTestCase)
  published
    /// create a PDF document, using the PDF Canvas property
    // - test font handling, especially standard font substitution
    procedure _TPdfDocument;
    /// create a PDF document, using a EMF content
    // - validates the EMF/TMetaFile enumeration, and its conversion into the
    // PDF content
    // - this method will produce a .pdf file in the executable directory,
    // if you want to check out the result (it's simply a curve drawing, with
    // data from NIST)
    procedure _TPdfDocumentGDI;
  end;
{$endif}
{$endif}

{$ifndef FPC}
{$ifndef DELPHI5OROLDER}
{$ifndef CPU64}
type
  /// this test case will test most functions, classes and types defined and
  // implemented in the SQLite3 unit, i.e. the SQLite3 engine itself,
  // used as a HTTP/1.1 server and client
  // - test a HTTP/1.1 server and client on the port 888 of the local machine
  // - require the 'test.db3' SQLite3 database file, as created by TTestFileBased
  TTestClientServerAccess = class(TSynTestCase)
  protected
    { these values are used internaly by the published methods below }
    Model: TSQLModel;
    DataBase: TSQLRestServerDB;
    Server: TSQLite3HttpServer;
    Client: TSQLRestClient;
    /// perform the tests of the current Client instance
    procedure ClientTest;
  public
    /// release used instances (e.g. http server) and memory
    destructor Destroy; override;
    /// this could be called as administrator for THttpApiServer to work
    class function RegisterAddUrl(OnlyDelete: boolean): string;
  published
    /// initialize a TSQLite3HttpServer instance
    // - uses the 'test.db3' SQLite3 database file generated by TTestSQLite3Engine
    // - creates and validates a HTTP/1.1 server on the port 888 of the local
    // machine, using the THttpApiServer (using kernel mode http.sys) class
    // if available
    procedure _TSQLite3HttpServer;
    /// validate the HTTP/1.1 client implementation
    // - by using a request of all records data
    procedure _TSQLite3HttpClient;
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection for the all queries
    // - this method keep alive the HTTP connection, so is somewhat faster
    // - it runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure HttpClientKeepAlive;
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection initialized per query
    // - this method don't keep alive the HTTP connection, so is somewhat slower:
    // a new HTTP connection is created for every query
    // - it runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure HttpClientMultiConnect;
{
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection for all queries, and the THttpServer class instead
    // of the THttpApiServer kernel mode server
    procedure HttpClientKeepAliveDelphi;
    /// validate the HTTP/1.1 client multi-query implementation with one
    // connection initialized per query, and the THttpServer class instead
    // of the THttpApiServer kernel mode server
    // - this method don't keep alive the HTTP connection, so is somewhat slower:
    // a new HTTP connection is created for every query
    procedure HttpClientMultiConnectDelphi;
}
    /// validate the Named-Pipe client implementation
    // - it first launch the Server as Named-Pipe
    // - it then runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure NamedPipeAccess;
    /// validate the Windows GDI Messages based client implementation
    // - it first launch the Server to handle GDI messages
    // - it then runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure LocalWindowMessages;
    /// validate the client implementation, using direct access to the server
    // - it connects directly the client to the server, therefore use the same
    // process and memory during the run: it's the fastest possible way of
    // communicating
    // - it then runs 1000 remote SQL queries, and check the JSON data retrieved
    // - the time elapsed for this step is computed, and displayed on the report
    procedure DirectInProcessAccess;
  end;

{$ifndef LVCL}
  /// a test case which will test most external DB functions of the SQLite3DB unit
  // - the external DB will be in fact a SynDBSQLite3 instance, expecting a
  // test.db3 SQlite3 file available in the current directory, populated with
  // some TSQLRecordPeople rows
  // - note that SQL statement caching at SQLite3 engine level makes those test
  // 2 times faster
  TTestExternalDatabase = class(TSynTestCase)
  protected
    fModel: TSQLModel;
    procedure Test(StaticVirtualTableDirect: boolean);
  public
    /// release the associated memory and object instances
    destructor Destroy; override;
  published
    /// initialize needed RESTful client (and server) instances
    // - i.e. a RESTful direct access to an external DB
    procedure ExternalRecords;
    /// check the SQL auto-adaptation features
    procedure AutoAdaptSQL;
    /// check the per-db encryption
    // - the testpass.db3-wal file is not encrypted, but the main
    // testpass.db3 file will (after the first 1024 bytes)
    procedure CryptedDatabase;
    /// test external DB implementation via faster REST calls
    // - will mostly call directly the TSQLRestServerStaticExternal instance,
    // bypassing the Virtual Table mechanism of SQLite3
    procedure ExternalViaREST;
    /// test external DB implementation via slower Virtual Table calls
    // - using the Virtual Table mechanism of SQLite3 is more than 2 times
    // slower than direct REST access
    procedure ExternalViaVirtualTable;
  end;

  TCollTest = class(TCollectionItem)
  private
    FLength: Integer;
    FColor: Integer;
    FName: RawUTF8;
  published
    property Color: Integer read FColor write FColor;
    property Length: Integer read FLength write FLength;
    property Name: RawUTF8 read FName write FName;
  end;

  TCollTests = class(TInterfacedCollection)
  protected
    function GetClass: TCollectionItemClass; override;
  end;
{$endif LVCL}

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test basic and high-level remote service calls
  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    /// add two signed 32 bit integers
    function Add(n1,n2: integer): integer;
    /// multiply two signed 64 bit integers
    function Multiply(n1,n2: Int64): Int64;
    /// substract two floating-point values
    function Subtract(n1,n2: double): double;
    /// convert a currency value into text
    procedure ToText(Value: Currency; var Result: RawUTF8);
    /// convert a floating-point value into text
    function ToTextFunc(Value: double): string;
    /// do some work with strings, sets and enumerates parameters,
    // testing also var (in/out) parameters and set as a function result
    function SpecialCall(Txt: RawUTF8; var Int: integer; var Card: cardinal; field: TSynTableFieldTypes;
      fields: TSynTableFieldTypes; var options: TSynTableFieldOptions): TSynTableFieldTypes;
    /// test integer, strings and wide strings dynamic arrays, together with records
    function ComplexCall(const Ints: TIntegerDynArray; Strs1: TRawUTF8DynArray;
      var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
      var Rec2: TSQLRestCacheEntryValue): TSQLRestCacheEntryValue;
  end;

  /// a test class, used by TTestServiceOrientedArchitecture
  // - to test TPersistent objects used as parameters for remote service calls 
  TComplexNumber = class(TPersistent)
  private
    fReal: Double;
    fImaginary: Double;
  public
    /// create an instance to store a complex number
    constructor Create(aReal, aImaginary: double); reintroduce;
  published
    /// the real part of this complex number
    property Real: Double read fReal write fReal;
    /// the imaginary part of this complex number
    property Imaginary: Double read fImaginary write fImaginary;
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test remote service calls with objects as parameters (its published
  // properties will be serialized as standard JSON objects)
  // - since it inherits from ICalculator interface, it will also test
  // the proper interface inheritance handling (i.e. it will test that
  // ICalculator methods are also available)
  IComplexCalculator = interface(ICalculator)
    ['{8D0F3839-056B-4488-A616-986CF8D4DEB7}']
    /// purpose of this method is to substract two complex numbers
    // - using class instances as parameters
    procedure Substract(n1,n2: TComplexNumber; out Result: TComplexNumber);
    /// purpose of this method is to check for boolean handling
    function IsNull(n: TComplexNumber): boolean;
    /// this will test the BLOB kind of remote answer
    function TestBlob(n: TComplexNumber): TServiceCustomAnswer;
{$ifndef LVCL}
    /// test in/out collections
    procedure Collections(Item: TCollTest; var List: TCollTests; out Copy: TCollTests);
{$endif}
  end;

  /// a test interface, used by TTestServiceOrientedArchitecture
  // - to test sicClientDriven implementation pattern: data will remain on
  // the server until the IComplexNumber instance is out of scope
  IComplexNumber = interface(IInvokable)
    ['{29D753B2-E7EF-41B3-B7C3-827FEB082DC1}']
    procedure Assign(aReal, aImaginary: double);
    function GetImaginary: double;
    function GetReal: double;
    procedure SetImaginary(const Value: double);
    procedure SetReal(const Value: double);
    procedure Add(aReal, aImaginary: double);
    property Real: double read GetReal write SetReal;
    property Imaginary: double read GetImaginary write SetImaginary;
  end;


const
  IID_ICalculator: TGUID = '{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}';

type
  /// a test case which will test the interface-based SOA implementation of
  // the mORMot framework
  TTestServiceOrientedArchitecture = class(TSynTestCase)
  protected
    fModel: TSQLModel;
    fClient: TSQLRestClientDB;
    procedure Test(const I: ICalculator; const CC: IComplexCalculator; const CN: IComplexNumber);
    procedure ClientTest(aRouting: TServiceRoutingMode; RunInMainThread: boolean=false);
    class function CustomReader(P: PUTF8Char; var aValue; out aValid: Boolean): PUTF8Char;
    class procedure CustomWriter(const aWriter: TTextWriter; const aValue);
  public
    /// release the associated memory and object instances
    destructor Destroy; override;
  published
    /// test the SetWeak/SetWeakZero weak interface functions
    procedure WeakInterfaces;
    /// initialize the SOA implementation
    procedure ServiceInitialization;
    /// test direct call to the class instance
    procedure DirectCall;
    /// test the server-side implementation
    procedure ServerSide;
    /// test the client-side implementation in RESTful mode
    procedure ClientSideREST;
    /// test the client-side implementation in JSON-RPC mode
    procedure ClientSideJSONRPC;
    {$ifndef LVCL}
    /// test the client-side implementation of ExecuteInMainThread() method
    procedure ClientSideSynchronizedREST;
    {$endif}
    /// test the security features
    procedure Security;
    /// test the custom record JSON serialization
    procedure CustomRecordLayout;
  end;

{$endif CPU64}
{$endif DELPHI5OROLDER}
{$endif FPC}


implementation

uses
{$ifndef FPC}
{$ifndef LVCL}
  SynPdf,
{$endif}
{$ifdef ISDELPHIXE2}
  VCL.Graphics,
{$else}
  Graphics,
{$endif}
{$endif}
  SynCrypto,
  SynCrtSock,
  SynLZ,
  SynLzo,
  SynZip;


{ TTestCompression }

destructor TTestCompression.Destroy;
begin
  M.Free;
  inherited;
end;

const
  // uses a const table instead of a dynamic array, for better regression test
  crc32Tab: array[byte] of cardinal =
    ($00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);
function UpdateCrc32(aCRC32: cardinal; inBuf: pointer; inLen: integer) : cardinal;
var i: integer;
begin // slowest but always accurate version
  result := not aCRC32;
  for i := 1 to inLen do begin
    result := crc32Tab[byte(result xor pByte(inBuf)^)] xor (result shr 8);
    inc(PByte(inBuf));
  end;
  result := not result;
end;

procedure TTestCompression.GZipFormat;
var Z: TSynZipCompressor;
    L,n: integer;
    P: PAnsiChar;
    crc2: Cardinal;
    s: RawByteString;
begin
  Check(crc32(0,@crc32Tab,5)=$DF4EC16C,'crc32');
  Check(UpdateCrc32(0,@crc32Tab,5)=$DF4EC16C,'crc32');
  Check(crc32(0,@crc32Tab,1024)=$6FCF9E13,'crc32');
  Check(UpdateCrc32(0,@crc32Tab,1024)=$6FCF9E13);
  Check(crc32(0,@crc32Tab,1024-5)=$70965738,'crc32');
  Check(UpdateCrc32(0,@crc32Tab,1024-5)=$70965738);
  Check(crc32(0,pointer(PtrInt(@crc32Tab)+1),2)=$41D912FF,'crc32');
  Check(UpdateCrc32(0,pointer(PtrInt(@crc32Tab)+1),2)=$41D912FF);
  Check(crc32(0,pointer(PtrInt(@crc32Tab)+3),1024-5)=$E5FAEC6C,'crc32');
  Check(UpdateCrc32(0,pointer(PtrInt(@crc32Tab)+3),1024-5)=$E5FAEC6C);
  M := SynCommons.THeapMemoryStream.Create;
  Z := TSynZipCompressor.Create(M,6,szcfGZ);
  L := length(Data);
  P := Pointer(Data);
  crc := 0;
  crc2 := 0;
  while L<>0 do begin
    if L>1000 then
      n := 1000 else
      n := L;
    Z.Write(P^,n); // compress by little chunks to test streaming
    crc := crc32(crc,P,n);
    crc2 := UpdateCrc32(crc2,P,n);
    inc(P,n);
    dec(L,n);
  end;
  Check(crc=Z.CRC,'crc32');
  Check(crc2=crc,'crc32');
  Z.Free;
  Check(GZRead(M.Memory,M.Position)=Data);
  s := Data;
  Check(CompressGZip(s,true)='gzip');
  Check(CompressGZip(s,false)='gzip');
  Check(s=Data);
  s := Data;
  Check(CompressDeflate(s,true)='deflate');
  Check(CompressDeflate(s,false)='deflate');
  Check(s=Data);
end;

procedure TTestCompression.InMemoryCompression;
var comp: Integer;
    tmp: RawByteString;
begin
  Check(CRC32string('TestCRC32')=$2CB8CDF3);
  tmp := RawByteString(Ident);
  for comp := 0 to 9 do
    Check(UnCompressString(CompressString(tmp,False,comp))=tmp);
  Data := StringFromFile(ParamStr(0));
  Check(UnCompressString(CompressString(Data,False,6))=Data);
end;

procedure TTestCompression.ZipFormat;
var FN: TFileName;
procedure Test(aCount: integer);
var i: integer;
    tmp: RawByteString;
    crc: Cardinal;
begin
  with TZipRead.Create(FN) do
  try
    Check(Count=aCount);
    i := NameToIndex('REP1\ONE.exe');
    Check(i=0);
    Check(UnZip(i)=Data);
    i := NameToIndex('REp2\ident.gz');
    Check(i=1);
    crc := crc32(0,M.Memory,M.Position);
    Check(Entry[i].info^.zcrc32=crc);
    tmp := UnZip(i);
    Check(tmp<>'');
    Check(crc32(0,pointer(tmp),length(tmp))=crc);
    i := NameToIndex(ExtractFileName(paramstr(0)));
    Check(i=2);
    Check(UnZip(i)=Data);
    Check(Entry[i].info^.zcrc32=crc32(0,pointer(Data),length(Data)));
    i := NameToIndex('REp2\ident2.gz');
    Check(i=3);
    Check(Entry[i].info^.zcrc32=crc);
    tmp := UnZip(i);
    Check(tmp<>'');
    Check(crc32(0,pointer(tmp),length(tmp))=crc);
    if aCount=4 then
      Exit;
    i := NameToIndex('REP1\twO.exe');
    Check(i=4);
    Check(UnZip(i)=Data);
  finally
    Free;
  end;
end;
begin
  FN := ChangeFileExt(paramstr(0),'.zip');
  with TZipWrite.Create(FN) do
  try
    AddDeflated('rep1\one.exe',pointer(Data),length(Data));
    Check(Count=1);
    AddDeflated('rep2\ident.gz',M.Memory,M.Position);
    Check(Count=2);
    AddDeflated(ParamStr(0));
    Check(Count=3,'direct zip file');
    AddStored('rep2\ident2.gz',M.Memory,M.Position);
    Check(Count=4);
  finally
    Free;
  end;
  Test(4);
  with TZipWrite.CreateFrom(FN) do
  try
    Check(Count=4);
    AddDeflated('rep1\two.exe',pointer(Data),length(Data));
    Check(Count=5);
  finally
    Free;
  end;
  Test(5);
  DeleteFile(FN);
end;

procedure TTestCompression._SynLZO;
var s,t: AnsiString;
    i: integer;
begin
  for i := 0 to 1000 do begin
    t := RandomString(i*8);
    s := t;
    Check(CompressSynLZO(s,true)='synlzo');
    Check(CompressSynLZO(s,false)='synlzo');
    Check(s=t);
  end;
  s := Data;
  Check(CompressSynLZO(s,true)='synlzo');
  Check(CompressSynLZO(s,false)='synlzo');
  Check(s=Data);
end;

{$ifndef CONDITIONALEXPRESSIONS} // Delphi 5 linking error?
function CompressSynLZ(var Data: AnsiString; Compress: boolean): AnsiString;
var DataLen, len: integer;
    P: PAnsiChar;
begin
  DataLen := length(Data);
  if DataLen<>0 then // '' is compressed and uncompressed to ''
  if Compress then begin
    len := SynLZcompressdestlen(DataLen)+8;
    SetString(result,nil,len);
    P := pointer(result);
    PCardinal(P)^ := Hash32(pointer(Data),DataLen);
    len := SynLZcompress1asm(pointer(Data),DataLen,P+8);
    PCardinal(P+4)^ := Hash32(pointer(P+8),len);
    SetString(Data,P,len+8);
  end else begin
    result := '';
    P := pointer(Data);
    if (DataLen<=8) or (Hash32(pointer(P+8),DataLen-8)<>PCardinal(P+4)^) then
      exit;
    len := SynLZdecompressdestlen(P+8);
    SetLength(result,len);
    if (len<>0) and ((SynLZdecompress1asm(P+8,DataLen-8,pointer(result))<>len) or
       (Hash32(pointer(result),len)<>PCardinal(P)^)) then begin
      result := '';
      exit;
    end else
      SetString(Data,PAnsiChar(pointer(result)),len);
  end;
  result := 'synlz';
end;
{$endif}

procedure TTestCompression._SynLZ;
var s,t: RawByteString;
    i: integer;
    {$ifndef PUREPASCAL}
    comp1,comp2,dec1,dec2: array of byte;
    complen1,complen2: integer;
    {$endif}
begin
  for i := 0 to 1000 do begin
    t := RandomString(i*8);
    SetString(s,PAnsiChar(pointer(t)),length(t)); // =UniqueString
    Check(CompressSynLZ(s,true)='synlz');
    Check(CompressSynLZ(s,false)='synlz');
    Check(s=t);
    {$ifndef PUREPASCAL}
    SetLength(comp1,SynLZcompressdestlen(length(s)));
    complen1 := SynLZcompress1asm(Pointer(s),length(s),pointer(comp1));
    Check(complen1<length(comp1));
    SetLength(comp2,SynLZcompressdestlen(length(s)));
    complen2 := SynLZcompress1pas(Pointer(s),length(s),pointer(comp2));
    Check(complen2<length(comp2));
    Check(complen1=complen2);
    Check(CompareMem(pointer(comp1),pointer(comp2),complen1));
    Check(SynLZdecompressdestlen(pointer(comp1))=length(s));
    Check(SynLZdecompressdestlen(pointer(comp2))=length(s));
    SetLength(dec1,Length(s));
    Check(SynLZdecompress1pas(Pointer(comp1),complen1,pointer(dec1))=length(s));
    Check(CompareMem(pointer(dec1),pointer(s),length(s)));
    SetLength(dec2,Length(s));
    Check(SynLZdecompress1asm(Pointer(comp2),complen2,pointer(dec2))=length(s));
    Check(CompareMem(pointer(dec1),pointer(s),length(s)));
    {$endif}
  end;
  s := Data;
  Check(CompressSynLZ(s,true)='synlz');
  Check(CompressSynLZ(s,false)='synlz');
  Check(s=Data);
end;


{ TTestCryptographicRoutines }

procedure TTestCryptographicRoutines.Adler32;
begin
  Check(Adler32SelfTest);
end;

procedure TTestCryptographicRoutines.Base64;
const
  Value: WinAnsiString = 'Hello /c'''+chr(233)+'tait '+chr(231)+chr(224)+'+';
  Value64: RawByteString = 'SGVsbG8gL2Mn6XRhaXQg5+Ar';
var tmp, b64: RawByteString;
    i, L: Integer;
begin
  Check(not IsBase64(Value));
  Check(Base64Encode(Value)=Value64);
  Check(BinToBase64(Value)=Value64);
  Check(IsBase64(Value64));
  tmp := StringFromFile(paramstr(0));
  b64 := Base64Encode(tmp);
  Check(IsBase64(b64));
  Check(Base64Decode(b64)=tmp);
  Check(BinToBase64(tmp)=b64);
  Check(Base64ToBin(b64)=tmp);
  tmp := '';
  for i := 1 to 1998 do begin
    b64 := Base64Encode(tmp);
    Check(Base64Decode(b64)=tmp);
    Check((tmp='') or IsBase64(b64));
    Check(BinToBase64(tmp)=b64);
    Check(Base64ToBin(b64)=tmp);
    if tmp<>'' then begin
      L := length(b64);
      Check(not IsBase64(pointer(b64),L-1));
      b64[Random(L)+1] := '&';
      Check(not IsBase64(pointer(b64),L));
    end;
    tmp := tmp+AnsiChar(Random(255));
  end;
end;

procedure TTestCryptographicRoutines._AES256;
var A: TAES;
    st, orig, crypted, s2: RawByteString;
    Key: TSHA256Digest;
    s,b,p: TAESBlock;
    i,k,ks,m, len: integer;
    AES: TAESFull;
    PC: PAnsiChar;
const MAX = 4096*1024;  // test 4 MB data, i.e. multi-threaded AES
      MODES: array[0..4] of TAESAbstractClass =
        (TAESECB, TAESCBC, TAESCFB, TAESOFB, TAESCTR);
begin
  SetLength(orig,MAX);
  SetLength(crypted,MAX+256);
  st := '1234essai';
  pInteger(@st[1])^ := Random(MaxInt);
  for k := 0 to 2 do begin
    ks := 128+k*64; // test keysize of 128, 192 and 256 bits
    for i := 1 to 100 do begin
      SHA256Weak(st,Key);
      move(Key,s,16);
      A.EncryptInit(Key,ks);
      A.Encrypt(s,b);
      A.Done;
      A.DecryptInit(Key,ks);
      A.Decrypt(b,p);
      A.Done;
      Check(CompareMem(@p,@s,AESBLockSize));
      Check(AESSHA256(AESSHA256(st,st,true),st,False)=st);
      st := st+AnsiChar(Random(255));
    end;
    PC := Pointer(orig);
    len := MAX;
    repeat // populate orig with random data
      if len>length(st) then
        i := length(st) else
        i := len;
      dec(len,i);
      move(pointer(st)^,PC^,i);
      inc(PC,i);
    until len=0;
    len := AES.EncodeDecode(Key,ks,MAX,True,nil,nil,pointer(orig),pointer(crypted));
    Check(len<MAX+256);
    Check(len>=MAX);
    len := AES.EncodeDecode(Key,ks,len,False,nil,nil,pointer(crypted),nil);
    try
      Check(len=MAX);
      Check(CompareMem(AES.outStreamCreated.Memory,pointer(orig),MAX));
      for m := low(MODES) to high(MODES) do
      with MODES[m].Create(Key,ks,b) do
      try
        for i := 0 to 127 do begin
          FillChar(pointer(crypted)^,i,0);
          Encrypt(AES.outStreamCreated.Memory,pointer(crypted),i);
          FillChar(pointer(orig)^,i,0);
          Decrypt(pointer(crypted),pointer(orig),i);
          Check((i=0) or (not IsZero(pointer(orig),i)));
          Check(CompareMem(AES.outStreamCreated.Memory,pointer(orig),i));
          SetString(s2,PAnsiChar(pointer(orig)),i);
          Check(DecryptPKCS7(EncryptPKCS7(s2))=s2);
        end;
      finally
        Free;
      end;
    finally
      AES.outStreamCreated.Free;
    end;
  end;
end;

procedure TTestCryptographicRoutines._MD5;
begin
  Check(MD5SelfTest);
end;

procedure TTestCryptographicRoutines._SHA1;
procedure SingleTest(const s: AnsiString; TDig: TSHA1Digest);
var SHA: TSHA1;
    Digest: TSHA1Digest;
    i: integer;
begin
  // 1. Hash complete AnsiString
  SHA.Full(pointer(s),length(s),Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
  // 2. one update call for all chars
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
  // 3. test consistency with Padlock engine down results 
{$ifdef USEPADLOCK}
  if not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true; // restore previous value
{$endif}
end;
const
  Test1Out: TSHA1Digest=
    ($A9,$99,$3E,$36,$47,$06,$81,$6A,$BA,$3E,$25,$71,$78,$50,$C2,$6C,$9C,$D0,$D8,$9D);
  Test2Out: TSHA1Digest=
    ($84,$98,$3E,$44,$1C,$3B,$D2,$6E,$BA,$AE,$4A,$A1,$F9,$51,$29,$E5,$E5,$46,$70,$F1);
var
  s: AnsiString;
  SHA: TSHA1;
  Digest: TSHA1Digest;
begin
  SingleTest('abc',Test1Out);
  SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',Test2Out);
  s := 'Wikipedia, l''encyclopedie libre et gratuite';
  SHA.Full(pointer(s),length(s),Digest);
  Check(SHA1DigestToString(Digest)='c18cc65028bbdc147288a2d136313287782b9c73');
end;

procedure TTestCryptographicRoutines._SHA256;
procedure SingleTest(const s: AnsiString; const TDig: TSHA256Digest);
var SHA: TSHA256;
  Digest: TSHA256Digest;
  i: integer;
begin
  // 1. Hash complete AnsiString
  SHA.Full(pointer(s),length(s),Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
  // 2. one update call for all chars
  SHA.Init;
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
  // 3. test consistency with Padlock engine down results
{$ifdef USEPADLOCK}
  if not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  Check(CompareMem(@Digest,@TDig,sizeof(Digest)));
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true;
{$endif}
end;
var Digest: TSHA256Digest;
const
  D1: TSHA256Digest =
    ($ba,$78,$16,$bf,$8f,$01,$cf,$ea,$41,$41,$40,$de,$5d,$ae,$22,$23,
     $b0,$03,$61,$a3,$96,$17,$7a,$9c,$b4,$10,$ff,$61,$f2,$00,$15,$ad);
  D2: TSHA256Digest =
    ($24,$8d,$6a,$61,$d2,$06,$38,$b8,$e5,$c0,$26,$93,$0c,$3e,$60,$39,
     $a3,$3c,$e4,$59,$64,$ff,$21,$67,$f6,$ec,$ed,$d4,$19,$db,$06,$c1);
  D3: TSHA256Digest =
    ($94,$E4,$A9,$D9,$05,$31,$23,$1D,$BE,$D8,$7E,$D2,$E4,$F3,$5E,$4A,
     $0B,$F4,$B3,$BC,$CE,$EB,$17,$16,$D5,$77,$B1,$E0,$8B,$A9,$BA,$A3);
begin
//  result := true; exit;
  SingleTest('abc', D1);
  SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2);
  SHA256Weak('lagrangehommage',Digest); // test with len=256>64
  Check(Comparemem(@Digest,@D3,sizeof(Digest)));
end;


{$ifndef FPC}
{$ifndef LVCL}
{ TTestSynopsePDF }

const
  Date = 40339.803675; // forced date to have the exact same PDF content (Hash32)

procedure TTestSynopsePDF._TPdfDocument;
var MS: THeapMemoryStream;
    i,y: integer;
    embed: boolean;
    WS: SynUnicode;
const
  Hash: array[boolean] of Cardinal =
    ($1780F68C,$A1201D84);
  Name: array[boolean] of PDFString =
    ('Arial','Helvetica');
begin
  MS := THeapMemoryStream.Create;
  with TPdfDocument.Create do
  try
    for embed := false to true do begin
      Info.CreationDate := Date; // no date nor creator variation in hashed value
      Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
      StandardFontsReplace := embed;
      AddPage;
      Canvas.SetFont('arial',10,[]);
      Check(Canvas.Page.Font.Name=Name[embed]);
      y := 800;
      for i := 1 to 30 do begin
        Canvas.SetFont('Arial',9+i,[]);
        WS := 'Texte accentue n°'+IntToString(i);
        PWordArray(WS)^[13] := 233;
        Canvas.TextOutW(100,y,pointer(WS));
        dec(y,9+i);
      end;
      SaveToStream(MS,Date);
      // MS.SaveToFile(ChangeFileExt(paramstr(0),'.pdf'));
      Check(Hash32(MS.Memory,MS.Position)=Hash[embed]);
      if not embed then begin
        if CharSet<>ANSI_CHARSET then
          break; // StandardFontsReplace will work only with ANSI code page
        NewDoc;
        MS.Clear;
      end;
    end;
  finally
    Free;
    MS.Free;
  end;
end;

procedure TTestSynopsePDF._TPdfDocumentGDI;
const
  EMF: RawByteString = // some compressed EMF file content
  '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'+
  'TcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZig0sady5L92TWYpNw5NZik3Dk1mKTcOTWYpNw5NZik3Dk1mK'+
  'TcOTWYpNw5NZik3Dk1mKTcOTWYpNw5NZLDbWy3syS7GBJY0796V7MkuxaXgyi8XGenlPZik2DU9msdhY'+
  'L+/JLBYb6+U9maXYNDyZxWJjvbwnsxSbhiezFJuGJ7NYbKyX92SWYtPwZJZiA0saG+tL92QWi4318p7M'+
  'UmwansxSbBqezGKxsV7ek1mKTcOTWSw21st7MovFxnp5T2YpNg1PZrHYWC/vySzFpuHJLMWm4cksFhvr'+
  '5T2Zpdg0PJml2DQ8mcViY728J7MUm4YnsxSbhiezWGysl/dklmLT8GQWi41NWNLYWENsGp7MUmwansxi'+
  'sbFe3pNZLDbWy3syS7FpeDKLxcZ6eU9mKTYNT2YpNg1PZrHYWMYm4cksxabhySzFpuHJLBYb6+U9maXY'+
  'NDyZpdg0PJnFYmO9vCezFJuGJ7NYbGzCksbGGmLT8GSWYtPwZBaLjU1YwCbhySzFpuHJLMWm4cksFhvr'+
  '5T2Zpdg0PJml2DQ8mcViY728J7MUm4YnsxSbhiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MUm4Yns1hs'+
  'rIIns1hsrIInsxSbhiezWGysgiezFJuGJ7MUm4Yns1hsrIInsxSbhiezFJuGJ7NYbKyCJ7MUm4YnsxSb'+
  'hiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MUm4Yns1hsrIInsxSbhiezFJuGJ7NYbKyCJ7MUm4YnsxSb'+
  'hiezWGysgiezFJuGJ7MUm4Yns1hsrIInsxSbhiezWGysgiezWGysgiezFJuGJ7NYbKyCJ7NYbKyCJ7MU'+
  'W7GezMWwYjowNF/uYhwkLfNBa2urywyrfFtT6t0MN9vNc3PD+NCG6Y1TrprWiAIr0vlD3I6Fw9woP8Q1'+
  'jz/dDXej3ZClzk14r9LVXTW1kca7FgzLrfNaWG5KyG/40tuNtM7LvavpM665+Q+G+dsXdVoc3x5vK5fK'+
  'WP0ebanRbmgol+oYnDQrW+6mTLl3Hln9FZS7n5W7K+Gi8ZIw3vqLIZ2yZd44wufWeWzS1Ebaxn17Dfzy'+
  'CW6jv67RdXcuf1/o69I+T+fTdim0j3Ry8TccmqZuqswsX2i+d/n7Qnv9f1RSHw29k+n/Bw==';

var S: RawByteString;
    MS: THeapMemoryStream;
    MF: TMetaFile;
    H: cardinal;
    i,j: integer;
//    E: RawByteString; i,L,n: integer;
begin
{  S := Base64Encode(CompressString(StringFromFile('d:\temp\tmpCurve.emf')));
  E := '  EMF: RawByteString = // some compressed simple EMF file'#13#10;
  L := length(S);
  i := 1;
  while L>0 do begin
    if L>80 then
      n := 80 else
      n := L;
    E := E+'  '''+copy(S,i,n)+'''+'#13#10;
    dec(L,n);
    inc(i,n);
  end;
  FileFromString(E,'test.pas');}
  S := UncompressString(Base64Decode(EMF));
  Check(Hash32(S)=$5BB4C8B1);
  MS := THeapMemoryStream.Create;
  with TPdfDocument.Create do
  try
    Info.CreationDate := Date; // force fixed date and creator for Hash32() below
    Info.Data.PdfTextByName('Producer').Value := 'Synopse PDF engine';
    //CompressionMethod := cmNone; useful for debugg purposes of metafile enum
    AddPage;
    MF := TMetaFile.Create;
    try
      MS.Write(pointer(S)^,length(S));
      MS.Position := 0;
      MF.LoadFromStream(MS);
      Canvas.RenderMetaFile(MF);
      Check(Canvas.Page.Font.Name='Tahoma');
    finally
      MF.Free;
    end;
    MS.Clear;
    SaveToStream(MS,Date);
    // force constant Arial,Bold and Tahoma FontBBox
    SetString(s,PAnsiChar(MS.Memory),MS.Position);
    i := PosEx('/FontBBox [',s);
    if CheckFailed(i=5590) then exit;
    fillchar(s[i],32,32);
    j := PosEx('/FontBBox [',s);
    if CheckFailed(j=5949) then exit;
    fillchar(s[j],32,32);
    i := PosEx('/FontBBox [',s);
    if CheckFailed(i=6365) then exit;
    fillchar(s[i],32,32);
    H := Hash32(s);
    Check(H=$BA622F83);
    MS.SaveToFile(ChangeFileExt(paramstr(0),'.pdf'));
  finally
    Free;
    MS.Free;
  end;
end;
{$endif}
{$endif}

{$ifndef FPC}
{$ifndef DELPHI5OROLDER}
{$ifndef CPU64}

{ TTestClientServerAccess }

{$WARN SYMBOL_PLATFORM OFF}
procedure TTestClientServerAccess._TSQLite3HttpClient;
var Resp: TSQLTable;
begin
  Client := TSQLite3HttpClient.Create('127.0.0.1','888',Model);
  if (DebugHook<>0) and Client.InheritsFrom(TSQLite3HttpClientWinSock) then
    TSQLite3HttpClientWinSock(Client).Socket.TimeOut := 60000; // 1 minute time out
  Resp := Client.List([TSQLRecordPeople],'*');
  if CheckFailed(Resp<>nil) then
    exit;
  try
    Check(Resp.InheritsFrom(TSQLTableJSON));
    Check(Hash32(TSQLTableJSON(Resp).PrivateInternalCopy)=$352A6CCA);
  finally
    Resp.Free;
  end;
end;
{$WARN SYMBOL_PLATFORM ON}

class function TTestClientServerAccess.RegisterAddUrl(OnlyDelete: boolean): string;
begin
  result := THttpApiServer.AddUrlAuthorize('root','888',false,'+',OnlyDelete);
end;

procedure TTestClientServerAccess._TSQLite3HttpServer;
begin
  Model := TSQLModel.Create([TSQLRecordPeople],'root');
  Check(Model<>nil);
  Check(Model.GetTableIndex('people')>=0);
  try
    DataBase := TSQLRestServerDB.Create(Model,'test.db3');
    DataBase.DB.Synchronous := smOff;
    Server := TSQLite3HttpServer.Create('888',[DataBase],'+');
    fRunConsole := fRunConsole+'using '+Server.HttpServer.ClassName;
    Database.NoAJAXJSON := true; // expect not expanded JSON from now on
  except
    on E: Exception do
      Check(false,E.Message);
  end;
end;

destructor TTestClientServerAccess.Destroy;
begin
  Client.Free;
  Server.Free;
  DataBase.Free;
  Model.Free;
  inherited;
end;

{$ifdef MSWINDOWS}
{$define WTIME}
{$endif}

procedure TTestClientServerAccess.ClientTest;
{$ifdef WTIME}
var Timer: TPrecisionTimer;
const LOOP=1000;
{$else}
const LOOP=100;
{$endif}
const Req='FirstName Like "Sergei1%"';
var i: integer;
    Resp: TSQLTable;
    Rec, Rec2: TSQLRecordPeople;
    Refreshed: boolean;

  procedure TestOne;
  var i: integer;
  begin
    i := Rec.YearOfBirth;
    Rec.YearOfBirth := 1982;
    Check(Client.Update(Rec));
    Rec2.ClearProperties;
    Check(Client.Retrieve(10,Rec2));
    Check(Rec2.YearOfBirth=1982);
    Rec.YearOfBirth := i;
    Check(Client.Update(Rec));
    if Client.InheritsFrom(TSQLRestClientURI) then begin
      Check(TSQLRestClientURI(Client).UpdateFromServer([Rec2],Refreshed));
      Check(Refreshed);
    end else
      Check(Client.Retrieve(10,Rec2));
    Check(Rec.SameRecord(Rec2));
  end;

begin
{$ifdef WTIME}
  Timer.Start;
{$endif}
  // first calc result: all transfert protocols have to work from cache
  Resp := Client.List([TSQLRecordPeople],'*',Req);
  Check(Resp<>nil);
  Resp.Free;
  // test global connection speed and caching (both client and server sides)
  Rec2 := TSQLRecordPeople.Create;
  Rec := TSQLRecordPeople.Create(Client,10);
  try
    Check(Database.Cache.CachedEntries=0);
    Check(Client.Cache.CachedEntries=0);
    Check(Client.Cache.CachedMemory=0);
    TestOne;
    Check(Client.Cache.CachedEntries=0);
    Client.Cache.SetCache(TSQLRecordPeople); // cache whole table
    Check(Client.Cache.CachedEntries=0);
    Check(Client.Cache.CachedMemory=0);
    TestOne;
    Check(Client.Cache.CachedEntries=1);
    Check(Client.Cache.CachedMemory>0);
    Client.Cache.Clear; // reset cache settings
    Check(Client.Cache.CachedEntries=0);
    Client.Cache.SetCache(Rec); // cache one = SetCache(TSQLRecordPeople,Rec.ID)
    Check(Client.Cache.CachedEntries=0);
    Check(Client.Cache.CachedMemory=0);
    TestOne;
    Check(Client.Cache.CachedEntries=1);
    Check(Client.Cache.CachedMemory>0);
    Client.Cache.SetCache(TSQLRecordPeople);
    TestOne;
    Check(Client.Cache.CachedEntries=1);
    Client.Cache.Clear;
    Check(Client.Cache.CachedEntries=0);
    TestOne;
    Check(Client.Cache.CachedEntries=0);
    if not (Client.InheritsFrom(TSQLRestClientDB)) then begin // server-side
      Database.Cache.SetCache(TSQLRecordPeople);
      TestOne;
      Check(Client.Cache.CachedEntries=0);
      Check(Database.Cache.CachedEntries=1);
      Database.Cache.Clear;
      Check(Client.Cache.CachedEntries=0);
      Check(Database.Cache.CachedEntries=0);
      Database.Cache.SetCache(TSQLRecordPeople,Rec.ID);
      TestOne;
      Check(Client.Cache.CachedEntries=0);
      Check(Database.Cache.CachedEntries=1);
      Database.Cache.SetCache(TSQLRecordPeople);
      Check(Database.Cache.CachedEntries=0);
      TestOne;
      Check(Database.Cache.CachedEntries=1);
      if Client.InheritsFrom(TSQLRestClientURI) then
        TSQLRestClientURI(Client).ServerCacheFlush else
        Database.Cache.Flush;
      Check(Database.Cache.CachedEntries=0);
      Check(Database.Cache.CachedMemory=0);
      Database.Cache.Clear;
    end;
  finally
    Rec2.Free;
    Rec.Free;
  end;
  // test average speed for a 5 KB request 
{$ifdef WTIME}
  fRunConsole := fRunConsole+'first in '+string(Timer.Stop)+', ';
  Timer.Start;
{$endif}
  for i := 1 to LOOP do begin
    Resp := Client.List([TSQLRecordPeople],'*',Req);
    if CheckFailed(Resp<>nil) then
      exit;
    try
      Check(Resp.InheritsFrom(TSQLTableJSON));
      // every answer contains 113 rows, for a total JSON size of 4803 bytes
      Check(Hash32(TSQLTableJSON(Resp).PrivateInternalCopy)=$77039F89);
    finally
      Resp.Free;
    end;
  end;
{$ifdef WTIME}
  fRunConsole := format('%sdone in %s i.e. %d/s, aver. %s, %s/s',
    [fRunConsole,Timer.Stop,Timer.PerSec(LOOP),Timer.ByCount(LOOP),KB(Timer.PerSec(4898*(LOOP+1)))]);
{$endif}
end;

procedure TTestClientServerAccess.HttpClientKeepAlive;
begin
  ClientTest;
end;

procedure TTestClientServerAccess.HttpClientMultiConnect;
begin
  (Client as TSQLite3HttpClientGeneric).KeepAliveMS := 0;
  ClientTest;
end;

{
procedure TTestClientServerAccess.HttpClientMultiConnectDelphi;
begin
  if (self=nil) or (Server=nil) then
    exit; // if already Delphi code, nothing to test
  (Client as TSQLite3HttpClientGeneric).KeepAliveMS := 0;
  ClientTest;
end;

procedure TTestClientServerAccess.HttpClientKeepAliveDelphi;
begin
  if (self=nil) or (Server=nil) or (Server.HttpServer is THttpServer) then
    exit; // if already Delphi code, nothing to test
  Server.Free;
  Server := TSQLite3HttpServer.Create('888',[DataBase],'+',true);
  (Client as TSQLite3HttpClientGeneric).KeepAliveMS := 10000;
  ClientTest;
end;
}

procedure TTestClientServerAccess.NamedPipeAccess;
begin
  Check(DataBase.ExportServerNamedPipe('test'));
  Client.Free;
  Client := TSQLRestClientURINamedPipe.Create(Model,'test');
  ClientTest;
  // note: 1st connection is slower than with HTTP (about 100ms), because of
  // Sleep(128) in TSQLRestServerNamedPipe.Execute: but we should connect
  // localy only once, and avoiding Context switching is a must-have
  FreeAndNil(Client);
  Check(DataBase.CloseServerNamedPipe);
end;

procedure TTestClientServerAccess.DirectInProcessAccess;
begin
  Client := TSQLRestClientDB.Create(Model,
    TSQLModel.Create([TSQLRecordPeople],'root'),
    DataBase.DB,TSQLRestServerTest);
  ClientTest;
  FreeAndNil(Client);
end;

procedure TTestClientServerAccess.LocalWindowMessages;
begin
  Check(DataBase.ExportServerMessage('test'));
  Client := TSQLRestClientURIMessage.Create(Model,'test','Client',1000);
  ClientTest;
  FreeAndNil(Client);
end;

{$ifndef LVCL}

{ TTestExternalDatabase }

type
  TSQLRecordPeopleExt = class(TSQLRecord)
  private
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
    fLastChange: TModTime;
    fCreatedAt: TCreateTime;
  published
    property FirstName: RawUTF8 index 40 read fFirstName write fFirstName;
    property LastName: RawUTF8 index 40 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
    property LastChange: TModTime read fLastChange;
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
  end;

  TSQLDBConnectionPropertiesHook = class(TSQLDBConnectionProperties);
  TSQLRestServerStaticExternalHook = class(TSQLRestServerStaticExternal);

procedure TTestExternalDatabase.ExternalRecords;
begin
  if CheckFailed(fModel=nil) then exit; // should be called once
  fModel := TSQLModel.Create([TSQLRecordPeople,TSQLRecordPeopleExt]);
end;

procedure TTestExternalDatabase.AutoAdaptSQL;
var SQLOrigin: RawUTF8;
procedure Test(DBMS: TSQLDBDefinition; AdaptShouldWork: boolean;
  const SQLExpected: RawUTF8='');
var Props: TSQLDBConnectionProperties;
    SQL: RawUTF8;
begin
  Props := TSQLDBSQLite3ConnectionProperties.Create(':memory:','','','');
  try
    TSQLDBConnectionPropertiesHook(Props).fDBMS := DBMS;
    Check((Props.DBMS=DBMS)or(DBMS=dUnknown));
    VirtualTableExternalRegister(fModel,TSQLRecordPeopleExt,Props,'SampleRecord');
    with TSQLRestServerStaticExternalHook.Create(TSQLRecordPeopleExt,nil) do
    try
      SQL := SQLOrigin;
      Check(AdaptSQLForEngineList(SQL)=AdaptShouldWork);
      Check((SQL=SQLExpected)or not AdaptShouldWork);
    finally
      Free;
    end;
  finally
    Props.Free;
  end;
end;
procedure Test2(const Orig,Expected: RawUTF8);
var DBMS: TSQLDBDefinition;
begin
  SQLOrigin := Orig;
  for DBMS := low(DBMS) to high(DBMS) do
    Test(DBMS,true,Expected);
end;
begin
  Test2('select rowid,firstname from PeopleExt where rowid=2',
        'select id,firstname from SampleRecord where id=2');
  Test2('select rowid,firstname from PeopleExt where rowid=2 order by RowID',
        'select id,firstname from SampleRecord where id=2 order by ID');
  Test2('select rowid,firstname from PeopleExt where firstname like :(''test''): order by lastname',
        'select id,firstname from SampleRecord where firstname like :(''test''): order by lastname');
  SQLOrigin := 'select rowid,firstname from PeopleExt where rowid=2 limit 2';
  Test(dUnknown,false);
  Test(dDefault,false);
  Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2 and id=2');
  Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord where id=2');
  Test(dJet,true,'select top 2 id,firstname from SampleRecord where id=2');
  Test(dMySQL,true,'select id,firstname from SampleRecord where id=2 limit 2');
  Test(dSQLite,true,'select id,firstname from SampleRecord where id=2 limit 2');
  SQLOrigin := 'select rowid,firstname from PeopleExt where rowid=2 order by LastName limit 2';
  Test(dUnknown,false);
  Test(dDefault,false);
  Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2 and id=2 order by LastName');
  Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord where id=2 order by LastName');
  Test(dJet,true,'select top 2 id,firstname from SampleRecord where id=2 order by LastName');
  Test(dMySQL,true,'select id,firstname from SampleRecord where id=2 order by LastName limit 2');
  Test(dSQLite,true,'select id,firstname from SampleRecord where id=2 order by LastName limit 2');
  SQLOrigin := 'select rowid,firstname from PeopleExt where firstname=:(''test''): limit 2';
  Test(dUnknown,false);
  Test(dDefault,false);
  Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2 and firstname=:(''test''):');
  Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord where firstname=:(''test''):');
  Test(dJet,true,'select top 2 id,firstname from SampleRecord where firstname=:(''test''):');
  Test(dMySQL,true,'select id,firstname from SampleRecord where firstname=:(''test''): limit 2');
  Test(dSQLite,true,'select id,firstname from SampleRecord where firstname=:(''test''): limit 2');
  SQLOrigin := 'select id,firstname from PeopleExt limit 2';
  Test(dUnknown,false);
  Test(dDefault,false);
  Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2');
  Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord');
  Test(dJet,true,'select top 2 id,firstname from SampleRecord');
  Test(dMySQL,true,'select id,firstname from SampleRecord limit 2');
  Test(dSQLite,true,'select id,firstname from SampleRecord limit 2');
  SQLOrigin := 'select id,firstname from PeopleExt order by firstname limit 2';
  Test(dUnknown,false);
  Test(dDefault,false);
  Test(dOracle,true,'select id,firstname from SampleRecord where rownum<=2 order by firstname');
  Test(dMSSQL,true,'select top(2) id,firstname from SampleRecord order by firstname');
  Test(dJet,true,'select top 2 id,firstname from SampleRecord order by firstname');
  Test(dMySQL,true,'select id,firstname from SampleRecord order by firstname limit 2');
  Test(dSQLite,true,'select id,firstname from SampleRecord order by firstname limit 2');
end;


destructor TTestExternalDatabase.Destroy;
begin
  fModel.Free;
  inherited;
end;

procedure TTestExternalDatabase.ExternalViaREST;
begin
  Test(true);
end;

procedure TTestExternalDatabase.ExternalViaVirtualTable;
begin
  Test(false);
end;

procedure TTestExternalDatabase.CryptedDatabase;
var R,R2: TSQLRecordPeople;
    Model: TSQLModel;
    aID: integer;
    Client, Client2: TSQLRestClientDB;
    Res: TIntegerDynArray;
procedure CheckFilledRow;
begin
  Check(R.FillRewind);
  while R.FillOne do
  if not CheckFailed(R2.FillOne) then begin
    Check(R.ID<>0);
    Check(R2.ID<>0);
    Check(R.FirstName=R2.FirstName);
    Check(R.LastName=R2.LastName);
    Check(R.YearOfBirth=R2.YearOfBirth);
    Check(R.YearOfDeath=R2.YearOfDeath);
  end;
end;
const password ='pass';
begin
  DeleteFile('testpass.db3');
  Model := TSQLModel.Create([TSQLRecordPeople]);
  try
    Client := TSQLRestClientDB.Create(Model,nil,'test.db3',TSQLRestServerDB,false,'');
    try
      R := TSQLRecordPeople.CreateAndFillPrepare(Client,'');
      try
        Client2 := TSQLRestClientDB.Create(Model,nil,'testpass.db3',TSQLRestServerDB,false,password);
        try
          Client2.Server.DB.Synchronous := smOff;
          Client2.Server.DB.WALMode := true;
          Client2.Server.CreateMissingTables;
          Check(Client2.TransactionBegin(TSQLRecordPeople));
          Check(Client2.BatchStart(TSQLRecordPeople));
          while R.FillOne do begin
            Check(R.ID<>0);
            Check(Client2.BatchAdd(R,true)>=0);
          end;
          Check(Client2.BatchSend(Res)=200);
          Client2.Commit;
        finally
          Client2.Free;
        end;
        Check(IsSQLite3File('testpass.db3'));
        Check(IsSQLite3FileEncrypted('testpass.db3'));
        // try to read then update the crypted file
        Client2 := TSQLRestClientDB.Create(Model,nil,'testpass.db3',TSQLRestServerDB,false,password);
        try
          Client2.Server.DB.Synchronous := smOff;
          R2 := TSQLRecordPeople.CreateAndFillPrepare(Client2,'');
          try
            CheckFilledRow;
            R2.FirstName := 'One';
            aID := Client2.Add(R2,true);
            Check(aID<>0);
            R2.FillPrepare(Client2,'');
            CheckFilledRow;
            R2.ClearProperties;
            Check(R2.FirstName='');
            Check(Client2.Retrieve(aID,R2));
            Check(R2.FirstName='One');
          finally
            R2.Free;
          end;
        finally
          Client2.Free;
        end;
        Check(IsSQLite3File('testpass.db3'));
        Check(IsSQLite3FileEncrypted('testpass.db3'));
        // now read it after uncypher
        ChangeSQLEncryptTablePassWord('testpass.db3',password,'');
        Check(IsSQLite3File('testpass.db3'));
        Check(not IsSQLite3FileEncrypted('testpass.db3'));
        Client2 := TSQLRestClientDB.Create(Model,nil,'testpass.db3',TSQLRestServerDB,false,'');
        try
          R2 := TSQLRecordPeople.CreateAndFillPrepare(Client2,'');
          try
            CheckFilledRow;
            R2.ClearProperties;
            Check(R2.FirstName='');
            Check(Client2.Retrieve(aID,R2));
            Check(R2.FirstName='One');
          finally
            R2.Free;
          end;
        finally
          Client2.Free;
        end;
      finally
        R.Free;
      end;
    finally
      Client.Free;
    end;
  finally
    Model.Free;
  end;
end;


procedure TTestExternalDatabase.Test(StaticVirtualTableDirect: boolean);
var RInt: TSQLRecordPeople;
    RExt: TSQLRecordPeopleExt;
//    Tables: TRawUTF8DynArray;
    aID, i: integer;
    ok: Boolean;
    fClient: TSQLRestClientDB;
    fConnection: TSQLDBConnectionProperties;
    Start, Updated: TTimeLog; // will work with both TModTime and TCreateTime properties
begin
  // run tests over an in-memory SQLite3 database (much faster than file)
  fConnection := TSQLDBSQLite3ConnectionProperties.Create(':memory:','','','');
  Check(VirtualTableExternalRegister(fModel,TSQLRecordPeopleExt,fConnection,'PeopleExternal'));
  fClient := TSQLRestClientDB.Create(fModel,nil,'test.db3',TSQLRestServerDB);
  try
    fClient.Server.DB.Synchronous := smOff;
{    fClient.Server.DB.GetTableNames(Tables);
    if FindRawUTF8(Tables,'PeopleExt',false)>=0 then
      fClient.Server.EngineExecuteAll('drop table PeopleExt'); }
    Start := fClient.ServerTimeStamp;
    fClient.Server.StaticVirtualTableDirect := StaticVirtualTableDirect;
    fClient.Server.CreateMissingTables;
    Check(fClient.Server.CreateSQLMultiIndex(
      TSQLRecordPeopleExt,['FirstName','LastName'],false));
    RInt := TSQLRecordPeople.CreateAndFillPrepare(fClient,'');
    try
      Check(RInt.FillTable<>nil);
      Check(RInt.FillTable.RowCount>0);
      RExt := TSQLRecordPeopleExt.Create;
      try
        aID := 0;
        while RInt.FillOne do begin
          RExt.Data := RInt.Data;
          RExt.FirstName := RInt.FirstName;
          RExt.LastName := RInt.LastName;
          RExt.YearOfBirth := RInt.YearOfBirth;
          RExt.YearOfDeath := RInt.YearOfDeath;
          RExt.fLastChange := 0;
          RExt.CreatedAt := 0;
          aID := fClient.Add(RExt,true);
          Check(aID<>0);
          Check(RExt.LastChange>=Start);
          Check(RExt.CreatedAt>=Start);
          RExt.ClearProperties;
          Check(RExt.YearOfBirth=0);
          Check(RExt.FirstName='');
          Check(fClient.Retrieve(aID,RExt));
          Check(RExt.FirstName=RInt.FirstName);
          Check(RExt.LastName=RInt.LastName);
          Check(RExt.YearOfBirth=RInt.YearOfBirth);
          Check(RExt.YearOfDeath=RInt.YearOfDeath);
          Check(RExt.YearOfBirth<>RExt.YearOfDeath);
        end;
        Check(RInt.FillRewind);
        while RInt.FillOne do begin
          RExt.FillPrepare(fClient,'FirstName=? and LastName=?',
            [RInt.FirstName,RInt.LastName]); // query will use index -> fast :)
          while RExt.FillOne do begin
            Check(RExt.FirstName=RInt.FirstName);
            Check(RExt.LastName=RInt.LastName);
            Check(RExt.YearOfBirth=RInt.YearOfBirth);
            Check(RExt.YearOfDeath=RInt.YearOfDeath);
            Check(RExt.YearOfBirth<>RExt.YearOfDeath);
          end;
        end;
        Updated := fClient.ServerTimeStamp;
        for i := 1 to aID do
          if i mod 100=0 then begin
            RExt.fLastChange := 0;
            RExt.CreatedAt := 0;
            Check(fClient.Retrieve(i,RExt,true),'for update');
            Check(RExt.YearOfBirth<>RExt.YearOfDeath);
            Check(RExt.CreatedAt<=Updated);
            RExt.YearOfBirth := RExt.YearOfDeath;
            Check(fClient.Update(RExt),'Update 1/100 rows');
            Check(fClient.UnLock(RExt));
            Check(RExt.LastChange>=Updated);
            RExt.ClearProperties;
            Check(RExt.YearOfDeath=0);
            Check(RExt.YearOfBirth=0);
            Check(RExt.CreatedAt=0);
            Check(fClient.Retrieve(i,RExt),'after update');
            Check(RExt.YearOfBirth=RExt.YearOfDeath);
            Check(RExt.CreatedAt>=Start);
            Check(RExt.CreatedAt<=Updated);
            Check(RExt.LastChange>=Updated);
          end;
        for i := 1 to aID do
          if i and 127=0 then
            Check(fClient.Delete(TSQLRecordPeopleExt,i),'Delete 1/128 rows');
        for i := 1 to aID do begin
          RExt.fLastChange := 0;
          RExt.CreatedAt := 0;
          RExt.YearOfBirth := 0;
          ok := fClient.Retrieve(i,RExt,false);
          Check(ok=(i and 127<>0),'deletion');
          if ok then begin
            Check(RExt.CreatedAt>=Start);
            Check(RExt.CreatedAt<=Updated);
            if i mod 100=0 then begin
              Check(RExt.YearOfBirth=RExt.YearOfDeath,'Update');
              Check(RExt.LastChange>=Updated);
            end else begin
              Check(RExt.YearOfBirth<>RExt.YearOfDeath,'Update');
              Check(RExt.LastChange>=Start);
              Check(RExt.LastChange<=Updated);
            end;
          end;
        end;
      finally
        RExt.Free;
      end;
    finally
      RInt.Free;
    end;
  finally
    fClient.Free;
    fConnection.Free;
  end;
end;


{ TCollTests }

function TCollTests.GetClass: TCollectionItemClass;
begin
  result := TCollTest;
end;
{$endif LVCL}


{ TServiceCalculator }

type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1,n2: integer): integer;
    function Subtract(n1,n2: double): double;
    function Multiply(n1,n2: Int64): Int64;
    procedure ToText(Value: Currency; var Result: RawUTF8);
    function ToTextFunc(Value: double): string;
    function SpecialCall(Txt: RawUTF8; var Int: integer; var Card: cardinal; field: TSynTableFieldTypes;
      fields: TSynTableFieldTypes; var options: TSynTableFieldOptions): TSynTableFieldTypes;
    function ComplexCall(const Ints: TIntegerDynArray; Strs1: TRawUTF8DynArray;
      var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
      var Rec2: TSQLRestCacheEntryValue): TSQLRestCacheEntryValue;
  end;

  TServiceComplexCalculator = class(TServiceCalculator,IComplexCalculator)
  public
    procedure Substract(n1,n2: TComplexNumber; out Result: TComplexNumber);
    function IsNull(n: TComplexNumber): boolean;
    function TestBlob(n: TComplexNumber): TServiceCustomAnswer;
{$ifndef LVCL}
    procedure Collections(Item: TCollTest; var List: TCollTests; out Copy: TCollTests);
{$endif LVCL}
  end;

  TServiceComplexNumber = class(TInterfacedObject,IComplexNumber)
  private
    fReal: double;
    fImaginary: double;
    function GetImaginary: double;
    function GetReal: double;
    procedure SetImaginary(const Value: double);
    procedure SetReal(const Value: double);
  public
    procedure Assign(aReal, aImaginary: double);
    procedure Add(aReal, aImaginary: double);
    property Real: double read GetReal write SetReal;
    property Imaginary: double read GetImaginary write SetImaginary;
  end;


function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1+n2;
end;

function TServiceCalculator.Multiply(n1, n2: Int64): Int64;
begin
  result := n1*n2;
end;

function TServiceCalculator.SpecialCall(Txt: RawUTF8; var Int: integer;
  var Card: cardinal; field, fields: TSynTableFieldTypes;
  var options: TSynTableFieldOptions): TSynTableFieldTypes;
begin
  inc(Int,length(Txt));
  inc(Card);
  result := fields+field;
  Include(options,tfoUnique);
  Exclude(options,tfoIndex);
end;

function TServiceCalculator.Subtract(n1, n2: double): double;
begin
  result := n1-n2;
end;

procedure TServiceCalculator.ToText(Value: Currency; var Result: RawUTF8);
begin
  result := Curr64ToStr(PInt64(@Value)^);
end;

function TServiceCalculator.ToTextFunc(Value: double): string;
begin
  result := DoubleToString(Value);
end;

function TServiceCalculator.ComplexCall(const Ints: TIntegerDynArray;
  Strs1: TRawUTF8DynArray; var Str2: TWideStringDynArray; const Rec1: TVirtualTableModuleProperties;
  var Rec2: TSQLRestCacheEntryValue): TSQLRestCacheEntryValue;
var i: integer;
begin
  result := Rec2;
  result.JSON := StringToUTF8(Rec1.FileExtension);
  i := length(Str2);
  SetLength(Str2,i+1);
  Str2[i] := UTF8ToWideString(RawUTF8ArrayToCSV(Strs1));
  inc(Rec2.ID);
  dec(Rec2.TimeStamp);
  Rec2.JSON := IntegerDynArrayToCSV(Ints,length(Ints));
end;


{ TServiceComplexCalculator }

constructor TComplexNumber.Create(aReal, aImaginary: double);
begin
  Real := aReal;
  Imaginary := aImaginary;
end;

function TServiceComplexCalculator.IsNull(n: TComplexNumber): boolean;
begin
  result := (n.Real=0) and (n.Imaginary=0);
end;

procedure TServiceComplexCalculator.Substract(n1, n2: TComplexNumber; out Result: TComplexNumber);
begin
  result.Real := n1.Real-n2.Real;
  result.Imaginary := n1.Imaginary-n2.Imaginary;
end;

function TServiceComplexCalculator.TestBlob(n: TComplexNumber): TServiceCustomAnswer;
begin
  Result.Header := TEXT_CONTENT_TYPE_HEADER;
  Result.Content := FormatUTF8('%,%',[n.Real,n.Imaginary]);
  assert(GetCurrentThreadID=MainThreadID,'shall always be in main thread');
end;

{$ifndef LVCL}
procedure TServiceComplexCalculator.Collections(Item: TCollTest;
  var List: TCollTests; out Copy: TCollTests);
begin
  CopyObject(Item,List.Add);
  CopyObject(List,Copy);
end;
{$endif LVCL}


{ TServiceComplexNumber }

procedure TServiceComplexNumber.Add(aReal, aImaginary: double);
begin
  fReal := fReal+aReal;
  fImaginary := fImaginary+aImaginary;
end;

procedure TServiceComplexNumber.Assign(aReal, aImaginary: double);
begin
  fReal := aReal;
  fImaginary := aImaginary;
end;

function TServiceComplexNumber.GetImaginary: double;
begin
  result := fImaginary;
end;

function TServiceComplexNumber.GetReal: double;
begin
  result := fReal;
end;

procedure TServiceComplexNumber.SetImaginary(const Value: double);
begin
  fImaginary := Value;
end;

procedure TServiceComplexNumber.SetReal(const Value: double);
begin
  fReal := Value;
end;


{ TTestServiceOrientedArchitecture }

procedure TTestServiceOrientedArchitecture.Test(const I: ICalculator;
  const CC: IComplexCalculator; const CN: IComplexNumber);
procedure TestCalculator(const I: ICalculator);
var t,i1,i2,i3: integer;
    c: cardinal;
    cu: currency;
    n1,n2: double;
    o: TSynTableFieldOptions;
    Ints: TIntegerDynArray;
    Strs1: TRawUTF8DynArray;
    Str2: TWideStringDynArray;
    Rec1: TVirtualTableModuleProperties;
    Rec2, RecRes: TSQLRestCacheEntryValue;
    s: RawUTF8;
begin
  Setlength(Ints,2);
  CSVToRawUTF8DynArray('one,two,three',Strs1);
  for t := 1 to 1000 do begin
    i1 := Random(MaxInt)-Random(MaxInt);
    i2 := Random(MaxInt)-i1;
    Check(I.Add(i1,i2)=i1+i2);
    Check(I.Multiply(i1,i2)=Int64(i1)*Int64(i2));
    n1 := Random*1E-17-Random*1E-9;
    n2 := n1*Random;
    CheckSame(I.Subtract(n1,n2),n1-n2);
    cu := i1*0.01;
    I.ToText(cu,s);
    Check(s=Curr64ToStr(PInt64(@cu)^));
    Check(I.ToTextFunc(n1)=DoubleToString(n1));
    o := [tfoIndex,tfoCaseInsensitive];
    i3 := i1;
    c := cardinal(i2);
    Check(I.SpecialCall(s,i3,c,
      [tftDouble],[tftWinAnsi,tftVarInt64],o)=
      [tftWinAnsi,tftVarInt64,tftDouble]);
    Check(i3=i1+length(s));
    Check(c=cardinal(i2)+1);
    Check(o=[tfoUnique,tfoCaseInsensitive]);
    Ints[0] := i1;
    Ints[1] := i2;
    SetLength(Str2,3);
    Str2[0] := 'ABC';
    Str2[1] := 'DEF';
    Str2[2] := 'GHIJK';
    fillchar(Rec1,sizeof(Rec1),0);
    Rec1.Features := [vtTransaction,vtSavePoint];
    Rec1.FileExtension := ExeVersion.ProgramFileName;
    Rec2.ID := i1;
    Rec2.TimeStamp := c;
    Rec2.JSON := 'abc';
    RecRes := I.ComplexCall(Ints,Strs1,Str2,Rec1,Rec2);
    Check(length(Str2)=4);
    Check(Str2[0]='ABC');
    Check(Str2[1]='DEF');
    Check(Str2[2]='GHIJK');
    Check(Str2[3]='one,two,three');
    Check(Rec1.Features=[vtTransaction,vtSavePoint]);
    Check(Rec1.FileExtension=ExeVersion.ProgramFileName);
    Check(Rec2.ID=i1+1);
    Check(Rec2.TimeStamp=c-1);
    Check(Rec2.JSON=IntegerDynArrayToCSV(Ints,length(Ints)));
    Check(RecRes.ID=i1);
    Check(RecRes.TimeStamp=c);
    Check(RecRes.JSON=StringToUTF8(Rec1.FileExtension));
    Rec1.FileExtension := ''; // to avoid memory leak
  end;
end;
var s: RawUTF8;
    c: cardinal;
    n1,n2: double;
{$ifndef LVCL}
    C1,C2,C3: TComplexNumber;
    Item: TCollTest;
    List,Copy: TCollTests;
    j: integer;
{$endif}
begin
  Check(I.Add(1,2)=3);
  Check(I.Multiply(2,3)=6);
  CheckSame(I.Subtract(23,20),3);
  I.ToText(3.14,s);
  Check(s='3.14');
  Check(I.ToTextFunc(777)='777');
  TestCalculator(I);
  TestCalculator(CC); // test the fact that CC inherits from ICalculator
  {$ifndef LVCL}   /// in LVCL, TPersistent doesn't have any RTTI information 
  C3 := TComplexNumber.Create(0,0);
  C1 := TComplexNumber.Create(2,3);
  C2 := TComplexNumber.Create(20,30);
  List := TCollTests.Create;
  Copy := TCollTests.Create;
  Item := TCollTest.Create(nil);
  try
    Check(CC.IsNull(C3));
    for c := 0 to 700 do begin
      Check(not CC.IsNull(C1));
      C3.Imaginary := 0;
      CC.Substract(C1,C2,C3);
      CheckSame(C3.Real,c-18.0);
      CheckSame(C3.Imaginary,-27);
      with CC.TestBlob(C3) do begin
        Check(Header=TEXT_CONTENT_TYPE_HEADER);
        Check(Content=FormatUTF8('%,%',[C3.Real,C3.Imaginary]));
      end;
      if c mod 10=1 then begin
        Item.Color := Item.Color+1;
        Item.Length := Item.Color*2;
        Item.Name := Int32ToUtf8(Item.Color);
        CC.Collections(Item,List,Copy);
      end;
      if not CheckFailed(List.Count=Item.Color) or
         not CheckFailed(Copy.Count=List.Count) then
        for j := 0 to List.Count-1 do begin
          with TCollTest(List.Items[j]) do begin
            Check(Color=j+1);
            Check(Length=Color*2);
            Check(GetInteger(pointer(Name))=Color);
          end;
          with TCollTest(Copy.Items[j]) do begin
            Check(Color=j+1);
            Check(Length=Color*2);
            Check(GetInteger(pointer(Name))=Color);
          end;
        end;
      C1.Real := C1.Real+1;
    end;
  finally
    C3.Free;
    C1.Free;
    C2.Free;
    Item.Free;
    List.Free;
    Copy.Free;
  end;
  {$endif}
  n2 := CN.Imaginary;
  for c := 0 to 200 do begin
    CheckSame(CN.Imaginary,n2);
    n1 := Random*1000;
    CN.Real := n1;
    CheckSame(CN.Real,n1);
    CheckSame(CN.Imaginary,n2);
    n2 := Random*1000;
    CN.Imaginary := n2;
    CheckSame(CN.Real,n1);
    CheckSame(CN.Imaginary,n2);
    CN.Add(1,2);
    CheckSame(CN.Real,n1+1);
    n2 := n2+2;
    CheckSame(CN.Imaginary,n2);
  end;
  CN.Assign(3.14,1.05946);
  CheckSame(CN.Real,3.14);
  CheckSame(CN.Imaginary,1.05946);
end;

procedure TTestServiceOrientedArchitecture.ClientTest(aRouting: TServiceRoutingMode;
  RunInMainThread: boolean=false);
var I: ICalculator;
    CC: IComplexCalculator;
    CN: IComplexNumber;
    O: TObject;
    s: integer;
begin
  if RunInMainThread then 
    with fClient.Server.Services do
    for s := 0 to Count-1 do
      (Index(s) as TServiceFactoryServer).ExecuteInMainThread([]);
  fClient.Server.ServicesRouting := aRouting;
  fClient.ServicesRouting := aRouting;
  (fClient.Server.Services as TServiceContainerServer).PublishSignature := true;
  Check(fClient.Services['Calculator'].RetrieveSignature=
    fClient.Server.Services['Calculator'].RetrieveSignature);
  (fClient.Server.Services as TServiceContainerServer).PublishSignature := false;
  Check(fClient.Services['Calculator'].RetrieveSignature='');
  // once registered, can be accessed by its GUID or URI
  if CheckFailed(fClient.Services.Info(TypeInfo(ICalculator)).Get(I)) or
     CheckFailed(fClient.Services.Info(TypeInfo(IComplexCalculator)).Get(CC)) or
     // here, IComplexNumber is not yet registered on the client side -> done now
     CheckFailed(fClient.Services.Info(TypeInfo(IComplexNumber)).Get(CN)) then
    exit;
  O := ObjectFromInterface(I);
  Check((O<>nil) and (O.ClassName='TInterfacedObjectFake'),
    'ObjectFromInterface for '+O.ClassName);
  Test(I,CC,CN);
  I := nil;
  if CheckFailed(fClient.Services.GUID(IID_ICalculator).Get(I)) then
    exit;
  Test(I,CC,CN);
  I := nil;
  CC := nil;
  CN := nil;
  if CheckFailed(fClient.Services['Calculator'].Get(I)) or
     CheckFailed(fClient.Services['ComplexCalculator'].Get(CC)) or
     CheckFailed(fClient.Services['ComplexNumber'].Get(CN)) then
    exit;
  Test(I,CC,CN);
  if RunInMainThread then
    with fClient.Server.Services do
    for s := 0 to Count-1 do
      (Index(s) as TServiceFactoryServer).ExecuteInMainThread([],false);
end;

destructor TTestServiceOrientedArchitecture.Destroy;
begin
  fClient.Free;
  fModel.Free;
  inherited;
end;

procedure TTestServiceOrientedArchitecture.DirectCall;
var I: ICalculator;
    CC: IComplexCalculator;
    CN: IComplexNumber;
begin
  I := TServiceCalculator.Create;
  CC := TServiceComplexCalculator.Create;
  CN := TServiceComplexNumber.Create;
  Test(I,CC,CN);
  Test(I,CC,CN);
  Test(I,CC,CN);
end;

procedure TTestServiceOrientedArchitecture.ServerSide;
var I: ICalculator;
    CC: IComplexCalculator;
    CN: IComplexNumber;
begin
  if CheckFailed(fModel<>nil) or CheckFailed(fClient<>nil) or
     CheckFailed(fClient.Server.Services.Count=3) or
     CheckFailed(fClient.Server.Services.Index(0).Get(I)) or
     CheckFailed(Assigned(I)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(IComplexCalculator)).Get(CC)) or
     CheckFailed(fClient.Server.Services.Info(TypeInfo(IComplexNumber)).Get(CN)) then
    exit;
  Test(I,CC,CN);
  I := nil;
  CC := nil;
  CN := nil;
  if CheckFailed(fClient.Server.Services['Calculator'].Get(I)) or
     CheckFailed(fClient.Server.Services['ComplexCalculator'].Get(CC)) or
     CheckFailed(fClient.Server.Services['ComplexNumber'].Get(CN)) then
    exit;
  Test(I,CC,CN);
  Test(I,CC,CN);
end;

procedure TTestServiceOrientedArchitecture.ServiceInitialization;
  function Ask(Method, Params: RawUTF8; ExpectedResult: cardinal): RawUTF8;
  var resp,data: RawUTF8;
  begin
    Params := ' [ '+Params+' ]';
    case fClient.Server.ServicesRouting of
    rmREST: begin
      SetString(data,PAnsiChar(pointer(Params)),length(Params)); // =UniqueString
      Check(fClient.URI('root/calculator.'+Method,'POST',@resp,nil,@Params).Lo=ExpectedResult);
      if ExpectedResult=200 then begin
        Check(fClient.URI('root/CALCulator.'+Method+'?'+UrlEncode(data),'POST',@data).Lo=ExpectedResult);
        Check(data=resp,'optional URI-encoded-inlined parameters use');
      end;
    end;
    rmJSON_RPC: begin
      data := '{"method":"'+Method+'", "params":'+Params+'}';
      Check(fClient.URI('root/calculator','POST',@resp,nil,@data).Lo=ExpectedResult);
    end;
    end;
    result := JSONDecode(resp,'result',nil,true);
  end;
var S: TServiceFactory;
    i: integer;
    routing: TServiceRoutingMode;
const ExpectedURI: array[0..4] of RawUTF8 =
        ('Add','Multiply','Subtract','ToText','ToTextFunc');
      ExpectedParCount: array[0..4] of Integer = (4,4,4,3,3);
      ExpectedArgs: array[0..4] of TServiceMethodValueTypes =
        ([smvSelf,smvInteger],[smvSelf,smvInt64],[smvSelf,smvDouble],
         [smvSelf,smvCurrency,smvRawUTF8],[smvSelf,smvDouble,smvString]);
      ExpectedTypes: array[0..4] of String[10] =
        ('Integer','Int64','Double','Currency','Double');
      ExpectedType: array[0..4] of TServiceMethodValueType =
        (smvInteger,smvInt64,smvDouble,smvCurrency,smvDouble);
      ExpectedResult: array[0..2] of String[10] = ('Integer','Int64','Double');
begin
  if CheckFailed(fModel=nil) then exit; // should be called once
  // create model, client and server
  fModel := TSQLModel.Create([TSQLRecordPeople]);
  fClient := TSQLRestClientDB.Create(fModel,nil,'test.db3',TSQLRestServerDB,true);
  Check(fClient.SetUser('User','synopse')); // default user for Security tests
  // register TServiceCalculator as the ICalculator implementation on the server
  Check(fClient.Server.
    ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared)<>nil);
  // verify ICalculator RTTI-generated details
  Check(fClient.Server.Services<>nil);
  if CheckFailed(fClient.Server.Services.Count=1) then exit;
  S := fClient.Server.Services.Index(0);
  if CheckFailed(S<>nil) then exit;
  Check(S.InterfaceURI='Calculator');
  Check(S.InstanceCreation=sicShared);
  Check(S.InterfaceTypeInfo^.Kind=tkInterface);
  Check(S.InterfaceTypeInfo^.Name='ICalculator');
  Check(GUIDToString(S.InterfaceIID)='{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}');
  Check(S.InterfaceMangledURI='7chgmrLOCU6H1EoW9Jbl_g');
  fClient.Server.Services.ExpectMangledURI := true;
  Check(fClient.Server.Services[S.InterfaceMangledURI]=S);
  fClient.Server.Services.ExpectMangledURI := false;
  Check(fClient.Server.Services['CALCULAtor']=S);
  Check(fClient.Server.Services['CALCULAtors']=nil);
  if CheckFailed(length(S.Methods)=7) then exit;
  {$ifdef UNICODE}
  Check(S.ContractHash='"554BCF1AE51C19C1"'); // string -> utf8
  {$else}
  if CurrentAnsiConvert.CodePage=CODEPAGE_US then
    Check(S.ContractHash='"0B03FB378D76EAE4"'); // string -> ansi1252
  {$endif}
  for i := 0 to high(ExpectedURI) do // SpecialCall interface not checked
    with S.Methods[i] do begin
      Check(URI=ExpectedURI[i]);
      Check(length(Args)=ExpectedParCount[i]);
      Check(ArgsUsed=ExpectedArgs[i]);
      Check(Args[0].ParamName^='Self');
      Check(Args[0].ValueDirection=smdConst);
      Check(Args[0].ValueType=smvSelf);
      Check(Args[0].TypeName^='ICalculator');
      Check(Args[1].ValueDirection=smdConst);
      Check(Args[1].ValueType=ExpectedType[i]);
      if i<3 then
        Check(Args[1].ParamName^='n1') else
        Check(Args[1].ParamName^='Value');
      if i<3 then begin
        Check(Args[2].ParamName^='n2');
        Check(Args[2].ValueDirection=smdConst);
        Check(Args[2].ValueType=ExpectedType[i]);
        Check(IdemPropName(Args[3].TypeName^,ExpectedTypes[i]));
        Check(Args[3].ValueDirection=smdResult);
        Check(Args[3].ValueType=ExpectedType[i]);
      end else begin
        Check(Args[2].ParamName^='Result');
        if i<4 then
          Check(Args[2].ValueDirection=smdVar) else
          Check(Args[2].ValueDirection=smdResult);
        if i<4 then
          Check(Args[2].ValueType=smvRawUTF8) else
          Check(Args[2].ValueType=smvString);
      end;
    end;
  // IComplexCalculator + IComplexNumber services
  Check(fClient.Server.ServiceRegister(TServiceComplexCalculator,[TypeInfo(IComplexCalculator)],sicShared)<>nil);
  Check(fClient.Server.ServiceRegister(TServiceComplexNumber,[TypeInfo(IComplexNumber)],sicClientDriven)<>nil);
  // JSON-level access
  for routing := low(routing) to high(routing) do begin
    fClient.Server.ServicesRouting := routing;
    Check(Ask('None','1,2',400)='');
    Check(Ask('Add','1,2',200)='[3]');
    Check(Ask('Multiply','2,3',200)='[6]');
    Check(Ask('Subtract','23,20',200)='[3]');
    Check(Ask('ToText','777,"abc"',200)='["777"]'); // "abc" for var parameter
    Check(Ask('ToTextFunc','777',200)='["777"]');
  end;
  fClient.Server.ServicesRouting := rmREST; // back to default
end;

procedure TTestServiceOrientedArchitecture.Security;
  procedure Test(Expected: TSQLFieldTables; const msg: string);
    function Ask(const Method, Params: RawUTF8): RawUTF8;
    var resp,data: RawUTF8;
    begin
      data := '{"method":"'+Method+'", "params": [ '+Params+' ]}';
      fClient.URI('root/calculator','POST',@resp,nil,@data);
      result := JSONDecode(resp,'result',nil,true);
    end;
  begin
    Check((Ask('None','1,2')=''),msg);
    Check((Ask('Add','1,2')='[3]')=(1 in Expected),msg);
    Check((Ask('Multiply','2,3')='[6]')=(2 in Expected),msg);
    Check((Ask('Subtract','23,20')='[3]')=(3 in Expected),msg);
    Check((Ask('ToText','777,"abc"')='["777"]')=(4 in Expected),msg);
    Check((Ask('ToTextFunc','777')='["777"]')=(5 in Expected),msg);
  end;
var S: TServiceFactoryServer;
    GroupID: integer;
begin
  GroupID := fClient.MainFieldID(TSQLAuthGroup,'User');
  S := fClient.Server.Services['Calculator'] as TServiceFactoryServer;
  Test([1,2,3,4,5],'by default, all methods are allowed');
  S.AllowAll;
  Test([1,2,3,4,5],'AllowAll should change nothing');
  S.DenyAll;
  Test([],'DenyAll will reset all settings');
  S.AllowAll;
  Test([1,2,3,4,5],'back to full acccess for everybody');
  S.DenyAllByID([GroupID]);
  Test([],'our current user shall be denied');
  S.AllowAll;
  Test([1,2,3,4,5],'restore allowed for everybody');
  S.DenyAllByID([GroupID+1]);
  Test([1,2,3,4,5],'this group ID won''t affect the current user');
  S.DenyByID(['Add'],GroupID);
  Test([2,3,4,5],'exclude a specific method for the current user');
  S.DenyByID(['totext'],GroupID);
  Test([2,3,5],'exclude another method for the current user');
  S.AllowByID(['Add'],[GroupID+1]);
  Test([2,3,5],'this group ID won''t affect the current user');
  S.AllowByID(['Add'],[GroupID]);
  Test([1,2,3,5],'allow a specific method for the current user');
  S.AllowAllByID([0]);
  Test([1,2,3,5],'invalid group ID won''t affect the current user');
  S.AllowAllByID([GroupID]);
  Test([1,2,3,4,5],'restore allowed for the current user');
  Check(not fClient.SetUser('unknown','wrongpass'));
  Test([],'no authentication -> access denied');
  Check(fClient.SetUser('Admin','synopse'));
  Test([1,2,3,4,5],'authenticated user');
  S.DenyAll;
  Test([],'DenyAll works even for admins');
  S.AllowAll;
  Test([1,2,3,4,5],'restore allowed for everybody');
  S.AllowAllByName(['Supervisor']);
  Test([1,2,3,4,5],'this group name won''t affect the current Admin user');
  S.DenyAllByName(['Supervisor']);
  Test([1,2,3,4,5],'this group name won''t affect the current Admin user');
  S.DenyAllByName(['Supervisor','Admin']);
  Test([],'Admin group user was explicitely denied access');
  S.AllowAllByName(['Admin']);
  Test([1,2,3,4,5],'restore allowed for current Admin user');
  S.AllowAll;
  Check(fClient.SetUser('User','synopse'));
  Test([1,2,3,4,5],'restore allowed for everybody');
end;

procedure TTestServiceOrientedArchitecture.ClientSideJSONRPC;
begin
  ClientTest(rmJSON_RPC);
end;

procedure TTestServiceOrientedArchitecture.ClientSideREST;
begin
  Check(fClient.ServiceRegister([TypeInfo(ICalculator)],sicShared));
  Check(fClient.ServiceRegister([TypeInfo(IComplexCalculator)],sicShared));
  ClientTest(rmREST);
end;

type
  TTestThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Test: TTestServiceOrientedArchitecture;
  end;

  
{ TTestThread }

procedure TTestThread.Execute;
begin
  try
    Test.fClient.Server.BeginCurrentThread(self);
    Test.ClientTest(rmREST,true);
  finally
    Test := nil; // mark tests finished
  end;
end;


{$ifndef LVCL}
procedure TTestServiceOrientedArchitecture.ClientSideSynchronizedREST;
begin
  with TTestThread.Create(true) do
  try
    Test := self;
    {$ifdef ISDELPHI2010}
    Start;
    {$else}
    Resume;
    {$endif}
    while Test<>nil do
      CheckSynchronize(1);
  finally
    Free;
  end;
  fClient.Server.ServicesRouting := rmJSON_RPC; // back to default
end;
{$endif}

procedure TTestServiceOrientedArchitecture.CustomRecordLayout;
begin
  TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TSQLRestCacheEntryValue),
    TTestServiceOrientedArchitecture.CustomReader,
    TTestServiceOrientedArchitecture.CustomWriter);
  try
    ClientTest(rmREST);
  finally
    TTextWriter.RegisterCustomJSONSerializer(TypeInfo(TSQLRestCacheEntryValue),nil,nil);
  end;
end;

class function TTestServiceOrientedArchitecture.CustomReader(P: PUTF8Char;
  var aValue; out aValid: Boolean): PUTF8Char;
var V: TSQLRestCacheEntryValue absolute aValue;
    Values: TPUtf8CharDynArray;
begin // {"ID":1786554763,"TimeStamp":323618765,"JSON":"D:\\TestSQL3.exe"}
  result := JSONDecode(P,['ID','TimeStamp','JSON'],Values);
  if result=nil then
    aValid := false else begin
    V.ID := GetInteger(Values[0]);
    V.TimeStamp := GetCardinal(Values[1]);
    V.JSON := Values[2];
    aValid := true;
  end;
end;

class procedure TTestServiceOrientedArchitecture.CustomWriter(
  const aWriter: TTextWriter; const aValue);
var V: TSQLRestCacheEntryValue absolute aValue;
begin
  aWriter.AddJSONEscape(['ID',V.ID,'TimeStamp',Int64(V.TimeStamp),'JSON',V.JSON]);
end;

type
  IChild = interface;

  IParent = interface
    procedure SetChild(const Value: IChild);
    function GetChild: IChild;
    function HasChild: boolean;
    property Child: IChild read GetChild write SetChild;
  end;

  IChild = interface
    procedure SetParent(const Value: IParent);
    function GetParent: IParent;
    property Parent: IParent read GetParent write SetParent;
  end;

  TParent = class(TInterfacedObject, IParent)
  private
    FChild: IChild;
    procedure SetChild(const Value: IChild);
    function GetChild: IChild;
  public
    destructor Destroy; override;
    function HasChild: boolean;
    property Child: IChild read GetChild write SetChild;
  end;

  TChild = class(TInterfacedObject, IChild)
  private
    FParent: IParent;
    procedure SetParent(const Value: IParent);
    function GetParent: IParent;
  public
    constructor Create(const AParent: IParent; SetChild: boolean);
    destructor Destroy; override;
    property Parent: IParent read GetParent write SetParent;
  end;

  TUseWeakRef = (direct,weakref,zeroing);

var
  ParentDestroyed, ChildDestroyed: boolean;
  UseWeakRef: TUseWeakRef;

procedure TTestServiceOrientedArchitecture.WeakInterfaces;
var Parent: IParent;
    Child, Child2: IChild;
    P: TParent;
    C: TChild;
procedure Init(aWeakRef: TUseWeakRef);
begin
  ParentDestroyed := false;
  ChildDestroyed := false;
  UseWeakRef := aWeakRef;
  Check(Parent=nil);
  Check(Child=nil);
  P := TParent.Create;
  Parent := P;
  Check(ObjectFromInterface(Parent)=P);
  C := TChild.Create(Parent,true);
  Child := C;
  Check(ObjectFromInterface(Child)=C);
  Parent.Child := Child;
end;
procedure WeakTest(aWeakRef: TUseWeakRef);
var Child2: IChild;
begin
  Init(aWeakRef);
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=false);
  Child2 := Parent.Child;
  Child2 := nil; // otherwise memory leak, but it is OK
  Check(ChildDestroyed=false);
  Child := nil;
  Check(ChildDestroyed=true);
  Check(ParentDestroyed=false);
  Check(Parent.HasChild=(aWeakRef=weakref),'ZEROed Weak');
  Parent := nil;
end;
begin
  Init(direct);
  Parent := nil;
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=false);
  Child := nil;
  Check(ParentDestroyed=false,'Without weak reference: memory leak');
  Check(ChildDestroyed=false);
  P._Release;
  Check(ParentDestroyed=true,'Manual release');
  Check(ChildDestroyed=true);
  WeakTest(weakref);
  Init(zeroing);
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=false);
  Child2 := Parent.Child;
  Child2 := nil; 
  Check(ChildDestroyed=false);
  Parent := nil;
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=false);
  Child := nil;
  Check(ParentDestroyed=true);
  Check(ChildDestroyed=true);
  WeakTest(zeroing);
  Init(zeroing);
  Check(Parent.HasChild);
  Child2 := TChild.Create(Parent,false);
  Check(Parent.HasChild);
  Parent.Child := Child2;
  Check(Parent.HasChild);
  Child2 := nil;
  Check(not Parent.HasChild);
  Check(ChildDestroyed=true);
  ChildDestroyed := false;
  Check(not Parent.HasChild);
  Child := nil;
  Check(ParentDestroyed=false);
  Check(ChildDestroyed=true);
  Check(not Parent.HasChild);
  ChildDestroyed := false;
  Parent := nil;
  Check(ParentDestroyed=true);
  Check(ChildDestroyed=false);
end;


{ TParent }

destructor TParent.Destroy;
begin
  ParentDestroyed := true;
  inherited;
end;

function TParent.GetChild: IChild;
begin
  result := FChild;
end;

function TParent.HasChild: boolean;
begin
  result := FChild<>nil;
end;

procedure TParent.SetChild(const Value: IChild);
begin
  case UseWeakRef of
  direct:  FChild := Value;
  weakref: SetWeak(@FChild,Value);
  zeroing: SetWeakZero(self,@FChild,Value);
  end;
end;

{ TChild }

constructor TChild.Create(const AParent: IParent; SetChild: boolean);
begin
  FParent := AParent;
  if SetChild then
    FParent.Child := self;
end;

destructor TChild.Destroy;
begin
  ChildDestroyed := true;
  inherited;
end;

function TChild.GetParent: IParent;
begin
  result := FParent;
end;

procedure TChild.SetParent(const Value: IParent);
begin
  case UseWeakRef of
  direct:  FParent := Value;
  weakref: SetWeak(@FParent,Value);
  zeroing: SetWeakZero(self,@FParent,Value);
  end;
end;

{$endif DELPHI5OROLDER}
{$endif CPU64}
{$endif FPC}


end.

