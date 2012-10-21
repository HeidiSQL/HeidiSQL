{==============================================================================|
| Project : Ararat Synapse                                       | 007.005.004 |
|==============================================================================|
| Content: Serial port support                                                 |
|==============================================================================|
| Copyright (c)2001-2012, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2001-2012.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|  (c)2002, Hans-Georg Joepgen (cpom Comport Ownership Manager and bugfixes)   |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{: @abstract(Serial port communication library)
This unit contains a class that implements serial port communication 
 for Windows, Linux, Unix or MacOSx. This class provides numerous methods with 
 same name and functionality as methods of the Ararat Synapse TCP/IP library.

The following is a small example how establish a connection by modem (in this
case with my USB modem):
@longcode(#
  ser:=TBlockSerial.Create;
  try
    ser.Connect('COM3');
    ser.config(460800,8,'N',0,false,true);
    ser.ATCommand('AT');
    if (ser.LastError <> 0) or (not ser.ATResult) then
      Exit;
    ser.ATConnect('ATDT+420971200111');
    if (ser.LastError <> 0) or (not ser.ATResult) then
      Exit;
    // you are now connected to a modem at +420971200111
    // you can transmit or receive data now
  finally
    ser.free;
  end;
#)
}

//old Delphi does not have MSWINDOWS define.
{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

//Kylix does not known UNIX define
{$IFDEF LINUX}
  {$IFNDEF UNIX}
    {$DEFINE UNIX}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
  {$IFDEF MSWINDOWS}
    {$ASMMODE intel}
  {$ENDIF}
  {define working mode w/o LIBC for fpc}
  {$DEFINE NO_LIBC}
{$ENDIF}
{$Q-}
{$H+}
{$M+}

unit synaser;

interface

uses
{$IFNDEF MSWINDOWS}
  {$IFNDEF NO_LIBC}
  Libc,
  KernelIoctl,
  {$ELSE}
  termio, baseunix, unix,
  {$ENDIF}
  {$IFNDEF FPC}
  Types,
  {$ENDIF}
{$ELSE}
  Windows, registry,
  {$IFDEF FPC}
  winver,
  {$ENDIF}
{$ENDIF}
  synafpc,
  Classes, SysUtils, synautil;

const
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  cSerialChunk = 8192;

  LockfileDirectory = '/var/lock'; {HGJ}
  PortIsClosed = -1;               {HGJ}
  ErrAlreadyOwned = 9991;          {HGJ}
  ErrAlreadyInUse = 9992;          {HGJ}
  ErrWrongParameter = 9993;        {HGJ}
  ErrPortNotOpen = 9994;           {HGJ}
  ErrNoDeviceAnswer =  9995;       {HGJ}
  ErrMaxBuffer = 9996;
  ErrTimeout = 9997;
  ErrNotRead = 9998;
  ErrFrame = 9999;
  ErrOverrun = 10000;
  ErrRxOver = 10001;
  ErrRxParity = 10002;
  ErrTxFull = 10003;

  dcb_Binary = $00000001;
  dcb_ParityCheck = $00000002;
  dcb_OutxCtsFlow = $00000004;
  dcb_OutxDsrFlow = $00000008;
  dcb_DtrControlMask = $00000030;
  dcb_DtrControlDisable = $00000000;
  dcb_DtrControlEnable = $00000010;
  dcb_DtrControlHandshake = $00000020;
  dcb_DsrSensivity = $00000040;
  dcb_TXContinueOnXoff = $00000080;
  dcb_OutX = $00000100;
  dcb_InX = $00000200;
  dcb_ErrorChar = $00000400;
  dcb_NullStrip = $00000800;
  dcb_RtsControlMask = $00003000;
  dcb_RtsControlDisable = $00000000;
  dcb_RtsControlEnable = $00001000;
  dcb_RtsControlHandshake = $00002000;
  dcb_RtsControlToggle = $00003000;
  dcb_AbortOnError = $00004000;
  dcb_Reserveds = $FFFF8000;

  {:stopbit value for 1 stopbit}
  SB1 = 0;
  {:stopbit value for 1.5 stopbit}
  SB1andHalf = 1;
  {:stopbit value for 2 stopbits}
  SB2 = 2;

{$IFNDEF MSWINDOWS}
const
  INVALID_HANDLE_VALUE = THandle(-1);
  CS7fix = $0000020;

type
  TDCB = record
    DCBlength: DWORD;
    BaudRate: DWORD;
    Flags: Longint;
    wReserved: Word;
    XonLim: Word;
    XoffLim: Word;
    ByteSize: Byte;
    Parity: Byte;
    StopBits: Byte;
    XonChar: CHAR;
    XoffChar: CHAR;
    ErrorChar: CHAR;
    EofChar: CHAR;
    EvtChar: CHAR;
    wReserved1: Word;
  end;
  PDCB = ^TDCB;

const
{$IFDEF UNIX}
  {$IFDEF BSD}
  MaxRates = 18;  //MAC
  {$ELSE}
   MaxRates = 30; //UNIX
  {$ENDIF}
{$ELSE}
  MaxRates = 19;  //WIN
{$ENDIF}
  Rates: array[0..MaxRates, 0..1] of cardinal =
  (
    (0, B0),
    (50, B50),
    (75, B75),
    (110, B110),
    (134, B134),
    (150, B150),
    (200, B200),
    (300, B300),
    (600, B600),
    (1200, B1200),
    (1800, B1800),
    (2400, B2400),
    (4800, B4800),
    (9600, B9600),
    (19200, B19200),
    (38400, B38400),
    (57600, B57600),
    (115200, B115200),
    (230400, B230400)
{$IFNDEF BSD}
    ,(460800, B460800)
  {$IFDEF UNIX}
    ,(500000, B500000),
    (576000, B576000),
    (921600, B921600),
    (1000000, B1000000),
    (1152000, B1152000),
    (1500000, B1500000),
    (2000000, B2000000),
    (2500000, B2500000),
    (3000000, B3000000),
    (3500000, B3500000),
    (4000000, B4000000)
  {$ENDIF}
{$ENDIF}
    );
{$ENDIF}

{$IFDEF DARWIN}
const // From fcntl.h
  O_SYNC = $0080;  { synchronous writes }
{$ENDIF}

const
  sOK = 0;
  sErr = integer(-1);

type

  {:Possible status event types for @link(THookSerialStatus)}
  THookSerialReason = (
    HR_SerialClose,
    HR_Connect,
    HR_CanRead,
    HR_CanWrite,
    HR_ReadCount,
    HR_WriteCount,
    HR_Wait
    );

  {:procedural prototype for status event hooking}
  THookSerialStatus = procedure(Sender: TObject; Reason: THookSerialReason;
    const Value: string) of object;

  {:@abstract(Exception type for SynaSer errors)}
  ESynaSerError = class(Exception)
  public
    ErrorCode: integer;
    ErrorMessage: string;
  end;

  {:@abstract(Main class implementing all communication routines)}
  TBlockSerial = class(TObject)
  protected
    FOnStatus: THookSerialStatus;
    Fhandle: THandle;
    FTag: integer;
    FDevice: string;
    FLastError: integer;
    FLastErrorDesc: string;
    FBuffer: AnsiString;
    FRaiseExcept: boolean;
    FRecvBuffer: integer;
    FSendBuffer: integer;
    FModemWord: integer;
    FRTSToggle: Boolean;
    FDeadlockTimeout: integer;
    FInstanceActive: boolean;      {HGJ}
    FTestDSR: Boolean;
    FTestCTS: Boolean;
    FLastCR: Boolean;
    FLastLF: Boolean;
    FMaxLineLength: Integer;
    FLinuxLock: Boolean;
    FMaxSendBandwidth: Integer;
    FNextSend: LongWord;
    FMaxRecvBandwidth: Integer;
    FNextRecv: LongWord;
    FConvertLineEnd: Boolean;
    FATResult: Boolean;
    FAtTimeout: integer;
    FInterPacketTimeout: Boolean;
    FComNr: integer;
{$IFDEF MSWINDOWS}
    FPortAddr: Word;
    function CanEvent(Event: dword; Timeout: integer): boolean;
    procedure DecodeCommError(Error: DWord); virtual;
 {$IFDEF WIN32}
    function GetPortAddr: Word;  virtual;
    function ReadTxEmpty(PortAddr: Word): Boolean; virtual;
 {$ENDIF}
{$ENDIF}
    procedure SetSizeRecvBuffer(size: integer); virtual;
    function GetDSR: Boolean; virtual;
    procedure SetDTRF(Value: Boolean); virtual;
    function GetCTS: Boolean; virtual;
    procedure SetRTSF(Value: Boolean); virtual;
    function GetCarrier: Boolean; virtual;
    function GetRing: Boolean; virtual;
    procedure DoStatus(Reason: THookSerialReason; const Value: string); virtual;
    procedure GetComNr(Value: string); virtual;
    function PreTestFailing: boolean; virtual;{HGJ}
    function TestCtrlLine: Boolean; virtual;
{$IFDEF UNIX}    
    procedure DcbToTermios(const dcb: TDCB; var term: termios); virtual;
    procedure TermiosToDcb(const term: termios; var dcb: TDCB); virtual;
    function ReadLockfile: integer; virtual;
    function LockfileName: String; virtual;
    procedure CreateLockfile(PidNr: integer); virtual;
{$ENDIF}
    procedure LimitBandwidth(Length: Integer; MaxB: integer; var Next: LongWord); virtual;
    procedure SetBandwidth(Value: Integer); virtual;
  public
    {: data Control Block with communication parameters. Usable only when you
     need to call API directly.}
    DCB: Tdcb;
{$IFDEF UNIX}
    TermiosStruc: termios;
{$ENDIF}
    {:Object constructor.}
    constructor Create;
    {:Object destructor.}
    destructor Destroy; override;

    {:Returns a string containing the version number of the library.}
    class function GetVersion: string; virtual;

    {:Destroy handle in use. It close connection to serial port.}
    procedure CloseSocket; virtual;

    {:Reconfigure communication parameters on the fly. You must be connected to
     port before!
     @param(baud Define connection speed. Baud rate can be from 50 to 4000000
      bits per second. (it depends on your hardware!))
     @param(bits Number of bits in communication.)
     @param(parity Define communication parity (N - None, O - Odd, E - Even, M - Mark or S - Space).)
     @param(stop Define number of stopbits. Use constants @link(SB1),
      @link(SB1andHalf) and @link(SB2).)
     @param(softflow Enable XON/XOFF handshake.)
     @param(hardflow Enable CTS/RTS handshake.)}
    procedure Config(baud, bits: integer; parity: char; stop: integer;
      softflow, hardflow: boolean); virtual;

    {:Connects to the port indicated by comport. Comport can be used in Windows
     style (COM2), or in Linux style (/dev/ttyS1). When you use windows style
     in Linux, then it will be converted to Linux name. And vice versa! However
     you can specify any device name! (other device names then standart is not
     converted!)

     After successfull connection the DTR signal is set (if you not set hardware
     handshake, then the RTS signal is set, too!)

     Connection parameters is predefined by your system configuration. If you
     need use another parameters, then you can use Config method after.
     Notes:

      - Remember, the commonly used serial Laplink cable does not support
       hardware handshake.

      - Before setting any handshake you must be sure that it is supported by
       your hardware.

      - Some serial devices are slow. In some cases you must wait up to a few
       seconds after connection for the device to respond.

      - when you connect to a modem device, then is best to test it by an empty
       AT command. (call ATCommand('AT'))}
    procedure Connect(comport: string); virtual;

    {:Set communication parameters from the DCB structure (the DCB structure is
     simulated under Linux).}
    procedure SetCommState; virtual;

    {:Read communication parameters into the DCB structure (DCB structure is
     simulated under Linux).}
    procedure GetCommState; virtual;

    {:Sends Length bytes of data from Buffer through the connected port.}
    function SendBuffer(buffer: pointer; length: integer): integer; virtual;

    {:One data BYTE is sent.}
    procedure SendByte(data: byte); virtual;

    {:Send the string in the data parameter. No terminator is appended by this
     method. If you need to send a string with CR/LF terminator, you must append
     the CR/LF characters to the data string!

     Since no terminator is appended, you can use this function for sending
     binary data too.}
    procedure SendString(data: AnsiString); virtual;

    {:send four bytes as integer.}
    procedure SendInteger(Data: integer); virtual;

    {:send data as one block. Each block begins with integer value with Length
     of block.}
    procedure SendBlock(const Data: AnsiString); virtual;

    {:send content of stream from current position}
    procedure SendStreamRaw(const Stream: TStream); virtual;

    {:send content of stream as block. see @link(SendBlock)}
    procedure SendStream(const Stream: TStream); virtual;

    {:send content of stream as block, but this is compatioble with Indy library.
     (it have swapped lenght of block). See @link(SendStream)}
    procedure SendStreamIndy(const Stream: TStream); virtual;

    {:Waits until the allocated buffer is filled by received data. Returns number
     of data bytes received, which equals to the Length value under normal
     operation. If it is not equal, the communication channel is possibly broken.

     This method not using any internal buffering, like all others receiving
     methods. You cannot freely combine this method with all others receiving
     methods!}
    function RecvBuffer(buffer: pointer; length: integer): integer; virtual;

    {:Method waits until data is received. If no data is received within
     the Timeout (in milliseconds) period, @link(LastError) is set to
     @link(ErrTimeout). This method is used to read any amount of data
     (e. g. 1MB), and may be freely combined with all receviving methods what
     have Timeout parameter, like the @link(RecvString), @link(RecvByte) or
     @link(RecvTerminated) methods.}
    function RecvBufferEx(buffer: pointer; length: integer; timeout: integer): integer; virtual;

    {:It is like recvBufferEx, but data is readed to dynamicly allocated binary
     string.}
    function RecvBufferStr(Length: Integer; Timeout: Integer): AnsiString; virtual;

    {:Read all available data and return it in the function result string. This
     function may be combined with @link(RecvString), @link(RecvByte) or related
     methods.}
    function RecvPacket(Timeout: Integer): AnsiString; virtual;

    {:Waits until one data byte is received which is returned as the function
     result. If no data is received within the Timeout (in milliseconds) period,
     @link(LastError) is set to @link(ErrTimeout).}
    function RecvByte(timeout: integer): byte; virtual;

    {:This method waits until a terminated data string is received. This string
     is terminated by the Terminator string. The resulting string is returned
     without this termination string! If no data is received within the Timeout
     (in milliseconds) period, @link(LastError) is set to @link(ErrTimeout).}
    function RecvTerminated(Timeout: Integer; const Terminator: AnsiString): AnsiString; virtual;

    {:This method waits until a terminated data string is received. The string
     is terminated by a CR/LF sequence. The resulting string is returned without
     the terminator (CR/LF)! If no data is received within the Timeout (in
     milliseconds) period, @link(LastError) is set to @link(ErrTimeout).

     If @link(ConvertLineEnd) is used, then the CR/LF sequence may not be exactly
     CR/LF. See the description of @link(ConvertLineEnd).

     This method serves for line protocol implementation and uses its own
     buffers to maximize performance. Therefore do NOT use this method with the
     @link(RecvBuffer) method to receive data as it may cause data loss.}
    function Recvstring(timeout: integer): AnsiString; virtual;

    {:Waits until four data bytes are received which is returned as the function
     integer result. If no data is received within the Timeout (in milliseconds) period,
     @link(LastError) is set to @link(ErrTimeout).}
    function RecvInteger(Timeout: Integer): Integer; virtual;

    {:Waits until one data block is received. See @link(sendblock). If no data
     is received within the Timeout (in milliseconds) period, @link(LastError)
     is set to @link(ErrTimeout).}
    function RecvBlock(Timeout: Integer): AnsiString; virtual;

    {:Receive all data to stream, until some error occured. (for example timeout)}
    procedure RecvStreamRaw(const Stream: TStream; Timeout: Integer); virtual;

    {:receive requested count of bytes to stream}
    procedure RecvStreamSize(const Stream: TStream; Timeout: Integer; Size: Integer); virtual;

    {:receive block of data to stream. (Data can be sended by @link(sendstream)}
    procedure RecvStream(const Stream: TStream; Timeout: Integer); virtual;

    {:receive block of data to stream. (Data can be sended by @link(sendstreamIndy)}
    procedure RecvStreamIndy(const Stream: TStream; Timeout: Integer); virtual;

    {:Returns the number of received bytes waiting for reading. 0 is returned
     when there is no data waiting.}
    function WaitingData: integer; virtual;

    {:Same as @link(WaitingData), but in respect to data in the internal
     @link(LineBuffer).}
    function WaitingDataEx: integer; virtual;

    {:Returns the number of bytes waiting to be sent in the output buffer.
     0 is returned when the output buffer is empty.}
    function SendingData: integer; virtual;

    {:Enable or disable RTS driven communication (half-duplex). It can be used
     to communicate with RS485 converters, or other special equipment. If you
     enable this feature, the system automatically controls the RTS signal.

     Notes:

     - On Windows NT (or higher) ir RTS signal driven by system driver.

     - On Win9x family is used special code for waiting until last byte is
      sended from your UART.

     - On Linux you must have kernel 2.1 or higher!}
    procedure EnableRTSToggle(value: boolean); virtual;

    {:Waits until all data to is sent and buffers are emptied.
     Warning: On Windows systems is this method returns when all buffers are
     flushed to the serial port controller, before the last byte is sent!}
    procedure Flush; virtual;

    {:Unconditionally empty all buffers. It is good when you need to interrupt
     communication and for cleanups.}
    procedure Purge; virtual;

    {:Returns @True, if you can from read any data from the port. Status is
     tested for a period of time given by the Timeout parameter (in milliseconds).
     If the value of the Timeout parameter is 0, the status is tested only once
     and the function returns immediately. If the value of the Timeout parameter
     is set to -1, the function returns only after it detects data on the port
     (this may cause the process to hang).}
    function CanRead(Timeout: integer): boolean; virtual;

    {:Returns @True, if you can write any data to the port (this function is not
     sending the contents of the buffer). Status is tested for a period of time
     given by the Timeout parameter (in milliseconds). If the value of
     the Timeout parameter is 0, the status is tested only once and the function
     returns immediately. If the value of the  Timeout parameter is set to -1,
     the function returns only after it detects that it can write data to
     the port (this may cause the process to hang).}
    function CanWrite(Timeout: integer): boolean; virtual;

    {:Same as @link(CanRead), but the test is against data in the internal
    @link(LineBuffer) too.}
    function CanReadEx(Timeout: integer): boolean; virtual;

    {:Returns the status word of the modem. Decoding the status word could yield
     the status of carrier detect signaland other signals. This method is used
     internally by the modem status reading properties. You usually do not need
     to call this method directly.}
    function ModemStatus: integer; virtual;

    {:Send a break signal to the communication device for Duration milliseconds.}
    procedure SetBreak(Duration: integer); virtual;

    {:This function is designed to send AT commands to the modem. The AT command
     is sent in the Value parameter and the response is returned in the function
     return value (may contain multiple lines!).
     If the AT command is processed successfully (modem returns OK), then the
     @link(ATResult) property is set to True.

     This function is designed only for AT commands that return OK or ERROR
     response! To call connection commands the @link(ATConnect) method.
     Remember, when you connect to a modem device, it is in AT command mode.
     Now you can send AT commands to the modem. If you need to transfer data to
     the modem on the other side of the line, you must first switch to data mode
     using the @link(ATConnect) method.}
    function ATCommand(value: AnsiString): AnsiString; virtual;

    {:This function is used to send connect type AT commands to the modem. It is
     for commands to switch to connected state. (ATD, ATA, ATO,...)
     It sends the AT command in the Value parameter and returns the modem's
     response (may be multiple lines - usually with connection parameters info).
     If the AT command is processed successfully (the modem returns CONNECT),
     then the ATResult property is set to @True.

     This function is designed only for AT commands which respond by CONNECT,
     BUSY, NO DIALTONE NO CARRIER or ERROR. For other AT commands use the
     @link(ATCommand) method.

     The connect timeout is 90*@link(ATTimeout). If this command is successful
     (@link(ATresult) is @true), then the modem is in data state. When you now
     send or receive some data, it is not to or from your modem, but from the
     modem on other side of the line. Now you can transfer your data.
     If the connection attempt failed (@link(ATResult) is @False), then the
     modem is still in AT command mode.}
    function ATConnect(value: AnsiString): AnsiString; virtual;

    {:If you "manually" call API functions, forward their return code in
     the SerialResult parameter to this function, which evaluates it and sets
     @link(LastError) and @link(LastErrorDesc).}
    function SerialCheck(SerialResult: integer): integer; virtual;

    {:If @link(Lasterror) is not 0 and exceptions are enabled, then this procedure
     raises an exception. This method is used internally. You may need it only
     in special cases.}
    procedure ExceptCheck; virtual;

    {:Set Synaser to error state with ErrNumber code. Usually used by internal
     routines.}
    procedure SetSynaError(ErrNumber: integer); virtual;

    {:Raise Synaser error with ErrNumber code. Usually used by internal routines.}
    procedure RaiseSynaError(ErrNumber: integer); virtual;
{$IFDEF UNIX}
    function  cpomComportAccessible: boolean; virtual;{HGJ}
    procedure cpomReleaseComport; virtual; {HGJ}
{$ENDIF}
    {:True device name of currently used port}
    property Device: string read FDevice;

    {:Error code of last operation. Value is defined by the host operating
     system, but value 0 is always OK.}
    property LastError: integer read FLastError;

    {:Human readable description of LastError code.}
    property LastErrorDesc: string read FLastErrorDesc;

    {:Indicates if the last @link(ATCommand) or @link(ATConnect) method was successful}
    property ATResult: Boolean read FATResult;

    {:Read the value of the RTS signal.}
    property RTS: Boolean write SetRTSF;

    {:Indicates the presence of the CTS signal}
    property CTS: boolean read GetCTS;

    {:Use this property to set the value of the DTR signal.}
    property DTR: Boolean write SetDTRF;

    {:Exposes the status of the DSR signal.}
    property DSR: boolean read GetDSR;

    {:Indicates the presence of the Carrier signal}
    property Carrier: boolean read GetCarrier;

    {:Reflects the status of the Ring signal.}
    property Ring: boolean read GetRing;

    {:indicates if this instance of SynaSer is active. (Connected to some port)}
    property InstanceActive: boolean read FInstanceActive; {HGJ}

    {:Defines maximum bandwidth for all sending operations in bytes per second.
     If this value is set to 0 (default), bandwidth limitation is not used.}
    property MaxSendBandwidth: Integer read FMaxSendBandwidth Write FMaxSendBandwidth;

    {:Defines maximum bandwidth for all receiving operations in bytes per second.
     If this value is set to 0 (default), bandwidth limitation is not used.}
    property MaxRecvBandwidth: Integer read FMaxRecvBandwidth Write FMaxRecvBandwidth;

    {:Defines maximum bandwidth for all sending and receiving operations
     in bytes per second. If this value is set to 0 (default), bandwidth
     limitation is not used.}
    property MaxBandwidth: Integer Write SetBandwidth;

    {:Size of the Windows internal receive buffer. Default value is usually
     4096 bytes. Note: Valid only in Windows versions!}
    property SizeRecvBuffer: integer read FRecvBuffer write SetSizeRecvBuffer;
  published
    {:Returns the descriptive text associated with ErrorCode. You need this
     method only in special cases. Description of LastError is now accessible
     through the LastErrorDesc property.}
    class function GetErrorDesc(ErrorCode: integer): string;

    {:Freely usable property}
    property Tag: integer read FTag write FTag;

    {:Contains the handle of the open communication port.
    You may need this value to directly call communication functions outside
    SynaSer.}
    property Handle: THandle read Fhandle write FHandle;

    {:Internally used read buffer.}
    property LineBuffer: AnsiString read FBuffer write FBuffer;

    {:If @true, communication errors raise exceptions. If @false (default), only
     the @link(LastError) value is set.}
    property RaiseExcept: boolean read FRaiseExcept write FRaiseExcept;

    {:This event is triggered when the communication status changes. It can be
     used to monitor communication status.}
    property OnStatus: THookSerialStatus read FOnStatus write FOnStatus;

    {:If you set this property to @true, then the value of the DSR signal
     is tested before every data transfer. It can be used to detect the presence
     of a communications device.}
    property TestDSR: boolean read FTestDSR write FTestDSR;

    {:If you set this property to @true, then the value of the CTS signal
     is tested before every data transfer. It can be used to detect the presence
     of a communications device. Warning: This property cannot be used if you
     need hardware handshake!}
    property TestCTS: boolean read FTestCTS write FTestCTS;

    {:Use this property you to limit the maximum size of LineBuffer
     (as a protection against unlimited memory allocation for LineBuffer).
     Default value is 0 - no limit.}
    property MaxLineLength: Integer read FMaxLineLength Write FMaxLineLength;

    {:This timeout value is used as deadlock protection when trying to send data
     to (or receive data from) a device that stopped communicating during data
     transmission (e.g. by physically disconnecting the device).
     The timeout value is in milliseconds. The default value is 30,000 (30 seconds).}
    property DeadlockTimeout: Integer read FDeadlockTimeout Write FDeadlockTimeout;

    {:If set to @true (default value), port locking is enabled (under Linux only).
     WARNING: To use this feature, the application must run by a user with full
     permission to the /var/lock directory!}
    property LinuxLock: Boolean read FLinuxLock write FLinuxLock;

    {:Indicates if non-standard line terminators should be converted to a CR/LF pair
     (standard DOS line terminator). If @TRUE, line terminators CR, single LF
     or LF/CR are converted to CR/LF. Defaults to @FALSE.
     This property has effect only on the behavior of the RecvString method.}
    property ConvertLineEnd: Boolean read FConvertLineEnd Write FConvertLineEnd;

    {:Timeout for AT modem based operations}
    property AtTimeout: integer read FAtTimeout Write FAtTimeout;

    {:If @true (default), then all timeouts is timeout between two characters.
     If @False, then timeout is overall for whoole reading operation.}
    property InterPacketTimeout: Boolean read FInterPacketTimeout Write FInterPacketTimeout;
  end;

{:Returns list of existing computer serial ports. Working properly only in Windows!}
function GetSerialPortNames: string;

implementation

constructor TBlockSerial.Create;
begin
  inherited create;
  FRaiseExcept := false;
  FHandle := INVALID_HANDLE_VALUE;
  FDevice := '';
  FComNr:= PortIsClosed;               {HGJ}
  FInstanceActive:= false;             {HGJ}
  Fbuffer := '';
  FRTSToggle := False;
  FMaxLineLength := 0;
  FTestDSR := False;
  FTestCTS := False;
  FDeadlockTimeout := 30000;
  FLinuxLock := True;
  FMaxSendBandwidth := 0;
  FNextSend := 0;
  FMaxRecvBandwidth := 0;
  FNextRecv := 0;
  FConvertLineEnd := False;
  SetSynaError(sOK);
  FRecvBuffer := 4096;
  FLastCR := False;
  FLastLF := False;
  FAtTimeout := 1000;
  FInterPacketTimeout := True;
end;

destructor TBlockSerial.Destroy;
begin
  CloseSocket;
  inherited destroy;
end;

class function TBlockSerial.GetVersion: string;
begin
	Result := 'SynaSer 7.5.4';
end;

procedure TBlockSerial.CloseSocket;
begin
  if Fhandle <> INVALID_HANDLE_VALUE then
  begin
    Purge;
    RTS := False;
    DTR := False;
    FileClose(FHandle);
  end;
  if InstanceActive then
  begin
    {$IFDEF UNIX}
    if FLinuxLock then
      cpomReleaseComport;
    {$ENDIF}
    FInstanceActive:= false
  end;
  Fhandle := INVALID_HANDLE_VALUE;
  FComNr:= PortIsClosed;
  SetSynaError(sOK);
  DoStatus(HR_SerialClose, FDevice);
end;

{$IFDEF WIN32}
function TBlockSerial.GetPortAddr: Word;
begin
  Result := 0;
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
  begin
    EscapeCommFunction(FHandle, 10);
    asm
      MOV @Result, DX;
    end;
  end;
end;

function TBlockSerial.ReadTxEmpty(PortAddr: Word): Boolean;
begin
  Result := True;
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
  begin
    asm
      MOV DX, PortAddr;
      ADD DX, 5;
      IN AL, DX;
      AND AL, $40;
      JZ @K;
      MOV AL,1;
    @K: MOV @Result, AL;
    end;
  end;
end;
{$ENDIF}

procedure TBlockSerial.GetComNr(Value: string);
begin
  FComNr := PortIsClosed;
  if pos('COM', uppercase(Value)) = 1 then
    FComNr := StrToIntdef(copy(Value, 4, Length(Value) - 3), PortIsClosed + 1) - 1;
  if pos('/DEV/TTYS', uppercase(Value)) = 1 then
    FComNr := StrToIntdef(copy(Value, 10, Length(Value) - 9), PortIsClosed - 1);
end;

procedure TBlockSerial.SetBandwidth(Value: Integer);
begin
  MaxSendBandwidth := Value;
  MaxRecvBandwidth := Value;
end;

procedure TBlockSerial.LimitBandwidth(Length: Integer; MaxB: integer; var Next: LongWord);
var
  x: LongWord;
  y: LongWord;
begin
  if MaxB > 0 then
  begin
    y := GetTick;
    if Next > y then
    begin
      x := Next - y;
      if x > 0 then
      begin
        DoStatus(HR_Wait, IntToStr(x));
        sleep(x);
      end;
    end;
    Next := GetTick + Trunc((Length / MaxB) * 1000);
  end;
end;

procedure TBlockSerial.Config(baud, bits: integer; parity: char; stop: integer;
  softflow, hardflow: boolean);
begin
  FillChar(dcb, SizeOf(dcb), 0);
  GetCommState;
  dcb.DCBlength := SizeOf(dcb);
  dcb.BaudRate := baud;
  dcb.ByteSize := bits;
  case parity of
    'N', 'n': dcb.parity := 0;
    'O', 'o': dcb.parity := 1;
    'E', 'e': dcb.parity := 2;
    'M', 'm': dcb.parity := 3;
    'S', 's': dcb.parity := 4;
  end;
  dcb.StopBits := stop;
  dcb.XonChar := #17;
  dcb.XoffChar := #19;
  dcb.XonLim := FRecvBuffer div 4;
  dcb.XoffLim := FRecvBuffer div 4;
  dcb.Flags := dcb_Binary;
  if softflow then
    dcb.Flags := dcb.Flags or dcb_OutX or dcb_InX;
  if hardflow then
    dcb.Flags := dcb.Flags or dcb_OutxCtsFlow or dcb_RtsControlHandshake
  else
    dcb.Flags := dcb.Flags or dcb_RtsControlEnable;
  dcb.Flags := dcb.Flags or dcb_DtrControlEnable;
  if dcb.Parity > 0 then
    dcb.Flags := dcb.Flags or dcb_ParityCheck;
  SetCommState;
end;

procedure TBlockSerial.Connect(comport: string);
{$IFDEF MSWINDOWS}
var
  CommTimeouts: TCommTimeouts;
{$ENDIF}
begin
  // Is this TBlockSerial Instance already busy?
  if InstanceActive then           {HGJ}
  begin                            {HGJ}
    RaiseSynaError(ErrAlreadyInUse);
    Exit;                          {HGJ}
  end;                             {HGJ}
  FBuffer := '';
  FDevice := comport;
  GetComNr(comport);
{$IFDEF MSWINDOWS}
  SetLastError (sOK);
{$ELSE}
  {$IFNDEF FPC}
  SetLastError (sOK);
  {$ELSE}
  fpSetErrno(sOK);
  {$ENDIF}
{$ENDIF}
{$IFNDEF MSWINDOWS}
  if FComNr <> PortIsClosed then
    FDevice := '/dev/ttyS' + IntToStr(FComNr);
  // Comport already owned by another process?          {HGJ}
  if FLinuxLock then
    if not cpomComportAccessible then
    begin
      RaiseSynaError(ErrAlreadyOwned);
      Exit;
    end;
{$IFNDEF FPC}
  FHandle := THandle(Libc.open(pchar(FDevice), O_RDWR or O_SYNC));
{$ELSE}
  FHandle := THandle(fpOpen(FDevice, O_RDWR or O_SYNC));
{$ENDIF}
  if FHandle = INVALID_HANDLE_VALUE then  //because THandle is not integer on all platforms!
    SerialCheck(-1)
  else
    SerialCheck(0);
  {$IFDEF UNIX}
  if FLastError <> sOK then
    if FLinuxLock then
      cpomReleaseComport;
  {$ENDIF}
  ExceptCheck;
  if FLastError <> sOK then
    Exit;
{$ELSE}
  if FComNr <> PortIsClosed then
    FDevice := '\\.\COM' + IntToStr(FComNr + 1);
  FHandle := THandle(CreateFile(PChar(FDevice), GENERIC_READ or GENERIC_WRITE,
    0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED, 0));
  if FHandle = INVALID_HANDLE_VALUE then  //because THandle is not integer on all platforms!
    SerialCheck(-1)
  else
    SerialCheck(0);
  ExceptCheck;
  if FLastError <> sOK then
    Exit;
  SetCommMask(FHandle, 0);
  SetupComm(Fhandle, FRecvBuffer, 0);
  CommTimeOuts.ReadIntervalTimeout := MAXWORD;
  CommTimeOuts.ReadTotalTimeoutMultiplier := 0;
  CommTimeOuts.ReadTotalTimeoutConstant := 0;
  CommTimeOuts.WriteTotalTimeoutMultiplier := 0;
  CommTimeOuts.WriteTotalTimeoutConstant := 0;
  SetCommTimeOuts(FHandle, CommTimeOuts);
  {$IFDEF WIN32}
  FPortAddr := GetPortAddr;
  {$ENDIF}
{$ENDIF}
  SetSynaError(sOK);
  if not TestCtrlLine then  {HGJ}
  begin
    SetSynaError(ErrNoDeviceAnswer);
    FileClose(FHandle);         {HGJ}
    {$IFDEF UNIX}
    if FLinuxLock then
      cpomReleaseComport;                {HGJ}
    {$ENDIF}                             {HGJ}
    Fhandle := INVALID_HANDLE_VALUE;     {HGJ}
    FComNr:= PortIsClosed;               {HGJ}
  end
  else
  begin
    FInstanceActive:= True;
    RTS := True;
    DTR := True;
    Purge;
  end;
  ExceptCheck;
  DoStatus(HR_Connect, FDevice);
end;

function TBlockSerial.SendBuffer(buffer: pointer; length: integer): integer;
{$IFDEF MSWINDOWS}
var
  Overlapped: TOverlapped;
  x, y, Err: DWord;
{$ENDIF}
begin
  Result := 0;
  if PreTestFailing then   {HGJ}
    Exit;                  {HGJ}
  LimitBandwidth(Length, FMaxSendBandwidth, FNextsend);
  if FRTSToggle then
  begin
    Flush;
    RTS := True;
  end;
{$IFNDEF MSWINDOWS}
  result := FileWrite(Fhandle, Buffer^, Length);
  serialcheck(result);
{$ELSE}
  FillChar(Overlapped, Sizeof(Overlapped), 0);
  SetSynaError(sOK);
  y := 0;
  if not WriteFile(FHandle, Buffer^, Length, DWord(Result), @Overlapped) then
    y := GetLastError;
  if y = ERROR_IO_PENDING then
  begin
    x := WaitForSingleObject(FHandle, FDeadlockTimeout);
    if x = WAIT_TIMEOUT then
    begin
      PurgeComm(FHandle, PURGE_TXABORT);
      SetSynaError(ErrTimeout);
    end;
    GetOverlappedResult(FHandle, Overlapped, Dword(Result), False);
  end
  else
    SetSynaError(y);
  ClearCommError(FHandle, err, nil);
  if err <> 0 then
    DecodeCommError(err);
{$ENDIF}
  if FRTSToggle then
  begin
    Flush;
    CanWrite(255);
    RTS := False;
  end;
  ExceptCheck;
  DoStatus(HR_WriteCount, IntToStr(Result));
end;

procedure TBlockSerial.SendByte(data: byte);
begin
  SendBuffer(@Data, 1);
end;

procedure TBlockSerial.SendString(data: AnsiString);
begin
  SendBuffer(Pointer(Data), Length(Data));
end;

procedure TBlockSerial.SendInteger(Data: integer);
begin
  SendBuffer(@data, SizeOf(Data));
end;

procedure TBlockSerial.SendBlock(const Data: AnsiString);
begin
  SendInteger(Length(data));
  SendString(Data);
end;

procedure TBlockSerial.SendStreamRaw(const Stream: TStream);
var
  si: integer;
  x, y, yr: integer;
  s: AnsiString;
begin
  si := Stream.Size - Stream.Position;
  x := 0;
  while x < si do
  begin
    y := si - x;
    if y > cSerialChunk then
      y := cSerialChunk;
    Setlength(s, y);
    yr := Stream.read(PAnsiChar(s)^, y);
    if yr > 0 then
    begin
      SetLength(s, yr);
      SendString(s);
      Inc(x, yr);
    end
    else
      break;
  end;
end;

procedure TBlockSerial.SendStreamIndy(const Stream: TStream);
var
  si: integer;
begin
  si := Stream.Size - Stream.Position;
  si := Swapbytes(si);
  SendInteger(si);
  SendStreamRaw(Stream);
end;

procedure TBlockSerial.SendStream(const Stream: TStream);
var
  si: integer;
begin
  si := Stream.Size - Stream.Position;
  SendInteger(si);
  SendStreamRaw(Stream);
end;

function TBlockSerial.RecvBuffer(buffer: pointer; length: integer): integer;
{$IFNDEF MSWINDOWS}
begin
  Result := 0;
  if PreTestFailing then   {HGJ}
    Exit;                  {HGJ}
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
  result := FileRead(FHandle, Buffer^, length);
  serialcheck(result);
{$ELSE}
var
  Overlapped: TOverlapped;
  x, y, Err: DWord;
begin
  Result := 0;
  if PreTestFailing then   {HGJ}
    Exit;                  {HGJ}
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
  FillChar(Overlapped, Sizeof(Overlapped), 0);
  SetSynaError(sOK);
  y := 0;
  if not ReadFile(FHandle, Buffer^, length, Dword(Result), @Overlapped) then
    y := GetLastError;
  if y = ERROR_IO_PENDING then
  begin
    x := WaitForSingleObject(FHandle, FDeadlockTimeout);
    if x = WAIT_TIMEOUT then
    begin
      PurgeComm(FHandle, PURGE_RXABORT);
      SetSynaError(ErrTimeout);
    end;
    GetOverlappedResult(FHandle, Overlapped, Dword(Result), False);
  end
  else
    SetSynaError(y);
  ClearCommError(FHandle, err, nil);
  if err <> 0 then
    DecodeCommError(err);
{$ENDIF}
  ExceptCheck;
  DoStatus(HR_ReadCount, IntToStr(Result));
end;

function TBlockSerial.RecvBufferEx(buffer: pointer; length: integer; timeout: integer): integer;
var
  s: AnsiString;
  rl, l: integer;
  ti: LongWord;
begin
  Result := 0;
  if PreTestFailing then   {HGJ}
    Exit;                  {HGJ}
  SetSynaError(sOK);
  rl := 0;
  repeat
    ti := GetTick;
    s := RecvPacket(Timeout);
    l := System.Length(s);
    if (rl + l) > Length then
      l := Length - rl;
    Move(Pointer(s)^, IncPoint(Buffer, rl)^, l);
    rl := rl + l;
    if FLastError <> sOK then
      Break;
    if rl >= Length then
      Break;
    if not FInterPacketTimeout then
    begin
      Timeout := Timeout - integer(TickDelta(ti, GetTick));
      if Timeout <= 0 then
      begin
        SetSynaError(ErrTimeout);
        Break;
      end;
    end;
  until False;
  delete(s, 1, l);
  FBuffer := s;
  Result := rl;
end;

function TBlockSerial.RecvBufferStr(Length: Integer; Timeout: Integer): AnsiString;
var
  x: integer;
begin
  Result := '';
  if PreTestFailing then   {HGJ}
    Exit;                  {HGJ}
  SetSynaError(sOK);
  if Length > 0 then
  begin
    Setlength(Result, Length);
    x := RecvBufferEx(PAnsiChar(Result), Length , Timeout);
    if FLastError = sOK then
      SetLength(Result, x)
    else
      Result := '';
  end;
end;

function TBlockSerial.RecvPacket(Timeout: Integer): AnsiString;
var
  x: integer;
begin
  Result := '';
  if PreTestFailing then   {HGJ}
    Exit;                  {HGJ}
  SetSynaError(sOK);
  if FBuffer <> '' then
  begin
    Result := FBuffer;
    FBuffer := '';
  end
  else
  begin
    //not drain CPU on large downloads...
    Sleep(0);
    x := WaitingData;
    if x > 0 then
    begin
      SetLength(Result, x);
      x := RecvBuffer(Pointer(Result), x);
      if x >= 0 then
        SetLength(Result, x);
    end
    else
    begin
      if CanRead(Timeout) then
      begin
        x := WaitingData;
        if x = 0 then
          SetSynaError(ErrTimeout);
        if x > 0 then
        begin
          SetLength(Result, x);
          x := RecvBuffer(Pointer(Result), x);
          if x >= 0 then
            SetLength(Result, x);
        end;
      end
      else
        SetSynaError(ErrTimeout);
    end;
  end;
  ExceptCheck;
end;


function TBlockSerial.RecvByte(timeout: integer): byte;
begin
  Result := 0;
  if PreTestFailing then   {HGJ}
    Exit;                  {HGJ}
  SetSynaError(sOK);
  if FBuffer = '' then
    FBuffer := RecvPacket(Timeout);
  if (FLastError = sOK) and (FBuffer <> '') then
  begin
    Result := Ord(FBuffer[1]);
    System.Delete(FBuffer, 1, 1);
  end;
  ExceptCheck;
end;

function TBlockSerial.RecvTerminated(Timeout: Integer; const Terminator: AnsiString): AnsiString;
var
  x: Integer;
  s: AnsiString;
  l: Integer;
  CorCRLF: Boolean;
  t: ansistring;
  tl: integer;
  ti: LongWord;
begin
  Result := '';
  if PreTestFailing then   {HGJ}
    Exit;                  {HGJ}
  SetSynaError(sOK);
  l := system.Length(Terminator);
  if l = 0 then
    Exit;
  tl := l;
  CorCRLF := FConvertLineEnd and (Terminator = CRLF);
  s := '';
  x := 0;
  repeat
    ti := GetTick;
    //get rest of FBuffer or incomming new data...
    s := s + RecvPacket(Timeout);
    if FLastError <> sOK then
      Break;
    x := 0;
    if Length(s) > 0 then
      if CorCRLF then
      begin
        if FLastCR and (s[1] = LF) then
          Delete(s, 1, 1);
        if FLastLF and (s[1] = CR) then
          Delete(s, 1, 1);
        FLastCR := False;
        FLastLF := False;
        t := '';
        x := PosCRLF(s, t);
        tl := system.Length(t);
        if t = CR then
          FLastCR := True;
        if t = LF then
          FLastLF := True;
      end
      else
      begin
        x := pos(Terminator, s);
        tl := l;
      end;
    if (FMaxLineLength <> 0) and (system.Length(s) > FMaxLineLength) then
    begin
      SetSynaError(ErrMaxBuffer);
      Break;
    end;
    if x > 0 then
      Break;
    if not FInterPacketTimeout then
    begin
      Timeout := Timeout - integer(TickDelta(ti, GetTick));
      if Timeout <= 0 then
      begin
        SetSynaError(ErrTimeout);
        Break;
      end;
    end;
  until False;
  if x > 0 then
  begin
    Result := Copy(s, 1, x - 1);
    System.Delete(s, 1, x + tl - 1);
  end;
  FBuffer := s;
  ExceptCheck;
end;


function TBlockSerial.RecvString(Timeout: Integer): AnsiString;
var
  s: AnsiString;
begin
  Result := '';
  s := RecvTerminated(Timeout, #13 + #10);
  if FLastError = sOK then
    Result := s;
end;

function TBlockSerial.RecvInteger(Timeout: Integer): Integer;
var
  s: AnsiString;
begin
  Result := 0;
  s := RecvBufferStr(4, Timeout);
  if FLastError = 0 then
    Result := (ord(s[1]) + ord(s[2]) * 256) + (ord(s[3]) + ord(s[4]) * 256) * 65536;
end;

function TBlockSerial.RecvBlock(Timeout: Integer): AnsiString;
var
  x: integer;
begin
  Result := '';
  x := RecvInteger(Timeout);
  if FLastError = 0 then
    Result := RecvBufferStr(x, Timeout);
end;

procedure TBlockSerial.RecvStreamRaw(const Stream: TStream; Timeout: Integer);
var
  s: AnsiString;
begin
  repeat
    s := RecvPacket(Timeout);
    if FLastError = 0 then
      WriteStrToStream(Stream, s);
  until FLastError <> 0;
end;

procedure TBlockSerial.RecvStreamSize(const Stream: TStream; Timeout: Integer; Size: Integer);
var
  s: AnsiString;
  n: integer;
begin
  for n := 1 to (Size div cSerialChunk) do
  begin
    s := RecvBufferStr(cSerialChunk, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(PAnsichar(s)^, cSerialChunk);
  end;
  n := Size mod cSerialChunk;
  if n > 0 then
  begin
    s := RecvBufferStr(n, Timeout);
    if FLastError <> 0 then
      Exit;
    Stream.Write(PAnsichar(s)^, n);
  end;
end;

procedure TBlockSerial.RecvStreamIndy(const Stream: TStream; Timeout: Integer);
var
  x: integer;
begin
  x := RecvInteger(Timeout);
  x := SwapBytes(x);
  if FLastError = 0 then
    RecvStreamSize(Stream, Timeout, x);
end;

procedure TBlockSerial.RecvStream(const Stream: TStream; Timeout: Integer);
var
  x: integer;
begin
  x := RecvInteger(Timeout);
  if FLastError = 0 then
    RecvStreamSize(Stream, Timeout, x);
end;

{$IFNDEF MSWINDOWS}
function TBlockSerial.WaitingData: integer;
begin
{$IFNDEF FPC}
  serialcheck(ioctl(FHandle, FIONREAD, @result));
{$ELSE}
  serialcheck(fpIoctl(FHandle, FIONREAD, @result));
{$ENDIF}
  if FLastError <> 0 then
    Result := 0;
  ExceptCheck;
end;
{$ELSE}
function TBlockSerial.WaitingData: integer;
var
  stat: TComStat;
  err: DWORD;
begin
  if ClearCommError(FHandle, err, @stat) then
  begin
    SetSynaError(sOK);
    Result := stat.cbInQue;
  end
  else
  begin
    SerialCheck(sErr);
    Result := 0;
  end;
  ExceptCheck;
end;
{$ENDIF}

function TBlockSerial.WaitingDataEx: integer;
begin
	if FBuffer <> '' then
  	Result := Length(FBuffer)
  else
  	Result := Waitingdata;
end;

{$IFNDEF MSWINDOWS}
function TBlockSerial.SendingData: integer;
begin
  SetSynaError(sOK);
  Result := 0;
end;
{$ELSE}
function TBlockSerial.SendingData: integer;
var
  stat: TComStat;
  err: DWORD;
begin
  SetSynaError(sOK);
  if not ClearCommError(FHandle, err, @stat) then
    serialcheck(sErr);
  ExceptCheck;
  result := stat.cbOutQue;
end;
{$ENDIF}

{$IFNDEF MSWINDOWS}
procedure TBlockSerial.DcbToTermios(const dcb: TDCB; var term: termios);
var
  n: integer;
  x: cardinal;
begin
  //others
  cfmakeraw(term);
  term.c_cflag := term.c_cflag or CREAD;
  term.c_cflag := term.c_cflag or CLOCAL;
  term.c_cflag := term.c_cflag or HUPCL;
  //hardware handshake
  if (dcb.flags and dcb_RtsControlHandshake) > 0 then
    term.c_cflag := term.c_cflag or CRTSCTS
  else
    term.c_cflag := term.c_cflag and (not CRTSCTS);
  //software handshake
  if (dcb.flags and dcb_OutX) > 0 then
    term.c_iflag := term.c_iflag or IXON or IXOFF or IXANY
  else
    term.c_iflag := term.c_iflag and (not (IXON or IXOFF or IXANY));
  //size of byte
  term.c_cflag := term.c_cflag and (not CSIZE);
  case dcb.bytesize of
    5:
      term.c_cflag := term.c_cflag or CS5;
    6:
      term.c_cflag := term.c_cflag or CS6;
    7:
{$IFDEF FPC}
      term.c_cflag := term.c_cflag or CS7;
{$ELSE}
      term.c_cflag := term.c_cflag or CS7fix;
{$ENDIF}
    8:
      term.c_cflag := term.c_cflag or CS8;
  end;
  //parity
  if (dcb.flags and dcb_ParityCheck) > 0 then
    term.c_cflag := term.c_cflag or PARENB
  else
    term.c_cflag := term.c_cflag and (not PARENB);
  case dcb.parity of
    1: //'O'
      term.c_cflag := term.c_cflag or PARODD;
    2: //'E'
      term.c_cflag := term.c_cflag and (not PARODD);
  end;
  //stop bits
  if dcb.stopbits > 0 then
    term.c_cflag := term.c_cflag or CSTOPB
  else
    term.c_cflag := term.c_cflag and (not CSTOPB);
  //set baudrate;
  x := 0;
  for n := 0 to Maxrates do
    if rates[n, 0] = dcb.BaudRate then
    begin
      x := rates[n, 1];
      break;
    end;
  cfsetospeed(term, x);
  cfsetispeed(term, x);
end;

procedure TBlockSerial.TermiosToDcb(const term: termios; var dcb: TDCB);
var
  n: integer;
  x: cardinal;
begin
  //set baudrate;
  dcb.baudrate := 0;
 {$IFDEF FPC}
  //why FPC not have cfgetospeed???
  x := term.c_oflag and $0F;
 {$ELSE}
  x := cfgetospeed(term);
 {$ENDIF}
  for n := 0 to Maxrates do
    if rates[n, 1] = x then
    begin
      dcb.baudrate := rates[n, 0];
      break;
    end;
  //hardware handshake
  if (term.c_cflag and CRTSCTS) > 0 then
    dcb.flags := dcb.flags or dcb_RtsControlHandshake or dcb_OutxCtsFlow
  else
    dcb.flags := dcb.flags and (not (dcb_RtsControlHandshake or dcb_OutxCtsFlow));
  //software handshake
  if (term.c_cflag and IXOFF) > 0 then
    dcb.flags := dcb.flags or dcb_OutX or dcb_InX
  else
    dcb.flags := dcb.flags and (not (dcb_OutX or dcb_InX));
  //size of byte
  case term.c_cflag and CSIZE of
    CS5:
      dcb.bytesize := 5;
    CS6:
      dcb.bytesize := 6;
    CS7fix:
      dcb.bytesize := 7;
    CS8:
      dcb.bytesize := 8;
  end;
  //parity
  if (term.c_cflag and PARENB) > 0 then
    dcb.flags := dcb.flags or dcb_ParityCheck
  else
    dcb.flags := dcb.flags and (not dcb_ParityCheck);
  dcb.parity := 0;
  if (term.c_cflag and PARODD) > 0 then
    dcb.parity := 1
  else
    dcb.parity := 2;
  //stop bits
  if (term.c_cflag and CSTOPB) > 0 then
    dcb.stopbits := 2
  else
    dcb.stopbits := 0;
end;
{$ENDIF}

{$IFNDEF MSWINDOWS}
procedure TBlockSerial.SetCommState;
begin
  DcbToTermios(dcb, termiosstruc);
  SerialCheck(tcsetattr(FHandle, TCSANOW, termiosstruc));
  ExceptCheck;
end;
{$ELSE}
procedure TBlockSerial.SetCommState;
begin
  SetSynaError(sOK);
  if not windows.SetCommState(Fhandle, dcb) then
    SerialCheck(sErr);
  ExceptCheck;
end;
{$ENDIF}

{$IFNDEF MSWINDOWS}
procedure TBlockSerial.GetCommState;
begin
  SerialCheck(tcgetattr(FHandle, termiosstruc));
  ExceptCheck;
  TermiostoDCB(termiosstruc, dcb);
end;
{$ELSE}
procedure TBlockSerial.GetCommState;
begin
  SetSynaError(sOK);
  if not windows.GetCommState(Fhandle, dcb) then
    SerialCheck(sErr);
  ExceptCheck;
end;
{$ENDIF}

procedure TBlockSerial.SetSizeRecvBuffer(size: integer);
begin
{$IFDEF MSWINDOWS}
  SetupComm(Fhandle, size, 0);
  GetCommState;
  dcb.XonLim := size div 4;
  dcb.XoffLim := size div 4;
  SetCommState;
{$ENDIF}
  FRecvBuffer := size;
end;

function TBlockSerial.GetDSR: Boolean;
begin
  ModemStatus;
{$IFNDEF MSWINDOWS}
  Result := (FModemWord and TIOCM_DSR) > 0;
{$ELSE}
  Result := (FModemWord and MS_DSR_ON) > 0;
{$ENDIF}
end;

procedure TBlockSerial.SetDTRF(Value: Boolean);
begin
{$IFNDEF MSWINDOWS}
  ModemStatus;
  if Value then
    FModemWord := FModemWord or TIOCM_DTR
  else
    FModemWord := FModemWord and not TIOCM_DTR;
  {$IFNDEF FPC}
  ioctl(FHandle, TIOCMSET, @FModemWord);
  {$ELSE}
  fpioctl(FHandle, TIOCMSET, @FModemWord);
  {$ENDIF}
{$ELSE}
  if Value then
    EscapeCommFunction(FHandle, SETDTR)
  else
    EscapeCommFunction(FHandle, CLRDTR);
{$ENDIF}
end;

function TBlockSerial.GetCTS: Boolean;
begin
  ModemStatus;
{$IFNDEF MSWINDOWS}
  Result := (FModemWord and TIOCM_CTS) > 0;
{$ELSE}
  Result := (FModemWord and MS_CTS_ON) > 0;
{$ENDIF}
end;

procedure TBlockSerial.SetRTSF(Value: Boolean);
begin
{$IFNDEF MSWINDOWS}
  ModemStatus;
  if Value then
    FModemWord := FModemWord or TIOCM_RTS
  else
    FModemWord := FModemWord and not TIOCM_RTS;
  {$IFNDEF FPC}
  ioctl(FHandle, TIOCMSET, @FModemWord);
  {$ELSE}
  fpioctl(FHandle, TIOCMSET, @FModemWord);
  {$ENDIF}
{$ELSE}
  if Value then
    EscapeCommFunction(FHandle, SETRTS)
  else
    EscapeCommFunction(FHandle, CLRRTS);
{$ENDIF}
end;

function TBlockSerial.GetCarrier: Boolean;
begin
  ModemStatus;
{$IFNDEF MSWINDOWS}
  Result := (FModemWord and TIOCM_CAR) > 0;
{$ELSE}
  Result := (FModemWord and MS_RLSD_ON) > 0;
{$ENDIF}
end;

function TBlockSerial.GetRing: Boolean;
begin
  ModemStatus;
{$IFNDEF MSWINDOWS}
  Result := (FModemWord and TIOCM_RNG) > 0;
{$ELSE}
  Result := (FModemWord and MS_RING_ON) > 0;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
function TBlockSerial.CanEvent(Event: dword; Timeout: integer): boolean;
var
  ex: DWord;
  y: Integer;
  Overlapped: TOverlapped;
begin
  FillChar(Overlapped, Sizeof(Overlapped), 0);
  Overlapped.hEvent := CreateEvent(nil, True, False, nil);
  try
    SetCommMask(FHandle, Event);
    SetSynaError(sOK);
    if (Event = EV_RXCHAR) and (Waitingdata > 0) then
      Result := True
    else
    begin
      y := 0;
      if not WaitCommEvent(FHandle, ex, @Overlapped) then
        y := GetLastError;
      if y = ERROR_IO_PENDING then
      begin
        //timedout
        WaitForSingleObject(Overlapped.hEvent, Timeout);
        SetCommMask(FHandle, 0);
        GetOverlappedResult(FHandle, Overlapped, DWord(y), True);
      end;
      Result := (ex and Event) = Event;
    end;
  finally
    SetCommMask(FHandle, 0);
    CloseHandle(Overlapped.hEvent);
  end;
end;
{$ENDIF}

{$IFNDEF MSWINDOWS}
function TBlockSerial.CanRead(Timeout: integer): boolean;
var
  FDSet: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  {$IFNDEF FPC}
  FD_ZERO(FDSet);
  FD_SET(FHandle, FDSet);
  x := Select(FHandle + 1, @FDSet, nil, nil, TimeVal);
  {$ELSE}
  fpFD_ZERO(FDSet);
  fpFD_SET(FHandle, FDSet);
  x := fpSelect(FHandle + 1, @FDSet, nil, nil, TimeVal);
  {$ENDIF}
  SerialCheck(x);
  if FLastError <> sOK then
    x := 0;
  Result := x > 0;
  ExceptCheck;
  if Result then
    DoStatus(HR_CanRead, '');
end;
{$ELSE}
function TBlockSerial.CanRead(Timeout: integer): boolean;
begin
  Result := WaitingData > 0;
  if not Result then
    Result := CanEvent(EV_RXCHAR, Timeout) or (WaitingData > 0);
    //check WaitingData again due some broken virtual ports
  if Result then
    DoStatus(HR_CanRead, '');
end;
{$ENDIF}

{$IFNDEF MSWINDOWS}
function TBlockSerial.CanWrite(Timeout: integer): boolean;
var
  FDSet: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  {$IFNDEF FPC}
  FD_ZERO(FDSet);
  FD_SET(FHandle, FDSet);
  x := Select(FHandle + 1, nil, @FDSet, nil, TimeVal);
  {$ELSE}
  fpFD_ZERO(FDSet);
  fpFD_SET(FHandle, FDSet);
  x := fpSelect(FHandle + 1, nil, @FDSet, nil, TimeVal);
  {$ENDIF}
  SerialCheck(x);
  if FLastError <> sOK then
    x := 0;
  Result := x > 0;
  ExceptCheck;
  if Result then
    DoStatus(HR_CanWrite, '');
end;
{$ELSE}
function TBlockSerial.CanWrite(Timeout: integer): boolean;
var
  t: LongWord;
begin
  Result := SendingData = 0;
  if not Result then
	  Result := CanEvent(EV_TXEMPTY, Timeout);
  {$IFDEF WIN32}
  if Result and (Win32Platform <> VER_PLATFORM_WIN32_NT) then
  begin
    t := GetTick;
    while not ReadTxEmpty(FPortAddr) do
    begin
      if TickDelta(t, GetTick) > 255 then
        Break;
      Sleep(0);
    end;
  end;
  {$ENDIF}
  if Result then
    DoStatus(HR_CanWrite, '');
end;
{$ENDIF}

function TBlockSerial.CanReadEx(Timeout: integer): boolean;
begin
	if Fbuffer <> '' then
  	Result := True
  else
  	Result := CanRead(Timeout);
end;

procedure TBlockSerial.EnableRTSToggle(Value: boolean);
begin
  SetSynaError(sOK);
{$IFNDEF MSWINDOWS}
  FRTSToggle := Value;
  if Value then
    RTS:=False;
{$ELSE}
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    GetCommState;
    if value then
      dcb.Flags := dcb.Flags or dcb_RtsControlToggle
    else
      dcb.flags := dcb.flags and (not dcb_RtsControlToggle);
    SetCommState;
  end
  else
  begin
    FRTSToggle := Value;
    if Value then
      RTS:=False;
  end;
{$ENDIF}
end;

procedure TBlockSerial.Flush;
begin
{$IFNDEF MSWINDOWS}
  SerialCheck(tcdrain(FHandle));
{$ELSE}
  SetSynaError(sOK);
  if not Flushfilebuffers(FHandle) then
    SerialCheck(sErr);
{$ENDIF}
  ExceptCheck;
end;

{$IFNDEF MSWINDOWS}
procedure TBlockSerial.Purge;
begin
  {$IFNDEF FPC}
  SerialCheck(ioctl(FHandle, TCFLSH, TCIOFLUSH));
  {$ELSE}
    {$IFDEF DARWIN}
    SerialCheck(fpioctl(FHandle, TCIOflush, TCIOFLUSH));
    {$ELSE}
    SerialCheck(fpioctl(FHandle, TCFLSH, Pointer(PtrInt(TCIOFLUSH))));
    {$ENDIF}
  {$ENDIF}
  FBuffer := '';
  ExceptCheck;
end;
{$ELSE}
procedure TBlockSerial.Purge;
var
  x: integer;
begin
  SetSynaError(sOK);
  x := PURGE_TXABORT or PURGE_TXCLEAR or PURGE_RXABORT or PURGE_RXCLEAR;
  if not PurgeComm(FHandle, x) then
    SerialCheck(sErr);
  FBuffer := '';
  ExceptCheck;
end;
{$ENDIF}

function TBlockSerial.ModemStatus: integer;
begin
  Result := 0;
{$IFNDEF MSWINDOWS}
  {$IFNDEF FPC}
  SerialCheck(ioctl(FHandle, TIOCMGET, @Result));
  {$ELSE}
  SerialCheck(fpioctl(FHandle, TIOCMGET, @Result));
  {$ENDIF}
{$ELSE}
  SetSynaError(sOK);
  if not GetCommModemStatus(FHandle, dword(Result)) then
    SerialCheck(sErr);
{$ENDIF}
  ExceptCheck;
  FModemWord := Result;
end;

procedure TBlockSerial.SetBreak(Duration: integer);
begin
{$IFNDEF MSWINDOWS}
  SerialCheck(tcsendbreak(FHandle, Duration));
{$ELSE}
  SetCommBreak(FHandle);
  Sleep(Duration);
  SetSynaError(sOK);
  if not ClearCommBreak(FHandle) then
    SerialCheck(sErr);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TBlockSerial.DecodeCommError(Error: DWord);
begin
  if (Error and DWord(CE_FRAME)) > 1 then
    FLastError := ErrFrame;
  if (Error and DWord(CE_OVERRUN)) > 1 then
    FLastError := ErrOverrun;
  if (Error and DWord(CE_RXOVER)) > 1 then
    FLastError := ErrRxOver;
  if (Error and DWord(CE_RXPARITY)) > 1 then
    FLastError := ErrRxParity;
  if (Error and DWord(CE_TXFULL)) > 1 then
    FLastError := ErrTxFull;
end;
{$ENDIF}

//HGJ
function TBlockSerial.PreTestFailing: Boolean;
begin
  if not FInstanceActive then
  begin
    RaiseSynaError(ErrPortNotOpen);
    result:= true;
    Exit;
  end;
  Result := not TestCtrlLine;
  if result then
    RaiseSynaError(ErrNoDeviceAnswer)
end;

function TBlockSerial.TestCtrlLine: Boolean;
begin
  result := ((not FTestDSR) or DSR) and ((not FTestCTS) or CTS);
end;

function TBlockSerial.ATCommand(value: AnsiString): AnsiString;
var
  s: AnsiString;
  ConvSave: Boolean;
begin
  result := '';
  FAtResult := False;
  ConvSave := FConvertLineEnd;
  try
    FConvertLineEnd := True;
    SendString(value + #$0D);
    repeat
      s := RecvString(FAtTimeout);
      if s <> Value then
        result := result + s + CRLF;
      if s = 'OK' then
      begin
        FAtResult := True;
        break;
      end;
      if s = 'ERROR' then
        break;
    until FLastError <> sOK;
  finally
    FConvertLineEnd := Convsave;
  end;
end;


function TBlockSerial.ATConnect(value: AnsiString): AnsiString;
var
  s: AnsiString;
  ConvSave: Boolean;
begin
  result := '';
  FAtResult := False;
  ConvSave := FConvertLineEnd;
  try
    FConvertLineEnd := True;
    SendString(value + #$0D);
    repeat
      s := RecvString(90 * FAtTimeout);
      if s <> Value then
        result := result + s + CRLF;
      if s = 'NO CARRIER' then
        break;
      if s = 'ERROR' then
        break;
      if s = 'BUSY' then
        break;
      if s = 'NO DIALTONE' then
        break;
      if Pos('CONNECT', s) = 1 then
      begin
        FAtResult := True;
        break;
      end;
    until FLastError <> sOK;
  finally
    FConvertLineEnd := Convsave;
  end;
end;

function TBlockSerial.SerialCheck(SerialResult: integer): integer;
begin
  if SerialResult = integer(INVALID_HANDLE_VALUE) then
{$IFDEF MSWINDOWS}
    result := GetLastError
{$ELSE}
  {$IFNDEF FPC}
    result := GetLastError
  {$ELSE}
    result := fpGetErrno
  {$ENDIF}
{$ENDIF}
  else
    result := sOK;
  FLastError := result;
  FLastErrorDesc := GetErrorDesc(FLastError);
end;

procedure TBlockSerial.ExceptCheck;
var
  e: ESynaSerError;
  s: string;
begin
  if FRaiseExcept and (FLastError <> sOK) then
  begin
    s := GetErrorDesc(FLastError);
    e := ESynaSerError.CreateFmt('Communication error %d: %s', [FLastError, s]);
    e.ErrorCode := FLastError;
    e.ErrorMessage := s;
    raise e;
  end;
end;

procedure TBlockSerial.SetSynaError(ErrNumber: integer);
begin
  FLastError := ErrNumber;
  FLastErrorDesc := GetErrorDesc(FLastError);
end;

procedure TBlockSerial.RaiseSynaError(ErrNumber: integer);
begin
  SetSynaError(ErrNumber);
  ExceptCheck;
end;

procedure TBlockSerial.DoStatus(Reason: THookSerialReason; const Value: string);
begin
  if assigned(OnStatus) then
    OnStatus(Self, Reason, Value);
end;

{======================================================================}

class function TBlockSerial.GetErrorDesc(ErrorCode: integer): string;
begin
  Result:= '';
  case ErrorCode of
    sOK:               Result := 'OK';
    ErrAlreadyOwned:   Result := 'Port owned by other process';{HGJ}
    ErrAlreadyInUse:   Result := 'Instance already in use';    {HGJ}
    ErrWrongParameter: Result := 'Wrong parameter at call';     {HGJ}
    ErrPortNotOpen:    Result := 'Instance not yet connected'; {HGJ}
    ErrNoDeviceAnswer: Result := 'No device answer detected';  {HGJ}
    ErrMaxBuffer:      Result := 'Maximal buffer length exceeded';
    ErrTimeout:        Result := 'Timeout during operation';
    ErrNotRead:        Result := 'Reading of data failed';
    ErrFrame:          Result := 'Receive framing error';
    ErrOverrun:        Result := 'Receive Overrun Error';
    ErrRxOver:         Result := 'Receive Queue overflow';
    ErrRxParity:       Result := 'Receive Parity Error';
    ErrTxFull:         Result := 'Tranceive Queue is full';
  end;
  if Result = '' then
  begin
    Result := SysErrorMessage(ErrorCode);
  end;
end;


{---------- cpom Comport Ownership Manager Routines -------------
 by Hans-Georg Joepgen of Stuttgart, Germany.
 Copyright (c) 2002, by Hans-Georg Joepgen

  Stefan Krauss of Stuttgart, Germany, contributed literature and Internet
  research results, invaluable advice and excellent answers to the Comport
  Ownership Manager.
}

{$IFDEF UNIX}

function TBlockSerial.LockfileName: String;
var
  s: string;
begin
  s := SeparateRight(FDevice, '/dev/');
  result := LockfileDirectory + '/LCK..' + s;
end;

procedure TBlockSerial.CreateLockfile(PidNr: integer);
var
  f: TextFile;
  s: string;
begin
  // Create content for file
  s := IntToStr(PidNr);
  while length(s) < 10 do
    s := ' ' + s;
  // Create file
  try
    AssignFile(f, LockfileName);
    try
      Rewrite(f);
      writeln(f, s);
    finally
      CloseFile(f);
    end;
    // Allow all users to enjoy the benefits of cpom
    s := 'chmod a+rw ' + LockfileName;
{$IFNDEF FPC}
    FileSetReadOnly( LockfileName, False ) ;
 // Libc.system(pchar(s));
{$ELSE}
    fpSystem(s);
{$ENDIF}
  except
    // not raise exception, if you not have write permission for lock.
    on Exception do
      ;
  end;
end;

function TBlockSerial.ReadLockfile: integer;
{Returns PID from Lockfile. Lockfile must exist.}
var
  f: TextFile;
  s: string;
begin
  AssignFile(f, LockfileName);
  Reset(f);
  try
    readln(f, s);
  finally
    CloseFile(f);
  end;
  Result := StrToIntDef(s, -1)
end;

function TBlockSerial.cpomComportAccessible: boolean;
var
  MyPid: integer;
  Filename: string;
begin
  Filename := LockfileName;
  {$IFNDEF FPC}
  MyPid := Libc.getpid;
  {$ELSE}
  MyPid := fpGetPid;
  {$ENDIF}
  // Make sure, the Lock Files Directory exists. We need it.
  if not DirectoryExists(LockfileDirectory) then
    CreateDir(LockfileDirectory);
  // Check the Lockfile
  if not FileExists (Filename) then
  begin // comport is not locked. Lock it for us.
    CreateLockfile(MyPid);
    result := true;
    exit;  // done.
  end;
  // Is port owned by orphan? Then it's time for error recovery.
  //FPC forgot to add getsid.. :-(
  {$IFNDEF FPC}
  if Libc.getsid(ReadLockfile) = -1 then
  begin //  Lockfile was left from former desaster
    DeleteFile(Filename); // error recovery
    CreateLockfile(MyPid);
    result := true;
    exit;
  end;
  {$ENDIF}
  result := false // Sorry, port is owned by living PID and locked
end;

procedure TBlockSerial.cpomReleaseComport;
begin
  DeleteFile(LockfileName);
end;

{$ENDIF}
{----------------------------------------------------------------}

{$IFDEF MSWINDOWS}
function GetSerialPortNames: string;
var
  reg: TRegistry;
  l, v: TStringList;
  n: integer;
begin
  l := TStringList.Create;
  v := TStringList.Create;
  reg := TRegistry.Create;
  try
{$IFNDEF VER100}
{$IFNDEF VER120}
    reg.Access := KEY_READ;
{$ENDIF}
{$ENDIF}
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('\HARDWARE\DEVICEMAP\SERIALCOMM', false);
    reg.GetValueNames(l);
    for n := 0 to l.Count - 1 do
      v.Add(reg.ReadString(l[n]));
    Result := v.CommaText;
  finally
    reg.Free;
    l.Free;
    v.Free;
  end;
end;
{$ENDIF}
{$IFNDEF MSWINDOWS}
function GetSerialPortNames: string;
var
  sr : TSearchRec;
begin
  Result := '';
  if FindFirst('/dev/ttyS*', $FFFFFFFF, sr) = 0 then
  begin
    repeat
      if (sr.Attr and $FFFFFFFF) = Sr.Attr then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + sr.Name;
      end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);
end;
{$ENDIF}

end.
