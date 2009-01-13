{Portable Network Graphics Delphi ZLIB linking  (16 May 2002) }

{This unit links ZLIB to pngimage unit in order to implement  }
{the library. It's now using the new ZLIB version, 1.1.4      }
{Note: The .obj files must be located in the subdirectory \obj}

{  Changed 2004.11.25 by Vladimir Bochkarev:                  }
{  Upgrade to the new ZLIB version, 1.2.1                     }

{  Changed 2004.12.03 by Vladimir Bochkarev:                  }
{  Upgrade to the new ZLIB version, 1.2.2                     }

unit pngzlib;

interface

type

  TAlloc = function (AppData: Pointer; Items, Size: Integer): Pointer;
  TFree = procedure (AppData, Block: Pointer);

  // Internal structure.  Ignore.
  TZStreamRec = packed record
    next_in: PChar;       // next input byte
    avail_in: Integer;    // number of bytes available at next_in
    total_in: Integer;    // total nb of input bytes read so far

    next_out: PChar;      // next output byte should be put here
    avail_out: Integer;   // remaining free space at next_out
    total_out: Integer;   // total nb of bytes output so far

    msg: PChar;           // last error message, NULL if no error
    internal: Pointer;    // not visible by applications

    zalloc: TAlloc;       // used to allocate the internal state
    zfree: TFree;         // used to free the internal state
    AppData: Pointer;     // private data object passed to zalloc and zfree

    data_type: Integer;   //  best guess about the data type: ascii or binary
    adler: Integer;       // adler32 value of the uncompressed data
    reserved: Integer;    // reserved for future use
  end;

function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer; forward;
function inflate(var strm: TZStreamRec; flush: Integer): Integer; forward;
function inflateEnd(var strm: TZStreamRec): Integer; forward;
function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; forward;
function deflate(var strm: TZStreamRec; flush: Integer): Integer; forward;
function deflateEnd(var strm: TZStreamRec): Integer; forward;

const
  zlib_version = '1.2.2';

function adler32(adler: Integer; buf: PChar; len: Integer): Integer;

const
  Z_NO_FLUSH      = 0;
  Z_PARTIAL_FLUSH = 1;
  Z_SYNC_FLUSH    = 2;
  Z_FULL_FLUSH    = 3;
  Z_FINISH        = 4;

  Z_OK            = 0;
  Z_STREAM_END    = 1;
  Z_NEED_DICT     = 2;
  Z_ERRNO         = (-1);
  Z_STREAM_ERROR  = (-2);
  Z_DATA_ERROR    = (-3);
  Z_MEM_ERROR     = (-4);
  Z_BUF_ERROR     = (-5);
  Z_VERSION_ERROR = (-6);

  Z_NO_COMPRESSION       =   0;
  Z_BEST_SPEED           =   1;
  Z_BEST_COMPRESSION     =   9;
  Z_DEFAULT_COMPRESSION  = (-1);

  Z_FILTERED            = 1;
  Z_HUFFMAN_ONLY        = 2;
  Z_DEFAULT_STRATEGY    = 0;

  Z_BINARY   = 0;
  Z_ASCII    = 1;
  Z_UNKNOWN  = 2;

  Z_DEFLATED = 8;

  _z_errmsg: array[0..9] of PChar = (
    'need dictionary',      // Z_NEED_DICT      (2)
    'stream end',           // Z_STREAM_END     (1)
    '',                     // Z_OK             (0)
    'file error',           // Z_ERRNO          (-1)
    'stream error',         // Z_STREAM_ERROR   (-2)
    'data error',           // Z_DATA_ERROR     (-3)
    'insufficient memory',  // Z_MEM_ERROR      (-4)
    'buffer error',         // Z_BUF_ERROR      (-5)
    'incompatible version', // Z_VERSION_ERROR  (-6)
    ''
  );

implementation

{$L obj\adler32.obj}
{$L obj\compress.obj}
{$L obj\deflate.obj}
{$L obj\infback.obj}
{$L obj\inffast.obj}
{$L obj\inflate.obj}
{$L obj\inftrees.obj}
{$L obj\trees.obj}

function adler32; external;
procedure compressBound; external;

function zcalloc(AppData: Pointer; Items, Size: Integer): Pointer;
begin
  GetMem(Result, Items*Size);
end;

procedure zcfree(AppData, Block: Pointer);
begin
  FreeMem(Block);
end;

procedure _memset(P: Pointer; B: Byte; count: Integer); cdecl;
begin
  FillChar(P^, count, B);
end;

procedure _memcpy(dest, source: Pointer; count: Integer); cdecl;
begin
  Move(source^, dest^, count);
end;

// inflate decompresses data
function inflateInit_(var strm: TZStreamRec; version: PChar;
  recsize: Integer): Integer; external;
function inflate(var strm: TZStreamRec; flush: Integer): Integer; external;
function inflateEnd(var strm: TZStreamRec): Integer; external;
function inflateReset(var strm: TZStreamRec): Integer; external;
// deflate compresses data
function deflateInit_(var strm: TZStreamRec; level: Integer; version: PChar;
  recsize: Integer): Integer; external;
function deflate(var strm: TZStreamRec; flush: Integer): Integer; external;
function deflateEnd(var strm: TZStreamRec): Integer; external;

end.
