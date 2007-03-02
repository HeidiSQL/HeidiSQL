unit mysql;

// -------------------------------------
// MySQL Constants, Variables and Types
// -------------------------------------

interface

uses
  Classes;

type
  // MySQL Index structure
  TMysqlIndex = record
    Name:     String[64];
    Columns:  TStringList;
    Unique:   Boolean;
    Fulltext: Boolean;
    Modified: Boolean;
    Ready:    Boolean;
  end;

  // MySQL Data Type structure
  TMysqlDataTypeRecord = record
    Index:     Integer;
    Name:      String[15];
    Flags:     Integer; // bitset : (bit 0 = NeedsLength, bit 1 = ....
    DefLength: Integer; // Can we auto-determine this from server ??
  end;

  // MySQL Field structure
  TMysqlField = record
    Name:          String[64];
    Typ:           Byte;
    LengthSet:     String;
    Default:       String;
    Primary:       Boolean;
    Index:         Boolean;
    Unique:        Boolean;
    Binary:        Boolean;
    Unsigned:      Boolean;
    Zerofill:      Boolean;
    NotNull:       Boolean;
    AutoIncrement: Boolean;
  end;

const
  // field flags
  FF_HAS_LENGTH_PROP = 1;
  // FF_xxx = 2
  // FF_yyy = 4
  // FF_zzz = 8
  // ....

// MySQL Field Types Constants
const
  tpTINYINT    = 0;
  tpSMALLINT   = 1;
  tpMEDIUMINT  = 2;
  tpINT        = 3;
  tpBIGINT     = 4;
  tpFLOAT      = 5;
  tpDOUBLE     = 6;
  tpDECIMAL    = 7;
  tpDATE       = 8;
  tpDATETIME   = 9;
  tpTIMESTAMP  = 10;
  tpTIME       = 11;
  tpYEAR       = 12;
  tpCHAR       = 13;
  tpVARCHAR    = 14;
  tpTINYBLOB   = 15;
  tpTINYTEXT   = 16;
  tpTEXT       = 17;
  tpBLOB       = 18;
  tpMEDIUMBLOB = 19;
  tpMEDIUMTEXT = 20;
  tpLONGBLOB   = 21;
  tpLONGTEXT   = 22;
  tpENUM       = 23;
  tpSET        = 24;
  tpBIT        = 25;

var
  // MySQL Data Type List and Properties
  MySqlDataTypeArray: array [0..25] of TMysqlDataTypeRecord =
  (
    (
      Index: tpTINYINT;
      Name: 'TINYINT'
    ),
    (
      Index: tpSMALLINT;
      Name: 'SMALLINT'
    ),
    (
      Index: tpMEDIUMINT;
      Name: 'MEDIUMINT'
    ),
    (
      Index: tpINT;
      Name: 'INT'
    ),
    (
      Index: tpBIGINT;
      Name: 'BIGINT'
    ),
    (
      Index: tpFLOAT;
      Name: 'FLOAT'
    ),
    (
      Index: tpDOUBLE;
      Name: 'DOUBLE'
    ),
    (
      Index: tpDECIMAL;
      Name: 'DECIMAL'
    ),
    (
      Index: tpDATE;
      Name: 'DATE'
    ),
    (
      Index: tpDATETIME;
      Name: 'DATETIME'
    ),
    (
      Index: tpTIMESTAMP;
      Name: 'TIMESTAMP'
    ),
    (
      Index: tpTIME;
      Name: 'TIME'
    ),
    (
      Index: tpYEAR;
      Name: 'YEAR'
    ),
    (
      Index: tpCHAR;
      Name: 'CHAR'
    ),
    (
      Index: tpVARCHAR;
      Name: 'VARCHAR';
      Flags: FF_HAS_LENGTH_PROP;
      DefLength: 50
    ),
    (
      Index: tpTINYBLOB;
      Name: 'TINYBLOB'
    ),
    (
      Index: tpTINYTEXT;
      Name: 'TINYTEXT'
    ),
    (
      Index: tpTEXT;
      Name: 'TEXT'
    ),
    (
      Index: tpBLOB;
      Name: 'BLOB'
    ),
    (
      Index: tpMEDIUMBLOB;
      Name: 'MEDIUMBLOB'
    ),
    (
      Index: tpMEDIUMTEXT;
      Name: 'MEDIUMTEXT'
    ),
    (
      Index: tpLONGBLOB;
      Name: 'LONGBLOB'
    ),
    (
      Index: tpLONGTEXT;
      Name: 'LONGTEXT'
    ),
    (
      Index: tpENUM;
      Name: 'ENUM'
    ),
    (
      Index: tpSET;
      Name: 'SET'
    ),
    (
      Index: tpBIT;
      Name: 'BIT'
    )
  );

implementation
end.
