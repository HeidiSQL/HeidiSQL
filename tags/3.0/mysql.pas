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
    Index:           Integer;
    Name:            String[15];
    HasLength:       Boolean; // Can have Length- or Set-attribute?
    RequiresLength:  Boolean; // Must have a Length- or Set-attribute?
    HasUnsigned:     Boolean; // Can be unsigned?
    HasZerofill:     Boolean; // Can be zerofilled?
    HasBinary:       Boolean; // Can be binary?
    HasDefault:      Boolean; // Can have a default value?
    DefLengthSet:    String;  // Should be set for types which require a length/set
  end;

  // MySQL Field structure
  TMysqlField = record
    Name:          String[64];
    FieldType:     Byte;
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
      Index:           tpTINYINT;
      Name:            'TINYINT';
      HasLength:       True;
      RequiresLength:  False;
      HasUnsigned:     True;
      HasZerofill:     True;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpSMALLINT;
      Name:            'SMALLINT';
      HasLength:       True;
      RequiresLength:  False;
      HasUnsigned:     True;
      HasZerofill:     True;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpMEDIUMINT;
      Name:            'MEDIUMINT';
      HasLength:       True;
      RequiresLength:  False;
      HasUnsigned:     True;
      HasZerofill:     True;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpINT;
      Name:            'INT';
      HasLength:       True;
      RequiresLength:  False;
      HasUnsigned:     True;
      HasZerofill:     True;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpBIGINT;
      Name:            'BIGINT';
      HasLength:       True;
      RequiresLength:  False;
      HasUnsigned:     True;
      HasZerofill:     True;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpFLOAT;
      Name:            'FLOAT';
      HasLength:       True;
      RequiresLength:  False;
      HasUnsigned:     True;
      HasZerofill:     True;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpDOUBLE;
      Name:            'DOUBLE';
      HasLength:       True;
      RequiresLength:  False;
      HasUnsigned:     True;
      HasZerofill:     True;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpDECIMAL;
      Name:            'DECIMAL';
      HasLength:       True;
      RequiresLength:  True;
      HasUnsigned:     True;
      HasZerofill:     True;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpDATE;
      Name:            'DATE';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpDATETIME;
      Name:            'DATETIME';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpTIMESTAMP;
      Name:            'TIMESTAMP';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpTIME;
      Name:            'TIME';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpYEAR;
      Name:            'YEAR';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpCHAR;
      Name:            'CHAR';
      HasLength:       True;
      RequiresLength:  True;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       True;
      HasDefault:      True;
      DefLengthSet:    '50';
    ),
    (
      Index:           tpVARCHAR;
      Name:            'VARCHAR';
      HasLength:       True;
      RequiresLength:  True;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       True; // MySQL-Help says the opposite but it's valid for older versions at least.
      HasDefault:      True;
      DefLengthSet:    '50';
    ),
    (
      Index:           tpTINYBLOB;
      Name:           'TINYBLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
    ),
    (
      Index:           tpTINYTEXT;
      Name:            'TINYTEXT';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       True;
      HasDefault:      False;
    ),
    (
      Index:           tpTEXT;
      Name:            'TEXT';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       True;
      HasDefault:      False;
    ),
    (
      Index:           tpBLOB;
      Name:            'BLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
    ),
    (
      Index:           tpMEDIUMBLOB;
      Name:            'MEDIUMBLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
    ),
    (
      Index:           tpMEDIUMTEXT;
      Name:            'MEDIUMTEXT';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       True;
      HasDefault:      False;
    ),
    (
      Index:           tpLONGBLOB;
      Name:            'LONGBLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
    ),
    (
      Index:           tpLONGTEXT;
      Name:            'LONGTEXT';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       True;
      HasDefault:      False;
    ),
    (
      Index:           tpENUM;
      Name:            'ENUM';
      HasLength:       True; // Obviously this is not meant as "length", but as "set of values"
      RequiresLength:  True;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
      DefLengthSet:    '''Y'',''N''';
    ),
    (
      Index:           tpSET;
      Name:            'SET';
      HasLength:       True; // Same as for ENUM
      RequiresLength:  True;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
      DefLengthSet:    '''Value A'',''Value B''';
    ),
    (
      Index:           tpBIT;
      Name:            'BIT';
      HasLength:       True;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    )
  );

implementation
end.
