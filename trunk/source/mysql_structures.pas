unit mysql_structures;

// -------------------------------------
// MySQL Constants, Variables and Types
// -------------------------------------

interface

uses
  Classes;

{$I const.inc}

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
    Name:            String[18];
    HasLength:       Boolean; // Can have Length- or Set-attribute?
    RequiresLength:  Boolean; // Must have a Length- or Set-attribute?
    HasUnsigned:     Boolean; // Can be unsigned?
    HasZerofill:     Boolean; // Can be zerofilled?
    HasBinary:       Boolean; // Can be binary?
    HasDefault:      Boolean; // Can have a default value?
    DefLengthSet:    String;  // Should be set for types which require a length/set
    Category:        Integer;
  end;

  // MySQL Data Type category structure
  TMysqlDataTypeCategory = record
    Index:           Integer;
    Name:            String[32];
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

  // MySQL Functions structure
  TMySQLFunction = record
    Name:         String;
    Declaration:  String;
    Category:     String;
    Version:      Integer; // Minimum MySQL version where function is available
    Description:  String;
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
  tpTIME       = 9;
  tpYEAR       = 10;
  tpDATETIME   = 11;
  tpTIMESTAMP  = 12;
  tpCHAR       = 13;
  tpVARCHAR    = 14;
  tpTINYTEXT   = 15;
  tpMEDIUMTEXT = 16;
  tpTEXT       = 17;
  tpLONGTEXT   = 18;
  tpBINARY     = 19;
  tpVARBINARY  = 20;
  tpTINYBLOB   = 21;
  tpBLOB       = 22;
  tpMEDIUMBLOB = 23;
  tpLONGBLOB   = 24;
  tpENUM       = 25;
  tpSET        = 26;
  tpBIT        = 27;
  tpPOINT      = 28;
  tpLINESTRING = 29;
  tpPOLYGON    = 30;
  tpGEOMETRY   = 31;
  tpMULTIPOINT = 32;
  tpMULTILINESTRING    = 33;
  tpMULTIPOLYGON       = 34;
  tpGEOMETRYCOLLECTION = 35;

  // MySQL field type categorization
const
  catInteger       = 0;
  catReal          = 1;
  catTemporal      = 2;
  catText          = 3;
  catBinary        = 4;
  catIntegerNamed  = 5;
  catSet           = 6;
  catSetNamed      = 7;
  catSpatial       = 8;

var
  // MySQL data type categories
  MySqlDataTypeCategories: array[0..8] of TMysqlDataTypeCategory = (
    (
      Index:           catInteger;
      Name:            'Integer'
    ),
    (
      Index:           catIntegerNamed;
      Name:            'Integer, named (nonstandard)'
    ),
    (
      Index:           catReal;
      Name:            'Real'
    ),
    (
      Index:           catText;
      Name:            'Text'
    ),
    (
      Index:           catBinary;
      Name:            'Binary'
    ),
    (
      Index:           catTemporal;
      Name:            'Temporal (time)'
    ),
    (
      Index:           catSpatial;
      Name:            'Spatial (geometry)'
    ),
    (
      Index:           catSet;
      Name:            'Set of Bits (nonstandard)'
    ),
    (
      Index:           catSetNamed;
      Name:            'Set of Bits, named (nonstandard)'
    )
  );

  // MySQL Data Type List and Properties
  MySqlDataTypeArray: array [0..35] of TMysqlDataTypeRecord =
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
      Category:        catInteger;
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
      Category:        catInteger;
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
      Category:        catInteger;
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
      Category:        catInteger;
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
      Category:        catInteger;
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
      Category:        catReal;
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
      Category:        catReal;
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
      Category:        catReal;
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
      Category:        catTemporal;
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
      Category:        catTemporal;
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
      Category:        catTemporal;
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
      Category:        catTemporal;
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
      Category:        catTemporal;
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
      Category:        catText;
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
      Category:        catText;
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
      Category:        catText;
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
      Category:        catText;
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
      Category:        catText;
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
      Category:        catText;
    ),
    (
      Index:           tpBINARY;
      Name:            'BINARY';
      HasLength:       True;
      RequiresLength:  True;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
      DefLengthSet:    '50';
      Category:        catBinary;
    ),
    (
      Index:           tpVARBINARY;
      Name:            'VARBINARY';
      HasLength:       True;
      RequiresLength:  True;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
      DefLengthSet:    '50';
      Category:        catBinary;
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
      Category:        catBinary;
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
      Category:        catBinary;
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
      Category:        catBinary;
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
      Category:        catBinary;
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
      Category:        catIntegerNamed;
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
      Category:        catSetNamed;
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
      Category:        catSet;
    ),
    (
      Index:           tpPOINT;
      Name:            'POINT';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
      Category:        catSpatial;
    ),
    (
      Index:           tpLINESTRING;
      Name:            'LINESTRING';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
      Category:        catSpatial;
    ),
    (
      Index:           tpPOLYGON;
      Name:            'POLYGON';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
      Category:        catSpatial;
    ),
    (
      Index:           tpGEOMETRY;
      Name:            'GEOMETRY';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
      Category:        catSpatial;
    ),
    (
      Index:           tpMULTIPOINT;
      Name:            'MULTIPOINT';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
      Category:        catSpatial;
    ),
    (
      Index:           tpMULTILINESTRING;
      Name:            'MULTILINESTRING';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
      Category:        catSpatial;
    ),
    (
      Index:           tpMULTIPOLYGON;
      Name:            'MULTIPOLYGON';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
      Category:        catSpatial;
    ),
    (
      Index:           tpGEOMETRYCOLLECTION;
      Name:            'GEOMETRYCOLLECTION';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      False;
      Category:        catSpatial;
    )
  );


  MySqlFunctions: Array [0..291] of TMysqlFunction =
  (
    // Function nr. 1
    (
      Name:         '!';
      Declaration:  '';
      Category:     'Logical operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Logical NOT. Evaluates to 1 if the operand is 0, to 0 if the operand '
        +'is non-zero, and NOT NULL returns NULL.'
    ),

    // Function nr. 2
    (
      Name:         '!=';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Not equal:'
    ),

    // Function nr. 3
    (
      Name:         '&&';
      Declaration:  '';
      Category:     'Logical operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Logical AND. Evaluates to 1 if all operands are non-zero and not NULL, '
        +'to 0 if one or more operands are 0, otherwise NULL is returned.'
    ),

    // Function nr. 4
    (
      Name:         '&';
      Declaration:  '';
      Category:     'Bit Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Bitwise AND:'
    ),

    // Function nr. 5
    (
      Name:         '*';
      Declaration:  '';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Multiplication:'
    ),

    // Function nr. 6
    (
      Name:         '+';
      Declaration:  '';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Addition:'
    ),

    // Function nr. 7
    (
      Name:         '-';
      Declaration:  '';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Subtraction:'
    ),

    // Function nr. 8
    (
      Name:         '/';
      Declaration:  '';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Division:'
    ),

    // Function nr. 9
    (
      Name:         '<';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Less than:'
    ),

    // Function nr. 10
    (
      Name:         '<<';
      Declaration:  '';
      Category:     'Bit Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Shifts a longlong (BIGINT) number to the left.'
    ),

    // Function nr. 11
    (
      Name:         '<=';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Less than or equal:'
    ),

    // Function nr. 12
    (
      Name:         '<=>';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'NULL-safe equal. This operator performs an equality comparison like '
        +'the = operator, but returns 1 rather than NULL if both operands are '
        +'NULL, and 0 rather than NULL if one operand is NULL.'
    ),

    // Function nr. 13
    (
      Name:         '=';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Equal:'
    ),

    // Function nr. 14
    (
      Name:         '>';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Greater than:'
    ),

    // Function nr. 15
    (
      Name:         '>=';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Greater than or equal:'
    ),

    // Function nr. 16
    (
      Name:         '>>';
      Declaration:  '';
      Category:     'Bit Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Shifts a longlong (BIGINT) number to the right.'
    ),

    // Function nr. 17
    (
      Name:         'ABS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the absolute value of X.'
    ),

    // Function nr. 18
    (
      Name:         'ACOS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the arc cosine of X, that is, the value whose cosine is X. '
        +'Returns NULL if X is not in the range -1 to 1.'
    ),

    // Function nr. 19
    (
      Name:         'ADDDATE';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'When invoked with the INTERVAL form of the second argument, ADDDATE() '
        +'is a synonym for DATE_ADD(). The related function SUBDATE() is a '
        +'synonym for DATE_SUB(). For information on the INTERVAL unit argument, '
        +'see the discussion for DATE_ADD(). mysql> SELECT '
        +'DATE_ADD(''1998-01-02'', INTERVAL 31 DAY); -> ''1998-02-02'' mysql> '
        +'SELECT ADDDATE(''1998-01-02'', INTERVAL 31 DAY); -> ''1998-02-02'' '
        +'When invoked with the days form of the second argument, MySQL treats '
        +'it as an integer number of days to be added to expr.'
    ),

    // Function nr. 20
    (
      Name:         'ADDTIME';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'ADDTIME() adds expr2 to expr1 and returns the result. expr1 is a time '
        +'or datetime expression, and expr2 is a time expression.'
    ),

    // Function nr. 21
    (
      Name:         'AES_DECRYPT';
      Declaration:  '(str,key_str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'These functions allow encryption and decryption of data using the '
        +'official AES (Advanced Encryption Standard) algorithm, previously '
        +'known as "Rijndael." Encoding with a 128-bit key length is used, but '
        +'you can extend it up to 256 bits by modifying the source. We chose 128 '
        +'bits because it is much faster and it is secure enough for most '
        +'purposes. AES_ENCRYPT() encrypts a string and returns a binary string. '
        +'AES_DECRYPT() decrypts the encrypted string and returns the original '
        +'string. The input arguments may be any length. If either argument is '
        +'NULL, the result of this function is also NULL. Because AES is a '
        +'block-level algorithm, padding is used to encode uneven length strings '
        +'and so the result string length may be calculated using this formula: '
        +'16 x (trunc(string_length / 16) + 1) If AES_DECRYPT() detects invalid '
        +'data or incorrect padding, it returns NULL. However, it is possible '
        +'for AES_DECRYPT() to return a non-NULL value (possibly garbage) if the '
        +'input data or the key is invalid. You can use the AES functions to '
        +'store data in an encrypted form by modifying your queries:'
    ),

    // Function nr. 22
    (
      Name:         'AREA';
      Declaration:  '(poly)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns as a double-precision number the area of the Polygon value '
        +'poly, as measured in its spatial reference system.'
    ),

    // Function nr. 23
    (
      Name:         'ASBINARY';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Converts a value in internal geometry format to its WKB representation '
        +'and returns the binary result.'
    ),

    // Function nr. 24
    (
      Name:         'ASCII';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the numeric value of the leftmost character of the string str. '
        +'Returns 0 if str is the empty string. Returns NULL if str is NULL. '
        +'ASCII() works for characters with numeric values from 0 to 255.'
    ),

    // Function nr. 25
    (
      Name:         'ASIN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the arc sine of X, that is, the value whose sine is X. Returns '
        +'NULL if X is not in the range -1 to 1.'
    ),

    // Function nr. 26
    (
      Name:         'ASTEXT';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Converts a value in internal geometry format to its WKT representation '
        +'and returns the string result.'
    ),

    // Function nr. 27
    (
      Name:         'ATAN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the arc tangent of X, that is, the value whose tangent is X.'
    ),

    // Function nr. 28
    (
      Name:         'ATAN2';
      Declaration:  '(Y,X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the arc tangent of the two variables X and Y. It is similar to '
        +'calculating the arc tangent of Y / X, except that the signs of both '
        +'arguments are used to determine the quadrant of the result.'
    ),

    // Function nr. 29
    (
      Name:         'AVG';
      Declaration:  '([DISTINCT] expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the average value of expr. The DISTINCT option can be used to '
        +'return the average of the distinct values of expr. AVG() returns NULL '
        +'if there were no matching rows.'
    ),

    // Function nr. 30
    (
      Name:         'BENCHMARK';
      Declaration:  '(count,expr)';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The BENCHMARK() function executes the expression expr repeatedly count '
        +'times. It may be used to time how quickly MySQL processes the '
        +'expression. The result value is always 0. The intended use is from '
        +'within the mysql client, which reports query execution times:'
    ),

    // Function nr. 31
    (
      Name:         'BETWEEN';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'If expr is greater than or equal to min and expr is less than or equal '
        +'to max, BETWEEN returns 1, otherwise it returns 0. This is equivalent '
        +'to the expression (min <= expr AND expr <= max) if all the arguments '
        +'are of the same type. Otherwise type conversion takes place according '
        +'to the rules described in '
        +'http://dev.mysql.com/doc/refman/5.1/en/type-conversion.html, but '
        +'applied to all the three arguments.'
    ),

    // Function nr. 32
    (
      Name:         'BIN';
      Declaration:  '(N)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string representation of the binary value of N, where N is a '
        +'longlong (BIGINT) number. This is equivalent to CONV(N,10,2). Returns '
        +'NULL if N is NULL.'
    ),

    // Function nr. 33
    (
      Name:         'BINARY';
      Declaration:  '';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The BINARY operator casts the string following it to a binary string. '
        +'This is an easy way to force a column comparison to be done byte by '
        +'byte rather than character by character. This causes the comparison to '
        +'be case sensitive even if the column isn''t defined as BINARY or BLOB. '
        +'BINARY also causes trailing spaces to be significant.'
    ),

    // Function nr. 34
    (
      Name:         'BIT_AND';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the bitwise AND of all bits in expr. The calculation is '
        +'performed with 64-bit (BIGINT) precision.'
    ),

    // Function nr. 35
    (
      Name:         'BIT_COUNT';
      Declaration:  '(N)';
      Category:     'Bit Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of bits that are set in the argument N.'
    ),

    // Function nr. 36
    (
      Name:         'BIT_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the length of the string str in bits.'
    ),

    // Function nr. 37
    (
      Name:         'BIT_OR';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the bitwise OR of all bits in expr. The calculation is '
        +'performed with 64-bit (BIGINT) precision.'
    ),

    // Function nr. 38
    (
      Name:         'BIT_XOR';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      40101;
      Description:  'Returns the bitwise XOR of all bits in expr. The calculation is '
        +'performed with 64-bit (BIGINT) precision.'
    ),

    // Function nr. 39
    (
      Name:         'BOUNDARY';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a geometry that is the closure of the combinatorial boundary '
        +'of the geometry value g.'
    ),

    // Function nr. 40
    (
      Name:         'CASE';
      Declaration:  '';
      Category:     'Control flow functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CASE WHEN [condition] THEN result [WHEN [condition] THEN result ...] '
        +'[ELSE result] END The first version returns the result where '
        +'value=compare_value. The second version returns the result for the '
        +'first condition that is true. If there was no matching result value, '
        +'the result after ELSE is returned, or NULL if there is no ELSE part.'
    ),

    // Function nr. 41
    (
      Name:         'CAST';
      Declaration:  '(expr AS type)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The CAST() and CONVERT() functions take a value of one type and '
        +'produce a value of another type. The type can be one of the following '
        +'values: BINARY[(N)] CHAR[(N)] DATE DATETIME DECIMAL SIGNED [INTEGER] '
        +'TIME UNSIGNED [INTEGER] BINARY produces a string with the BINARY data '
        +'type. See http://dev.mysql.com/doc/refman/5.1/en/binary-varbinary.html '
        +'for a description of how this affects comparisons. If the optional '
        +'length N is given, BINARY(N) causes the cast to use no more than N '
        +'bytes of the argument. Values shorter than N bytes are padded with '
        +'0x00 bytes to a length of N. CHAR(N) causes the cast to use no more '
        +'than N characters of the argument. CAST() and CONVERT(... USING ...) '
        +'are standard SQL syntax. The non-USING form of CONVERT() is ODBC '
        +'syntax. CONVERT() with USING is used to convert data between different '
        +'character sets. In MySQL, transcoding names are the same as the '
        +'corresponding character set names. For example, this statement '
        +'converts the string ''abc'' in the default character set to the '
        +'corresponding string in the utf8 character set: SELECT CONVERT(''abc'' '
        +'USING utf8);'
    ),

    // Function nr. 42
    (
      Name:         'CEILING';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the smallest integer value not less than X.'
    ),

    // Function nr. 43
    (
      Name:         'CHAR';
      Declaration:  '(N,... [USING charset_name])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CHAR() interprets each argument N as an integer and returns a string '
        +'consisting of the characters given by the code values of those '
        +'integers. NULL values are skipped. By default, CHAR() returns a binary '
        +'string. To produce a string in a given character set, use the optional '
        +'USING clause: mysql> SELECT CHARSET(CHAR(0x65)), CHARSET(CHAR(0x65 '
        +'USING utf8)); +---------------------+--------------------------------+ '
        +'| CHARSET(CHAR(0x65)) | CHARSET(CHAR(0x65 USING utf8)) | '
        +'+---------------------+--------------------------------+ | binary | '
        +'utf8 | +---------------------+--------------------------------+ If '
        +'USING is given and the result string is illegal for the given '
        +'character set, a warning is issued. Also, if strict SQL mode is '
        +'enabled, the result from CHAR() becomes NULL.'
    ),

    // Function nr. 44
    (
      Name:         'CHARACTER_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CHARACTER_LENGTH() is a synonym for CHAR_LENGTH().'
    ),

    // Function nr. 45
    (
      Name:         'CHARSET';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Version:      40100;
      Description:  'Returns the character set of the string argument.'
    ),

    // Function nr. 46
    (
      Name:         'CHAR_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the length of the string str, measured in characters. A '
        +'multi-byte character counts as a single character. This means that for '
        +'a string containing five two-byte characters, LENGTH() returns 10, '
        +'whereas CHAR_LENGTH() returns 5.'
    ),

    // Function nr. 47
    (
      Name:         'COALESCE';
      Declaration:  '(value,...)';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the first non-NULL value in the list, or NULL if there are no '
        +'non-NULL values.'
    ),

    // Function nr. 48
    (
      Name:         'COERCIBILITY';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Version:      40101;
      Description:  'Returns the collation coercibility value of the string argument.'
    ),

    // Function nr. 49
    (
      Name:         'COLLATION';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Version:      40100;
      Description:  'Returns the collation of the string argument.'
    ),

    // Function nr. 50
    (
      Name:         'COMPRESS';
      Declaration:  '(string_to_compress)';
      Category:     'Encryption Functions';
      Version:      40101;
      Description:  'Compresses a string and returns the result as a binary string. This '
        +'function requires MySQL to have been compiled with a compression '
        +'library such as zlib. Otherwise, the return value is always NULL. The '
        +'compressed string can be uncompressed with UNCOMPRESS().'
    ),

    // Function nr. 51
    (
      Name:         'CONCAT';
      Declaration:  '(str1,str2,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string that results from concatenating the arguments. May '
        +'have one or more arguments. If all arguments are non-binary strings, '
        +'the result is a non-binary string. If the arguments include any binary '
        +'strings, the result is a binary string. A numeric argument is '
        +'converted to its equivalent binary string form; if you want to avoid '
        +'that, you can use an explicit type cast, as in this example: SELECT '
        +'CONCAT(CAST(int_col AS CHAR), char_col); CONCAT() returns NULL if any '
        +'argument is NULL.'
    ),

    // Function nr. 52
    (
      Name:         'CONCAT_WS';
      Declaration:  '(separator,str1,str2,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CONCAT_WS() stands for Concatenate With Separator and is a special '
        +'form of CONCAT(). The first argument is the separator for the rest of '
        +'the arguments. The separator is added between the strings to be '
        +'concatenated. The separator can be a string, as can the rest of the '
        +'arguments. If the separator is NULL, the result is NULL.'
    ),

    // Function nr. 53
    (
      Name:         'CONNECTION_ID';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the connection ID (thread ID) for the connection. Every '
        +'connection has an ID that is unique among the set of currently '
        +'connected clients.'
    ),

    // Function nr. 54
    (
      Name:         'CONTAINS';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 completely contains g2.'
    ),

    // Function nr. 55
    (
      Name:         'CONV';
      Declaration:  '(N,from_base,to_base)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Converts numbers between different number bases. Returns a string '
        +'representation of the number N, converted from base from_base to base '
        +'to_base. Returns NULL if any argument is NULL. The argument N is '
        +'interpreted as an integer, but may be specified as an integer or a '
        +'string. The minimum base is 2 and the maximum base is 36. If to_base '
        +'is a negative number, N is regarded as a signed number. Otherwise, N '
        +'is treated as unsigned. CONV() works with 64-bit precision.'
    ),

    // Function nr. 56
    (
      Name:         'CONVERT_TZ';
      Declaration:  '(dt,from_tz,to_tz)';
      Category:     'Date and Time Functions';
      Version:      40103;
      Description:  'CONVERT_TZ() converts a datetime value dt from the time zone given by '
        +'from_tz to the time zone given by to_tz and returns the resulting '
        +'value. Time zones are specified as described in '
        +'http://dev.mysql.com/doc/refman/5.1/en/time-zone-support.html. This '
        +'function returns NULL if the arguments are invalid.'
    ),

    // Function nr. 57
    (
      Name:         'COS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the cosine of X, where X is given in radians.'
    ),

    // Function nr. 58
    (
      Name:         'COT';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the cotangent of X.'
    ),

    // Function nr. 59
    (
      Name:         'COUNT';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a count of the number of non-NULL values in the rows retrieved '
        +'by a SELECT statement. The result is a BIGINT value. COUNT() returns 0 '
        +'if there were no matching rows.'
    ),

    // Function nr. 60
    (
      Name:         'CRC32';
      Declaration:  '(expr)';
      Category:     'Numeric Functions';
      Version:      40100;
      Description:  'Computes a cyclic redundancy check value and returns a 32-bit unsigned '
        +'value. The result is NULL if the argument is NULL. The argument is '
        +'expected to be a string and (if possible) is treated as one if it is '
        +'not.'
    ),

    // Function nr. 61
    (
      Name:         'CROSSES';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if g1 spatially crosses g2. Returns NULL if g1 is a Polygon '
        +'or a MultiPolygon, or if g2 is a Point or a MultiPoint. Otherwise, '
        +'returns 0. The term spatially crosses denotes a spatial relation '
        +'between two given geometries that has the following properties: The '
        +'two geometries intersect Their intersection results in a geometry that '
        +'has a dimension that is one less than the maximum dimension of the two '
        +'given geometries Their intersection is not equal to either of the two '
        +'given geometries'
    ),

    // Function nr. 62
    (
      Name:         'CURDATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current date as a value in ''YYYY-MM-DD'' or YYYYMMDD '
        +'format, depending on whether the function is used in a string or '
        +'numeric context.'
    ),

    // Function nr. 63
    (
      Name:         'CURRENT_DATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CURRENT_DATE and CURRENT_DATE() are synonyms for CURDATE().'
    ),

    // Function nr. 64
    (
      Name:         'CURRENT_TIME';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CURRENT_TIME and CURRENT_TIME() are synonyms for CURTIME().'
    ),

    // Function nr. 65
    (
      Name:         'CURRENT_TIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'CURRENT_TIMESTAMP and CURRENT_TIMESTAMP() are synonyms for NOW().'
    ),

    // Function nr. 66
    (
      Name:         'CURRENT_USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the username and hostname combination for the MySQL account '
        +'that the server used to authenticate the current client. This account '
        +'determines your access privileges. Within a stored routine that is '
        +'defined with the SQL SECURITY DEFINER characteristic, CURRENT_USER() '
        +'returns the creator of the routine. The return value is a string in '
        +'the utf8 character set. The value of CURRENT_USER() can differ from '
        +'the value of USER().'
    ),

    // Function nr. 67
    (
      Name:         'CURTIME';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current time as a value in ''HH:MM:SS'' or HHMMSS format, '
        +'depending on whether the function is used in a string or numeric '
        +'context.'
    ),

    // Function nr. 68
    (
      Name:         'DATABASE';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the default (current) database name as a string in the utf8 '
        +'character set. If there is no default database, DATABASE() returns '
        +'NULL. Within a stored routine, the default database is the database '
        +'that the routine is associated with, which is not necessarily the same '
        +'as the database that is the default in the calling context.'
    ),

    // Function nr. 69
    (
      Name:         'DATE';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Extracts the date part of the date or datetime expression expr.'
    ),

    // Function nr. 70
    (
      Name:         'DATEDIFF';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'DATEDIFF() returns expr1 - expr2 expressed as a value in days from one '
        +'date to the other. expr1 and expr2 are date or date-and-time '
        +'expressions. Only the date parts of the values are used in the '
        +'calculation.'
    ),

    // Function nr. 71
    (
      Name:         'DATE_ADD';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'These functions perform date arithmetic. date is a DATETIME or DATE '
        +'value specifying the starting date. expr is an expression specifying '
        +'the interval value to be added or subtracted from the starting date. '
        +'expr is a string; it may start with a `-'' for negative intervals. '
        +'unit is a keyword indicating the units in which the expression should '
        +'be interpreted.'
    ),

    // Function nr. 72
    (
      Name:         'DATE_FORMAT';
      Declaration:  '(date,format)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Formats the date value according to the format string.'
    ),

    // Function nr. 73
    (
      Name:         'DATE_SUB';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'See DATE_ADD().'
    ),

    // Function nr. 74
    (
      Name:         'DAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'DAY() is a synonym for DAYOFMONTH().'
    ),

    // Function nr. 75
    (
      Name:         'DAYNAME';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      40121;
      Description:  'Returns the name of the weekday for date.'
    ),

    // Function nr. 76
    (
      Name:         'DAYOFMONTH';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the day of the month for date, in the range 0 to 31.'
    ),

    // Function nr. 77
    (
      Name:         'DAYOFWEEK';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the weekday index for date (1 = Sunday, 2 = Monday, ..., 7 = '
        +'Saturday). These index values correspond to the ODBC standard.'
    ),

    // Function nr. 78
    (
      Name:         'DAYOFYEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the day of the year for date, in the range 1 to 366.'
    ),

    // Function nr. 79
    (
      Name:         'DECODE';
      Declaration:  '(crypt_str,pass_str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Decrypts the encrypted string crypt_str using pass_str as the '
        +'password. crypt_str should be a string returned from ENCODE().'
    ),

    // Function nr. 80
    (
      Name:         'DEFAULT';
      Declaration:  '(col_name)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the default value for a table column. An error results if the '
        +'column has no default value.'
    ),

    // Function nr. 81
    (
      Name:         'DEGREES';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the argument X, converted from radians to degrees.'
    ),

    // Function nr. 82
    (
      Name:         'DES_DECRYPT';
      Declaration:  '(crypt_str[,key_str])';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Decrypts a string encrypted with DES_ENCRYPT(). If an error occurs, '
        +'this function returns NULL. Note that this function works only if '
        +'MySQL has been configured with SSL support. See '
        +'http://dev.mysql.com/doc/refman/5.1/en/secure-connections.html. If no '
        +'key_str argument is given, DES_DECRYPT() examines the first byte of '
        +'the encrypted string to determine the DES key number that was used to '
        +'encrypt the original string, and then reads the key from the DES key '
        +'file to decrypt the message. For this to work, the user must have the '
        +'SUPER privilege. The key file can be specified with the --des-key-file '
        +'server option. If you pass this function a key_str argument, that '
        +'string is used as the key for decrypting the message. If the crypt_str '
        +'argument does not appear to be an encrypted string, MySQL returns the '
        +'given crypt_str.'
    ),

    // Function nr. 83
    (
      Name:         'DES_ENCRYPT';
      Declaration:  '(str[,{key_num|key_str}])';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Encrypts the string with the given key using the Triple-DES algorithm.'
    ),

    // Function nr. 84
    (
      Name:         'DIMENSION';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the inherent dimension of the geometry value g. The result can '
        +'be -1, 0, 1, or 2. The meaning of these values is given in [HELP MBR '
        +'definition].'
    ),

    // Function nr. 85
    (
      Name:         'DISJOINT';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 is spatially disjoint from (does '
        +'not intersect) g2.'
    ),

    // Function nr. 86
    (
      Name:         'DIV';
      Declaration:  '';
      Category:     'Numeric Functions';
      Version:      40100;
      Description:  'Integer division. Similar to FLOOR(), but is safe with BIGINT values.'
    ),

    // Function nr. 87
    (
      Name:         'ELT';
      Declaration:  '(N,str1,str2,str3,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns str1 if N = 1, str2 if N = 2, and so on. Returns NULL if N is '
        +'less than 1 or greater than the number of arguments. ELT() is the '
        +'complement of FIELD().'
    ),

    // Function nr. 88
    (
      Name:         'ENCODE';
      Declaration:  '(str,pass_str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Encrypt str using pass_str as the password. To decrypt the result, use '
        +'DECODE(). The result is a binary string of the same length as str. The '
        +'strength of the encryption is based on how good the random generator '
        +'is. It should suffice for short strings.'
    ),

    // Function nr. 89
    (
      Name:         'ENCRYPT';
      Declaration:  '(str[,salt])';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Encrypts str using the Unix crypt() system call and returns a binary '
        +'string. The salt argument should be a string with at least two '
        +'characters. If no salt argument is given, a random value is used.'
    ),

    // Function nr. 90
    (
      Name:         'ENDPOINT';
      Declaration:  '(ls)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the Point that is the endpoint of the LineString value ls.'
    ),

    // Function nr. 91
    (
      Name:         'ENVELOPE';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the Minimum Bounding Rectangle (MBR) for the geometry value g. '
        +'The result is returned as a Polygon value. The polygon is defined by '
        +'the corner points of the bounding box: POLYGON((MINX MINY, MAXX MINY, '
        +'MAXX MAXY, MINX MAXY, MINX MINY))'
    ),

    // Function nr. 92
    (
      Name:         'EQUALS';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 is spatially equal to g2.'
    ),

    // Function nr. 93
    (
      Name:         'EXP';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the value of e (the base of natural logarithms) raised to the '
        +'power of X.'
    ),

    // Function nr. 94
    (
      Name:         'EXPORT_SET';
      Declaration:  '(bits,on,off[,separator[,number_of_bits]])';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string such that for every bit set in the value bits, you '
        +'get an on string and for every reset bit, you get an off string. Bits '
        +'in bits are examined from right to left (from low-order to high-order '
        +'bits). Strings are added to the result from left to right, separated '
        +'by the separator string (the default being the comma character `,''). '
        +'The number of bits examined is given by number_of_bits (defaults to '
        +'64).'
    ),

    // Function nr. 95
    (
      Name:         'EXTERIORRING';
      Declaration:  '(poly)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the exterior ring of the Polygon value poly as a LineString.'
    ),

    // Function nr. 96
    (
      Name:         'EXTRACT';
      Declaration:  '(unit FROM date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The EXTRACT() function uses the same kinds of unit specifiers as '
        +'DATE_ADD() or DATE_SUB(), but extracts parts from the date rather than '
        +'performing date arithmetic.'
    ),

    // Function nr. 97
    (
      Name:         'EXTRACTVALUE';
      Declaration:  '(xml_frag, xpath_expr)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'ExtractValue() takes two string arguments, a fragment of XML markup '
        +'xml_frag and an XPath expression xpath_expr (also known as a locator); '
        +'it returns the text (CDATA) of the first text node which is a child of '
        +'the element(s) matched by the XPath expression. It is the equivalent '
        +'of performing a match using the xpath_expr after appending /text(). In '
        +'other words, ExtractValue(''<a><b>Sakila</b></a>'', ''/a/b'') and '
        +'ExtractValue(''<a><b>Sakila</b></a>'', ''/a/b/text()'') produce the '
        +'same result. If multiple matches are found, then the content of the '
        +'first child text node of each matching element is returned (in the '
        +'order matched) as a single, space-delimited string. If no matching '
        +'text node is found for the (augmented) expression --- for whatever '
        +'reason, as long as xpth_expr is valid, and xml_frag is well-formed --- '
        +'an empty string is returned. No distinction is made between a match on '
        +'an empty element and no match at all. This is by design. If you need '
        +'to determine whether no matching element was found in xml_frag or such '
        +'an element was found but contained no child text nodes, you should '
        +'test the result of an expression that uses the XPath count() function. '
        +'For example, both of these statements return an empty string, as shown '
        +'here: mysql> SELECT ExtractValue(''<a><b/></a>'', ''/a/b''); '
        +'+-------------------------------------+ | '
        +'ExtractValue(''>a<>b/<>/a<'', ''/a/b'') | '
        +'+-------------------------------------+ | | '
        +'+-------------------------------------+ 1 row in set (0.00 sec) mysql> '
        +'SELECT ExtractValue(''<a><c/></a>'', ''/a/b''); '
        +'+-------------------------------------+ | '
        +'ExtractValue(''<a><c/></a>'', ''/a/b'') | '
        +'+-------------------------------------+ | | '
        +'+-------------------------------------+ 1 row in set (0.00 sec) '
        +'However, you can determine whether there was actually a matching '
        +'element using the following: mysql> SELECT '
        +'ExtractValue(''<a><b/></a>'', ''count(/a/b)''); '
        +'+-------------------------------------+ | '
        +'ExtractValue(''<a><b/></a>'', ''count(/a/b)'') | '
        +'+-------------------------------------+ | 1 | '
        +'+-------------------------------------+ 1 row in set (0.00 sec) mysql> '
        +'SELECT ExtractValue(''<a><c/></a>'', ''count(/a/b)''); '
        +'+-------------------------------------+ | '
        +'ExtractValue(''<a><c/></a>'', ''count(/a/b)'') | '
        +'+-------------------------------------+ | 0 | '
        +'+-------------------------------------+ 1 row in set (0.01 sec) Note '
        +'that ExtractValue() returns only CDATA, and does not return any tags '
        +'that might be contained within a matching tag, nor any of their '
        +'content (see the result returned as val1 in the following example).'
    ),

    // Function nr. 98
    (
      Name:         'FIELD';
      Declaration:  '(str,str1,str2,str3,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the index (position) of str in the str1, str2, str3, ... list. '
        +'Returns 0 if str is not found. If all arguments to FIELD() are '
        +'strings, all arguments are compared as strings. If all arguments are '
        +'numbers, they are compared as numbers. Otherwise, the arguments are '
        +'compared as double. If str is NULL, the return value is 0 because NULL '
        +'fails equality comparison with any value. FIELD() is the complement of '
        +'ELT().'
    ),

    // Function nr. 99
    (
      Name:         'FIND_IN_SET';
      Declaration:  '(str,strlist)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a value in the range of 1 to N if the string str is in the '
        +'string list strlist consisting of N substrings. A string list is a '
        +'string composed of substrings separated by `,'' characters. If the '
        +'first argument is a constant string and the second is a column of type '
        +'SET, the FIND_IN_SET() function is optimized to use bit arithmetic. '
        +'Returns 0 if str is not in strlist or if strlist is the empty string. '
        +'Returns NULL if either argument is NULL. This function does not work '
        +'properly if the first argument contains a comma (`,'') character.'
    ),

    // Function nr. 100
    (
      Name:         'FLOOR';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the largest integer value not greater than X.'
    ),

    // Function nr. 101
    (
      Name:         'FORMAT';
      Declaration:  '(X,D)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Formats the number X to a format like ''#,###,###.##'', rounded to D '
        +'decimal places, and returns the result as a string. If D is 0, the '
        +'result has no decimal point or fractional part.'
    ),

    // Function nr. 102
    (
      Name:         'FOUND_ROWS';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'A SELECT statement may include a LIMIT clause to restrict the number '
        +'of rows the server returns to the client. In some cases, it is '
        +'desirable to know how many rows the statement would have returned '
        +'without the LIMIT, but without running the statement again. To obtain '
        +'this row count, include a SQL_CALC_FOUND_ROWS option in the SELECT '
        +'statement, and then invoke FOUND_ROWS() afterward:'
    ),

    // Function nr. 103
    (
      Name:         'FROM_DAYS';
      Declaration:  '(N)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a day number N, returns a DATE value.'
    ),

    // Function nr. 104
    (
      Name:         'FROM_UNIXTIME';
      Declaration:  '(unix_timestamp)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a representation of the unix_timestamp argument as a value in '
        +'''YYYY-MM-DD HH:MM:SS'' or YYYYMMDDHHMMSS format, depending on whether '
        +'the function is used in a string or numeric context. unix_timestamp is '
        +'an internal timestamp value such as is produced by the '
        +'UNIX_TIMESTAMP() function. If format is given, the result is formatted '
        +'according to the format string, which is used the same way as listed '
        +'in the entry for the DATE_FORMAT() function.'
    ),

    // Function nr. 105
    (
      Name:         'GEOMCOLLFROMTEXT';
      Declaration:  '(wkt[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a GEOMETRYCOLLECTION value using its WKT representation and '
        +'SRID.'
    ),

    // Function nr. 106
    (
      Name:         'GEOMCOLLFROMWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a GEOMETRYCOLLECTION value using its WKB representation and '
        +'SRID.'
    ),

    // Function nr. 107
    (
      Name:         'GEOMETRY';
      Declaration:  '';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    // Function nr. 108
    (
      Name:         'GEOMETRYCOLLECTION';
      Declaration:  '(g1,g2,...)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB GeometryCollection. If any argument is not a '
        +'well-formed WKB representation of a geometry, the return value is '
        +'NULL.'
    ),

    // Function nr. 109
    (
      Name:         'GEOMETRYTYPE';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns as a string the name of the geometry type of which the '
        +'geometry instance g is a member. The name corresponds to one of the '
        +'instantiable Geometry subclasses.'
    ),

    // Function nr. 110
    (
      Name:         'GEOMFROMTEXT';
      Declaration:  '(wkt[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a geometry value of any type using its WKT representation '
        +'and SRID.'
    ),

    // Function nr. 111
    (
      Name:         'GEOMFROMWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a geometry value of any type using its WKB representation '
        +'and SRID.'
    ),

    // Function nr. 112
    (
      Name:         'GET_FORMAT';
      Declaration:  '(DATE|TIME|DATETIME, ''EUR''|''USA''|''JIS''|''ISO''|''INTERNAL'')';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Returns a format string. This function is useful in combination with '
        +'the DATE_FORMAT() and the STR_TO_DATE() functions.'
    ),

    // Function nr. 113
    (
      Name:         'GET_LOCK';
      Declaration:  '(str,timeout)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Tries to obtain a lock with a name given by the string str, using a '
        +'timeout of timeout seconds. Returns 1 if the lock was obtained '
        +'successfully, 0 if the attempt timed out (for example, because another '
        +'client has previously locked the name), or NULL if an error occurred '
        +'(such as running out of memory or the thread was killed with '
        +'mysqladmin kill). If you have a lock obtained with GET_LOCK(), it is '
        +'released when you execute RELEASE_LOCK(), execute a new GET_LOCK(), or '
        +'your connection terminates (either normally or abnormally). Locks '
        +'obtained with GET_LOCK() do not interact with transactions. That is, '
        +'committing a transaction does not release any such locks obtained '
        +'during the transaction. This function can be used to implement '
        +'application locks or to simulate record locks. Names are locked on a '
        +'server-wide basis. If a name has been locked by one client, GET_LOCK() '
        +'blocks any request by another client for a lock with the same name. '
        +'This allows clients that agree on a given lock name to use the name to '
        +'perform cooperative advisory locking. But be aware that it also allows '
        +'a client that is not among the set of cooperating clients to lock a '
        +'name, either inadvertently or deliberately, and thus prevent any of '
        +'the cooperating clients from locking that name. One way to reduce the '
        +'likelihood of this is to use lock names that are database-specific or '
        +'application-specific. For example, use lock names of the form '
        +'db_name.str or app_name.str.'
    ),

    // Function nr. 114
    (
      Name:         'GLENGTH';
      Declaration:  '(ls)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns as a double-precision number the length of the LineString '
        +'value ls in its associated spatial reference.'
    ),

    // Function nr. 115
    (
      Name:         'GREATEST';
      Declaration:  '(value1,value2,...)';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'With two or more arguments, returns the largest (maximum-valued) '
        +'argument. The arguments are compared using the same rules as for '
        +'LEAST().'
    ),

    // Function nr. 116
    (
      Name:         'GROUP_CONCAT';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      40100;
      Description:  'This function returns a string result with the concatenated non-NULL '
        +'values from a group. It returns NULL if there are no non-NULL values. '
        +'The full syntax is as follows: GROUP_CONCAT([DISTINCT] expr [,expr '
        +'...] [ORDER BY {unsigned_integer | col_name | expr} [ASC | DESC] '
        +'[,col_name ...]] [SEPARATOR str_val])'
    ),

    // Function nr. 117
    (
      Name:         'HEX';
      Declaration:  '(N_or_S)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If N_or_S is a number, returns a string representation of the '
        +'hexadecimal value of N, where N is a longlong (BIGINT) number. This is '
        +'equivalent to CONV(N,10,16). If N_or_S is a string, returns a '
        +'hexadecimal string representation of N_or_S where each character in '
        +'N_or_S is converted to two hexadecimal digits.'
    ),

    // Function nr. 118
    (
      Name:         'HOUR';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the hour for time. The range of the return value is 0 to 23 '
        +'for time-of-day values. However, the range of TIME values actually is '
        +'much larger, so HOUR can return values greater than 23.'
    ),

    // Function nr. 119
    (
      Name:         'IF';
      Declaration:  '(expr1,expr2,expr3)';
      Category:     'Control flow functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If expr1 is TRUE (expr1 <> 0 and expr1 <> NULL) then IF() returns '
        +'expr2; otherwise it returns expr3. IF() returns a numeric or string '
        +'value, depending on the context in which it is used.'
    ),

    // Function nr. 120
    (
      Name:         'IFNULL';
      Declaration:  '(expr1,expr2)';
      Category:     'Control flow functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If expr1 is not NULL, IFNULL() returns expr1; otherwise it returns '
        +'expr2. IFNULL() returns a numeric or string value, depending on the '
        +'context in which it is used.'
    ),

    // Function nr. 121
    (
      Name:         'IN';
      Declaration:  '(value,...)';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if expr is equal to any of the values in the IN list, else '
        +'returns 0. If all values are constants, they are evaluated according '
        +'to the type of expr and sorted. The search for the item then is done '
        +'using a binary search. This means IN is very quick if the IN value '
        +'list consists entirely of constants. Otherwise, type conversion takes '
        +'place according to the rules described in '
        +'http://dev.mysql.com/doc/refman/5.1/en/type-conversion.html, but '
        +'applied to all the arguments.'
    ),

    // Function nr. 122
    (
      Name:         'INET_ATON';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given the dotted-quad representation of a network address as a string, '
        +'returns an integer that represents the numeric value of the address. '
        +'Addresses may be 4- or 8-byte addresses.'
    ),

    // Function nr. 123
    (
      Name:         'INET_NTOA';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a numeric network address (4 or 8 byte), returns the dotted-quad '
        +'representation of the address as a string.'
    ),

    // Function nr. 124
    (
      Name:         'INSERT';
      Declaration:  '(str,pos,len,newstr)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str, with the substring beginning at position pos '
        +'and len characters long replaced by the string newstr. Returns the '
        +'original string if pos is not within the length of the string. '
        +'Replaces the rest of the string from position pos is len is not within '
        +'the length of the rest of the string. Returns NULL if any argument is '
        +'NULL.'
    ),

    // Function nr. 125
    (
      Name:         'INSTR';
      Declaration:  '(str,substr)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the position of the first occurrence of substring substr in '
        +'string str. This is the same as the two-argument form of LOCATE(), '
        +'except that the order of the arguments is reversed.'
    ),

    // Function nr. 126
    (
      Name:         'INTERIORRINGN';
      Declaration:  '(poly,N)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the N-th interior ring for the Polygon value poly as a '
        +'LineString. Rings are numbered beginning with 1.'
    ),

    // Function nr. 127
    (
      Name:         'INTERSECTS';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 spatially intersects g2.'
    ),

    // Function nr. 128
    (
      Name:         'INTERVAL';
      Declaration:  '(N,N1,N2,N3,...)';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 0 if N < N1, 1 if N < N2 and so on or -1 if N is NULL. All '
        +'arguments are treated as integers. It is required that N1 < N2 < N3 < '
        +'... < Nn for this function to work correctly. This is because a binary '
        +'search is used (very fast).'
    ),

    // Function nr. 129
    (
      Name:         'IS';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Tests a value against a boolean value, where boolean_value can be '
        +'TRUE, FALSE, or UNKNOWN.'
    ),

    // Function nr. 130
    (
      Name:         'ISEMPTY';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 if the geometry value g is the empty geometry, 0 if it is '
        +'not empty, and -1 if the argument is NULL. If the geometry is empty, '
        +'it represents the empty point set.'
    ),

    // Function nr. 131
    (
      Name:         'ISNULL';
      Declaration:  '(expr)';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'If expr is NULL, ISNULL() returns 1, otherwise it returns 0.'
    ),

    // Function nr. 132
    (
      Name:         'ISSIMPLE';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Currently, this function is a placeholder and should not be used. If '
        +'implemented, its behavior will be as described in the next paragraph. '
        +'Returns 1 if the geometry value g has no anomalous geometric points, '
        +'such as self-intersection or self-tangency. IsSimple() returns 0 if '
        +'the argument is not simple, and -1 if it is NULL. The description of '
        +'each instantiable geometric class given earlier in the chapter '
        +'includes the specific conditions that cause an instance of that class '
        +'to be classified as not simple. (See [HELP Geometry hierarchy].)'
    ),

    // Function nr. 133
    (
      Name:         'IS_FREE_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Checks whether the lock named str is free to use (that is, not '
        +'locked). Returns 1 if the lock is free (no one is using the lock), 0 '
        +'if the lock is in use, and NULL if an error occurs (such as an '
        +'incorrect argument).'
    ),

    // Function nr. 134
    (
      Name:         'IS_USED_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Version:      40100;
      Description:  'Checks whether the lock named str is in use (that is, locked). If so, '
        +'it returns the connection identifier of the client that holds the '
        +'lock. Otherwise, it returns NULL.'
    ),

    // Function nr. 135
    (
      Name:         'LAST_DAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Takes a date or datetime value and returns the corresponding value for '
        +'the last day of the month. Returns NULL if the argument is invalid.'
    ),

    // Function nr. 136
    (
      Name:         'LAST_INSERT_ID';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LAST_INSERT_ID() (with no argument) returns the first automatically '
        +'generated value that was set for an AUTO_INCREMENT column by the most '
        +'recently executed INSERT or UPDATE statement to affect such a column. '
        +'For example, after inserting a row that generates an AUTO_INCREMENT '
        +'value, you can get the value like this: mysql> SELECT '
        +'LAST_INSERT_ID(); -> 195 The currently executing statement does not '
        +'affect the value of LAST_INSERT_ID(). Suppose that you generate an '
        +'AUTO_INCREMENT value with one statement, and then refer to '
        +'LAST_INSERT_ID() in a multiple-row INSERT statement that inserts rows '
        +'into a table with its own AUTO_INCREMENT column. The value of '
        +'LAST_INSERT_ID() will remain stable in the second statement; its value '
        +'for the second and later rows is not affected by the earlier row '
        +'insertions. (However, if you mix references to LAST_INSERT_ID() and '
        +'LAST_INSERT_ID(expr), the effect is undefined.) Within the body of a '
        +'stored routine (procedure or function) or a trigger, the value of '
        +'LAST_INSERT_ID() changes the same way as for statements executed '
        +'outside the body of these kinds of objects. The effect of a stored '
        +'routine or trigger upon the value of LAST_INSERT_ID() that is seen by '
        +'following statements depends on the kind of routine: If a stored '
        +'procedure executes statements that change the value of '
        +'LAST_INSERT_ID(), the changed value will be seen by statements that '
        +'follow the procedure call. For stored functions and triggers that '
        +'change the value, the value is restored when the function or trigger '
        +'ends, so following statements will not see a changed value.'
    ),

    // Function nr. 137
    (
      Name:         'LCASE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LCASE() is a synonym for LOWER().'
    ),

    // Function nr. 138
    (
      Name:         'LEAST';
      Declaration:  '(value1,value2,...)';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'With two or more arguments, returns the smallest (minimum-valued) '
        +'argument. The arguments are compared using the following rules: If the '
        +'return value is used in an INTEGER context or all arguments are '
        +'integer-valued, they are compared as integers. If the return value is '
        +'used in a REAL context or all arguments are real-valued, they are '
        +'compared as reals. If any argument is a case-sensitive string, the '
        +'arguments are compared as case-sensitive strings. In all other cases, '
        +'the arguments are compared as case-insensitive strings. LEAST() '
        +'returns NULL if any argument is NULL.'
    ),

    // Function nr. 139
    (
      Name:         'LEFT';
      Declaration:  '(str,len)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the leftmost len characters from the string str, or NULL if '
        +'any argument is NULL.'
    ),

    // Function nr. 140
    (
      Name:         'LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the length of the string str, measured in bytes. A multi-byte '
        +'character counts as multiple bytes. This means that for a string '
        +'containing five two-byte characters, LENGTH() returns 10, whereas '
        +'CHAR_LENGTH() returns 5.'
    ),

    // Function nr. 141
    (
      Name:         'LIKE';
      Declaration:  '';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Pattern matching using SQL simple regular expression comparison. '
        +'Returns 1 (TRUE) or 0 (FALSE). If either expr or pat is NULL, the '
        +'result is NULL. The pattern need not be a literal string. For example, '
        +'it can be specified as a string expression or table column.'
    ),

    // Function nr. 142
    (
      Name:         'LINEFROMTEXT';
      Declaration:  '(wkt[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a LINESTRING value using its WKT representation and SRID.'
    ),

    // Function nr. 143
    (
      Name:         'LINEFROMWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a LINESTRING value using its WKB representation and SRID.'
    ),

    // Function nr. 144
    (
      Name:         'LINESTRING';
      Declaration:  '(pt1,pt2,...)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB LineString value from a number of WKB Point '
        +'arguments. If any argument is not a WKB Point, the return value is '
        +'NULL. If the number of Point arguments is less than two, the return '
        +'value is NULL.'
    ),

    // Function nr. 145
    (
      Name:         'LN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the natural logarithm of X; that is, the base-e logarithm of '
        +'X.'
    ),

    // Function nr. 146
    (
      Name:         'LOAD_FILE';
      Declaration:  '(file_name)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Reads the file and returns the file contents as a string. To use this '
        +'function, the file must be located on the server host, you must '
        +'specify the full pathname to the file, and you must have the FILE '
        +'privilege. The file must be readable by all and its size less than '
        +'max_allowed_packet bytes. If the file does not exist or cannot be read '
        +'because one of the preceding conditions is not satisfied, the function '
        +'returns NULL. As of MySQL 5.1.6, the character_set_filesystem system '
        +'variable controls interpretation of filenames that are given as '
        +'literal strings.'
    ),

    // Function nr. 147
    (
      Name:         'LOCALTIME';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'LOCALTIME and LOCALTIME() are synonyms for NOW().'
    ),

    // Function nr. 148
    (
      Name:         'LOCALTIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      40006;
      Description:  'LOCALTIMESTAMP and LOCALTIMESTAMP() are synonyms for NOW().'
    ),

    // Function nr. 149
    (
      Name:         'LOCATE';
      Declaration:  '(substr,str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The first syntax returns the position of the first occurrence of '
        +'substring substr in string str. The second syntax returns the position '
        +'of the first occurrence of substring substr in string str, starting at '
        +'position pos. Returns 0 if substr is not in str.'
    ),

    // Function nr. 150
    (
      Name:         'LOG';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If called with one parameter, this function returns the natural '
        +'logarithm of X.'
    ),

    // Function nr. 151
    (
      Name:         'LOG10';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the base-10 logarithm of X.'
    ),

    // Function nr. 152
    (
      Name:         'LOG2';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the base-2 logarithm of X.'
    ),

    // Function nr. 153
    (
      Name:         'LOWER';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with all characters changed to lowercase '
        +'according to the current character set mapping. The default is latin1 '
        +'(cp1252 West European).'
    ),

    // Function nr. 154
    (
      Name:         'LPAD';
      Declaration:  '(str,len,padstr)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str, left-padded with the string padstr to a length '
        +'of len characters. If str is longer than len, the return value is '
        +'shortened to len characters.'
    ),

    // Function nr. 155
    (
      Name:         'LTRIM';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with leading space characters removed.'
    ),

    // Function nr. 156
    (
      Name:         'MAKEDATE';
      Declaration:  '(year,dayofyear)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Returns a date, given year and day-of-year values. dayofyear must be '
        +'greater than 0 or the result is NULL.'
    ),

    // Function nr. 157
    (
      Name:         'MAKETIME';
      Declaration:  '(hour,minute,second)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Returns a time value calculated from the hour, minute, and second '
        +'arguments.'
    ),

    // Function nr. 158
    (
      Name:         'MAKE_SET';
      Declaration:  '(bits,str1,str2,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a set value (a string containing substrings separated by `,'' '
        +'characters) consisting of the strings that have the corresponding bit '
        +'in bits set. str1 corresponds to bit 0, str2 to bit 1, and so on. NULL '
        +'values in str1, str2, ... are not appended to the result.'
    ),

    // Function nr. 159
    (
      Name:         'MASTER_POS_WAIT';
      Declaration:  '(log_name,log_pos[,timeout])';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function is useful for control of master/slave synchronization. '
        +'It blocks until the slave has read and applied all updates up to the '
        +'specified position in the master log. The return value is the number '
        +'of log events the slave had to wait for to advance to the specified '
        +'position. The function returns NULL if the slave SQL thread is not '
        +'started, the slave''s master information is not initialized, the '
        +'arguments are incorrect, or an error occurs. It returns -1 if the '
        +'timeout has been exceeded. If the slave SQL thread stops while '
        +'MASTER_POS_WAIT() is waiting, the function returns NULL. If the slave '
        +'is past the specified position, the function returns immediately.'
    ),

    // Function nr. 160
    (
      Name:         'MATCH';
      Declaration:  '(col1,col2,...)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'search_modifier: { IN BOOLEAN MODE | IN NATURAL LANGUAGE MODE | IN '
        +'NATURAL LANGUAGE MODE WITH QUERY EXPANSION | WITH QUERY EXPANSION } '
        +'MySQL has support for full-text indexing and searching: A full-text '
        +'index in MySQL is an index of type FULLTEXT. Full-text indexes can be '
        +'used only with MyISAM tables, and can be created only for CHAR, '
        +'VARCHAR, or TEXT columns. A FULLTEXT index definition can be given in '
        +'the CREATE TABLE statement when a table is created, or added later '
        +'using ALTER TABLE or CREATE INDEX. For large datasets, it is much '
        +'faster to load your data into a table that has no FULLTEXT index and '
        +'then create the index after that, than to load data into a table that '
        +'has an existing FULLTEXT index. Full-text searching is performed using '
        +'MATCH() ... AGAINST syntax. MATCH() takes a comma-separated list that '
        +'names the columns to be searched. AGAINST takes a string to search '
        +'for, and an optional modifier that indicates what type of search to '
        +'perform. The search string must be a literal string, not a variable or '
        +'a column name. There are three types of full-text searches: A boolean '
        +'search interprets the search string using the rules of a special query '
        +'language. The string contains the words to search for. It can also '
        +'contain operators that specify requirements such that a word must be '
        +'present or absent in matching rows, or that it should be weighted '
        +'higher or lower than usual. Common words such as "some" or "then" are '
        +'stopwords and do not match if present in the search string. The IN '
        +'BOOLEAN MODE modifier specifies a boolean search. For more '
        +'information, see '
        +'http://dev.mysql.com/doc/refman/5.1/en/fulltext-boolean.html. A '
        +'natural language search interprets the search string as a phrase in '
        +'natural human language (a phrase in free text). There are no special '
        +'operators. The stopword list applies. In addition, words that are '
        +'present in more than 50% of the rows are considered common and do not '
        +'match. Full-text searches are natural language searches if the IN '
        +'NATURAL LANGUAGE MODE modifier is given or if no modifier is given. A '
        +'query expansion search is a modification of a natural language search. '
        +'The search string is used to perform a natural language search. Then '
        +'words from the most relevant rows returned by the search are added to '
        +'the search string and the search is done again. The query returns the '
        +'rows from the second search. The IN NATURAL LANGUAGE MODE WITH QUERY '
        +'EXPANSION or WITH QUERY EXPANSION modifier specifies a query expansion '
        +'search. For more information, see '
        +'http://dev.mysql.com/doc/refman/5.1/en/fulltext-query-expansion.html. '
        +'The IN NATURAL LANGUAGE MODE and IN NATURAL LANGUAGE MODE WITH QUERY '
        +'EXPANSION modifiers were added in MySQL 5.1.7.'
    ),

    // Function nr. 161
    (
      Name:         'MBR';
      Declaration:  '';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    // Function nr. 162
    (
      Name:         'MBRCONTAINS';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding Rectangle of '
        +'g1 contains the Minimum Bounding Rectangle of g2.'
    ),

    // Function nr. 163
    (
      Name:         'MBRDISJOINT';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding Rectangles of '
        +'the two geometries g1 and g2 are disjoint (do not intersect).'
    ),

    // Function nr. 164
    (
      Name:         'MBREQUAL';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding Rectangles of '
        +'the two geometries g1 and g2 are the same.'
    ),

    // Function nr. 165
    (
      Name:         'MBRINTERSECTS';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding Rectangles of '
        +'the two geometries g1 and g2 intersect.'
    ),

    // Function nr. 166
    (
      Name:         'MBROVERLAPS';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding Rectangles of '
        +'the two geometries g1 and g2 overlap.'
    ),

    // Function nr. 167
    (
      Name:         'MBRTOUCHES';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding Rectangles of '
        +'the two geometries g1 and g2 touch.'
    ),

    // Function nr. 168
    (
      Name:         'MBRWITHIN';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether the Minimum Bounding Rectangle of '
        +'g1 is within the Minimum Bounding Rectangle of g2.'
    ),

    // Function nr. 169
    (
      Name:         'MD5';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Calculates an MD5 128-bit checksum for the string. The value is '
        +'returned as a binary string of 32 hex digits, or NULL if the argument '
        +'was NULL. The return value can, for example, be used as a hash key.'
    ),

    // Function nr. 170
    (
      Name:         'MICROSECOND';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Returns the microseconds from the time or datetime expression expr as '
        +'a number in the range from 0 to 999999.'
    ),

    // Function nr. 171
    (
      Name:         'MID';
      Declaration:  '(str,pos,len)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'MID(str,pos,len) is a synonym for SUBSTRING(str,pos,len).'
    ),

    // Function nr. 172
    (
      Name:         'MIN';
      Declaration:  '([DISTINCT] expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the minimum or maximum value of expr. MIN() and MAX() may take '
        +'a string argument; in such cases they return the minimum or maximum '
        +'string value. See '
        +'http://dev.mysql.com/doc/refman/5.1/en/mysql-indexes.html. The '
        +'DISTINCT keyword can be used to find the minimum or maximum of the '
        +'distinct values of expr, however, this produces the same result as '
        +'omitting DISTINCT. MIN() and MAX() return NULL if there were no '
        +'matching rows.'
    ),

    // Function nr. 173
    (
      Name:         'MINUTE';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the minute for time, in the range 0 to 59.'
    ),

    // Function nr. 174
    (
      Name:         'MLINEFROMTEXT';
      Declaration:  '(wkt[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTILINESTRING value using its WKT representation and '
        +'SRID.'
    ),

    // Function nr. 175
    (
      Name:         'MLINEFROMWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTILINESTRING value using its WKB representation and '
        +'SRID.'
    ),

    // Function nr. 176
    (
      Name:         'MONTH';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the month for date, in the range 0 to 12.'
    ),

    // Function nr. 177
    (
      Name:         'MONTHNAME';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      40121;
      Description:  'Returns the full name of the month for date.'
    ),

    // Function nr. 178
    (
      Name:         'MPOINTFROMTEXT';
      Declaration:  '(wkt[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTIPOINT value using its WKT representation and SRID.'
    ),

    // Function nr. 179
    (
      Name:         'MPOINTFROMWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTIPOINT value using its WKB representation and SRID.'
    ),

    // Function nr. 180
    (
      Name:         'MPOLYFROMTEXT';
      Declaration:  '(wkt[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTIPOLYGON value using its WKT representation and SRID.'
    ),

    // Function nr. 181
    (
      Name:         'MPOLYFROMWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a MULTIPOLYGON value using its WKB representation and SRID.'
    ),

    // Function nr. 182
    (
      Name:         'MULTILINESTRING';
      Declaration:  '(ls1,ls2,...)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB MultiLineString value using WKB LineString arguments. '
        +'If any argument is not a WKB LineString, the return value is NULL.'
    ),

    // Function nr. 183
    (
      Name:         'MULTIPOINT';
      Declaration:  '(pt1,pt2,...)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB MultiPoint value using WKB Point arguments. If any '
        +'argument is not a WKB Point, the return value is NULL.'
    ),

    // Function nr. 184
    (
      Name:         'MULTIPOLYGON';
      Declaration:  '(poly1,poly2,...)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB MultiPolygon value from a set of WKB Polygon '
        +'arguments. If any argument is not a WKB Polygon, the return value is '
        +'NULL.'
    ),

    // Function nr. 185
    (
      Name:         'NAME_CONST';
      Declaration:  '(name,value)';
      Category:     'Miscellaneous Functions';
      Version:      50012;
      Description:  'Returns the given value. When used to produce a result set column, '
        +'NAME_CONST() causes the column to have the given name. mysql> SELECT '
        +'NAME_CONST(''myname'', 14); +--------+ | myname | +--------+ | 14 | '
        +'+--------+'
    ),

    // Function nr. 186
    (
      Name:         'NOT';
      Declaration:  '';
      Category:     'Comparison operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'This is the same as NOT (expr BETWEEN min AND max).'
    ),

    // Function nr. 187
    (
      Name:         'NOW';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current date and time as a value in ''YYYY-MM-DD '
        +'HH:MM:SS'' or YYYYMMDDHHMMSS format, depending on whether the function '
        +'is used in a string or numeric context.'
    ),

    // Function nr. 188
    (
      Name:         'NULLIF';
      Declaration:  '(expr1,expr2)';
      Category:     'Control flow functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns NULL if expr1 = expr2 is true, otherwise returns expr1. This '
        +'is the same as CASE WHEN expr1 = expr2 THEN NULL ELSE expr1 END.'
    ),

    // Function nr. 189
    (
      Name:         'NUMINTERIORRINGS';
      Declaration:  '(poly)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of interior rings in the Polygon value poly.'
    ),

    // Function nr. 190
    (
      Name:         'NUMPOINTS';
      Declaration:  '(ls)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of Point objects in the LineString value ls.'
    ),

    // Function nr. 191
    (
      Name:         'OCT';
      Declaration:  '(N)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string representation of the octal value of N, where N is a '
        +'longlong (BIGINT) number. This is equivalent to CONV(N,10,8). Returns '
        +'NULL if N is NULL.'
    ),

    // Function nr. 192
    (
      Name:         'OCTETLENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'OCTET_LENGTH() is a synonym for LENGTH().'
    ),

    // Function nr. 193
    (
      Name:         'OLD_PASSWORD';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Version:      40100;
      Description:  'OLD_PASSWORD() was added to MySQL when the implementation of '
        +'PASSWORD() was changed to improve security. OLD_PASSWORD() returns the '
        +'value of the old (pre-4.1) implementation of PASSWORD() as a binary '
        +'string, and is intended to permit you to reset passwords for any '
        +'pre-4.1 clients that need to connect to your version 5.1 MySQL server '
        +'without locking them out. See '
        +'http://dev.mysql.com/doc/refman/5.1/en/password-hashing.html.'
    ),

    // Function nr. 194
    (
      Name:         'ORD';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If the leftmost character of the string str is a multi-byte character, '
        +'returns the code for that character, calculated from the numeric '
        +'values of its constituent bytes using this formula: (1st byte code) + '
        +'(2nd byte code x 256) + (3rd byte code x 2562) ... If the leftmost '
        +'character is not a multi-byte character, ORD() returns the same value '
        +'as the ASCII() function.'
    ),

    // Function nr. 195
    (
      Name:         'OVERLAPS';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 spatially overlaps g2. The term '
        +'spatially overlaps is used if two geometries intersect and their '
        +'intersection results in a geometry of the same dimension but not equal '
        +'to either of the given geometries.'
    ),

    // Function nr. 196
    (
      Name:         'PASSWORD';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Calculates and returns a password string from the plaintext password '
        +'str and returns a binary string, or NULL if the argument was NULL. '
        +'This is the function that is used for encrypting MySQL passwords for '
        +'storage in the Password column of the user grant table.'
    ),

    // Function nr. 197
    (
      Name:         'PERIOD_ADD';
      Declaration:  '(P,N)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Adds N months to period P (in the format YYMM or YYYYMM). Returns a '
        +'value in the format YYYYMM. Note that the period argument P is not a '
        +'date value.'
    ),

    // Function nr. 198
    (
      Name:         'PERIOD_DIFF';
      Declaration:  '(P1,P2)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number of months between periods P1 and P2. P1 and P2 '
        +'should be in the format YYMM or YYYYMM. Note that the period arguments '
        +'P1 and P2 are not date values.'
    ),

    // Function nr. 199
    (
      Name:         'PI';
      Declaration:  '()';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the value of ? (pi). The default number of decimal places '
        +'displayed is seven, but MySQL uses the full double-precision value '
        +'internally.'
    ),

    // Function nr. 200
    (
      Name:         'POINT';
      Declaration:  '(x,y)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB Point using its coordinates.'
    ),

    // Function nr. 201
    (
      Name:         'POINTFROMTEXT';
      Declaration:  '(wkt[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a POINT value using its WKT representation and SRID.'
    ),

    // Function nr. 202
    (
      Name:         'POINTFROMWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a POINT value using its WKB representation and SRID.'
    ),

    // Function nr. 203
    (
      Name:         'POINTN';
      Declaration:  '(ls,N)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the N-th Point in the Linestring value ls. Points are numbered '
        +'beginning with 1.'
    ),

    // Function nr. 204
    (
      Name:         'POLYFROMTEXT';
      Declaration:  '(wkt[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a POLYGON value using its WKT representation and SRID.'
    ),

    // Function nr. 205
    (
      Name:         'POLYFROMWKB';
      Declaration:  '(wkb[,srid])';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a POLYGON value using its WKB representation and SRID.'
    ),

    // Function nr. 206
    (
      Name:         'POLYGON';
      Declaration:  '(ls1,ls2,...)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Constructs a WKB Polygon value from a number of WKB LineString '
        +'arguments. If any argument does not represent the WKB of a LinearRing '
        +'(that is, not a closed and simple LineString) the return value is '
        +'NULL.'
    ),

    // Function nr. 207
    (
      Name:         'POSITION';
      Declaration:  '(substr IN str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'POSITION(substr IN str) is a synonym for LOCATE(substr,str).'
    ),

    // Function nr. 208
    (
      Name:         'POWER';
      Declaration:  '(X,Y)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the value of X raised to the power of Y.'
    ),

    // Function nr. 209
    (
      Name:         'QUARTER';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the quarter of the year for date, in the range 1 to 4.'
    ),

    // Function nr. 210
    (
      Name:         'QUOTE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Quotes a string to produce a result that can be used as a properly '
        +'escaped data value in an SQL statement. The string is returned '
        +'enclosed by single quotes and with each instance of single quote '
        +'(`''''), backslash (`\''), ASCII NUL, and Control-Z preceded by a '
        +'backslash. If the argument is NULL, the return value is the word '
        +'"NULL" without enclosing single quotes.'
    ),

    // Function nr. 211
    (
      Name:         'RADIANS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the argument X, converted from degrees to radians. (Note that '
        +'? radians equals 180 degrees.)'
    ),

    // Function nr. 212
    (
      Name:         'RAND';
      Declaration:  '()';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a random floating-point value v between 0 and 1 inclusive '
        +'(that is, in the range 0 <= v <= 1.0). If an integer argument N is '
        +'specified, it is used as the seed value, which produces a repeatable '
        +'sequence of column values.'
    ),

    // Function nr. 213
    (
      Name:         'REGEXP';
      Declaration:  '';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Performs a pattern match of a string expression expr against a pattern '
        +'pat. The pattern can be an extended regular expression. The syntax for '
        +'regular expressions is discussed in '
        +'http://dev.mysql.com/doc/refman/5.1/en/regexp.html. Returns 1 if expr '
        +'matches pat; otherwise it returns 0. If either expr or pat is NULL, '
        +'the result is NULL. RLIKE is a synonym for REGEXP, provided for mSQL '
        +'compatibility. The pattern need not be a literal string. For example, '
        +'it can be specified as a string expression or table column. Note: '
        +'Because MySQL uses the C escape syntax in strings (for example, `\n'' '
        +'to represent the newline character), you must double any `\'' that you '
        +'use in your REGEXP strings. REGEXP is not case sensitive, except when '
        +'used with binary strings.'
    ),

    // Function nr. 214
    (
      Name:         'RELEASE_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Releases the lock named by the string str that was obtained with '
        +'GET_LOCK(). Returns 1 if the lock was released, 0 if the lock was not '
        +'established by this thread (in which case the lock is not released), '
        +'and NULL if the named lock did not exist. The lock does not exist if '
        +'it was never obtained by a call to GET_LOCK() or if it has previously '
        +'been released. The DO statement is convenient to use with '
        +'RELEASE_LOCK(). See [HELP DO].'
    ),

    // Function nr. 215
    (
      Name:         'REPEAT';
      Declaration:  '(str,count)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string consisting of the string str repeated count times. If '
        +'count is less than 1, returns an empty string. Returns NULL if str or '
        +'count are NULL.'
    ),

    // Function nr. 216
    (
      Name:         'REPLACE';
      Declaration:  '(str,from_str,to_str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with all occurrences of the string from_str '
        +'replaced by the string to_str. REPLACE() performs a case-sensitive '
        +'match when searching for from_str.'
    ),

    // Function nr. 217
    (
      Name:         'REVERSE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with the order of the characters reversed.'
    ),

    // Function nr. 218
    (
      Name:         'RIGHT';
      Declaration:  '(str,len)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the rightmost len characters from the string str, or NULL if '
        +'any argument is NULL.'
    ),

    // Function nr. 219
    (
      Name:         'ROUND';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the argument X, rounded to the nearest integer. With two '
        +'arguments, returns X rounded to D decimal places. D can be negative to '
        +'cause D digits left of the decimal point of the value X to become '
        +'zero.'
    ),

    // Function nr. 220
    (
      Name:         'ROW_COUNT';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      50001;
      Description:  'ROW_COUNT() returns the number of rows updated, inserted, or deleted '
        +'by the preceding statement. This is the same as the row count that the '
        +'mysql client displays and the value from the mysql_affected_rows() C '
        +'API function.'
    ),

    // Function nr. 221
    (
      Name:         'RPAD';
      Declaration:  '(str,len,padstr)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str, right-padded with the string padstr to a '
        +'length of len characters. If str is longer than len, the return value '
        +'is shortened to len characters.'
    ),

    // Function nr. 222
    (
      Name:         'RTRIM';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with trailing space characters removed.'
    ),

    // Function nr. 223
    (
      Name:         'SCHEMA';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      50002;
      Description:  'This function is a synonym for DATABASE().'
    ),

    // Function nr. 224
    (
      Name:         'SECOND';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the second for time, in the range 0 to 59.'
    ),

    // Function nr. 225
    (
      Name:         'SEC_TO_TIME';
      Declaration:  '(seconds)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the seconds argument, converted to hours, minutes, and '
        +'seconds, as a value in ''HH:MM:SS'' or HHMMSS format, depending on '
        +'whether the function is used in a string or numeric context.'
    ),

    // Function nr. 226
    (
      Name:         'SESSION_USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'SESSION_USER() is a synonym for USER().'
    ),

    // Function nr. 227
    (
      Name:         'SHA';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Calculates an SHA-1 160-bit checksum for the string, as described in '
        +'RFC 3174 (Secure Hash Algorithm). The value is returned as a binary '
        +'string of 40 hex digits, or NULL if the argument was NULL. One of the '
        +'possible uses for this function is as a hash key. You can also use it '
        +'as a cryptographic function for storing passwords. SHA() is synonymous '
        +'with SHA1().'
    ),

    // Function nr. 228
    (
      Name:         'SIGN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the sign of the argument as -1, 0, or 1, depending on whether '
        +'X is negative, zero, or positive.'
    ),

    // Function nr. 229
    (
      Name:         'SIN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the sine of X, where X is given in radians.'
    ),

    // Function nr. 230
    (
      Name:         'SLEEP';
      Declaration:  '(duration)';
      Category:     'Miscellaneous Functions';
      Version:      50012;
      Description:  'Sleeps (pauses) for the number of seconds given by the duration '
        +'argument, then returns 0. If SLEEP() is interrupted, it returns 1. The '
        +'duration may have a fractional part given in microseconds.'
    ),

    // Function nr. 231
    (
      Name:         'SOUNDEX';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a soundex string from str. Two strings that sound almost the '
        +'same should have identical soundex strings. A standard soundex string '
        +'is four characters long, but the SOUNDEX() function returns an '
        +'arbitrarily long string. You can use SUBSTRING() on the result to get '
        +'a standard soundex string. All non-alphabetic characters in str are '
        +'ignored. All international alphabetic characters outside the A-Z range '
        +'are treated as vowels.'
    ),

    // Function nr. 232
    (
      Name:         'SOUNDS';
      Declaration:  '';
      Category:     'String Functions';
      Version:      40100;
      Description:  'This is the same as SOUNDEX(expr1) = SOUNDEX(expr2).'
    ),

    // Function nr. 233
    (
      Name:         'SPACE';
      Declaration:  '(N)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string consisting of N space characters.'
    ),

    // Function nr. 234
    (
      Name:         'SPATIAL';
      Declaration:  '';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'o With CREATE TABLE: CREATE TABLE geom (g GEOMETRY NOT NULL, SPATIAL '
        +'INDEX(g)); With ALTER TABLE: ALTER TABLE geom ADD SPATIAL INDEX(g); '
        +'With CREATE INDEX: CREATE SPATIAL INDEX sp_index ON geom (g); For '
        +'MyISAM tables, SPATIAL INDEX creates an R-tree index. For other '
        +'storage engines that support spatial indexing, SPATIAL INDEX creates a '
        +'B-tree index. A B-tree index on spatial values will be useful for '
        +'exact-value lookups, but not for range scans. To drop spatial indexes, '
        +'use ALTER TABLE or DROP INDEX: With ALTER TABLE: ALTER TABLE geom DROP '
        +'INDEX g; With DROP INDEX: DROP INDEX sp_index ON geom; Example: '
        +'Suppose that a table geom contains more than 32,000 geometries, which '
        +'are stored in the column g of type GEOMETRY. The table also has an '
        +'AUTO_INCREMENT column fid for storing object ID values.'
    ),

    // Function nr. 235
    (
      Name:         'SQRT';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the square root of a non-negative number X.'
    ),

    // Function nr. 236
    (
      Name:         'SRID';
      Declaration:  '(g)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns an integer indicating the Spatial Reference System ID for the '
        +'geometry value g. In MySQL, the SRID value is just an integer '
        +'associated with the geometry value. All calculations are done assuming '
        +'Euclidean (planar) geometry.'
    ),

    // Function nr. 237
    (
      Name:         'STARTPOINT';
      Declaration:  '(ls)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the Point that is the start point of the LineString value ls.'
    ),

    // Function nr. 238
    (
      Name:         'STDDEV';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the population standard deviation of expr. This is an '
        +'extension to standard SQL. The STDDEV() form of this function is '
        +'provided for compatibility with Oracle. The standard SQL function '
        +'STDDEV_POP() can be used instead. These functions return NULL if there '
        +'were no matching rows.'
    ),

    // Function nr. 239
    (
      Name:         'STDDEV_POP';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      50003;
      Description:  'Returns the population standard deviation of expr (the square root of '
        +'VAR_POP()). You can also use STD() or STDDEV(), which are equivalent '
        +'but not standard SQL. STDDEV_POP() returns NULL if there were no '
        +'matching rows.'
    ),

    // Function nr. 240
    (
      Name:         'STDDEV_SAMP';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      50003;
      Description:  'Returns the sample standard deviation of expr (the square root of '
        +'VAR_SAMP(). STDDEV_SAMP() returns NULL if there were no matching rows.'
    ),

    // Function nr. 241
    (
      Name:         'STRCMP';
      Declaration:  '(expr1,expr2)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'STRCMP() returns 0 if the strings are the same, -1 if the first '
        +'argument is smaller than the second according to the current sort '
        +'order, and 1 otherwise.'
    ),

    // Function nr. 242
    (
      Name:         'STR_TO_DATE';
      Declaration:  '(str,format)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'This is the inverse of the DATE_FORMAT() function. It takes a string '
        +'str and a format string format. STR_TO_DATE() returns a DATETIME value '
        +'if the format string contains both date and time parts, or a DATE or '
        +'TIME value if the string contains only date or time parts. The date, '
        +'time, or datetime values contained in str should be given in the '
        +'format indicated by format. For the specifiers that can be used in '
        +'format, see the DATE_FORMAT() function description. If str contains an '
        +'illegal date, time, or datetime value, STR_TO_DATE() returns NULL. An '
        +'illegal value also produces a warning.'
    ),

    // Function nr. 243
    (
      Name:         'SUBDATE';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'When invoked with the INTERVAL form of the second argument, SUBDATE() '
        +'is a synonym for DATE_SUB(). For information on the INTERVAL unit '
        +'argument, see the discussion for DATE_ADD(). mysql> SELECT '
        +'DATE_SUB(''1998-01-02'', INTERVAL 31 DAY); -> ''1997-12-02'' mysql> '
        +'SELECT SUBDATE(''1998-01-02'', INTERVAL 31 DAY); -> ''1997-12-02'' The '
        +'second form allows the use of an integer value for days. In such '
        +'cases, it is interpreted as the number of days to be subtracted from '
        +'the date or datetime expression expr. mysql> SELECT '
        +'SUBDATE(''1998-01-02 12:00:00'', 31); -> ''1997-12-02 12:00:00'''
    ),

    // Function nr. 244
    (
      Name:         'SUBSTRING';
      Declaration:  '(str,pos)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'The forms without a len argument return a substring from string str '
        +'starting at position pos. The forms with a len argument return a '
        +'substring len characters long from string str, starting at position '
        +'pos. The forms that use FROM are standard SQL syntax. It is also '
        +'possible to use a negative value for pos. In this case, the beginning '
        +'of the substring is pos characters from the end of the string, rather '
        +'than the beginning. A negative value may be used for pos in any of the '
        +'forms of this function.'
    ),

    // Function nr. 245
    (
      Name:         'SUBSTRING_INDEX';
      Declaration:  '(str,delim,count)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the substring from string str before count occurrences of the '
        +'delimiter delim. If count is positive, everything to the left of the '
        +'final delimiter (counting from the left) is returned. If count is '
        +'negative, everything to the right of the final delimiter (counting '
        +'from the right) is returned. SUBSTRING_INDEX() performs a '
        +'case-sensitive match when searching for delim.'
    ),

    // Function nr. 246
    (
      Name:         'SUBTIME';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'SUBTIME() returns expr1 - expr2 expressed as a value in the same '
        +'format as expr1. expr1 is a time or datetime expression, and expr2 is '
        +'a time expression.'
    ),

    // Function nr. 247
    (
      Name:         'SUM';
      Declaration:  '([DISTINCT] expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the sum of expr. If the return set has no rows, SUM() returns '
        +'NULL. The DISTINCT keyword can be used in MySQL 5.1 to sum only the '
        +'distinct values of expr. SUM() returns NULL if there were no matching '
        +'rows.'
    ),

    // Function nr. 248
    (
      Name:         'SYSDATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current date and time as a value in ''YYYY-MM-DD '
        +'HH:MM:SS'' or YYYYMMDDHHMMSS format, depending on whether the function '
        +'is used in a string or numeric context. SYSDATE() returns the time at '
        +'which it executes. This differs from the behavior for NOW(), which '
        +'returns a constant time that indicates the time at which the statement '
        +'began to execute. (Within a stored routine or trigger, NOW() returns '
        +'the time at which the routine or triggering statement began to '
        +'execute.) mysql> SELECT NOW(), SLEEP(2), NOW(); '
        +'+---------------------+----------+---------------------+ | NOW() | '
        +'SLEEP(2) | NOW() | '
        +'+---------------------+----------+---------------------+ | 2006-04-12 '
        +'13:47:36 | 0 | 2006-04-12 13:47:36 | '
        +'+---------------------+----------+---------------------+ mysql> SELECT '
        +'SYSDATE(), SLEEP(2), SYSDATE(); '
        +'+---------------------+----------+---------------------+ | SYSDATE() | '
        +'SLEEP(2) | SYSDATE() | '
        +'+---------------------+----------+---------------------+ | 2006-04-12 '
        +'13:47:44 | 0 | 2006-04-12 13:47:46 | '
        +'+---------------------+----------+---------------------+ In addition, '
        +'the SET TIMESTAMP statement affects the value returned by NOW() but '
        +'not by SYSDATE(). This means that timestamp settings in the binary log '
        +'have no effect on invocations of SYSDATE(). Because SYSDATE() can '
        +'return different values even within the same statement, and is not '
        +'affected by SET TIMESTAMP, it is non-deterministic and therefore '
        +'unsafe for replication if statement-based binary logging is used. If '
        +'that is a problem, you can use row-based logging, or start the server '
        +'with the --sysdate-is-now option to cause SYSDATE() to be an alias for '
        +'NOW().'
    ),

    // Function nr. 249
    (
      Name:         'SYSTEM_USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'SYSTEM_USER() is a synonym for USER().'
    ),

    // Function nr. 250
    (
      Name:         'TAN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the tangent of X, where X is given in radians.'
    ),

    // Function nr. 251
    (
      Name:         'TIME';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Extracts the time part of the time or datetime expression expr and '
        +'returns it as a string.'
    ),

    // Function nr. 252
    (
      Name:         'TIMEDIFF';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'TIMEDIFF() returns expr1 - expr2 expressed as a time value. expr1 and '
        +'expr2 are time or date-and-time expressions, but both must be of the '
        +'same type.'
    ),

    // Function nr. 253
    (
      Name:         'TIMESTAMP';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'With a single argument, this function returns the date or datetime '
        +'expression expr as a datetime value. With two arguments, it adds the '
        +'time expression expr2 to the date or datetime expression expr1 and '
        +'returns the result as a datetime value.'
    ),

    // Function nr. 254
    (
      Name:         'TIMESTAMPADD';
      Declaration:  '(unit,interval,datetime_expr)';
      Category:     'Date and Time Functions';
      Version:      50000;
      Description:  'Adds the integer expression interval to the date or datetime '
        +'expression datetime_expr. The unit for interval is given by the unit '
        +'argument, which should be one of the following values: FRAC_SECOND, '
        +'SECOND, MINUTE, HOUR, DAY, WEEK, MONTH, QUARTER, or YEAR. The unit '
        +'value may be specified using one of keywords as shown, or with a '
        +'prefix of SQL_TSI_. For example, DAY and SQL_TSI_DAY both are legal.'
    ),

    // Function nr. 255
    (
      Name:         'TIMESTAMPDIFF';
      Declaration:  '(unit,datetime_expr1,datetime_expr2)';
      Category:     'Date and Time Functions';
      Version:      50000;
      Description:  'Returns the integer difference between the date or datetime '
        +'expressions datetime_expr1 and datetime_expr2. The unit for the result '
        +'is given by the unit argument. The legal values for unit are the same '
        +'as those listed in the description of the TIMESTAMPADD() function.'
    ),

    // Function nr. 256
    (
      Name:         'TIME_FORMAT';
      Declaration:  '(time,format)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This is used like the DATE_FORMAT() function, but the format string '
        +'may contain format specifiers only for hours, minutes, and seconds. '
        +'Other specifiers produce a NULL value or 0.'
    ),

    // Function nr. 257
    (
      Name:         'TIME_TO_SEC';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the time argument, converted to seconds.'
    ),

    // Function nr. 258
    (
      Name:         'TOUCHES';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 spatially touches g2. Two '
        +'geometries spatially touch if the interiors of the geometries do not '
        +'intersect, but the boundary of one of the geometries intersects either '
        +'the boundary or the interior of the other.'
    ),

    // Function nr. 259
    (
      Name:         'TO_DAYS';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Given a date date, returns a day number (the number of days since year '
        +'0).'
    ),

    // Function nr. 260
    (
      Name:         'TRIM';
      Declaration:  '([{BOTH | LEADING | TRAILING} [remstr] FROM] str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with all remstr prefixes or suffixes removed. '
        +'If none of the specifiers BOTH, LEADING, or TRAILING is given, BOTH is '
        +'assumed. remstr is optional and, if not specified, spaces are removed.'
    ),

    // Function nr. 261
    (
      Name:         'TRUNCATE';
      Declaration:  '(X,D)';
      Category:     'Numeric Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the number X, truncated to D decimal places. If D is 0, the '
        +'result has no decimal point or fractional part. D can be negative to '
        +'cause D digits left of the decimal point of the value X to become '
        +'zero.'
    ),

    // Function nr. 262
    (
      Name:         'UCASE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'UCASE() is a synonym for UPPER().'
    ),

    // Function nr. 263
    (
      Name:         'UNCOMPRESS';
      Declaration:  '(string_to_uncompress)';
      Category:     'Encryption Functions';
      Version:      40101;
      Description:  'Uncompresses a string compressed by the COMPRESS() function. If the '
        +'argument is not a compressed value, the result is NULL. This function '
        +'requires MySQL to have been compiled with a compression library such '
        +'as zlib. Otherwise, the return value is always NULL.'
    ),

    // Function nr. 264
    (
      Name:         'UNCOMPRESSED_LENGTH';
      Declaration:  '(compressed_string)';
      Category:     'Encryption Functions';
      Version:      40101;
      Description:  'Returns the length that the compressed string had before being '
        +'compressed.'
    ),

    // Function nr. 265
    (
      Name:         'UNHEX';
      Declaration:  '';
      Category:     'String Functions';
      Version:      40102;
      Description:  'UNHEX(str) Performs the inverse operation of HEX(str). That is, it '
        +'interprets each pair of hexadecimal digits in the argument as a number '
        +'and converts it to the character represented by the number. The '
        +'resulting characters are returned as a binary string.'
    ),

    // Function nr. 266
    (
      Name:         'UNIX_TIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'If called with no argument, returns a Unix timestamp (seconds since '
        +'''1970-01-01 00:00:00'' UTC) as an unsigned integer. If '
        +'UNIX_TIMESTAMP() is called with a date argument, it returns the value '
        +'of the argument as seconds since ''1970-01-01 00:00:00'' UTC. date may '
        +'be a DATE string, a DATETIME string, a TIMESTAMP, or a number in the '
        +'format YYMMDD or YYYYMMDD. The server interprets date as a value in '
        +'the current time zone and converts it to an internal value in UTC. '
        +'Clients can set their time zone as described in '
        +'http://dev.mysql.com/doc/refman/5.1/en/time-zone-support.html.'
    ),

    // Function nr. 267
    (
      Name:         'UPDATEXML';
      Declaration:  '(xml_target, xpath_expr, new_xml)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function replaces a single portion of a given fragment of XML '
        +'markup xml_target with a new XML fragment new_xml, and then returns '
        +'the changed XML. The portion of xml_target that is replaced matches an '
        +'XPath expression xpath_expr supplied by the user. If no expression '
        +'matching xpath_expr is found, or if multiple matches are found, the '
        +'function returns the original xml_target XML fragment. All three '
        +'arguments must be strings.'
    ),

    // Function nr. 268
    (
      Name:         'UPPER';
      Declaration:  '(str)';
      Category:     'String Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the string str with all characters changed to uppercase '
        +'according to the current character set mapping. The default is latin1 '
        +'(cp1252 West European).'
    ),

    // Function nr. 269
    (
      Name:         'USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the current MySQL username and hostname as a string in the '
        +'utf8 character set.'
    ),

    // Function nr. 270
    (
      Name:         'UTC_DATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Returns the current UTC date as a value in ''YYYY-MM-DD'' or YYYYMMDD '
        +'format, depending on whether the function is used in a string or '
        +'numeric context.'
    ),

    // Function nr. 271
    (
      Name:         'UTC_TIME';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Returns the current UTC time as a value in ''HH:MM:SS'' or HHMMSS '
        +'format, depending on whether the function is used in a string or '
        +'numeric context.'
    ),

    // Function nr. 272
    (
      Name:         'UTC_TIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Returns the current UTC date and time as a value in ''YYYY-MM-DD '
        +'HH:MM:SS'' or YYYYMMDDHHMMSS format, depending on whether the function '
        +'is used in a string or numeric context.'
    ),

    // Function nr. 273
    (
      Name:         'UUID';
      Declaration:  '()';
      Category:     'Miscellaneous Functions';
      Version:      40102;
      Description:  'Returns a Universal Unique Identifier (UUID) generated according to '
        +'"DCE 1.1: Remote Procedure Call" (Appendix A) CAE (Common Applications '
        +'Environment) Specifications published by The Open Group in October '
        +'1997 (Document Number C706, '
        +'http://www.opengroup.org/public/pubs/catalog/c706.htm). A UUID is '
        +'designed as a number that is globally unique in space and time. Two '
        +'calls to UUID() are expected to generate two different values, even if '
        +'these calls are performed on two separate computers that are not '
        +'connected to each other. A UUID is a 128-bit number represented by a '
        +'string of five hexadecimal numbers in '
        +'aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee format: The first three numbers '
        +'are generated from a timestamp. The fourth number preserves temporal '
        +'uniqueness in case the timestamp value loses monotonicity (for '
        +'example, due to daylight saving time). The fifth number is an IEEE 802 '
        +'node number that provides spatial uniqueness. A random number is '
        +'substituted if the latter is not available (for example, because the '
        +'host computer has no Ethernet card, or we do not know how to find the '
        +'hardware address of an interface on your operating system). In this '
        +'case, spatial uniqueness cannot be guaranteed. Nevertheless, a '
        +'collision should have very low probability. Currently, the MAC address '
        +'of an interface is taken into account only on FreeBSD and Linux. On '
        +'other operating systems, MySQL uses a randomly generated 48-bit '
        +'number.'
    ),

    // Function nr. 274
    (
      Name:         'VALUES';
      Declaration:  '(col_name)';
      Category:     'Miscellaneous Functions';
      Version:      40101;
      Description:  'In an INSERT ... ON DUPLICATE KEY UPDATE statement, you can use the '
        +'VALUES(col_name) function in the UPDATE clause to refer to column '
        +'values from the INSERT portion of the statement. In other words, '
        +'VALUES(col_name) in the UPDATE clause refers to the value of col_name '
        +'that would be inserted, had no duplicate-key conflict occurred. This '
        +'function is especially useful in multiple-row inserts. The VALUES() '
        +'function is meaningful only in INSERT ... ON DUPLICATE KEY UPDATE '
        +'statements and returns NULL otherwise. '
        +'http://dev.mysql.com/doc/refman/5.1/en/insert-on-duplicate.html.'
    ),

    // Function nr. 275
    (
      Name:         'VARIANCE';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      40100;
      Description:  'Returns the population standard variance of expr. This is an extension '
        +'to standard SQL. The standard SQL function VAR_POP() can be used '
        +'instead. VARIANCE() returns NULL if there were no matching rows.'
    ),

    // Function nr. 276
    (
      Name:         'VAR_POP';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      50003;
      Description:  'Returns the population standard variance of expr. It considers rows as '
        +'the whole population, not as a sample, so it has the number of rows as '
        +'the denominator. You can also use VARIANCE(), which is equivalent but '
        +'is not standard SQL. VAR_POP() returns NULL if there were no matching '
        +'rows.'
    ),

    // Function nr. 277
    (
      Name:         'VAR_SAMP';
      Declaration:  '(expr)';
      Category:     'Functions and Modifiers for Use with GROUP BY';
      Version:      50003;
      Description:  'Returns the sample variance of expr. That is, the denominator is the '
        +'number of rows minus one. VAR_SAMP() returns NULL if there were no '
        +'matching rows.'
    ),

    // Function nr. 278
    (
      Name:         'VERSION';
      Declaration:  '()';
      Category:     'Information Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns a string that indicates the MySQL server version. The string '
        +'uses the utf8 character set.'
    ),

    // Function nr. 279
    (
      Name:         'WEEK';
      Declaration:  '(date[,mode])';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'This function returns the week number for date. The two-argument form '
        +'of WEEK() allows you to specify whether the week starts on Sunday or '
        +'Monday and whether the return value should be in the range from 0 to '
        +'53 or from 1 to 53. If the mode argument is omitted, the value of the '
        +'default_week_format system variable is used. See '
        +'http://dev.mysql.com/doc/refman/5.1/en/server-system-variables.html.'
    ),

    // Function nr. 280
    (
      Name:         'WEEKDAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the weekday index for date (0 = Monday, 1 = Tuesday, ... 6 = '
        +'Sunday).'
    ),

    // Function nr. 281
    (
      Name:         'WEEKOFYEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      40101;
      Description:  'Returns the calendar week of the date as a number in the range from 1 '
        +'to 53. WEEKOFYEAR() is a compatibility function that is equivalent to '
        +'WEEK(date,3).'
    ),

    // Function nr. 282
    (
      Name:         'WITHIN';
      Declaration:  '(g1,g2)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns 1 or 0 to indicate whether g1 is spatially within g2.'
    ),

    // Function nr. 283
    (
      Name:         'WKT';
      Declaration:  '';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  ''
    ),

    // Function nr. 284
    (
      Name:         'X';
      Declaration:  '(p)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the X-coordinate value for the point p as a double-precision '
        +'number.'
    ),

    // Function nr. 285
    (
      Name:         'XOR';
      Declaration:  '';
      Category:     'Logical operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Logical XOR. Returns NULL if either operand is NULL. For non-NULL '
        +'operands, evaluates to 1 if an odd number of operands is non-zero, '
        +'otherwise 0 is returned.'
    ),

    // Function nr. 286
    (
      Name:         'Y';
      Declaration:  '(p)';
      Category:     'Geographic Features';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the Y-coordinate value for the point p as a double-precision '
        +'number.'
    ),

    // Function nr. 287
    (
      Name:         'YEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns the year for date, in the range 1000 to 9999, or 0 for the '
        +'"zero" date.'
    ),

    // Function nr. 288
    (
      Name:         'YEARWEEK';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Returns year and week for a date. The start argument works exactly '
        +'like the start argument to WEEK(). The year in the result may be '
        +'different from the year in the date argument for the first and the '
        +'last week of the year.'
    ),

    // Function nr. 289
    (
      Name:         '^';
      Declaration:  '';
      Category:     'Bit Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Bitwise XOR:'
    ),

    // Function nr. 290
    (
      Name:         '|';
      Declaration:  '';
      Category:     'Bit Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Bitwise OR:'
    ),

    // Function nr. 291
    (
      Name:         '||';
      Declaration:  '';
      Category:     'Logical operators';
      Version:      SQL_VERSION_ANSI;
      Description:  'Logical OR. When both operands are non-NULL, the result is 1 if any '
        +'operand is non-zero, and 0 otherwise. With a NULL operand, the result '
        +'is 1 if the other operand is non-zero, and NULL otherwise. If both '
        +'operands are NULL, the result is NULL.'
    ),

    // Function nr. 292
    (
      Name:         '~';
      Declaration:  '';
      Category:     'Bit Functions';
      Version:      SQL_VERSION_ANSI;
      Description:  'Invert all bits.'
    )

  );

  function GetFunctionCategories: TStringList;

implementation

function GetFunctionCategories: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i:=0 to Length(MySqlFunctions)-1 do
  begin
    if Result.IndexOf( MySqlFunctions[i].Category ) = -1 then
    begin
      Result.Add( MySqlFunctions[i].Category );
    end;
  end;
  Result.Sort;
end;

end.
