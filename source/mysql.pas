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
    Name:            String[18];
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

  // MySQL Functions structure
  TMySQLFunction = record
    Name:         String;
    Declaration:  String;
    Category:     String;
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

var
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
      Index:           tpBINARY;
      Name:            'BINARY';
      HasLength:       True;
      RequiresLength:  True;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
      DefLengthSet:    '50';
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
    ),
    (
      Index:           tpPOINT;
      Name:            'POINT';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpLINESTRING;
      Name:            'LINESTRING';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpPOLYGON;
      Name:            'POLYGON';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpGEOMETRY;
      Name:            'GEOMETRY';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpMULTIPOINT;
      Name:            'MULTIPOINT';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpMULTILINESTRING;
      Name:            'MULTILINESTRING';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpMULTIPOLYGON;
      Name:            'MULTIPOLYGON';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    ),
    (
      Index:           tpGEOMETRYCOLLECTION;
      Name:            'GEOMETRYCOLLECTION';
      HasLength:       False;
      RequiresLength:  False;
      HasUnsigned:     False;
      HasZerofill:     False;
      HasBinary:       False;
      HasDefault:      True;
    )
  );


  MySqlFunctions: Array [0..216] of TMysqlFunction =
  (

    // Function nr. 1
    (
      Name:         '!';
      Declaration:  '';
      Category:     'Logical operators';
      Description:  'Logical NOT. Evaluates to 1 if the operand is 0, to 0 if the operand '
        +'is non-zero, and NOT NULL returns NULL.'
    ),

    // Function nr. 2
    (
      Name:         '!=';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'Not equal:'
    ),

    // Function nr. 3
    (
      Name:         '&&';
      Declaration:  '';
      Category:     'Logical operators';
      Description:  'Logical AND. Evaluates to 1 if all operands are non-zero and not NULL, '
        +'to 0 if one or more operands are 0, otherwise NULL is returned.'
    ),

    // Function nr. 4
    (
      Name:         '&';
      Declaration:  '';
      Category:     'Bit Functions';
      Description:  'Bitwise AND:'
    ),

    // Function nr. 5
    (
      Name:         '*';
      Declaration:  '';
      Category:     'Numeric Functions';
      Description:  'Multiplication:'
    ),

    // Function nr. 6
    (
      Name:         '+';
      Declaration:  '';
      Category:     'Numeric Functions';
      Description:  'Addition:'
    ),

    // Function nr. 7
    (
      Name:         '-';
      Declaration:  '';
      Category:     'Numeric Functions';
      Description:  'Subtraction:'
    ),

    // Function nr. 8
    (
      Name:         '/';
      Declaration:  '';
      Category:     'Numeric Functions';
      Description:  'Division:'
    ),

    // Function nr. 9
    (
      Name:         '<';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'Less than:'
    ),

    // Function nr. 10
    (
      Name:         '<<';
      Declaration:  '';
      Category:     'Bit Functions';
      Description:  'Shifts a longlong (BIGINT) number to the left.'
    ),

    // Function nr. 11
    (
      Name:         '<=';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'Less than or equal:'
    ),

    // Function nr. 12
    (
      Name:         '<=>';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'NULL-safe equal. This operator performs an equality comparison like '
        +'the = operator, but returns 1 rather than NULL if both operands are '
        +'NULL, and 0 rather than NULL if one operand is NULL.'
    ),

    // Function nr. 13
    (
      Name:         '=';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  ''
    ),

    // Function nr. 14
    (
      Name:         '>';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'Greater than:'
    ),

    // Function nr. 15
    (
      Name:         '>=';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'Greater than or equal:'
    ),

    // Function nr. 16
    (
      Name:         '>>';
      Declaration:  '';
      Category:     'Bit Functions';
      Description:  'Shifts a longlong (BIGINT) number to the right.'
    ),

    // Function nr. 17
    (
      Name:         'ABS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the absolute value of X.'
    ),

    // Function nr. 18
    (
      Name:         'ACOS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the arc cosine of X, that is, the value whose cosine is X. '
        +'Returns NULL if X is not in the range -1 to 1.'
    ),

    // Function nr. 19
    (
      Name:         'ADDDATE';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
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
      Description:  'ADDTIME() adds expr2 to expr1 and returns the result. expr1 is a time '
        +'or datetime expression, and expr2 is a time expression.'
    ),

    // Function nr. 21
    (
      Name:         'AES_DECRYPT';
      Declaration:  '(str,key_str)';
      Category:     'Encryption Functions';
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
      Name:         'ASCII';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the numeric value of the leftmost character of the string str. '
        +'Returns 0 if str is the empty string. Returns NULL if str is NULL. '
        +'ASCII() works for characters with numeric values from 0 to 255.'
    ),

    // Function nr. 23
    (
      Name:         'ASIN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the arc sine of X, that is, the value whose sine is X. Returns '
        +'NULL if X is not in the range -1 to 1.'
    ),

    // Function nr. 24
    (
      Name:         'ATAN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the arc tangent of X, that is, the value whose tangent is X.'
    ),

    // Function nr. 25
    (
      Name:         'ATAN2';
      Declaration:  '(Y,X)';
      Category:     'Numeric Functions';
      Description:  'Returns the arc tangent of the two variables X and Y. It is similar to '
        +'calculating the arc tangent of Y / X, except that the signs of both '
        +'arguments are used to determine the quadrant of the result.'
    ),

    // Function nr. 26
    (
      Name:         'BENCHMARK';
      Declaration:  '(count,expr)';
      Category:     'Information Functions';
      Description:  'The BENCHMARK() function executes the expression expr repeatedly count '
        +'times. It may be used to time how quickly MySQL processes the '
        +'expression. The result value is always 0. The intended use is from '
        +'within the mysql client, which reports query execution times:'
    ),

    // Function nr. 27
    (
      Name:         'BETWEEN';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'If expr is greater than or equal to min and expr is less than or equal '
        +'to max, BETWEEN returns 1, otherwise it returns 0. This is equivalent '
        +'to the expression (min <= expr AND expr <= max) if all the arguments '
        +'are of the same type. Otherwise type conversion takes place according '
        +'to the rules described in '
        +'http://dev.mysql.com/doc/refman/5.1/en/type-conversion.html, but '
        +'applied to all the three arguments.'
    ),

    // Function nr. 28
    (
      Name:         'BIN';
      Declaration:  '(N)';
      Category:     'String Functions';
      Description:  'Returns a string representation of the binary value of N, where N is a '
        +'longlong (BIGINT) number. This is equivalent to CONV(N,10,2). Returns '
        +'NULL if N is NULL.'
    ),

    // Function nr. 29
    (
      Name:         'BINARY';
      Declaration:  '';
      Category:     'String Functions';
      Description:  'The BINARY operator casts the string following it to a binary string. '
        +'This is an easy way to force a column comparison to be done byte by '
        +'byte rather than character by character. This causes the comparison to '
        +'be case sensitive even if the column isn''t defined as BINARY or BLOB. '
        +'BINARY also causes trailing spaces to be significant.'
    ),

    // Function nr. 30
    (
      Name:         'BIT_COUNT';
      Declaration:  '(N)';
      Category:     'Bit Functions';
      Description:  'Returns the number of bits that are set in the argument N.'
    ),

    // Function nr. 31
    (
      Name:         'BIT_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the length of the string str in bits.'
    ),

    // Function nr. 32
    (
      Name:         'CASE';
      Declaration:  '';
      Category:     'Control flow functions';
      Description:  'CASE WHEN [condition] THEN result [WHEN [condition] THEN result ...] '
        +'[ELSE result] END The first version returns the result where '
        +'value=compare_value. The second version returns the result for the '
        +'first condition that is true. If there was no matching result value, '
        +'the result after ELSE is returned, or NULL if there is no ELSE part.'
    ),

    // Function nr. 33
    (
      Name:         'CAST';
      Declaration:  '(expr AS type)';
      Category:     'String Functions';
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

    // Function nr. 34
    (
      Name:         'CEILING';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the smallest integer value not less than X.'
    ),

    // Function nr. 35
    (
      Name:         'CHAR';
      Declaration:  '(N,... [USING charset_name])';
      Category:     'String Functions';
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

    // Function nr. 36
    (
      Name:         'CHARACTER_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'CHARACTER_LENGTH() is a synonym for CHAR_LENGTH().'
    ),

    // Function nr. 37
    (
      Name:         'CHARSET';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Description:  'Returns the character set of the string argument.'
    ),

    // Function nr. 38
    (
      Name:         'CHAR_LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the length of the string str, measured in characters. A '
        +'multi-byte character counts as a single character. This means that for '
        +'a string containing five two-byte characters, LENGTH() returns 10, '
        +'whereas CHAR_LENGTH() returns 5.'
    ),

    // Function nr. 39
    (
      Name:         'COALESCE';
      Declaration:  '(value,...)';
      Category:     'Comparison operators';
      Description:  'Returns the first non-NULL value in the list, or NULL if there are no '
        +'non-NULL values.'
    ),

    // Function nr. 40
    (
      Name:         'COERCIBILITY';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Description:  'Returns the collation coercibility value of the string argument.'
    ),

    // Function nr. 41
    (
      Name:         'COLLATION';
      Declaration:  '(str)';
      Category:     'Information Functions';
      Description:  'Returns the collation of the string argument.'
    ),

    // Function nr. 42
    (
      Name:         'COMPRESS';
      Declaration:  '(string_to_compress)';
      Category:     'Encryption Functions';
      Description:  'Compresses a string and returns the result as a binary string. This '
        +'function requires MySQL to have been compiled with a compression '
        +'library such as zlib. Otherwise, the return value is always NULL. The '
        +'compressed string can be uncompressed with UNCOMPRESS().'
    ),

    // Function nr. 43
    (
      Name:         'CONCAT';
      Declaration:  '(str1,str2,...)';
      Category:     'String Functions';
      Description:  'Returns the string that results from concatenating the arguments. May '
        +'have one or more arguments. If all arguments are non-binary strings, '
        +'the result is a non-binary string. If the arguments include any binary '
        +'strings, the result is a binary string. A numeric argument is '
        +'converted to its equivalent binary string form; if you want to avoid '
        +'that, you can use an explicit type cast, as in this example: SELECT '
        +'CONCAT(CAST(int_col AS CHAR), char_col); CONCAT() returns NULL if any '
        +'argument is NULL.'
    ),

    // Function nr. 44
    (
      Name:         'CONCAT_WS';
      Declaration:  '(separator,str1,str2,...)';
      Category:     'String Functions';
      Description:  'CONCAT_WS() stands for Concatenate With Separator and is a special '
        +'form of CONCAT(). The first argument is the separator for the rest of '
        +'the arguments. The separator is added between the strings to be '
        +'concatenated. The separator can be a string, as can the rest of the '
        +'arguments. If the separator is NULL, the result is NULL.'
    ),

    // Function nr. 45
    (
      Name:         'CONNECTION_ID';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'Returns the connection ID (thread ID) for the connection. Every '
        +'connection has an ID that is unique among the set of currently '
        +'connected clients.'
    ),

    // Function nr. 46
    (
      Name:         'CONV';
      Declaration:  '(N,from_base,to_base)';
      Category:     'String Functions';
      Description:  'Converts numbers between different number bases. Returns a string '
        +'representation of the number N, converted from base from_base to base '
        +'to_base. Returns NULL if any argument is NULL. The argument N is '
        +'interpreted as an integer, but may be specified as an integer or a '
        +'string. The minimum base is 2 and the maximum base is 36. If to_base '
        +'is a negative number, N is regarded as a signed number. Otherwise, N '
        +'is treated as unsigned. CONV() works with 64-bit precision.'
    ),

    // Function nr. 47
    (
      Name:         'CONVERT_TZ';
      Declaration:  '(dt,from_tz,to_tz)';
      Category:     'Date and Time Functions';
      Description:  'CONVERT_TZ() converts a datetime value dt from the time zone given by '
        +'from_tz to the time zone given by to_tz and returns the resulting '
        +'value. Time zones are specified as described in '
        +'http://dev.mysql.com/doc/refman/5.1/en/time-zone-support.html. This '
        +'function returns NULL if the arguments are invalid.'
    ),

    // Function nr. 48
    (
      Name:         'COS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the cosine of X, where X is given in radians.'
    ),

    // Function nr. 49
    (
      Name:         'COT';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the cotangent of X.'
    ),

    // Function nr. 50
    (
      Name:         'CRC32';
      Declaration:  '(expr)';
      Category:     'Numeric Functions';
      Description:  'Computes a cyclic redundancy check value and returns a 32-bit unsigned '
        +'value. The result is NULL if the argument is NULL. The argument is '
        +'expected to be a string and (if possible) is treated as one if it is '
        +'not.'
    ),

    // Function nr. 51
    (
      Name:         'CURDATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'Returns the current date as a value in ''YYYY-MM-DD'' or YYYYMMDD '
        +'format, depending on whether the function is used in a string or '
        +'numeric context.'
    ),

    // Function nr. 52
    (
      Name:         'CURRENT_DATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'CURRENT_DATE and CURRENT_DATE() are synonyms for CURDATE().'
    ),

    // Function nr. 53
    (
      Name:         'CURRENT_TIME';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'CURRENT_TIME and CURRENT_TIME() are synonyms for CURTIME().'
    ),

    // Function nr. 54
    (
      Name:         'CURRENT_TIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'CURRENT_TIMESTAMP and CURRENT_TIMESTAMP() are synonyms for NOW().'
    ),

    // Function nr. 55
    (
      Name:         'CURRENT_USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'Returns the username and hostname combination for the MySQL account '
        +'that the server used to authenticate the current client. This account '
        +'determines your access privileges. Within a stored routine that is '
        +'defined with the SQL SECURITY DEFINER characteristic, CURRENT_USER() '
        +'returns the creator of the routine. The return value is a string in '
        +'the utf8 character set. The value of CURRENT_USER() can differ from '
        +'the value of USER().'
    ),

    // Function nr. 56
    (
      Name:         'CURTIME';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'Returns the current time as a value in ''HH:MM:SS'' or HHMMSS format, '
        +'depending on whether the function is used in a string or numeric '
        +'context.'
    ),

    // Function nr. 57
    (
      Name:         'DATABASE';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'Returns the default (current) database name as a string in the utf8 '
        +'character set. If there is no default database, DATABASE() returns '
        +'NULL. Within a stored routine, the default database is the database '
        +'that the routine is associated with, which is not necessarily the same '
        +'as the database that is the default in the calling context.'
    ),

    // Function nr. 58
    (
      Name:         'DATE';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Description:  'Extracts the date part of the date or datetime expression expr.'
    ),

    // Function nr. 59
    (
      Name:         'DATEDIFF';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Description:  'DATEDIFF() returns expr1 - expr2 expressed as a value in days from one '
        +'date to the other. expr1 and expr2 are date or date-and-time '
        +'expressions. Only the date parts of the values are used in the '
        +'calculation.'
    ),

    // Function nr. 60
    (
      Name:         'DATE_ADD';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Description:  'These functions perform date arithmetic. date is a DATETIME or DATE '
        +'value specifying the starting date. expr is an expression specifying '
        +'the interval value to be added or subtracted from the starting date. '
        +'expr is a string; it may start with a `-'' for negative intervals. '
        +'unit is a keyword indicating the units in which the expression should '
        +'be interpreted.'
    ),

    // Function nr. 61
    (
      Name:         'DATE_FORMAT';
      Declaration:  '(date,format)';
      Category:     'Date and Time Functions';
      Description:  'Formats the date value according to the format string.'
    ),

    // Function nr. 62
    (
      Name:         'DATE_SUB';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
      Description:  'See DATE_ADD().'
    ),

    // Function nr. 63
    (
      Name:         'DAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'DAY() is a synonym for DAYOFMONTH().'
    ),

    // Function nr. 64
    (
      Name:         'DAYNAME';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the name of the weekday for date. As of MySQL 5.1.12, the '
        +'language used for the name is controlled by the value of the '
        +'lc_time_names system variable '
        +'(http://dev.mysql.com/doc/refman/5.1/en/locale-support.html).'
    ),

    // Function nr. 65
    (
      Name:         'DAYOFMONTH';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the day of the month for date, in the range 0 to 31.'
    ),

    // Function nr. 66
    (
      Name:         'DAYOFWEEK';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the weekday index for date (1 = Sunday, 2 = Monday, ..., 7 = '
        +'Saturday). These index values correspond to the ODBC standard.'
    ),

    // Function nr. 67
    (
      Name:         'DAYOFYEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the day of the year for date, in the range 1 to 366.'
    ),

    // Function nr. 68
    (
      Name:         'DECODE';
      Declaration:  '(crypt_str,pass_str)';
      Category:     'Encryption Functions';
      Description:  'Decrypts the encrypted string crypt_str using pass_str as the '
        +'password. crypt_str should be a string returned from ENCODE().'
    ),

    // Function nr. 69
    (
      Name:         'DEFAULT';
      Declaration:  '(col_name)';
      Category:     'Miscellaneous Functions';
      Description:  'Returns the default value for a table column. An error results if the '
        +'column has no default value.'
    ),

    // Function nr. 70
    (
      Name:         'DEGREES';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the argument X, converted from radians to degrees.'
    ),

    // Function nr. 71
    (
      Name:         'DES_DECRYPT';
      Declaration:  '(crypt_str[,key_str])';
      Category:     'Encryption Functions';
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

    // Function nr. 72
    (
      Name:         'DES_ENCRYPT';
      Declaration:  '(str[,{key_num|key_str}])';
      Category:     'Encryption Functions';
      Description:  'Encrypts the string with the given key using the Triple-DES algorithm.'
    ),

    // Function nr. 73
    (
      Name:         'DIV';
      Declaration:  '';
      Category:     'Numeric Functions';
      Description:  'Integer division. Similar to FLOOR(), but is safe with BIGINT values.'
    ),

    // Function nr. 74
    (
      Name:         'ELT';
      Declaration:  '(N,str1,str2,str3,...)';
      Category:     'String Functions';
      Description:  'Returns str1 if N = 1, str2 if N = 2, and so on. Returns NULL if N is '
        +'less than 1 or greater than the number of arguments. ELT() is the '
        +'complement of FIELD().'
    ),

    // Function nr. 75
    (
      Name:         'ENCODE';
      Declaration:  '(str,pass_str)';
      Category:     'Encryption Functions';
      Description:  'Encrypt str using pass_str as the password. To decrypt the result, use '
        +'DECODE(). The result is a binary string of the same length as str. The '
        +'strength of the encryption is based on how good the random generator '
        +'is. It should suffice for short strings.'
    ),

    // Function nr. 76
    (
      Name:         'ENCRYPT';
      Declaration:  '(str[,salt])';
      Category:     'Encryption Functions';
      Description:  'Encrypts str using the Unix crypt() system call and returns a binary '
        +'string. The salt argument should be a string with at least two '
        +'characters. If no salt argument is given, a random value is used.'
    ),

    // Function nr. 77
    (
      Name:         'EXP';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the value of e (the base of natural logarithms) raised to the '
        +'power of X.'
    ),

    // Function nr. 78
    (
      Name:         'EXPORT_SET';
      Declaration:  '(bits,on,off[,separator[,number_of_bits]])';
      Category:     'String Functions';
      Description:  'Returns a string such that for every bit set in the value bits, you '
        +'get an on string and for every reset bit, you get an off string. Bits '
        +'in bits are examined from right to left (from low-order to high-order '
        +'bits). Strings are added to the result from left to right, separated '
        +'by the separator string (the default being the comma character `,''). '
        +'The number of bits examined is given by number_of_bits (defaults to '
        +'64).'
    ),

    // Function nr. 79
    (
      Name:         'EXTRACT';
      Declaration:  '(unit FROM date)';
      Category:     'Date and Time Functions';
      Description:  'The EXTRACT() function uses the same kinds of unit specifiers as '
        +'DATE_ADD() or DATE_SUB(), but extracts parts from the date rather than '
        +'performing date arithmetic.'
    ),

    // Function nr. 80
    (
      Name:         'EXTRACTVALUE';
      Declaration:  '(xml_frag, xpath_expr)';
      Category:     'String Functions';
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

    // Function nr. 81
    (
      Name:         'FIELD';
      Declaration:  '(str,str1,str2,str3,...)';
      Category:     'String Functions';
      Description:  'Returns the index (position) of str in the str1, str2, str3, ... list. '
        +'Returns 0 if str is not found. If all arguments to FIELD() are '
        +'strings, all arguments are compared as strings. If all arguments are '
        +'numbers, they are compared as numbers. Otherwise, the arguments are '
        +'compared as double. If str is NULL, the return value is 0 because NULL '
        +'fails equality comparison with any value. FIELD() is the complement of '
        +'ELT().'
    ),

    // Function nr. 82
    (
      Name:         'FIND_IN_SET';
      Declaration:  '(str,strlist)';
      Category:     'String Functions';
      Description:  'Returns a value in the range of 1 to N if the string str is in the '
        +'string list strlist consisting of N substrings. A string list is a '
        +'string composed of substrings separated by `,'' characters. If the '
        +'first argument is a constant string and the second is a column of type '
        +'SET, the FIND_IN_SET() function is optimized to use bit arithmetic. '
        +'Returns 0 if str is not in strlist or if strlist is the empty string. '
        +'Returns NULL if either argument is NULL. This function does not work '
        +'properly if the first argument contains a comma (`,'') character.'
    ),

    // Function nr. 83
    (
      Name:         'FLOOR';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the largest integer value not greater than X.'
    ),

    // Function nr. 84
    (
      Name:         'FORMAT';
      Declaration:  '(X,D)';
      Category:     'String Functions';
      Description:  'Formats the number X to a format like ''#,###,###.##'', rounded to D '
        +'decimal places, and returns the result as a string. If D is 0, the '
        +'result has no decimal point or fractional part.'
    ),

    // Function nr. 85
    (
      Name:         'FOUND_ROWS';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'A SELECT statement may include a LIMIT clause to restrict the number '
        +'of rows the server returns to the client. In some cases, it is '
        +'desirable to know how many rows the statement would have returned '
        +'without the LIMIT, but without running the statement again. To obtain '
        +'this row count, include a SQL_CALC_FOUND_ROWS option in the SELECT '
        +'statement, and then invoke FOUND_ROWS() afterward:'
    ),

    // Function nr. 86
    (
      Name:         'FROM_DAYS';
      Declaration:  '(N)';
      Category:     'Date and Time Functions';
      Description:  'Given a day number N, returns a DATE value.'
    ),

    // Function nr. 87
    (
      Name:         'FROM_UNIXTIME';
      Declaration:  '(unix_timestamp)';
      Category:     'Date and Time Functions';
      Description:  'Returns a representation of the unix_timestamp argument as a value in '
        +'''YYYY-MM-DD HH:MM:SS'' or YYYYMMDDHHMMSS format, depending on whether '
        +'the function is used in a string or numeric context. unix_timestamp is '
        +'an internal timestamp value such as is produced by the '
        +'UNIX_TIMESTAMP() function. If format is given, the result is formatted '
        +'according to the format string, which is used the same way as listed '
        +'in the entry for the DATE_FORMAT() function.'
    ),

    // Function nr. 88
    (
      Name:         'GET_FORMAT';
      Declaration:  '(DATE|TIME|DATETIME, ''EUR''|''USA''|''JIS''|''ISO''|''INTERNAL'')';
      Category:     'Date and Time Functions';
      Description:  'Returns a format string. This function is useful in combination with '
        +'the DATE_FORMAT() and the STR_TO_DATE() functions.'
    ),

    // Function nr. 89
    (
      Name:         'GET_LOCK';
      Declaration:  '(str,timeout)';
      Category:     'Miscellaneous Functions';
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

    // Function nr. 90
    (
      Name:         'GREATEST';
      Declaration:  '(value1,value2,...)';
      Category:     'Comparison operators';
      Description:  'With two or more arguments, returns the largest (maximum-valued) '
        +'argument. The arguments are compared using the same rules as for '
        +'LEAST().'
    ),

    // Function nr. 91
    (
      Name:         'HEX';
      Declaration:  '(N_or_S)';
      Category:     'String Functions';
      Description:  'If N_or_S is a number, returns a string representation of the '
        +'hexadecimal value of N, where N is a longlong (BIGINT) number. This is '
        +'equivalent to CONV(N,10,16). If N_or_S is a string, returns a '
        +'hexadecimal string representation of N_or_S where each character in '
        +'N_or_S is converted to two hexadecimal digits.'
    ),

    // Function nr. 92
    (
      Name:         'HOUR';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Description:  'Returns the hour for time. The range of the return value is 0 to 23 '
        +'for time-of-day values. However, the range of TIME values actually is '
        +'much larger, so HOUR can return values greater than 23.'
    ),

    // Function nr. 93
    (
      Name:         'IF';
      Declaration:  '(expr1,expr2,expr3)';
      Category:     'Control flow functions';
      Description:  'If expr1 is TRUE (expr1 <> 0 and expr1 <> NULL) then IF() returns '
        +'expr2; otherwise it returns expr3. IF() returns a numeric or string '
        +'value, depending on the context in which it is used.'
    ),

    // Function nr. 94
    (
      Name:         'IFNULL';
      Declaration:  '(expr1,expr2)';
      Category:     'Control flow functions';
      Description:  'If expr1 is not NULL, IFNULL() returns expr1; otherwise it returns '
        +'expr2. IFNULL() returns a numeric or string value, depending on the '
        +'context in which it is used.'
    ),

    // Function nr. 95
    (
      Name:         'IN';
      Declaration:  '(value,...)';
      Category:     'Comparison operators';
      Description:  'Returns 1 if expr is equal to any of the values in the IN list, else '
        +'returns 0. If all values are constants, they are evaluated according '
        +'to the type of expr and sorted. The search for the item then is done '
        +'using a binary search. This means IN is very quick if the IN value '
        +'list consists entirely of constants. Otherwise, type conversion takes '
        +'place according to the rules described in '
        +'http://dev.mysql.com/doc/refman/5.1/en/type-conversion.html, but '
        +'applied to all the arguments.'
    ),

    // Function nr. 96
    (
      Name:         'INET_ATON';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Description:  'Given the dotted-quad representation of a network address as a string, '
        +'returns an integer that represents the numeric value of the address. '
        +'Addresses may be 4- or 8-byte addresses.'
    ),

    // Function nr. 97
    (
      Name:         'INET_NTOA';
      Declaration:  '(expr)';
      Category:     'Miscellaneous Functions';
      Description:  'Given a numeric network address (4 or 8 byte), returns the dotted-quad '
        +'representation of the address as a string.'
    ),

    // Function nr. 98
    (
      Name:         'INSERT';
      Declaration:  '(str,pos,len,newstr)';
      Category:     'String Functions';
      Description:  'Returns the string str, with the substring beginning at position pos '
        +'and len characters long replaced by the string newstr. Returns the '
        +'original string if pos is not within the length of the string. '
        +'Replaces the rest of the string from position pos is len is not within '
        +'the length of the rest of the string. Returns NULL if any argument is '
        +'NULL.'
    ),

    // Function nr. 99
    (
      Name:         'INSTR';
      Declaration:  '(str,substr)';
      Category:     'String Functions';
      Description:  'Returns the position of the first occurrence of substring substr in '
        +'string str. This is the same as the two-argument form of LOCATE(), '
        +'except that the order of the arguments is reversed.'
    ),

    // Function nr. 100
    (
      Name:         'INTERVAL';
      Declaration:  '(N,N1,N2,N3,...)';
      Category:     'Comparison operators';
      Description:  'Returns 0 if N < N1, 1 if N < N2 and so on or -1 if N is NULL. All '
        +'arguments are treated as integers. It is required that N1 < N2 < N3 < '
        +'... < Nn for this function to work correctly. This is because a binary '
        +'search is used (very fast).'
    ),

    // Function nr. 101
    (
      Name:         'IS';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'Tests a value against a boolean value, where boolean_value can be '
        +'TRUE, FALSE, or UNKNOWN.'
    ),

    // Function nr. 102
    (
      Name:         'ISNULL';
      Declaration:  '(expr)';
      Category:     'Comparison operators';
      Description:  'If expr is NULL, ISNULL() returns 1, otherwise it returns 0.'
    ),

    // Function nr. 103
    (
      Name:         'IS_FREE_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Description:  'Checks whether the lock named str is free to use (that is, not '
        +'locked). Returns 1 if the lock is free (no one is using the lock), 0 '
        +'if the lock is in use, and NULL if an error occurs (such as an '
        +'incorrect argument).'
    ),

    // Function nr. 104
    (
      Name:         'IS_USED_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Description:  'Checks whether the lock named str is in use (that is, locked). If so, '
        +'it returns the connection identifier of the client that holds the '
        +'lock. Otherwise, it returns NULL.'
    ),

    // Function nr. 105
    (
      Name:         'LAST_DAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Takes a date or datetime value and returns the corresponding value for '
        +'the last day of the month. Returns NULL if the argument is invalid.'
    ),

    // Function nr. 106
    (
      Name:         'LAST_INSERT_ID';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'For MySQL 5.1.12 and later, LAST_INSERT_ID() (no arguments) returns '
        +'the first automatically generated value successfully inserted for an '
        +'AUTO_INCREMENT column as a result of the most recently executed INSERT '
        +'statement. The value of LAST_INSERT_ID() remains unchanged if no rows '
        +'are successfully inserted. For example, after inserting a row that '
        +'generates an AUTO_INCREMENT value, you can get the value like this: '
        +'mysql> SELECT LAST_INSERT_ID(); -> 195 In MySQL 5.1.11 and earlier, '
        +'LAST_INSERT_ID() (no arguments) returns the first automatically '
        +'generated value if any rows were successfully inserted or updated. '
        +'This means that the returned value could be a value that was not '
        +'successfully inserted into the table. If no rows were successfully '
        +'inserted, LAST_INSERT_ID() returns 0. The value of LAST_INSERT_ID() '
        +'will be consistent across all versions if all rows in the INSERT or '
        +'UPDATE statement were successful. The currently executing statement '
        +'does not affect the value of LAST_INSERT_ID(). Suppose that you '
        +'generate an AUTO_INCREMENT value with one statement, and then refer to '
        +'LAST_INSERT_ID() in a multiple-row INSERT statement that inserts rows '
        +'into a table with its own AUTO_INCREMENT column. The value of '
        +'LAST_INSERT_ID() will remain stable in the second statement; its value '
        +'for the second and later rows is not affected by the earlier row '
        +'insertions. (However, if you mix references to LAST_INSERT_ID() and '
        +'LAST_INSERT_ID(expr), the effect is undefined.) If the previous '
        +'statement returned an error, the value of LAST_INSERT_ID() is '
        +'undefined. For transactional tables, if the statement is rolled back '
        +'due to an error, the value of LAST_INSERT_ID() is left undefined. For '
        +'manual ROLLBACK, the value of LAST_INSERT_ID() is not restored to that '
        +'before the transaction; it remains as it was at the point of the '
        +'ROLLBACK. Within the body of a stored routine (procedure or function) '
        +'or a trigger, the value of LAST_INSERT_ID() changes the same way as '
        +'for statements executed outside the body of these kinds of objects. '
        +'The effect of a stored routine or trigger upon the value of '
        +'LAST_INSERT_ID() that is seen by following statements depends on the '
        +'kind of routine: If a stored procedure executes statements that change '
        +'the value of LAST_INSERT_ID(), the changed value will be seen by '
        +'statements that follow the procedure call. For stored functions and '
        +'triggers that change the value, the value is restored when the '
        +'function or trigger ends, so following statements will not see a '
        +'changed value.'
    ),

    // Function nr. 107
    (
      Name:         'LCASE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'LCASE() is a synonym for LOWER().'
    ),

    // Function nr. 108
    (
      Name:         'LEAST';
      Declaration:  '(value1,value2,...)';
      Category:     'Comparison operators';
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

    // Function nr. 109
    (
      Name:         'LEFT';
      Declaration:  '(str,len)';
      Category:     'String Functions';
      Description:  'Returns the leftmost len characters from the string str, or NULL if '
        +'any argument is NULL.'
    ),

    // Function nr. 110
    (
      Name:         'LENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the length of the string str, measured in bytes. A multi-byte '
        +'character counts as multiple bytes. This means that for a string '
        +'containing five two-byte characters, LENGTH() returns 10, whereas '
        +'CHAR_LENGTH() returns 5.'
    ),

    // Function nr. 111
    (
      Name:         'LIKE';
      Declaration:  '';
      Category:     'String Functions';
      Description:  'Pattern matching using SQL simple regular expression comparison. '
        +'Returns 1 (TRUE) or 0 (FALSE). If either expr or pat is NULL, the '
        +'result is NULL. The pattern need not be a literal string. For example, '
        +'it can be specified as a string expression or table column.'
    ),

    // Function nr. 112
    (
      Name:         'LN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the natural logarithm of X; that is, the base-e logarithm of '
        +'X.'
    ),

    // Function nr. 113
    (
      Name:         'LOAD_FILE';
      Declaration:  '(file_name)';
      Category:     'String Functions';
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

    // Function nr. 114
    (
      Name:         'LOCALTIME';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'LOCALTIME and LOCALTIME() are synonyms for NOW().'
    ),

    // Function nr. 115
    (
      Name:         'LOCALTIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'LOCALTIMESTAMP and LOCALTIMESTAMP() are synonyms for NOW().'
    ),

    // Function nr. 116
    (
      Name:         'LOCATE';
      Declaration:  '(substr,str)';
      Category:     'String Functions';
      Description:  'The first syntax returns the position of the first occurrence of '
        +'substring substr in string str. The second syntax returns the position '
        +'of the first occurrence of substring substr in string str, starting at '
        +'position pos. Returns 0 if substr is not in str.'
    ),

    // Function nr. 117
    (
      Name:         'LOG';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'If called with one parameter, this function returns the natural '
        +'logarithm of X.'
    ),

    // Function nr. 118
    (
      Name:         'LOG10';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the base-10 logarithm of X.'
    ),

    // Function nr. 119
    (
      Name:         'LOG2';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the base-2 logarithm of X.'
    ),

    // Function nr. 120
    (
      Name:         'LOWER';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the string str with all characters changed to lowercase '
        +'according to the current character set mapping. The default is latin1 '
        +'(cp1252 West European).'
    ),

    // Function nr. 121
    (
      Name:         'LPAD';
      Declaration:  '(str,len,padstr)';
      Category:     'String Functions';
      Description:  'Returns the string str, left-padded with the string padstr to a length '
        +'of len characters. If str is longer than len, the return value is '
        +'shortened to len characters.'
    ),

    // Function nr. 122
    (
      Name:         'LTRIM';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the string str with leading space characters removed.'
    ),

    // Function nr. 123
    (
      Name:         'MAKEDATE';
      Declaration:  '(year,dayofyear)';
      Category:     'Date and Time Functions';
      Description:  'Returns a date, given year and day-of-year values. dayofyear must be '
        +'greater than 0 or the result is NULL.'
    ),

    // Function nr. 124
    (
      Name:         'MAKETIME';
      Declaration:  '(hour,minute,second)';
      Category:     'Date and Time Functions';
      Description:  'Returns a time value calculated from the hour, minute, and second '
        +'arguments.'
    ),

    // Function nr. 125
    (
      Name:         'MAKE_SET';
      Declaration:  '(bits,str1,str2,...)';
      Category:     'String Functions';
      Description:  'Returns a set value (a string containing substrings separated by `,'' '
        +'characters) consisting of the strings that have the corresponding bit '
        +'in bits set. str1 corresponds to bit 0, str2 to bit 1, and so on. NULL '
        +'values in str1, str2, ... are not appended to the result.'
    ),

    // Function nr. 126
    (
      Name:         'MASTER_POS_WAIT';
      Declaration:  '(log_name,log_pos[,timeout])';
      Category:     'Miscellaneous Functions';
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

    // Function nr. 127
    (
      Name:         'MATCH';
      Declaration:  '(col1,col2,...)';
      Category:     'String Functions';
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

    // Function nr. 128
    (
      Name:         'MD5';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Description:  'Calculates an MD5 128-bit checksum for the string. The value is '
        +'returned as a binary string of 32 hex digits, or NULL if the argument '
        +'was NULL. The return value can, for example, be used as a hash key.'
    ),

    // Function nr. 129
    (
      Name:         'MICROSECOND';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Description:  'Returns the microseconds from the time or datetime expression expr as '
        +'a number in the range from 0 to 999999.'
    ),

    // Function nr. 130
    (
      Name:         'MID';
      Declaration:  '(str,pos,len)';
      Category:     'String Functions';
      Description:  'MID(str,pos,len) is a synonym for SUBSTRING(str,pos,len).'
    ),

    // Function nr. 131
    (
      Name:         'MINUTE';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Description:  'Returns the minute for time, in the range 0 to 59.'
    ),

    // Function nr. 132
    (
      Name:         'MOD';
      Declaration:  '(N,M)';
      Category:     'Numeric Functions';
      Description:  'Modulo operation. Returns the remainder of N divided by M.'
    ),

    // Function nr. 133
    (
      Name:         'MONTH';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the month for date, in the range 0 to 12.'
    ),

    // Function nr. 134
    (
      Name:         'MONTHNAME';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the full name of the month for date. As of MySQL 5.1.12, the '
        +'language used for the name is controlled by the value of the '
        +'lc_time_names system variable '
        +'(http://dev.mysql.com/doc/refman/5.1/en/locale-support.html).'
    ),

    // Function nr. 135
    (
      Name:         'NAME_CONST';
      Declaration:  '(name,value)';
      Category:     'Miscellaneous Functions';
      Description:  'Returns the given value. When used to produce a result set column, '
        +'NAME_CONST() causes the column to have the given name. mysql> SELECT '
        +'NAME_CONST(''myname'', 14); +--------+ | myname | +--------+ | 14 | '
        +'+--------+'
    ),

    // Function nr. 136
    (
      Name:         'NOT';
      Declaration:  '';
      Category:     'Comparison operators';
      Description:  'This is the same as NOT (expr BETWEEN min AND max).'
    ),

    // Function nr. 137
    (
      Name:         'NOW';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'Returns the current date and time as a value in ''YYYY-MM-DD '
        +'HH:MM:SS'' or YYYYMMDDHHMMSS format, depending on whether the function '
        +'is used in a string or numeric context.'
    ),

    // Function nr. 138
    (
      Name:         'NULLIF';
      Declaration:  '(expr1,expr2)';
      Category:     'Control flow functions';
      Description:  'Returns NULL if expr1 = expr2 is true, otherwise returns expr1. This '
        +'is the same as CASE WHEN expr1 = expr2 THEN NULL ELSE expr1 END.'
    ),

    // Function nr. 139
    (
      Name:         'OCT';
      Declaration:  '(N)';
      Category:     'String Functions';
      Description:  'Returns a string representation of the octal value of N, where N is a '
        +'longlong (BIGINT) number. This is equivalent to CONV(N,10,8). Returns '
        +'NULL if N is NULL.'
    ),

    // Function nr. 140
    (
      Name:         'OCTETLENGTH';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'OCTET_LENGTH() is a synonym for LENGTH().'
    ),

    // Function nr. 141
    (
      Name:         'OLD_PASSWORD';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Description:  'OLD_PASSWORD() was added to MySQL when the implementation of '
        +'PASSWORD() was changed to improve security. OLD_PASSWORD() returns the '
        +'value of the old (pre-4.1) implementation of PASSWORD() as a binary '
        +'string, and is intended to permit you to reset passwords for any '
        +'pre-4.1 clients that need to connect to your version 5.1 MySQL server '
        +'without locking them out. See '
        +'http://dev.mysql.com/doc/refman/5.1/en/password-hashing.html.'
    ),

    // Function nr. 142
    (
      Name:         'ORD';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'If the leftmost character of the string str is a multi-byte character, '
        +'returns the code for that character, calculated from the numeric '
        +'values of its constituent bytes using this formula: (1st byte code) + '
        +'(2nd byte code x 256) + (3rd byte code x 2562) ... If the leftmost '
        +'character is not a multi-byte character, ORD() returns the same value '
        +'as the ASCII() function.'
    ),

    // Function nr. 143
    (
      Name:         'PASSWORD';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Description:  'Calculates and returns a password string from the plaintext password '
        +'str and returns a binary string, or NULL if the argument was NULL. '
        +'This is the function that is used for encrypting MySQL passwords for '
        +'storage in the Password column of the user grant table.'
    ),

    // Function nr. 144
    (
      Name:         'PERIOD_ADD';
      Declaration:  '(P,N)';
      Category:     'Date and Time Functions';
      Description:  'Adds N months to period P (in the format YYMM or YYYYMM). Returns a '
        +'value in the format YYYYMM. Note that the period argument P is not a '
        +'date value.'
    ),

    // Function nr. 145
    (
      Name:         'PERIOD_DIFF';
      Declaration:  '(P1,P2)';
      Category:     'Date and Time Functions';
      Description:  'Returns the number of months between periods P1 and P2. P1 and P2 '
        +'should be in the format YYMM or YYYYMM. Note that the period arguments '
        +'P1 and P2 are not date values.'
    ),

    // Function nr. 146
    (
      Name:         'PI';
      Declaration:  '()';
      Category:     'Numeric Functions';
      Description:  'Returns the value of ? (pi). The default number of decimal places '
        +'displayed is seven, but MySQL uses the full double-precision value '
        +'internally.'
    ),

    // Function nr. 147
    (
      Name:         'POSITION';
      Declaration:  '(substr IN str)';
      Category:     'String Functions';
      Description:  'POSITION(substr IN str) is a synonym for LOCATE(substr,str).'
    ),

    // Function nr. 148
    (
      Name:         'POWER';
      Declaration:  '(X,Y)';
      Category:     'Numeric Functions';
      Description:  'Returns the value of X raised to the power of Y.'
    ),

    // Function nr. 149
    (
      Name:         'QUARTER';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the quarter of the year for date, in the range 1 to 4.'
    ),

    // Function nr. 150
    (
      Name:         'QUOTE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Quotes a string to produce a result that can be used as a properly '
        +'escaped data value in an SQL statement. The string is returned '
        +'enclosed by single quotes and with each instance of single quote '
        +'(`''''), backslash (`\''), ASCII NUL, and Control-Z preceded by a '
        +'backslash. If the argument is NULL, the return value is the word '
        +'"NULL" without enclosing single quotes.'
    ),

    // Function nr. 151
    (
      Name:         'RADIANS';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the argument X, converted from degrees to radians. (Note that '
        +'? radians equals 180 degrees.)'
    ),

    // Function nr. 152
    (
      Name:         'RAND';
      Declaration:  '()';
      Category:     'Numeric Functions';
      Description:  'Returns a random floating-point value v in the range 0 <= v < 1.0. If '
        +'an integer argument N is specified, it is used as the seed value, '
        +'which produces a repeatable sequence of column values.'
    ),

    // Function nr. 153
    (
      Name:         'REGEXP';
      Declaration:  '';
      Category:     'String Functions';
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

    // Function nr. 154
    (
      Name:         'RELEASE_LOCK';
      Declaration:  '(str)';
      Category:     'Miscellaneous Functions';
      Description:  'Releases the lock named by the string str that was obtained with '
        +'GET_LOCK(). Returns 1 if the lock was released, 0 if the lock was not '
        +'established by this thread (in which case the lock is not released), '
        +'and NULL if the named lock did not exist. The lock does not exist if '
        +'it was never obtained by a call to GET_LOCK() or if it has previously '
        +'been released. The DO statement is convenient to use with '
        +'RELEASE_LOCK(). See [HELP DO].'
    ),

    // Function nr. 155
    (
      Name:         'REPEAT';
      Declaration:  '(str,count)';
      Category:     'String Functions';
      Description:  'Returns a string consisting of the string str repeated count times. If '
        +'count is less than 1, returns an empty string. Returns NULL if str or '
        +'count are NULL.'
    ),

    // Function nr. 156
    (
      Name:         'REPLACE';
      Declaration:  '(str,from_str,to_str)';
      Category:     'String Functions';
      Description:  'Returns the string str with all occurrences of the string from_str '
        +'replaced by the string to_str. REPLACE() performs a case-sensitive '
        +'match when searching for from_str.'
    ),

    // Function nr. 157
    (
      Name:         'REVERSE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the string str with the order of the characters reversed.'
    ),

    // Function nr. 158
    (
      Name:         'RIGHT';
      Declaration:  '(str,len)';
      Category:     'String Functions';
      Description:  'Returns the rightmost len characters from the string str, or NULL if '
        +'any argument is NULL.'
    ),

    // Function nr. 159
    (
      Name:         'ROUND';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Rounds the argument X to D decimal places. The rounding algorithm '
        +'depends on the data type of X. D defaults to 0 if not specified. D can '
        +'be negative to cause D digits left of the decimal point of the value X '
        +'to become zero.'
    ),

    // Function nr. 160
    (
      Name:         'ROW_COUNT';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'ROW_COUNT() returns the number of rows updated, inserted, or deleted '
        +'by the preceding statement. This is the same as the row count that the '
        +'mysql client displays and the value from the mysql_affected_rows() C '
        +'API function.'
    ),

    // Function nr. 161
    (
      Name:         'RPAD';
      Declaration:  '(str,len,padstr)';
      Category:     'String Functions';
      Description:  'Returns the string str, right-padded with the string padstr to a '
        +'length of len characters. If str is longer than len, the return value '
        +'is shortened to len characters.'
    ),

    // Function nr. 162
    (
      Name:         'RTRIM';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the string str with trailing space characters removed.'
    ),

    // Function nr. 163
    (
      Name:         'SCHEMA';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'This function is a synonym for DATABASE().'
    ),

    // Function nr. 164
    (
      Name:         'SECOND';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Description:  'Returns the second for time, in the range 0 to 59.'
    ),

    // Function nr. 165
    (
      Name:         'SEC_TO_TIME';
      Declaration:  '(seconds)';
      Category:     'Date and Time Functions';
      Description:  'Returns the seconds argument, converted to hours, minutes, and '
        +'seconds, as a value in ''HH:MM:SS'' or HHMMSS format, depending on '
        +'whether the function is used in a string or numeric context.'
    ),

    // Function nr. 166
    (
      Name:         'SESSION_USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'SESSION_USER() is a synonym for USER().'
    ),

    // Function nr. 167
    (
      Name:         'SHA';
      Declaration:  '(str)';
      Category:     'Encryption Functions';
      Description:  'Calculates an SHA-1 160-bit checksum for the string, as described in '
        +'RFC 3174 (Secure Hash Algorithm). The value is returned as a binary '
        +'string of 40 hex digits, or NULL if the argument was NULL. One of the '
        +'possible uses for this function is as a hash key. You can also use it '
        +'as a cryptographic function for storing passwords. SHA() is synonymous '
        +'with SHA1().'
    ),

    // Function nr. 168
    (
      Name:         'SIGN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the sign of the argument as -1, 0, or 1, depending on whether '
        +'X is negative, zero, or positive.'
    ),

    // Function nr. 169
    (
      Name:         'SIN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the sine of X, where X is given in radians.'
    ),

    // Function nr. 170
    (
      Name:         'SLEEP';
      Declaration:  '(duration)';
      Category:     'Miscellaneous Functions';
      Description:  'Sleeps (pauses) for the number of seconds given by the duration '
        +'argument, then returns 0. If SLEEP() is interrupted, it returns 1. The '
        +'duration may have a fractional part given in microseconds.'
    ),

    // Function nr. 171
    (
      Name:         'SOUNDEX';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns a soundex string from str. Two strings that sound almost the '
        +'same should have identical soundex strings. A standard soundex string '
        +'is four characters long, but the SOUNDEX() function returns an '
        +'arbitrarily long string. You can use SUBSTRING() on the result to get '
        +'a standard soundex string. All non-alphabetic characters in str are '
        +'ignored. All international alphabetic characters outside the A-Z range '
        +'are treated as vowels. Important: When using SOUNDEX(), you should be '
        +'aware of the following limitations: This function, as currently '
        +'implemented, is intended to work well with strings that are in the '
        +'English language only. Strings in other languages may not produce '
        +'reliable results. This function is not guaranteed to provide '
        +'consistent results with strings that use multi-byte character sets, '
        +'including utf-8. We hope to remove these limitations in a future '
        +'release. See Bug#22638 (http://bugs.mysql.com/22638) for more '
        +'information.'
    ),

    // Function nr. 172
    (
      Name:         'SOUNDS';
      Declaration:  '';
      Category:     'String Functions';
      Description:  'This is the same as SOUNDEX(expr1) = SOUNDEX(expr2).'
    ),

    // Function nr. 173
    (
      Name:         'SPACE';
      Declaration:  '(N)';
      Category:     'String Functions';
      Description:  'Returns a string consisting of N space characters.'
    ),

    // Function nr. 174
    (
      Name:         'SQRT';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the square root of a non-negative number X.'
    ),

    // Function nr. 175
    (
      Name:         'STRCMP';
      Declaration:  '(expr1,expr2)';
      Category:     'String Functions';
      Description:  'STRCMP() returns 0 if the strings are the same, -1 if the first '
        +'argument is smaller than the second according to the current sort '
        +'order, and 1 otherwise.'
    ),

    // Function nr. 176
    (
      Name:         'STR_TO_DATE';
      Declaration:  '(str,format)';
      Category:     'Date and Time Functions';
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

    // Function nr. 177
    (
      Name:         'SUBDATE';
      Declaration:  '(date,INTERVAL expr unit)';
      Category:     'Date and Time Functions';
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

    // Function nr. 178
    (
      Name:         'SUBSTRING';
      Declaration:  '(str,pos)';
      Category:     'String Functions';
      Description:  'The forms without a len argument return a substring from string str '
        +'starting at position pos. The forms with a len argument return a '
        +'substring len characters long from string str, starting at position '
        +'pos. The forms that use FROM are standard SQL syntax. It is also '
        +'possible to use a negative value for pos. In this case, the beginning '
        +'of the substring is pos characters from the end of the string, rather '
        +'than the beginning. A negative value may be used for pos in any of the '
        +'forms of this function.'
    ),

    // Function nr. 179
    (
      Name:         'SUBSTRING_INDEX';
      Declaration:  '(str,delim,count)';
      Category:     'String Functions';
      Description:  'Returns the substring from string str before count occurrences of the '
        +'delimiter delim. If count is positive, everything to the left of the '
        +'final delimiter (counting from the left) is returned. If count is '
        +'negative, everything to the right of the final delimiter (counting '
        +'from the right) is returned. SUBSTRING_INDEX() performs a '
        +'case-sensitive match when searching for delim.'
    ),

    // Function nr. 180
    (
      Name:         'SUBTIME';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Description:  'SUBTIME() returns expr1 - expr2 expressed as a value in the same '
        +'format as expr1. expr1 is a time or datetime expression, and expr2 is '
        +'a time expression.'
    ),

    // Function nr. 181
    (
      Name:         'SYSDATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
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

    // Function nr. 182
    (
      Name:         'SYSTEM_USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'SYSTEM_USER() is a synonym for USER().'
    ),

    // Function nr. 183
    (
      Name:         'TAN';
      Declaration:  '(X)';
      Category:     'Numeric Functions';
      Description:  'Returns the tangent of X, where X is given in radians.'
    ),

    // Function nr. 184
    (
      Name:         'TIME';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Description:  'Extracts the time part of the time or datetime expression expr and '
        +'returns it as a string.'
    ),

    // Function nr. 185
    (
      Name:         'TIMEDIFF';
      Declaration:  '(expr1,expr2)';
      Category:     'Date and Time Functions';
      Description:  'TIMEDIFF() returns expr1 - expr2 expressed as a time value. expr1 and '
        +'expr2 are time or date-and-time expressions, but both must be of the '
        +'same type.'
    ),

    // Function nr. 186
    (
      Name:         'TIMESTAMP';
      Declaration:  '(expr)';
      Category:     'Date and Time Functions';
      Description:  'With a single argument, this function returns the date or datetime '
        +'expression expr as a datetime value. With two arguments, it adds the '
        +'time expression expr2 to the date or datetime expression expr1 and '
        +'returns the result as a datetime value.'
    ),

    // Function nr. 187
    (
      Name:         'TIMESTAMPADD';
      Declaration:  '(unit,interval,datetime_expr)';
      Category:     'Date and Time Functions';
      Description:  'Adds the integer expression interval to the date or datetime '
        +'expression datetime_expr. The unit for interval is given by the unit '
        +'argument, which should be one of the following values: FRAC_SECOND, '
        +'SECOND, MINUTE, HOUR, DAY, WEEK, MONTH, QUARTER, or YEAR. The unit '
        +'value may be specified using one of keywords as shown, or with a '
        +'prefix of SQL_TSI_. For example, DAY and SQL_TSI_DAY both are legal.'
    ),

    // Function nr. 188
    (
      Name:         'TIMESTAMPDIFF';
      Declaration:  '(unit,datetime_expr1,datetime_expr2)';
      Category:     'Date and Time Functions';
      Description:  'Returns the integer difference between the date or datetime '
        +'expressions datetime_expr1 and datetime_expr2. The unit for the result '
        +'is given by the unit argument. The legal values for unit are the same '
        +'as those listed in the description of the TIMESTAMPADD() function.'
    ),

    // Function nr. 189
    (
      Name:         'TIME_FORMAT';
      Declaration:  '(time,format)';
      Category:     'Date and Time Functions';
      Description:  'This is used like the DATE_FORMAT() function, but the format string '
        +'may contain format specifiers only for hours, minutes, and seconds. '
        +'Other specifiers produce a NULL value or 0.'
    ),

    // Function nr. 190
    (
      Name:         'TIME_TO_SEC';
      Declaration:  '(time)';
      Category:     'Date and Time Functions';
      Description:  'Returns the time argument, converted to seconds.'
    ),

    // Function nr. 191
    (
      Name:         'TO_DAYS';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Given a date date, returns a day number (the number of days since year '
        +'0).'
    ),

    // Function nr. 192
    (
      Name:         'TRIM';
      Declaration:  '([{BOTH | LEADING | TRAILING} [remstr] FROM] str)';
      Category:     'String Functions';
      Description:  'Returns the string str with all remstr prefixes or suffixes removed. '
        +'If none of the specifiers BOTH, LEADING, or TRAILING is given, BOTH is '
        +'assumed. remstr is optional and, if not specified, spaces are removed.'
    ),

    // Function nr. 193
    (
      Name:         'TRUNCATE';
      Declaration:  '(X,D)';
      Category:     'Numeric Functions';
      Description:  'Returns the number X, truncated to D decimal places. If D is 0, the '
        +'result has no decimal point or fractional part. D can be negative to '
        +'cause D digits left of the decimal point of the value X to become '
        +'zero.'
    ),

    // Function nr. 194
    (
      Name:         'UCASE';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'UCASE() is a synonym for UPPER().'
    ),

    // Function nr. 195
    (
      Name:         'UNCOMPRESS';
      Declaration:  '(string_to_uncompress)';
      Category:     'Encryption Functions';
      Description:  'Uncompresses a string compressed by the COMPRESS() function. If the '
        +'argument is not a compressed value, the result is NULL. This function '
        +'requires MySQL to have been compiled with a compression library such '
        +'as zlib. Otherwise, the return value is always NULL.'
    ),

    // Function nr. 196
    (
      Name:         'UNCOMPRESSED_LENGTH';
      Declaration:  '(compressed_string)';
      Category:     'Encryption Functions';
      Description:  'Returns the length that the compressed string had before being '
        +'compressed.'
    ),

    // Function nr. 197
    (
      Name:         'UNHEX';
      Declaration:  '';
      Category:     'String Functions';
      Description:  'UNHEX(str) Performs the inverse operation of HEX(str). That is, it '
        +'interprets each pair of hexadecimal digits in the argument as a number '
        +'and converts it to the character represented by the number. The '
        +'resulting characters are returned as a binary string.'
    ),

    // Function nr. 198
    (
      Name:         'UNIX_TIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
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

    // Function nr. 199
    (
      Name:         'UPDATEXML';
      Declaration:  '(xml_target, xpath_expr, new_xml)';
      Category:     'String Functions';
      Description:  'This function replaces a single portion of a given fragment of XML '
        +'markup xml_target with a new XML fragment new_xml, and then returns '
        +'the changed XML. The portion of xml_target that is replaced matches an '
        +'XPath expression xpath_expr supplied by the user. If no expression '
        +'matching xpath_expr is found, or if multiple matches are found, the '
        +'function returns the original xml_target XML fragment. All three '
        +'arguments must be strings.'
    ),

    // Function nr. 200
    (
      Name:         'UPPER';
      Declaration:  '(str)';
      Category:     'String Functions';
      Description:  'Returns the string str with all characters changed to uppercase '
        +'according to the current character set mapping. The default is latin1 '
        +'(cp1252 West European).'
    ),

    // Function nr. 201
    (
      Name:         'USER';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'Returns the current MySQL username and hostname as a string in the '
        +'utf8 character set.'
    ),

    // Function nr. 202
    (
      Name:         'UTC_DATE';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'Returns the current UTC date as a value in ''YYYY-MM-DD'' or YYYYMMDD '
        +'format, depending on whether the function is used in a string or '
        +'numeric context.'
    ),

    // Function nr. 203
    (
      Name:         'UTC_TIME';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'Returns the current UTC time as a value in ''HH:MM:SS'' or HHMMSS '
        +'format, depending on whether the function is used in a string or '
        +'numeric context.'
    ),

    // Function nr. 204
    (
      Name:         'UTC_TIMESTAMP';
      Declaration:  '()';
      Category:     'Date and Time Functions';
      Description:  'Returns the current UTC date and time as a value in ''YYYY-MM-DD '
        +'HH:MM:SS'' or YYYYMMDDHHMMSS format, depending on whether the function '
        +'is used in a string or numeric context.'
    ),

    // Function nr. 205
    (
      Name:         'UUID';
      Declaration:  '()';
      Category:     'Miscellaneous Functions';
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

    // Function nr. 206
    (
      Name:         'VALUES';
      Declaration:  '(col_name)';
      Category:     'Miscellaneous Functions';
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

    // Function nr. 207
    (
      Name:         'VERSION';
      Declaration:  '()';
      Category:     'Information Functions';
      Description:  'Returns a string that indicates the MySQL server version. The string '
        +'uses the utf8 character set.'
    ),

    // Function nr. 208
    (
      Name:         'WEEK';
      Declaration:  '(date[,mode])';
      Category:     'Date and Time Functions';
      Description:  'This function returns the week number for date. The two-argument form '
        +'of WEEK() allows you to specify whether the week starts on Sunday or '
        +'Monday and whether the return value should be in the range from 0 to '
        +'53 or from 1 to 53. If the mode argument is omitted, the value of the '
        +'default_week_format system variable is used. See '
        +'http://dev.mysql.com/doc/refman/5.1/en/server-system-variables.html.'
    ),

    // Function nr. 209
    (
      Name:         'WEEKDAY';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the weekday index for date (0 = Monday, 1 = Tuesday, ... 6 = '
        +'Sunday).'
    ),

    // Function nr. 210
    (
      Name:         'WEEKOFYEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the calendar week of the date as a number in the range from 1 '
        +'to 53. WEEKOFYEAR() is a compatibility function that is equivalent to '
        +'WEEK(date,3).'
    ),

    // Function nr. 211
    (
      Name:         'XOR';
      Declaration:  '';
      Category:     'Logical operators';
      Description:  'Logical XOR. Returns NULL if either operand is NULL. For non-NULL '
        +'operands, evaluates to 1 if an odd number of operands is non-zero, '
        +'otherwise 0 is returned.'
    ),

    // Function nr. 212
    (
      Name:         'YEAR';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns the year for date, in the range 1000 to 9999, or 0 for the '
        +'"zero" date.'
    ),

    // Function nr. 213
    (
      Name:         'YEARWEEK';
      Declaration:  '(date)';
      Category:     'Date and Time Functions';
      Description:  'Returns year and week for a date. The mode argument works exactly like '
        +'the mode argument to WEEK(). The year in the result may be different '
        +'from the year in the date argument for the first and the last week of '
        +'the year.'
    ),

    // Function nr. 214
    (
      Name:         '^';
      Declaration:  '';
      Category:     'Bit Functions';
      Description:  'Bitwise XOR:'
    ),

    // Function nr. 215
    (
      Name:         '|';
      Declaration:  '';
      Category:     'Bit Functions';
      Description:  'Bitwise OR:'
    ),

    // Function nr. 216
    (
      Name:         '||';
      Declaration:  '';
      Category:     'Logical operators';
      Description:  'Logical OR. When both operands are non-NULL, the result is 1 if any '
        +'operand is non-zero, and 0 otherwise. With a NULL operand, the result '
        +'is 1 if the other operand is non-zero, and NULL otherwise. If both '
        +'operands are NULL, the result is NULL.'
    ),

    // Function nr. 217
    (
      Name:         '~';
      Declaration:  '';
      Category:     'Bit Functions';
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
