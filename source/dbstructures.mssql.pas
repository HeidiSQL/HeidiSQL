unit dbstructures.mssql;

{$mode delphi}{$H+}

interface

uses
  dbstructures;

var

  MSSQLDatatypes: array [0..33] of TDBDatatype =
  (
    (
      Index:           dbdtUnknown;
      NativeTypes:     '99999';
      Name:            'UNKNOWN';
      Description:     'Unknown data type';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index: dbdtTinyint;
      Name:            'TINYINT';
      Description:     'Integer data from 0 through 255.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtSmallint;
      Name:            'SMALLINT';
      Description:     'Integer data from -2^15 (-32,768) through 2^15 - 1 (32,767).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtInt;
      Name:            'INT';
      Description:     'Integer (whole number) data from -2^31 (-2,147,483,648) through 2^31 - 1 (2,147,483,647).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBigint;
      Name:            'BIGINT';
      Description:     'Integer (whole number) data from -2^63 (-9,223,372,036,854,775,808) through 2^63-1 (9,223,372,036,854,775,807).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBit;
      Name:            'BIT';
      Description:     '0 or 1';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtDecimal;
      Name:            'DECIMAL';
      Description:     'Fixed precision and scale numeric data from -10^38 +1 through 10^38 1.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '10,0';
      Category:        dtcReal;
    ),
    (
      Index:           dbdtNumeric;
      Name:            'NUMERIC';
      Description:     'Functionally equivalent to decimal.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '10,0';
      Category:        dtcReal;
    ),
    (
      Index:           dbdtMoney;
      Name:            'MONEY';
      Description:     'Monetary data values from -2^63 (-922,337,203,685,477.5808) through 2^63 - 1 (+922,337,203,685,477.5807), with accuracy to a ten-thousandth of a monetary unit.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtSmallmoney;
      Name:            'SMALLMONEY';
      Description:     'Monetary data values from -214,748.3648 through +214,748.3647, with accuracy to a ten-thousandth of a monetary unit.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtFloat;
      Name:            'FLOAT';
      Description:     'Floating precision number data with the following valid values: -1.79E + 308 through -2.23E - 308, 0 and 2.23E + 308 through 1.79E + 308.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtReal;
      Name:            'REAL';
      Description:     'Floating precision number data with the following valid values: -3.40E + 38 through -1.18E - 38, 0 and 1.18E - 38 through 3.40E + 38.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtTime;
      Name:            'TIME';
      Description:     'The time data type stores time values only, based on a 24-hour clock. '+
        'The time data type has a range of 00:00:00.0000000 through 23:59:59.9999999 with an '+
        'accuracy of 100 nanoseconds. The default value is 00:00:00.0000000 (midnight). The '+
        'time data type supports user-defined fractional second precision, and the storage '+
        'size varies from 3 to 6 bytes, based on the precision specified.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDate;
      Name:            'DATE';
      Description:     'The date data type has a range of January 1, 01 through December 31, '+
        '9999 with an accuracy of 1 day. The default value is January 1, 1900. The storage size '+
        'is 3 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime;
      Name:            'DATETIME';
      Description:     'Date and time data from January 1, 1753, through December 31, 9999, with an accuracy of three-hundredths of a second, or 3.33 milliseconds.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime2;
      Name:            'DATETIME2';
      Description:     'Date and time data from January 1,1 AD through December 31, 9999 AD, with an accuracy of three-hundredths of a second, or 3.33 milliseconds.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzzzzzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetimeOffset;
      Name:            'DATETIMEOFFSET';
      Description:     'Defines a date that is combined with a time of a day that has time zone awareness and is based on a 24-hour clock.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzzzzzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtSmalldatetime;
      Name:            'SMALLDATETIME';
      Description:     'Date and time data from January 1, 1900, through June 6, 2079, with an accuracy of one minute.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTimestamp;
      Name:            'TIMESTAMP';
      Description:     'A database-wide unique number that gets updated every time a row gets updated.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtChar;
      Name:            'CHAR';
      Description:     'Fixed-length non-Unicode character data with a maximum length of 8,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtVarchar;
      Name:            'VARCHAR';
      Description:     'Variable-length non-Unicode data with a maximum of 8,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtText;
      Name:            'TEXT';
      Description:     'Variable-length non-Unicode data with a maximum length of 2^31 - 1 (2,147,483,647) characters.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtNchar;
      Name:            'NCHAR';
      Description:     'Fixed-length Unicode data with a maximum length of 4,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtNvarchar;
      Name:            'NVARCHAR';
      Description:     'Variable-length Unicode data with a maximum length of 4,000 characters. sysname is a system-supplied user-defined data type that is functionally equivalent to nvarchar(128) and is used to reference database object names.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtNtext;
      Name:            'NTEXT';
      Description:     'Variable-length Unicode data with a maximum length of 2^30 - 1 (1,073,741,823) characters.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtBinary;
      Name:            'BINARY';
      Description:     'Fixed-length binary data with a maximum length of 8,000 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtVarbinary;
      Name:            'VARBINARY';
      Description:     'Variable-length binary data with a maximum length of 8,000 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtImage;
      Name:            'IMAGE';
      Description:     'Variable-length binary data with a maximum length of 2^31 - 1 (2,147,483,647) bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtCursor;
      Name:            'CURSOR';
      Description:     'A reference to a cursor.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtSqlvariant;
      Name:            'SQL_VARIANT';
      Description:     'A data type that stores values of various SQL Server-supported data types, except text, ntext, timestamp, and sql_variant.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtTable;
      Name:            'TABLE';
      Description:     'A special data type used to store a result set for later processing .';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtUniqueidentifier;
      Name:            'UNIQUEIDENTIFIER';
      Description:     'A globally unique identifier (GUID).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtHierarchyid;
      Name:            'HIERARCHYID';
      Description:     'Represents a position in a hierarchy.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtXML;
      Name:            'XML';
      Description:     'Lets you store XML documents and fragments.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    )
  );


implementation

end.
