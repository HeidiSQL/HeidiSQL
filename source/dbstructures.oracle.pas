unit dbstructures.oracle;


{$mode delphi}{$H+}

interface

uses
  dbstructures;

type
  TOracleProvider = class(TSqlProvider)
    public
      function GetSql(AId: TQueryId): string; override;
  end;

var

  // Oracle data types, names as in ALL_TAB_COLUMNS.DATA_TYPE, with the
  // precision of TIMESTAMP and INTERVAL types removed.
  // See https://docs.oracle.com/en/database/oracle/oracle-database/21/sqlrf/Data-Types.html
  OracleDatatypes: TDBDataTypeArray =
  [
    (
      Index:           dbdtUnknown;
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
      Index:           dbdtNumeric;
      Name:            'NUMBER';
      Names:           'INTEGER|INT|SMALLINT|DECIMAL|NUMERIC|DEC';
      Description:     'NUMBER[(p[, s])]' + sLineBreak +
        'Exact number with precision p (1 to 38) and scale s. Without precision, any value up to 38 digits.';
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
      Description:     'FLOAT[(p)]' + sLineBreak +
        'NUMBER subtype with binary precision p (1 to 126).';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtReal;
      Name:            'BINARY_FLOAT';
      Description:     '32-bit floating point number.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDouble;
      Name:            'BINARY_DOUBLE';
      Description:     '64-bit floating point number.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtChar;
      Name:            'CHAR';
      Description:     'CHAR[(size [BYTE | CHAR])]' + sLineBreak +
        'Fixed-length character data of size bytes or characters, maximum 2000 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '1';
      Category:        dtcText;
    ),
    (
      Index:           dbdtNchar;
      Name:            'NCHAR';
      Description:     'NCHAR[(size)]' + sLineBreak +
        'Fixed-length Unicode character data of size characters.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '1';
      Category:        dtcText;
    ),
    (
      Index:           dbdtVarchar;
      Name:            'VARCHAR2';
      Names:           'VARCHAR';
      Description:     'VARCHAR2(size [BYTE | CHAR])' + sLineBreak +
        'Variable-length character data, maximum 4000 bytes, or 32767 bytes with MAX_STRING_SIZE = EXTENDED.';
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
      Name:            'NVARCHAR2';
      Description:     'NVARCHAR2(size)' + sLineBreak +
        'Variable-length Unicode character data of size characters.';
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
      Name:            'CLOB';
      Description:     'Character large object, in the database character set.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtNtext;
      Name:            'NCLOB';
      Description:     'Character large object, in the national character set.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtLongtext;
      Name:            'LONG';
      Description:     'Deprecated variable-length character data, up to 2 gigabytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtJson;
      Name:            'JSON';
      Description:     'Native JSON data, Oracle 21c+.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtXML;
      Name:            'XMLTYPE';
      Description:     'XML data.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtDatetime;
      Name:            'DATE';
      Description:     'Date and time, to the second, without time zone.';
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
      Description:     'TIMESTAMP[(fractional_seconds_precision)]' + sLineBreak +
        'Date and time with fractional seconds, without time zone.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetimeOffset;
      Name:            'TIMESTAMP WITH TIME ZONE';
      Description:     'TIMESTAMP[(fractional_seconds_precision)] WITH TIME ZONE' + sLineBreak +
        'Timestamp including a time zone region or offset.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime2;
      Name:            'TIMESTAMP WITH LOCAL TIME ZONE';
      Description:     'TIMESTAMP[(fractional_seconds_precision)] WITH LOCAL TIME ZONE' + sLineBreak +
        'Timestamp normalized to the database time zone, shown in the session time zone.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtInterval;
      Name:            'INTERVAL YEAR TO MONTH';
      Description:     'INTERVAL YEAR[(year_precision)] TO MONTH' + sLineBreak +
        'Period of time in years and months.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtInterval;
      Name:            'INTERVAL DAY TO SECOND';
      Description:     'INTERVAL DAY[(day_precision)] TO SECOND[(fractional_seconds_precision)]' + sLineBreak +
        'Period of time in days, hours, minutes and seconds.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtVarbinary;
      Name:            'RAW';
      Description:     'RAW(size)' + sLineBreak +
        'Variable-length binary data, maximum 2000 bytes, or 32767 bytes with MAX_STRING_SIZE = EXTENDED.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '16';
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtLongblob;
      Name:            'LONG RAW';
      Description:     'Deprecated variable-length binary data, up to 2 gigabytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtBlob;
      Name:            'BLOB';
      Description:     'Binary large object.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtBinary;
      Name:            'BFILE';
      Description:     'Locator of a binary file outside the database.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtBool;
      Name:            'BOOLEAN';
      Description:     'TRUE or FALSE, Oracle 23ai+.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtUniqueidentifier;
      Name:            'ROWID';
      Names:           'UROWID';
      Description:     'Physical or logical address of a row.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtGeometry;
      Name:            'SDO_GEOMETRY';
      Description:     'Oracle Spatial geometry object.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    )
  ];


implementation


{ TOracleProvider }

function TOracleProvider.GetSql(AId: TQueryId): string;
begin
  case AId of
    qEmptyTable: Result := 'TRUNCATE TABLE %s';
    qRenameTable: Result := 'ALTER TABLE %s RENAME TO %s';
    // Only in the own schema
    qRenameView: Result := 'RENAME %s TO %s';
    qCurrentUserHost: Result := 'SELECT USER || ''@'' || SYS_CONTEXT(''USERENV'', ''HOST'') FROM dual';
    qLikeCompare: Result := '%s LIKE %s ESCAPE ''\''';
    qAddColumn: Result := 'ADD %s';
    qChangeColumn: Result := 'MODIFY %s';
    qRenameColumn: Result := 'RENAME COLUMN %s TO %s';
    qForeignKeyDrop: Result := 'DROP CONSTRAINT %s';
    // No ON UPDATE in Oracle, and no RESTRICT
    qForeignKeyEventAction: Result := 'NO ACTION,CASCADE,SET NULL';
    qSessionVariables: Result := 'SELECT PARAMETER, VALUE FROM NLS_SESSION_PARAMETERS ORDER BY PARAMETER';
    qUSEQuery: Result := 'ALTER SESSION SET CURRENT_SCHEMA = %s';
    qFuncLength: Result := 'LENGTH';
    qFuncCeil: Result := 'CEIL';
    // SUBSTR also works on CLOB/NCLOB, and returns a CLOB then
    qFuncLeft: Result := 'SUBSTR(%s, 1, %d)';
    qFuncNow: Result := ' TO_CHAR(SYSDATE, ''YYYY-MM-DD HH24:MI:SS'') FROM dual';
    qGetRowCountApprox: Result := 'SELECT NUM_ROWS FROM ALL_TABLES WHERE OWNER=:EscapedDatabase AND TABLE_NAME=:EscapedName';
    // Offset like "+01:00"
    qSetTimezone: Result := 'ALTER SESSION SET TIME_ZONE = %s';
    qAutoInc: Result := 'GENERATED BY DEFAULT AS IDENTITY';
    else Result := inherited;
  end;
end;


end.
