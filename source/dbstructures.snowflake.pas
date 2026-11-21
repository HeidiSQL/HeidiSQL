unit dbstructures.snowflake;


{$mode delphi}{$H+}

interface

uses
  dbstructures;

type
  TSnowflakeProvider = class(TSqlProvider)
    public
      function GetSql(AId: TQueryId): string; override;
  end;

var

  // Snowflake data types, names as in INFORMATION_SCHEMA.COLUMNS.DATA_TYPE, plus synonyms.
  // VARCHAR and its synonyms are reported as TEXT, all fixed point numbers as NUMBER.
  // See https://docs.snowflake.com/en/sql-reference/intro-summary-data-types
  SnowflakeDatatypes: TDBDataTypeArray =
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
      Names:           'DECIMAL|NUMERIC|INT|INTEGER|BIGINT|SMALLINT|TINYINT|BYTEINT';
      Description:     'NUMBER[(p[, s])]' + sLineBreak +
        'Exact number with precision p (1 to 38) and scale s (0 to 37). Default NUMBER(38,0). ' +
        'INT, INTEGER, BIGINT etc. are synonyms for NUMBER(38,0).';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDouble;
      Name:            'FLOAT';
      Names:           'FLOAT4|FLOAT8|DOUBLE|DOUBLE PRECISION|REAL';
      Description:     '64-bit floating point number. All floating point synonyms are stored as FLOAT.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDecfloat;
      Name:            'DECFLOAT';
      Description:     'Decimal floating point number with 38 significant digits.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtVarchar;
      Name:            'VARCHAR';
      Names:           'TEXT|STRING|CHAR|CHARACTER|NCHAR|NVARCHAR|NVARCHAR2|CHAR VARYING|NCHAR VARYING';
      Description:     'VARCHAR[(length)]' + sLineBreak +
        'Unicode text, up to 16 MB or 128 MB depending on the account. Without length, the maximum is used. ' +
        'CHAR, TEXT and STRING are synonyms.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtVarbinary;
      Name:            'BINARY';
      Names:           'VARBINARY';
      Description:     'BINARY[(length)]' + sLineBreak +
        'Binary data, up to 8 MB or 64 MB depending on the account.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtBool;
      Name:            'BOOLEAN';
      Description:     'TRUE, FALSE or NULL.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtDate;
      Name:            'DATE';
      Description:     'Date without time.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTime;
      Name:            'TIME';
      Description:     'TIME[(precision)]' + sLineBreak +
        'Time of day, with fractional seconds precision 0 to 9, default 9.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime;
      Name:            'TIMESTAMP_NTZ';
      Names:           'DATETIME|TIMESTAMPNTZ|TIMESTAMP WITHOUT TIME ZONE';
      Description:     'TIMESTAMP_NTZ[(precision)]' + sLineBreak +
        'Date and time without time zone ("wallclock" time).';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime2;
      Name:            'TIMESTAMP_LTZ';
      Names:           'TIMESTAMPLTZ|TIMESTAMP WITH LOCAL TIME ZONE';
      Description:     'TIMESTAMP_LTZ[(precision)]' + sLineBreak +
        'Timestamp stored in UTC, shown in the session time zone.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetimeOffset;
      Name:            'TIMESTAMP_TZ';
      Names:           'TIMESTAMPTZ|TIMESTAMP WITH TIME ZONE';
      Description:     'TIMESTAMP_TZ[(precision)]' + sLineBreak +
        'Timestamp in UTC with a time zone offset.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTimestamp;
      Name:            'TIMESTAMP';
      Description:     'TIMESTAMP[(precision)]' + sLineBreak +
        'Alias for one of the TIMESTAMP_* types, TIMESTAMP_NTZ by default (parameter TIMESTAMP_TYPE_MAPPING).';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtJson;
      Name:            'VARIANT';
      Description:     'Semi-structured value of any type, including OBJECT and ARRAY, up to 128 MB.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtJson;
      Name:            'OBJECT';
      Description:     'Key-value pairs, like a JSON object.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtJson;
      Name:            'ARRAY';
      Description:     'Sparse array of values, like a JSON array.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtGeometry;
      Name:            'GEOGRAPHY';
      Description:     'Points, lines and polygons on the earth''s surface (WGS 84).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtGeometry;
      Name:            'GEOMETRY';
      Description:     'Points, lines and polygons in a planar coordinate system.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtVector;
      Name:            'VECTOR';
      Description:     'VECTOR(INT | FLOAT, dimension)' + sLineBreak +
        'Vector of up to 4096 integers or floats.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      DefLengthSet:    'FLOAT, 256';
      Category:        dtcOther;
    )
  ];


implementation


{ TSnowflakeProvider }

function TSnowflakeProvider.GetSql(AId: TQueryId): string;
begin
  case AId of
    // Schemas are shown as databases
    qDatabaseDrop: Result := 'DROP SCHEMA %s';
    qEmptyTable: Result := 'TRUNCATE TABLE %s';
    qRenameTable: Result := 'ALTER TABLE %s RENAME TO %s';
    qRenameView: Result := 'ALTER VIEW %s RENAME TO %s';
    qCurrentUserHost: Result := 'SELECT CURRENT_USER() || ''@'' || CURRENT_ACCOUNT()';
    // Snowflake has no default escape character for LIKE
    qLikeCompare: Result := '%s LIKE %s ESCAPE ''\\''';
    qAddColumn: Result := 'ADD COLUMN %s';
    qChangeColumn: Result := 'ALTER COLUMN %s %s';
    qRenameColumn: Result := 'RENAME COLUMN %s TO %s';
    qForeignKeyDrop: Result := 'DROP CONSTRAINT %s';
    // Accepted, but not enforced
    qForeignKeyEventAction: Result := 'NO ACTION,RESTRICT,CASCADE,SET NULL,SET DEFAULT';
    qSessionVariables: Result := 'SHOW PARAMETERS IN SESSION';
    qGlobalVariables: Result := 'SHOW PARAMETERS IN ACCOUNT';
    qUSEQuery: Result := 'USE SCHEMA %s';
    qFuncLength: Result := 'LENGTH';
    qFuncCeil: Result := 'CEIL';
    qFuncLeft: Result := 'LEFT(%s, %d)';
    qFuncNow: Result := ' TO_VARCHAR(CURRENT_TIMESTAMP(), ''YYYY-MM-DD HH24:MI:SS'')';
    qGetRowCountApprox: Result := 'SELECT ROW_COUNT FROM INFORMATION_SCHEMA.TABLES'+
      ' WHERE TABLE_SCHEMA=:EscapedDatabase AND TABLE_NAME=:EscapedName';
    qCastAsText: Result := 'TO_VARCHAR(%s)';
    // Session time zone is set via the TIMEZONE connection parameter, with an IANA name. See TSnowflakeConnection.
    qAutoInc: Result := 'AUTOINCREMENT';
    else Result := inherited;
  end;
end;


end.
