unit dbstructures.postgresql;

interface

uses
  dbstructures, StrUtils;

type
  // PostgreSQL structures
  TPQConnectStatus = (CONNECTION_OK, CONNECTION_BAD, CONNECTION_STARTED, CONNECTION_MADE, CONNECTION_AWAITING_RESPONSE, CONNECTION_AUTH_OK, CONNECTION_SETENV, CONNECTION_SSL_STARTUP, CONNECTION_NEEDED);
  PPGconn = Pointer;
  PPGresult = Pointer;
  POid = Cardinal; // Object ID is a fundamental type in Postgres.
  TPostgreSQLLib = class(TDbLib)
    PQconnectdb: function(const ConnInfo: PAnsiChar): PPGconn cdecl;
    PQerrorMessage: function(const Handle: PPGconn): PAnsiChar cdecl;
    PQresultErrorMessage: function(const Result: PPGresult): PAnsiChar cdecl;
    PQresultErrorField: function(const Result: PPGresult; fieldcode: Integer): PAnsiChar;
    PQfinish: procedure(const Handle: PPGconn);
    PQstatus: function(const Handle: PPGconn): TPQConnectStatus cdecl;
    PQsendQuery: function(const Handle: PPGconn; command: PAnsiChar): Integer cdecl;
    PQgetResult: function(const Handle: PPGconn): PPGresult cdecl;
    PQbackendPID: function(const Handle: PPGconn): Integer cdecl;
    PQcmdTuples: function(Result: PPGresult): PAnsiChar; cdecl;
    PQntuples: function(Result: PPGresult): Integer; cdecl;
    PQclear: procedure(Result: PPGresult); cdecl;
    PQnfields: function(Result: PPGresult): Integer; cdecl;
    PQfname: function(const Result: PPGresult; column_number: Integer): PAnsiChar; cdecl;
    PQftype: function(const Result: PPGresult; column_number: Integer): POid; cdecl;
    PQftable: function(const Result: PPGresult; column_number: Integer): POid; cdecl;
    PQgetvalue: function(const Result: PPGresult; row_number: Integer; column_number: Integer): PAnsiChar; cdecl;
    PQgetlength: function(const Result: PPGresult; row_number: Integer; column_number: Integer): Integer; cdecl;
    PQgetisnull: function(const Result: PPGresult; row_number: Integer; column_number: Integer): Integer; cdecl;
    PQlibVersion: function(): Integer; cdecl;
    protected
      procedure AssignProcedures; override;
  end;

  TPostgreSQLProvider = class(TSqlProvider)
    public
      function GetSql(AId: TQueryId): string; override;
  end;

const InvalidOid: POid = 0;

var
  PostgreSQLDatatypes: Array[0..39] of TDBDatatype =
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
      Index:           dbdtSmallint;
      NativeTypes:     '21';
      Name:            'SMALLINT';
      Names:           'smallint|int2';
      Description:     'Small-range integer. Range: -32768 to +32767. Storage Size: 2 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^\d{1,5}$';
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtInt;
      // 26 = oid, 28 = xid
      NativeTypes:     '23|26|28';
      Name:            'INTEGER';
      Names:           'integer|int4|int|oid|xid';
      Description:     'Typical choice for integer. Range: -2147483648 to +2147483647. Storage Size: 4 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^\d{1,10}$';
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBigint;
      NativeTypes:     '20';
      Name:            'BIGINT';
      Names:           'bigint|int8';
      Description:     'Large-range integer. Range: -9223372036854775808 to 9223372036854775807. Storage Size: 8 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^\d{1,19}$';
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtSerial;
      Name:            'SERIAL';
      Names:           'serial|serial4';
      Description:     'Autoincrementing integer. Range: 1 to 2147483647. Storage Size: 4 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBigSerial;
      Name:            'BIGSERIAL';
      Names:           'bigserial|serial8';
      Description:     'Large autoincrementing integer. Range: 1 to 9223372036854775807. Storage Size: 8 Bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtVarBit;
      NativeTypes:     '1562';
      Name:            'BIT VARYING';
      Names:           'bit varying|varbit';
      Description:     'Variable-length bit string.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBit;
      NativeTypes:     '1560';
      Name:            'BIT';
      Names:           'bit';
      Description:     'Fixed-length bit string.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtNumeric;
      NativeTypes:     '1700';
      Name:            'NUMERIC';
      Names:           'numeric|float8|decimal';
      Description:     'User-specified precision, exact. Range: no limit. Storage Size: variable.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtReal;
      NativeTypes:     '700';
      Name:            'REAL';
      Names:           'real|float4';
      Description:     'Variable-precision, inexact. Range: 6 decimal digits precision. Storage Size: 4 Bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDoublePrecision;
      NativeTypes:     '701|1700';
      Name:            'DOUBLE PRECISION';
      Names:           'double precision|float8';
      Description:     'Variable-precision, inexact. Range: 15 decimal digits precision. Storage Size: 8 Bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtChar;
      NativeTypes:     '18|1042';
      Name:            'CHAR';
      Names:           'CHARACTER';
      Description:     'Fixed-length, blank padded.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtVarchar;
      NativeTypes:     '18|19|24|1043|1043';
      Name:            'VARCHAR';
      Names:           'char|bpchar|varchar|name|enum|character varying';
      Description:     'Variable-length with limit.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtText;
      NativeTypes:     '25|22|30|143|629|651|719|791|1000|1028|1040|1041|1115|1182|1183|1185|1187|1231|1263|1270|1561|1563|2201|2207|2211|2949|2951|3643|3644|3645|3735|3770';
      Name:            'TEXT';
      Names:           'text|int2vector|oidvector|bool';
      Description:     'Variable unlimited length.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtCiText;
      NativeTypes:     '?';
      Name:            'CITEXT';
      Names:           'citext';
      Description:     'A case-insensitive character string type. Essentially, it internally calls lower when comparing values. Otherwise, it behaves almost exactly like text.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtCidr;
      NativeTypes:     '650';
      Name:            'CIDR';
      Names:           'cidr';
      Description:     'IPv4 and IPv6 networks. Storage size: 7 or 19 bytes';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtInet;
      NativeTypes:     '869';
      Name:            'INET';
      Names:           'inet';
      Description:     'IPv4 and IPv6 hosts and networks. Storage size: 7 or 19 bytes';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtMacaddr;
      NativeTypes:     '829';
      Name:            'MACADDR';
      Names:           'macaddr';
      Description:     'MAC addresses. Storage size: 6 bytes';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtMoney;
      NativeTypes:     '790';
      Name:            'MONEY';
      Description:     'Currency amount. Range: -92233720368547758.08 to +92233720368547758.07. Storage Size: 8 Bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtDate;
      NativeTypes:     '1082';
      Name:            'DATE';
      Description:     'Calendar date (year, month, day).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTime;
      NativeTypes:     '1083';
      Name:            'TIME';
      Description:     'Time of day.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime;
      NativeTypes:     '1082|1114|702';
      Name:            'TIMESTAMP';
      Names:           'timestamp|datetime|abstime|timestamp without time zone';
      Description:     'Date and time without timezone, e.g. "2020-06-27 16:24:41".';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime2;
      NativeTypes:     '1184';
      Name:            'TIMESTAMPTZ';
      Names:           'timestamptz|timestamp with time zone';
      Description:     'Date and time with time zone, e.g. "2020-06-27 16:24:41+02".';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDate;
      NativeTypes:     '1082';
      Name:            'DATE';
      Description:     'Calendar date (year, month, day).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtInterval;
      NativeTypes:     '1186';
      Name:            'INTERVAL';
      Description:     'time interval	from -178000000 years to 178000000 years';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtBlob;
      NativeTypes:     '17';
      Name:            'BYTEA';
      Description:     'Binary data ("byte array").';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtPoint;
      NativeTypes:     '600';
      Name:            'POINT';
      Description:     'Point on a plane (x,y). Storage size: 16 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtLinestring;
      NativeTypes:     '628';
      Name:            'LINE';
      Description:     'Infinite line ((x1,y1),(x2,y2)). Storage size: 32 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtLineSegment;
      NativeTypes:     '601';
      Name:            'LSEG';
      Description:     'Finite line segment ((x1,y1),(x2,y2)). Storage size: 32 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtBox;
      NativeTypes:     '603';
      Name:            'BOX';
      Description:     'Rectangular box ((x1,y1),(x2,y2)). Storage size: 32 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtPath;
      NativeTypes:     '602';
      Name:            'PATH';
      Description:     'Closed path (similar to polygon) ((x1,y1),...). Storage size: 16+16n bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtPolygon;
      NativeTypes:     '604';
      Name:            'POLYGON';
      Description:     'Closed path (similar to polygon) ((x1,y1),...). Storage size: 40+16n bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtCircle;
      NativeTypes:     '718';
      Name:            'CIRCLE';
      Description:     'Circle <(x,y),r> (center point and radius). Storage size: 24 bytes.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcSpatial;
    ),
    (
      Index:           dbdtBool;
      NativeTypes:     '16';
      Name:            'BOOLEAN';
      Names:           'boolean|bool';
      Description:     'State of true or false. Storage size: 1 byte.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^(true|false)$';
      Category:        dtcOther;
    ),
    (
      Index:           dbdtRegClass;
      NativeTypes:     '2205';
      Name:            'REGCLASS';
      Names:           'regclass';
      Description:     'Relation name';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtRegProc;
      NativeTypes:     '24';
      Name:            'REGPROC';
      Names:           'regproc|regprocedure';
      Description:     'Function name';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtEnum;
      NativeTypes:     'e';
      Name:            'ENUM';
      Names:           '';
      Description:     'A list of quoted labels, each of which must be less than NAMEDATALEN bytes long (64 bytes in a standard PostgreSQL build)';
      HasLength:       True; // Enables the Length/set field in table editor
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtJson;
      NativeTypes:     '114';
      Name:            'JSON';
      Names:           'json';
      Description:     'JavaScript Object Notation data';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtJsonB;
      NativeTypes:     '3802';
      Name:            'JSONB';
      Names:           'jsonb';
      Description:     'JavaScript Object Notation data in a binary form';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtUniqueidentifier;
      NativeTypes:     '2950';
      Name:            'UUID';
      Names:           'uuid';
      Description:     'The data type uuid stores Universally Unique Identifiers (UUID) as defined by RFC 4122, ISO/IEC 9834-8:2005, and related standards.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      ValueMustMatch:  '^\{?[a-f0-9]{8}-?[a-f0-9]{4}-?[a-f0-9]{4}-?[a-f0-9]{4}-?[a-f0-9]{12}\}?$';
      Category:        dtcText;
    )
  );

implementation

procedure TPostgreSQLLib.AssignProcedures;
begin
  AssignProc(@PQconnectdb, 'PQconnectdb');
  AssignProc(@PQerrorMessage, 'PQerrorMessage');
  AssignProc(@PQresultErrorMessage, 'PQresultErrorMessage');
  AssignProc(@PQresultErrorField, 'PQresultErrorField');
  AssignProc(@PQfinish, 'PQfinish');
  AssignProc(@PQstatus, 'PQstatus');
  AssignProc(@PQsendQuery, 'PQsendQuery');
  AssignProc(@PQgetResult, 'PQgetResult');
  AssignProc(@PQbackendPID, 'PQbackendPID');
  AssignProc(@PQcmdTuples, 'PQcmdTuples');
  AssignProc(@PQntuples, 'PQntuples');
  AssignProc(@PQclear, 'PQclear');
  AssignProc(@PQnfields, 'PQnfields');
  AssignProc(@PQfname, 'PQfname');
  AssignProc(@PQftype, 'PQftype');
  AssignProc(@PQftable, 'PQftable');
  AssignProc(@PQgetvalue, 'PQgetvalue');
  AssignProc(@PQgetlength, 'PQgetlength');
  AssignProc(@PQgetisnull, 'PQgetisnull');
  AssignProc(@PQlibVersion, 'PQlibVersion');
end;


{ TPostgreSQLProvider }

function TPostgreSQLProvider.GetSql(AId: TQueryId): string;
begin
  case AId of
    qDatabaseDrop: Result := 'DROP SCHEMA %s';
    qEmptyTable: Result := 'DELETE FROM ';
    qRenameTable: Result := 'ALTER TABLE %s RENAME TO %s';
    qRenameView: Result := 'ALTER VIEW %s RENAME TO %s';
    qCurrentUserHost: Result := 'SELECT CURRENT_USER';
    qLikeCompare: Result := '%s ILIKE %s';
    qAddColumn: Result := 'ADD %s';
    qChangeColumn: Result := 'ALTER COLUMN %s %s';
    qRenameColumn: Result := 'RENAME COLUMN %s TO %s';
    qForeignKeyEventAction: Result := 'RESTRICT,CASCADE,SET NULL,NO ACTION,SET DEFAULT';
    qSessionVariables: Result := 'SHOW ALL';
    qGlobalVariables: Result := 'SHOW ALL';
    qISSchemaCol: Result := '%s_schema';
    qUSEQuery: Result := 'SET search_path TO %s';
    qKillQuery: Result := 'SELECT pg_cancel_backend(%d)';
    qKillProcess: Result := 'SELECT pg_cancel_backend(%d)';
    qFuncLength: Result := 'LENGTH';
    qFuncCeil: Result := 'CEIL';
    qFuncLeft: Result := 'SUBSTRING(%s, 1, %d)';
    qFuncNow: Result := 'NOW()';
    qFuncLastAutoIncNumber: Result := 'LASTVAL()';
    qLockedTables: Result := '';
    qDisableForeignKeyChecks: Result := '';
    qEnableForeignKeyChecks: Result := '';
    qForeignKeyDrop: Result := 'DROP CONSTRAINT %s';

    // This uses pg_attribute.attgenerated, which only exists starting in PostgreSQL 12
    qGetTableColumns: Result := IfThen(
      FServerVersion >= 120000,
      'SELECT ' +
      '    n.nspname AS table_schema, ' +
      '    c.relname AS table_name, ' +
      '    a.attname AS column_name, ' +
      '    a.attnum  AS ordinal_position, ' +
      '    pg_catalog.format_type(a.atttypid, a.atttypmod) AS data_type, ' +
      // YES/NO like information_schema.is_nullable
      '    CASE ' +
      '        WHEN a.attnotnull THEN ''NO'' ' +
      '        ELSE ''YES'' ' +
      '    END AS is_nullable, ' +
      // Character maximum length (in characters)
      '    CASE ' +
      '        WHEN (bt.typcategory = ''S'' OR (bt.oid IS NULL AND t.typcategory = ''S'')) ' +
      '             AND a.atttypmod <> -1 ' +
      '        THEN a.atttypmod - 4 ' +
      '        ELSE NULL ' +
      '    END AS character_maximum_length, ' +
      // Numeric precision / scale (NULL for non-numeric)
      '    CASE ' +
      '        WHEN (bt.typcategory IN (''N'',''F'')) OR (bt.oid IS NULL AND t.typcategory IN (''N'',''F'')) ' +
      '        THEN ' +
      '            CASE ' +
      '                WHEN a.atttypmod = -1 THEN NULL ' +
      '                ELSE ((a.atttypmod - 4) >> 16)::integer ' +
      '            END ' +
      '    END AS numeric_precision, ' +
      '    CASE ' +
      '        WHEN (bt.typcategory IN (''N'',''F'')) OR (bt.oid IS NULL AND t.typcategory IN (''N'',''F'')) ' +
      '        THEN ' +
      '            CASE ' +
      '                WHEN a.atttypmod = -1 THEN NULL ' +
      '                ELSE ((a.atttypmod - 4) & 65535)::integer ' +
      '            END ' +
      '    END AS numeric_scale, ' +
      // Datetime precision (for time/timestamp/interval)
      '    CASE ' +
      '        WHEN (bt.typcategory = ''D'' OR (bt.oid IS NULL AND t.typcategory = ''D'')) ' +
      '             AND a.atttypmod <> -1 ' +
      '        THEN a.atttypmod ' +
      '        ELSE NULL ' +
      '    END AS datetime_precision, ' +
      // Character set name: PostgreSQL has one per DB; mimic information_schema
      '    CASE ' +
      '        WHEN (bt.typcategory = ''S'' OR (bt.oid IS NULL AND t.typcategory = ''S'')) ' +
      '        THEN current_database() ' +
      '        ELSE NULL ' +
      '    END AS character_set_name, ' +
      // Collation name for collatable columns
      '    CASE ' +
      '        WHEN (bt.typcategory = ''S'' OR (bt.oid IS NULL AND t.typcategory = ''S'')) ' +
      '        THEN ' +
      '            CASE ' +
      '                WHEN a.attcollation <> t.typcollation ' +
      '                THEN coll.collname ' +
      '                ELSE NULL ' +
      '            END ' +
      '        ELSE NULL ' +
      '    END AS collation_name, ' +
      // Default expression for non-generated columns
      '    CASE ' +
      '        WHEN a.attgenerated = '''' AND a.atthasdef ' +
      '        THEN pg_get_expr(ad.adbin, ad.adrelid) ' +
      '        ELSE NULL ' +
      '    END AS column_default, ' +
      // Generation expression for generated columns
      '    CASE ' +
      '        WHEN a.attgenerated <> '''' AND a.atthasdef ' +
      '        THEN pg_get_expr(ad.adbin, ad.adrelid) ' +
      '        ELSE NULL ' +
      '    END AS generation_expression, ' +
      '    d.description AS column_comment ' +
      'FROM pg_catalog.pg_class     AS c ' +
      'JOIN pg_catalog.pg_namespace AS n  ON n.oid      = c.relnamespace ' +
      'JOIN pg_catalog.pg_attribute AS a  ON a.attrelid = c.oid ' +
      'JOIN pg_catalog.pg_type      AS t  ON t.oid      = a.atttypid ' +
      'LEFT JOIN pg_catalog.pg_type AS bt ON bt.oid     = t.typbasetype ' +
      'LEFT JOIN pg_catalog.pg_attrdef AS ad ' +
      '       ON ad.adrelid = a.attrelid ' +
      '      AND ad.adnum   = a.attnum ' +
      'LEFT JOIN pg_catalog.pg_description AS d ' +
      '       ON d.objoid   = a.attrelid ' +
      '      AND d.objsubid = a.attnum ' +
      'LEFT JOIN pg_catalog.pg_collation AS coll ' +
      '       ON coll.oid   = a.attcollation ' +
      'WHERE n.nspname = %s ' +
      '  AND a.attnum > 0 ' +
      '  AND NOT a.attisdropped ' +
      '  AND c.relname = %s ' +
      'ORDER BY ordinal_position',
      '' // ServerVersion < 12
      );

    qGetCharsets: Result := 'SELECT DISTINCT pg_encoding_to_char(enc) AS "Charset" FROM '+
      '(SELECT conforencoding AS enc FROM pg_catalog.pg_conversion '+
      '  UNION '+
      '  SELECT contoencoding AS enc FROM pg_catalog.pg_conversion) AS x';
    qGetRowCountApprox: Result := 'SELECT reltuples::bigint FROM pg_class'+
      ' LEFT JOIN pg_namespace ON pg_namespace.oid = pg_class.relnamespace'+
      ' WHERE pg_class.relkind=''r'''+
      '   AND pg_namespace.nspname=:EscapedDatabase'+
      '   AND pg_class.relname=:EscapedName';
    qGetSubDataTypes: Result := IfThen(
      FServerVersion >= 90000,
      'SELECT ' +
      '  n.nspname AS enum_schema, ' +
      '  t.typname AS enum_name, ' +
      '  string_agg(e.enumlabel, '','' ORDER BY e.enumsortorder) AS enum_labels ' +
      'FROM pg_type AS t ' +
      'JOIN pg_enum AS e ' +
      '  ON t.oid = e.enumtypid ' +
      'JOIN pg_namespace AS n ' +
      '  ON n.oid = t.typnamespace ' +
      'WHERE t.typtype = ''%s'' ' +
      'GROUP BY n.nspname, t.typname ' +
      'ORDER BY n.nspname, t.typname',
      '' // ServerVersion < 9
      );
    else Result := inherited;
  end;
end;


end.
