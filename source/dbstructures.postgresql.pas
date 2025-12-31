unit dbstructures.postgresql;

{$mode delphi}{$H+}

interface

uses
  dbstructures;

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

const InvalidOid: POid = 0;

var
  PostgreSQLDatatypes: Array[0..38] of TDBDatatype =
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


end.
