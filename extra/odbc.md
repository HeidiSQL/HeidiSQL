# ODBC support for Oracle and Snowflake: concept 2026-11

Status: phases 1 and 2 (ODBC base, experimental Oracle network type)
implemented, see "Implementation status" below. Snowflake and packaging open.
Motivation: GitHub issue #1091
(Snowflake) and the recurring Oracle requests. Neither DBMS offers a C client
library that is practical to ship, but both have free, official ODBC drivers.

## Basis: SQLDB `TODBCConnection` (FPC 3.2.2)

The lazarus branch already uses SQLDB for MS SQL and InterBase/Firebird, so an
ODBC connection fits the existing architecture. `odbcconn` as shipped is not
usable as-is, though. Verified empirically against MS SQL 16 (ODBC Driver 17)
and MySQL 8 (MySQL ODBC 8.3 Unicode), with `DefaultSystemCodePage=CP_UTF8`
as in HeidiSQL:

| Problem in `odbcconn` | Effect |
|---|---|
| `AddFieldDefs`: local `FieldSize` is a `word`, ColumnSize gets truncated | `nvarchar(max)`/`varchar(max)` (ColumnSize 0) and Snowflake `VARCHAR` (ColumnSize 16777216 → 0) arrive as **empty strings**, silently |
| Memo/WideMemo loaded with `SQL_C_BINARY` | raw bytes in server/driver encoding, interpreted as UTF-16 → garbage for `varchar(max)`, `text` and all MySQL long texts |
| `SQL_DECIMAL`/`SQL_NUMERIC` → `ftFloat` (or `ftCurrency`) | precision loss, e.g. `decimal(38,0)` → `1,23456789012346E37`. Snowflake stores every integer as `NUMBER(38,0)` |
| `LoadField` does not support `ftFmtBCD` at all | no exact decimal type without an override |
| Column names via ANSI `SQLDescribeCol` | non-ANSI identifiers may break (not tested) |
| Fetching via `SQLGetData` only, no `SQLBindCol` | slow on large result sets |

FPC main still has the `word` field size and the `ftFloat` mapping.

### Fix: subclass with three overrides (~115 lines, tested)

`AddFieldDefs`, `LoadField` and `LoadBlobIntoBuffer` are protected and
virtual. A cracker class `TODBCCursorAccess = class(TODBCCursor)` exposes the
protected `FSTMTHandle`. No FPC patch needed.

- `AddFieldDefs` (~40 lines): call inherited, then re-run `SQLDescribeCol` per
  column to get the real `SQLULEN` ColumnSize.
  - `SQL_CHAR/VARCHAR/WCHAR/WVARCHAR` with ColumnSize 0 or > `MaxSmallint`,
    and `SQL_LONGVARCHAR` → `ftWideMemo`.
  - Other `SQL_VARCHAR`/`SQL_CHAR` → `ftWideString`/`ftFixedWideChar`, so
    they are fetched as `SQL_C_WCHAR` instead of in the ANSI/client codepage
    (needed for Oracle, see below).
  - `SQL_BINARY/VARBINARY` with ColumnSize 0 or > `MaxSmallint` → `ftBlob`
    (needed for Snowflake `BINARY`, ColumnSize 64 MB → `word` 0 → empty).
  - `SQL_DECIMAL/NUMERIC` → `ftFmtBCD` (Precision = ColumnSize, Size = scale);
    above `MaxFmtBCDFractionSize` (64 digits) → `ftWideString`.
  - Oracle only: `SQL_FLOAT` with ColumnSize 38 (unconstrained `NUMBER`,
    `FLOAT(n)`) → `ftFmtBCD`, precision 38, scale 0.
- `LoadField` (~20 lines): for `ftFmtBCD` fetch `SQL_C_WCHAR` and convert
  with `StrToBCD` using `.` as decimal separator; otherwise inherited. The
  separator is only `.` if the driver is told so (Oracle: `NUM=US`).
- `LoadBlobIntoBuffer` (~40 lines): for `ftWideMemo` fetch in chunks with
  `SQL_C_WCHAR`, dropping the `#0` terminator of each chunk. The driver then
  converts from any server codepage. Otherwise inherited.

Results with the overrides: `(max)` types, `text`/`ntext`, NULL and empty
values, MySQL 20000–70000 character texts, `decimal(38,0)`, `decimal(38,10)`,
`numeric(18,4)` and `money` all exact, Unicode correct.

Remaining issues:
- Trailing zeros are dropped (`decimal(10,2)` 0.10 → `0,1`, `BCDToStr`).
  The display should format with the scale.
- MySQL ODBC 8.3 returns corrupt last digits for `DECIMAL(65,30)` already in
  the raw `SQLGetData`. Driver bug, irrelevant for Snowflake (max 38 digits).
- The truncation branch for ColumnSize > 65535 with a plain `SQL_VARCHAR` was
  not triggered by either test driver (both report long texts as
  LONGVARCHAR). Only Snowflake will really exercise it.
- `TSqlSrvQuery.Col` outputs `dtcReal` columns via `FloatToStr(AsFloat)`, and
  `dbdtDecimal` is `dtcReal`. An ODBC query class must output `ftFMTBcd` via
  `BCDToStr(AsBCD, FFormatSettings)` instead. The existing SQLDB-based MS SQL
  connection is probably affected too (from reading the code, not tested).

### Oracle results (21c XE, AL32UTF8, Instant Client 23.26 ODBC)

Tested with the overrides above plus two additions, all read-only queries
on `dual`:

- `NLS_LANG` unset → the client uses an ASCII charset: `VARCHAR2` and CLOB
  `'äöü€'` arrive as `aou?`, already in the SQL text sent to the server.
  `SetEnvironmentVariable('NLS_LANG', '.AL32UTF8')` in-process before
  `oci.dll` is loaded fixes it. HeidiSQL should do this if `NLS_LANG` is unset.
- With `NLS_LANG=.AL32UTF8`, plain `SQL_VARCHAR`/`SQL_CHAR` fetched via
  `SQL_C_CHAR` arrive as UTF-8 bytes interpreted as ANSI (`Ã¤Ã¶`). Extra
  mapping in `AddFieldDefs`: `SQL_VARCHAR` → `ftWideString`, `SQL_CHAR` →
  `ftFixedWideChar` (fetched as `SQL_C_WCHAR`). Then all text is correct.
- NCLOB via the original `SQL_C_BINARY` path is garbage; fine with the
  `SQL_C_WCHAR` blob override.
- Decimal text comes with the Windows locale separator (`-1234,5678`) →
  `StrToBCD` range error. Fixed by `NLS_LANG` with territory AMERICA (the
  default for `.AL32UTF8`) or the driver option `NUM=US` in the connection
  string. Use both.
- Unconstrained `NUMBER` and `FLOAT(n)` are reported as `SQL_FLOAT` with
  ColumnSize 38. Mapping that to `ftFmtBCD` (precision 38, scale 0) works for
  `FLOAT(126)` and small values, but the **driver itself rounds** unconstrained
  `NUMBER` to double precision (`123456789012345678901234567890.123456789` →
  `123456789012346000000000000000`), for `SQL_C_CHAR` and `SQL_C_WCHAR`
  alike. Possible workaround for the data grid: HeidiSQL builds the SELECT
  itself and can wrap such columns in `TO_CHAR()`. Ad-hoc queries keep the
  loss.
- `NUMBER(p,s)`, `INTEGER` (= `NUMBER(38)`), `BINARY_DOUBLE`, `DATE`,
  `TIMESTAMP`: correct. Fractional seconds of `TIMESTAMP` are not shown by
  `TDateTimeField.AsString` (display format only, not checked further).

### Snowflake results (trial account, "Snowflake ODBC" driver, Windows)

Password login with `Server=<account>.snowflakecomputing.com;UID=...;PWD=...;
Warehouse=COMPUTE_WH` worked without MFA. Constant expressions only, no
tables.

- `VARCHAR` without length is reported as `SQL_VARCHAR` with ColumnSize
  134217728 (128 MB) → `word` 0 → **empty** without the overrides. This is the
  `word` truncation case from above, now confirmed. With the overrides:
  70000 and 100000+ character values complete, Unicode correct.
- `VARIANT`, `ARRAY`, `OBJECT` are reported as the same 128 MB `SQL_VARCHAR`
  → `ftWideMemo`, content is pretty-printed JSON. Empty without overrides.
- `BINARY` is reported as `SQL_BINARY` with ColumnSize 67108864 → empty
  `ftBytes` without, correct `ftBlob` with the binary mapping.
- `NUMBER(38,0)`, `NUMBER(20,4)`, `NUMBER` and integer literals
  (`NUMBER(2,0)`) → `SQL_DECIMAL` → `ftFmtBCD`, exact. The decimal text uses
  `.` regardless of the Windows locale.
- `FLOAT` → `SQL_DOUBLE`, `BOOLEAN` → `SQL_BIT`, `DATE`, `TIME`: correct.
- `TIMESTAMP_TZ` and `TIMESTAMP_LTZ` are reported as plain
  `SQL_TYPE_TIMESTAMP` and delivered **in UTC, without offset**:
  `12:34:56 +01:00` → `11:34:56`, `08:00 -05:00` → `13:00`.
  `TIMESTAMP_LTZ` is interpreted in the session time zone (default
  America/Los_Angeles: `12:34:56` → `20:34:56` UTC).
  - `SQLColAttribute(SQL_DESC_TYPE_NAME)` returns `TIMESTAMP_NTZ`,
    `TIMESTAMP_TZ`, `TIMESTAMP_LTZ`, so the types can be told apart in
    `AddFieldDefs`. Mapping TZ/LTZ to `ftWideString` (fetched as
    `SQL_C_WCHAR`) gives fractional seconds, but the driver formats the text
    in UTC as well. `QUERY_RESULT_FORMAT=JSON` changes nothing. The offset
    cannot be recovered at the ODBC level.
  - `TIMEZONE=Europe/Berlin` in the connection string is accepted as session
    parameter and fixes the interpretation of LTZ input.
  - Working workaround on SQL level:
    `TO_VARCHAR(col, 'YYYY-MM-DD HH24:MI:SS.FF3 TZH:TZM')` returns
    `2026-11-18 12:34:56.789 +01:00` and `2026-07-01 08:00:00.000 -05:00`.
    HeidiSQL can wrap TZ/LTZ columns in its own data grid SELECT, like
    `TO_CHAR()` for unconstrained Oracle `NUMBER`. Ad-hoc queries show UTC.
  - Suggestion: always map TZ/LTZ to `ftWideString` (shows fractional seconds
    and makes no false claim of a local time). Session time zone: see
    "Use own client time zone" below.
- `TIMESTAMP_NTZ` as `ftDateTime` hides fractional seconds in `AsString`;
  as text it shows `.789`.

### Session option "Use own client time zone"

The session manager checkbox (`asLocalTimeZone`) is generic: after connecting,
`TDBConnection` runs `qSetTimezone` from the SQL provider with a fixed offset
`±hh:mm` from `GetLocalTimeOffset`. It is only enabled for `ngMySQL` in
`connections.pas`. Linking it means: a `qSetTimezone` in the Oracle and
Snowflake providers, and enabling the checkbox for both groups. Tested:

- Oracle: `ALTER SESSION SET TIME_ZONE = '+01:00'` works. OCI already sets
  the session time zone from the client OS (`SESSIONTIMEZONE` was `+01:00`
  without it), so "checked" is the default behavior anyway. To mirror MySQL
  ("unchecked" = server time zone), unchecked could run
  `ALTER SESSION SET TIME_ZONE = DBTIMEZONE`.
  - The driver reports `TIMESTAMP WITH TIME ZONE` and `WITH LOCAL TIME ZONE`
    both with type name `TIMESTAMP WITH LOCAL TIME ZONE` and delivers the
    values converted to the **session** time zone (not UTC):
    `08:00 -05:00` → `14:00` at `+01:00`. The offset is lost; `TO_CHAR(col,
    '... TZH:TZM')` keeps it. The type name check must match `TIME ZONE`,
    not only `TZ`.
- Snowflake: `ALTER SESSION SET TIMEZONE = '+01:00'` is **rejected**
  ("invalid value"), only IANA names are accepted. `'Etc/GMT-1'` works
  (inverted POSIX sign), but only for whole hours and without DST.
  Proper solution: IANA name of the client zone (Linux/macOS:
  `/etc/localtime` symlink; Windows: map the Windows time zone ID, e.g. via
  ICU's `ucal_getTimeZoneIDForWindowsID` or a CLDR table). Unchecked = account
  default (America/Los_Angeles on a trial account).
  - The session time zone only affects the interpretation of LTZ input and
    `TO_VARCHAR` output. Values fetched via ODBC stay UTC either way
    (`CURRENT_TIMESTAMP()` → `18:45` while the text says `19:45 +01:00`).

## Network types

Two entries, not one generic "ODBC": functionality in HeidiSQL depends on the
SQL dialect (SQL provider, `case NetTypeGroup of` everywhere), not on the
transport.

- "Oracle (ODBC, experimental)" → `ngOracle`
- "Snowflake (ODBC, experimental)" → `ngSnowflake`
- Shared base class, e.g. `TOdbcConnection` with the fixed `TODBCConnection`
  subclass. Separate SQL providers per dialect.
- A generic read-only "ODBC" type can be added later on the same base:
  `SQLTables`, `SQLColumns`, `SQLPrimaryKeys` and `SQLStatistics` give tree,
  grid and query support without dialect SQL; DDL editing needs a dialect.

## Library field

No DLL to ship. The driver manager is part of the OS on Windows (`odbc32.dll`,
hardcoded in FPC's `odbcsql.inc`), and the drivers are installed by the user
(no redistribution, also for license reasons).

- The Library field holds the **ODBC driver name**, e.g.
  "Oracle in instantclient_23" or "SnowflakeDSIIDriver".
- `GetLibraries` fills the list by enumerating `SQLDrivers` instead of
  scanning the lib dir, filtered by regex `oracle` / `snowflake`.

## Packaging

- Windows: nothing to add.
- Linux (deb): add `libodbc2` (unixODBC driver manager) to `Depends` in
  `deb-control.txt`; older distributions name it `libodbc1` (to verify).
  - FPC loads `libodbc.so`, which only exists with the `-dev` package. Call
    `InitialiseODBC('libodbc.so.2')` beforehand (override parameter in
    `odbcsql.inc`).
  - Drivers: Snowflake .deb, Oracle Instant Client ODBC, registered in
    `odbcinst.ini`.
- macOS: install unixODBC via brew in `create-macos-app.sh` and bundle
  `libodbc.2.dylib`, like libpq.
  - Snowflake has a macOS driver.
  - Oracle offers no ODBC driver for macOS as far as known (to verify). If so,
    Oracle via ODBC is Windows/Linux only.

## Connecting

- No DSN needed: `SQLDriverConnect` with
  `Driver={...};Server=...;...` (DSN-less) works on all platforms. On
  Linux/macOS the driver must be registered in `odbcinst.ini`, or
  `Driver=/path/to/lib.so` is used. An existing DSN could be allowed
  optionally.
- Port: not ODBC-specific, ODBC has no port. It goes into the driver-specific
  connection string:
  - Oracle: listener 1521, `DBQ=host:1521/SERVICE` (TCPS usually 2484).
  - Snowflake: 443 (HTTPS), host `<account>.snowflakecomputing.com`.

## Databases and schemas in the tree

- Oracle: one connection = one database (service/PDB). All readable schemas
  (schema = user, `ALL_USERS`/`ALL_TABLES`) appear at database level in the
  tree, like PostgreSQL schemas. Another PDB needs another session.
- Snowflake: one connection sees the whole account, three levels
  `DB.SCHEMA.TABLE`, but the HeidiSQL tree has two. Simplest: the "Databases"
  setting selects one Snowflake database, its schemas are shown in the tree
  like PostgreSQL. Alternative: `DB.SCHEMA` nodes.

## SSH tunnel

The HeidiSQL tunnel is a generic local port forward (already used for MS SQL
TCP/IP per `SshSupport`).

- Oracle: works, `DBQ=127.0.0.1:<local port>/SERVICE`.
- Snowflake: disable. TLS certificate validation fails for `localhost`, and
  the driver fetches large result sets directly from cloud storage (S3/Azure)
  and contacts OCSP servers, bypassing the tunnel. Offer the driver's proxy
  settings instead, if needed.

## SSL options

- Snowflake: disable; always TLS, nothing to configure. The "SSL private key"
  field could be reused for key-pair authentication (`PRIV_KEY_FILE`). New
  fields needed anyway: account, warehouse, role, authenticator (password,
  externalbrowser, programmatic access token).
- Oracle: mostly disable. TLS is configured via wallet and `sqlnet.ora` or a
  TCPS connect descriptor; PEM key/cert/CA files cannot be passed directly.
  At most a "TLS (TCPS)" switch plus a wallet directory field.

## Implementation plan

Scope of a first version: connect, browse, query, read-only data grid, like
the experimental InterBase support (`dbstructures.interbase.pas` ~300 lines,
~30 class methods plus `ngInterbase` branches in `dbconnection.pas`).

1. ODBC base: unit with the `TODBCConnection` subclass (overrides above),
   shared connection and query class (`Col` must output `ftFMTBcd` via
   `BCDToStr`), `NLS_LANG`/`InitialiseODBC` handling, net types appended at
   the end of `TNetType` (ordinals are stored in session settings), library
   dropdown from `SQLDrivers`, session manager fields (Snowflake account,
   warehouse, role), SSH/SSL enabling per net type.
2. Oracle provider: data types, schemas as databases, tables/views/columns/
   keys/foreign keys/routines from `ALL_*` views, CREATE code via
   `DBMS_METADATA.GET_DDL`, row limit `FETCH FIRST n ROWS ONLY`, `TO_CHAR()`
   wrapping of unconstrained `NUMBER` and TZ columns in the grid SELECT,
   `NUM=US`, time zone checkbox.
3. Snowflake provider: data types, `INFORMATION_SCHEMA` of the selected
   database, `GET_DDL`, `TO_VARCHAR()` wrapping of TZ/LTZ, time zone checkbox
   with IANA name.
4. Packaging (deb `libodbc2`, macOS unixODBC bundle), new strings in
   `heidisql.po`.

Later: grid editing, table editor, generic read-only "ODBC" net type.

## Implementation status (phases 1 and 2)

Code:
- `source/odbcfixed.pas`: `TFixedODBCConnection` with the three overrides
  (`Float38AsDecimal` switches the Oracle `SQL_FLOAT`/38 mapping),
  `GetOdbcDriverNames` (`SQLDrivers`), `PrepareOracleClientEnvironment`
  (`NLS_LANG`), `OdbcPreparedColumnCount`, `OdbcExecDirect`, `OdbcErrorText`.
- `source/dbstructures.oracle.pas`: `TOracleProvider`, `OracleDatatypes`.
- `source/dbconnection.pas`: `TOdbcConnection` (shared base: connect, query,
  ping, errors), `TOracleConnection` (dictionary queries), `TOdbcQuery`.
- Net type `ntOracle_ODBC` (last `TNetType` item), group `ngOracle`.
  "Databases" session field = service name, schemas from `ALL_USERS`
  (`ORACLE_MAINTAINED = 'N'` plus SYS and SYSTEM) are the tree's databases.
  Library dropdown = installed drivers matching `oracle`, label "ODBC driver:".
  SSH tunnel allowed, SSL tab disabled, "Use own client time zone" enabled.
- Grid editing is blocked with a message (`TDBQuery.CheckEditable`).

Learned while implementing:
- `TCustomSQLQuery.ExecSQL` with `sqoAutoCommit` commits the shared
  transaction, and `TSQLTransaction.Commit` closes all datasets which lack
  `sqoKeepOpenOnCommit`. The ODBC queries set it, plus `PacketRecords = -1`
  (all rows fetched in `Open`). The MS SQL and InterBase connections do not
  set it, which is why they need `IsResultAlive`.
- The MS SQL/InterBase pattern `Prepare; ExecSQL; read FSelectable; Open`
  executes every SELECT twice. With ODBC, `SQLNumResultCols` after
  `SQLPrepare` tells the column count before execution, so a SELECT is
  executed once by `Open`. Falls back to the old pattern if the driver fails.
  Oracle fails there for `INTERVAL` columns (no diagnostic record), so
  `SELECT *` on such a table fails in a query tab; the grid wraps them in
  `TO_CHAR()`.
- Oracle's driver takes `:NEW`/`:OLD` in a `CREATE TRIGGER` body for bind
  variables on `SQLPrepare` + `SQLExecute`: ORA-01036. `SQLExecDirectW`
  works, so statements without result columns run via `OdbcExecDirect`.
  `TSQLQuery.ParseSQL := False` keeps SQLDB from turning `:name` into params.
- `TO_CHAR(ltz_col, '... TZH:TZM')` raises ORA-01821 for `TIMESTAMP WITH
  LOCAL TIME ZONE`: cast to `TIMESTAMP WITH TIME ZONE` first.
- User defined object, VARRAY and nested table columns fail in the driver
  with ORA-01024 ("invalid datatype in OCI call"), already on describe. The
  grid selects them as JSON: `JSON_QUERY(JSON_OBJECT('v' VALUE col RETURNING
  CLOB), '$.v' RETURNING CLOB)` (Oracle 19c+), in a `CASE` that keeps NULL.
  `JSON_ARRAY(col)` would be ambiguous, it does not nest a collection.
  `SYS.ANYDATA` is rejected by JSON generation (ORA-40654). A query tab
  `SELECT *` on such tables still fails. Detected by `DATA_TYPE_OWNER` other
  than SYS, MDSYS, PUBLIC.
- `TO_CHAR(0.5)` returns `.5`; `REGEXP_REPLACE(..., '^(-?)\.', '\10.')` adds
  the leading zero on the server, so it also works where the result has no
  table column attributes. `TO_CHAR(n, 'TM9', 'NLS_NUMERIC_CHARACTERS=''.,''')`
  is locale independent.
- The grid builds a second SELECT in `TDBQuery.EnsureFullRow` (full row
  values, e.g. on right click). It uses `GetGridColumnExpression` as well.
- `TDBQuery.FFormatSettings` is the locale's (`,` on German Windows), but the
  grid expects `.` in `Col()` results. `TOdbcQuery` sets `.`; `TSqlSrvQuery`
  probably outputs `,` for floats on such systems (not tested).
- The trailing zeros of `NUMBER(p,s)` are restored in `Col()` from the field
  scale (`0.10`, not `0.1`).
- SQLDB error messages contain the full connection string, including the
  password ("Could not connect with connection string ..."). `OdbcErrorText`
  keeps only the driver messages.
- `ALL_TAB_COLS.DATA_DEFAULT` is a LONG, which arrives fine as `ftWideMemo`.
  Identity columns show `"ISEQ$$_..."nextval` there.
- `DBMS_METADATA.GET_DDL` output is preceded by a line break, and contains
  storage clauses unless `SEGMENT_ATTRIBUTES` is switched off per session.
- `CreateConnectionString` in odbcconn wraps values in `{}` only for some
  characters, `;` and `}` are not handled: a password with `;` breaks the
  connection string (not fixed).

Not done / open:
- No Oracle icon in the image list yet (`res/icons/server-oracle.png` exists
  untracked, not wired).
- Routines: listed and CREATE code shown, but no parameter parsing, so
  "Run routine" is not supported. No packages, sequences, synonyms in the tree.
- `qGetRowCountApprox` uses `ALL_TABLES.NUM_ROWS`, empty before statistics
  are gathered.
- No `functions-oracle.ini` for the SQL function list.

## Testing

- Oracle: Oracle Database Free, on Windows or in Docker, plus Instant Client
  ODBC.
- Snowflake: 30-day trial account, cloud only.
- Without a server: MS SQL ODBC and MySQL ODBC cover the `odbcconn` fixes
  (see above). MS Access (ACE driver) would add 32/64-bit and codepage cases.
