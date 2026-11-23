# ODBC support for Oracle and Snowflake: concept 2026-11

Status: phases 1 and 2 (ODBC base, experimental Oracle network type)
implemented, see "Implementation status" below. Linux packaging done,
macOS packaging done, Snowflake implemented (phase 3, see below), not packaged.
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
- No matching driver installed (empty list): the session manager shows the
  text hint "No ODBC driver found" in the dropdown, its tooltip says where to
  get the driver (`TConnectionParameters.OdbcDriverHint`: Oracle Instant
  Client "Basic" + "ODBC" download page, Snowflake ODBC download page).
  Connecting with an empty driver name raises "No ODBC driver selected" with
  that hint. If connecting fails and the selected driver is not among the
  installed ones (session copied from another computer, driver uninstalled),
  the error gets the hint "The ODBC driver "…" is not installed." plus the
  download hint.

## Packaging

- Windows (checked): nothing to add. `odbc32.dll` is part of Windows. The
  installer copies `extra\ini\functions-*.ini` by wildcard, which includes
  `functions-oracle.ini`. There is no `functions-snowflake.ini` yet; once
  added, the wildcard covers it, as do the Makefile (`extra/ini/*.ini` for
  deb, rpm, tar) and `create-macos-app.sh`. The server icons
  (`res/icons/server-oracle.png`, `server-snowflake.png`) are compiled into
  the exe, no files to ship.
- Linux (done):
  - deb: `libodbc2 | libodbc1` in `Depends` (`deb-control.txt`). `libodbc2`
    since unixODBC 2.3.7 (Debian 12, Ubuntu 22.04+), `libodbc1` before
    (Debian 11, Ubuntu 20.04).
  - rpm: `--depends "libodbc.so.2()(64bit)"` in the Makefile (Fedora/RHEL
    package `unixODBC`).
  - tar packages: no dependency metadata; users install unixODBC themselves.
  - No snap, AppImage or Flatpak definition in this repository (checked
    2026-11-23). A snap would need `libodbc2` in `stage-packages`; strict
    confinement hides `/opt/oracle` and the host's `/etc/odbcinst.ini`, so
    Oracle would most likely not work there anyway.
  - FPC loads `libodbc.so`, which only exists with the `-dev` package.
    `LoadOdbcDriverManager` in `odbcfixed.pas` calls `InitialiseODBC` with
    `libodbc.so.2`, `libodbc.so.1`, `libodbc.2.dylib` and finally the FPC
    default, before odbcconn's own `TODBCEnvironment.Create` (which then only
    increments the reference count). Called in `GetOdbcDriverNames` (missing
    driver manager → empty list) and before `TOdbcConnection` opens its
    handle (missing → error message "Please install unixODBC").
  - Drivers are not packaged: Oracle Instant Client "Basic" + "ODBC"
    packages, `odbc_update_ini.sh` registers the driver in
    `/etc/odbcinst.ini`. Snowflake: official .deb/.rpm.
- macOS (done; tested 2026-11-20 with the notarized CI bundle against 21c XE):
  - `create-macos-app.sh` brew-installs unixODBC and bundles
    `libodbc.2.dylib` (plus `libltdl.7.dylib` as its dependency) and
    `libodbcinst.2.dylib` (linked by `libsqora`) in `Contents/Frameworks`.
    `LoadOdbcDriverManager` loads `libodbc.2.dylib` by its bare name, which
    dyld finds via the executable's rpath. Without the bundled copy, nothing
    is found on Apple Silicon: dyld's fallback paths are `/usr/local/lib` and
    `/usr/lib`, not `/opt/homebrew/lib`.
  - macOS ships iODBC (`/usr/lib/libiodbc.2.dylib`, still loadable on 15).
    Not usable: its `SQLWCHAR` is a 4 byte `wchar_t`, FPC's is a 2 byte
    `WideChar`. Never fall back to it.
  - The hardened runtime enforces library validation: only libraries signed
    with the app's team ID are loaded. User installed drivers (Oracle's
    `libsqora.dylib`) are signed by someone else, so the app is signed with
    the entitlement `com.apple.security.cs.disable-library-validation`.
  - The bundled unixODBC has its config directory compiled in
    (`/opt/homebrew/etc`), which does not exist without Homebrew. unixODBC
    also reads `~/.odbcinst.ini` (verified with 2.3.14, `odbcinst -q -d`), so
    users register the driver there, no sudo needed. `ODBCSYSINI` adds
    another directory.
  - Oracle offers an ODBC package for macOS ARM64 (Instant Client 23.26,
    DMG/PKG, needs Basic or Basic Light). Tested with the DMGs (Basic Light
    + ODBC 23.26.3, `install_ic.sh` copies to `~/Downloads/instantclient_23_26`,
    no sudo) against 21c XE: `libsqora.dylib.23.1` has the rpath
    `@loader_path` for `libclntsh`/`libclntshcore`, and links
    `@rpath/libodbcinst.2.dylib`, which only resolves through the rpath of the
    loading executable, i.e. the bundled copy. Loaded via the bundled
    `libodbc.2.dylib`, `SQLDrivers` lists the driver from `~/.odbcinst.ini`,
    and `SQLDriverConnect` reaches the server (ORA-01017 with a wrong
    password). `odbc_update_ini.sh` is not usable for this setup: it expects
    `<DM home>/etc/odbcinst.ini` and writes `libsqora.so.23.1`. A manual entry
    is enough:
    `[Oracle 23 ODBC driver]` / `Driver = <dir>/libsqora.dylib.23.1`.
  - Snowflake: driver 4.2.0 (`.pkg`, universal, installs to
    `/opt/snowflake/snowflakeodbc/lib/libsfodbc.dylib`, signed by Snowflake)
    has no `simba.snowflake.ini` and no `libodbcinst` dependency anymore. Its
    shipped `sf.odbc.ini` sets `DriverManagerEncoding=UTF-32` for iODBC; with
    unixODBC every wide string would be mis-decoded. Users override it in
    `~/Library/Application Support/snowflake/sf.odbc.ini` (or `SF_ODBC_INI`)
    with `DriverManagerEncoding=UTF-16`, and register the driver in
    `~/.odbcinst.ini`:
    `[SnowflakeDSIIDriver]` / `Driver = /opt/snowflake/snowflakeodbc/lib/libsfodbc.dylib`.
    Tested 2026-11-23, see "macOS test".
  - What a user installs: Instant Client Basic + ODBC, and a driver entry in
    `~/.odbcinst.ini`. No Homebrew, no unixODBC.

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
   `heidisql.po`. Done, see "Implementation status (phase 4)".

Later: generic read-only "ODBC" net type. Grid editing and table editor: done.

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
- Grid editing: see "Grid editing" below.
- Icon: `server-oracle.png` (commit "feat: add Oracle connection icon").

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

Editors (second step):
- Table editor: ALTER and CREATE for Oracle. One `ALTER TABLE` per clause,
  Oracle has no comma separated clause list. Columns: `RENAME COLUMN`,
  `MODIFY (...)` with only the changed parts, compared to the server's column
  (ORA-01442 for NOT NULL on NOT NULL, ORA-01451 for NULL on nullable,
  ORA-22859 for any type on a LOB), `DEFAULT NULL` as there is no DROP
  DEFAULT, `DROP IDENTITY`, `ADD (...)`, `DROP COLUMN`. Comments via
  `COMMENT ON TABLE/COLUMN`. PK and UNIQUE are constraints (`ADD/DROP
  CONSTRAINT`, `DROP PRIMARY KEY`), normal indexes `CREATE/DROP INDEX`.
  Foreign keys without ON UPDATE, ON DELETE only CASCADE or SET NULL.
  `DEFAULT` before `NOT NULL`, `TIMESTAMP(n) WITH TIME ZONE`.
  `VARCHAR2`/`CHAR` lengths keep `CHAR` semantics (`CHAR_USED`), otherwise
  MODIFY would switch to the session's NLS_LENGTH_SEMANTICS (usually BYTE).
- Function based index parts appear as hidden `SYS_NC...$` columns in
  `ALL_IND_COLUMNS`, the expression is in `ALL_IND_EXPRESSIONS` (LONG). Oracle
  creates them itself for DESC parts (`"COL"`) and for any index on a
  `TIMESTAMP WITH TIME ZONE` column (`SYS_EXTRACT_UTC("COL")`). Both are
  mapped back to the column name; other expressions become `(expr)` key parts
  (`TTableKey.IsExpression`), which `CREATE INDEX` accepts in parentheses.
  Such columns cannot be MODIFYed while the index exists (ORA-30556).
- DDL commits implicitly in Oracle, so a failing statement in the middle of the
  editor's batch leaves the earlier ones applied (e.g. `DROP INDEX` done,
  `CREATE INDEX` failed). `ApplyModifications` counts executed statements and
  reloads the editor from the server after such a partial failure (all server
  types). Index types FULLTEXT and SPATIAL are not offered on Oracle (domain
  indexes with own syntax), and index USING/COMMENT are left out.
- Keys are dropped by what they are on the server, and added by their new
  type: `TTableKey.IsConstraint` (set in GetTableKeys, and after saving)
  tells PK/UNIQUE constraints from plain indexes, so a type change KEY <->
  UNIQUE gives `DROP INDEX` + `ADD CONSTRAINT ... UNIQUE` and vice versa.
  A UNIQUE index without constraint is turned into a constraint when edited.
- View editor: body = everything after the view name in GET_DDL, i.e. column
  list, `AS SELECT ...` and `WITH READ ONLY`/`CHECK OPTION`, saved with
  `CREATE OR REPLACE VIEW` (keeps grants). Rename via `RENAME` (own schema
  only). Materialized views: saving blocked.
- Routine and trigger editors: saving blocked with a message. The trigger
  editor would drop the trigger first and then fail on the MySQL syntax; GET_DDL
  of a trigger also appends `ALTER TRIGGER ... ENABLE`.
- Tree: `Drop` schema qualified, `DROP MATERIALIZED VIEW`; dropped tables go
  to the recycle bin. Dropping a schema (= user) is refused with a message.
  View rename in the list via `RENAME`.
- Copy table dialog and SQL export still produce MySQL table options and fail
  with an SQL error (not destructive).

Not done / open:
- Routines: listed and CREATE code shown, but no parameter parsing, so
  "Run routine" is not supported. No packages, sequences, synonyms in the tree.
- `qGetRowCountApprox` uses `ALL_TABLES.NUM_ROWS`, empty before statistics
  are gathered.
- SQL function list: `extra/ini/functions-oracle.ini`, generated from the SQL
  Language Reference (`generate-functions.php oracle`). Functions without
  parentheses (`SYSDATE`, `USER`) are not listed.

## Implementation status (phase 3, Snowflake)

Code:
- Net type `ntSnowflake_ODBC` (last `TNetType` item), group `ngSnowflake`,
  "Snowflake (ODBC, experimental)", port 443, no SSH tunnel, no SSL tab.
  Library dropdown = installed drivers matching `snowflake`. Icon index 209
  (`res/icons/server-snowflake.png`, appended to `ImageListMain` and
  `ImageListSilk` in `main.lfm`).
- `source/dbstructures.snowflake.pas`: `TSnowflakeProvider`,
  `SnowflakeDatatypes` (INFORMATION_SCHEMA names, e.g. `TEXT` → `VARCHAR`).
- `source/dbconnection.pas`: `TSnowflakeConnection` on `TOdbcConnection`.
  Connection string `Server`, `Port` (only if not 443), `Database` (session
  field "Database"), `Warehouse`, `Role` (new session fields
  `SnowflakeWarehouse`, `SnowflakeRole`). Without a current database the
  connection fails with a message, as the tree needs one.
- Tree: schemas from `INFORMATION_SCHEMA.SCHEMATA`, tables and (materialized)
  views from `INFORMATION_SCHEMA.TABLES` (exact `ROW_COUNT`, `BYTES`), columns
  from `INFORMATION_SCHEMA.COLUMNS`. Keys via `SHOW PRIMARY KEYS` /
  `SHOW UNIQUE KEYS IN TABLE`, foreign keys via `SHOW IMPORTED KEYS`
  (INFORMATION_SCHEMA has no `KEY_COLUMN_USAGE`). CREATE code via `GET_DDL`.
  No routines in the tree yet.
- Strings: Snowflake processes backslash escapes in `'...'`, so
  `EscapeString` doubles backslashes. LIKE has no default escape character:
  `qLikeCompare` adds `ESCAPE '\\'`. `$$` strings are known to the batch
  splitter.
- Grid: `TIMESTAMP_TZ`/`_LTZ` via `TO_VARCHAR(col, 'YYYY-MM-DD
  HH24:MI:SS.FFn TZH:TZM')`, `TIMESTAMP_NTZ` and `TIME` via `TO_VARCHAR`
  with fractional seconds. Grid editing: see "Grid editing" below.
- "Use own client time zone": `ALTER SESSION SET TIMEZONE = '<IANA name>'`
  after connect. `GetLocalIanaTimeZone` in `apphelpers.pas`: Windows maps the
  time zone key name via `ucal_getTimeZoneIDForWindowsID` from `icu.dll`
  (Windows 10 1903+, `icuin.dll` before); Linux/macOS use `TZ` or the
  `/etc/localtime` symlink; fallback `Etc/GMT±n` for whole hours.
- Table editor: one `ALTER TABLE` per clause. Columns: `RENAME COLUMN`,
  `ALTER COLUMN ... SET DATA TYPE`, `SET/DROP NOT NULL`, `DROP DEFAULT` /
  `SET DEFAULT` (only sequences), `COMMENT ON COLUMN`; only changed attributes
  are sent. Keys are constraints only (PRIMARY, UNIQUE), no plain indexes.
  `DEFAULT`/`AUTOINCREMENT` before `NOT NULL`, no explicit `NULL`, column
  comments inline.
- View editor: body = everything after the view name, saved with
  `CREATE OR REPLACE [SECURE] [MATERIALIZED] VIEW`, rename via
  `ALTER VIEW ... RENAME TO`. `CREATE OR REPLACE` drops grants unless the body
  contains `COPY GRANTS`.

- The driver describes `USE SCHEMA` and other commands with a status column on
  `SQLPrepare`, but returns no result set after `SQLExecute`. SQLDB then raises
  "Cannot open a non-select statement" after the statement ran.
  `TOdbcConnection.Query` takes that as "executed, no result"
  (`Cursor.FSelectable` is only reset by the execute).
- Driver messages end with a multi-line "error trace:" pointing to its Rust
  sources. `OdbcErrorText` cuts it off.
- Binary literals: `x'...'`. A `TO_BINARY('...', 'HEX')` prefix broke
  `IsHex()` and the binary editor with the "lowercase hex" option.
- A user's personal database (`USER$<name>`) does not allow CREATE TABLE.
- After F2, `EnsureFullRow` copies the row into the update row. `TOdbcQuery`
  reads binary values from there in that state (like `TMySQLQuery`), and
  `Col()` returns binary fields as raw bytes. Before, BINARY cells turned
  into `''` on editing (all ODBC types, also Oracle RAW/BLOB).

Tested 2026-11-21 against the trial account: connect, tree, empty schema,
grid with VARCHAR (70000 characters), NUMBER(20,4) with trailing zeros,
NUMBER(38,0) exact, VARIANT, TIMESTAMP_TZ with offset, TIMESTAMP_LTZ in the
session time zone (Europe/Berlin with the option, -08:00 without), column
changes incl. NOT NULL in the table editor, view editing, renaming tables and
views. Binary display after the `x'...'` change confirmed on macOS 2026-11-23.

## Implementation status (phase 4, packaging and localization)

- Packaging: Linux (deb, rpm) and macOS were done with phase 2, see
  "Packaging". Windows installer checked, nothing missing. No snap,
  AppImage or Flatpak in the repository.
- Missing driver: text hint, tooltip and connect error hints, see
  "Library field".
- Localization: all `_()`/`f_()` literals in `source/*.pas` and the new
  `connections.lfm` captions/hints (Warehouse, Role, connection parameters)
  are in `extra/locale/heidisql.po`, `heidisql.en.mo` rebuilt. Checked with a
  script comparing all literals against the msgids.
- Tested: Windows clean build and start. Not testable locally: the macOS
  bundle (unchanged in this phase). deb/rpm and the driver hints: see below.

### Linux package test (2026-11-23)

Qt6 build of this branch, `make deb-package rpm-package` (12.22), installed
in fresh LXD containers without any ODBC or Oracle packages:

| System | Install | unixODBC pulled in | Runtime |
|---|---|---|---|
| Debian 12 | `apt install ./heidisql_*.deb` ok | `libodbc2` 2.3.11 | ok |
| Ubuntu 22.04 | ok | `libodbc2` 2.3.9 | ok |
| Ubuntu 24.04 | ok | `libodbc2` 2.3.12 | ok |
| Fedora 43 | `dnf install ./heidisql-*.rpm` ok | `unixODBC` 2.3.14 | ok |

- Runtime check: started under Xvfb with
  `heidisql --nettype=19 --host=localhost --port=1521 --user=system
  --password=xx --library=NoSuchDriver`. All four show unixODBC's own error
  "Can't open lib 'NoSuchDriver' : file not found", so `libodbc.so.2` from
  the package was loaded, plus the hint "The ODBC driver ... is not
  installed".
- Without `libodbc2` (removed with `--force-depends` on Ubuntu 24.04): "Can
  not load the ODBC driver manager. Please install unixODBC (tried
  libodbc.so.2, ...)" as intended.
- Debian 11 not tested (no LXD image anymore, and no Qt6 there anyway), so
  the `libodbc1` alternative is untested.
- Not ODBC related, found on the way:
  - `libqt6pas6` is in none of these distributions' repositories (installed
    here from a manually downloaded .deb), so neither package installs on a
    stock system without it. For the test it was repackaged from the host.
  - Fixed: the rpm had no Firebird dependency, the deb has `libfbclient2`.
    Now `libfbclient.so.2()(64bit)`, Fedora package `libfbclient2`
    (Firebird 4). Tested on Fedora 43: fbclient's own network error
    appears.
  - Command line connect with `--nettype` but without `--library` uses the
    MySQL default library (`libmariadb.so.3` for Firebird), not checked
    further.
  - Fixed: the hint said "The ODBC driver ... is not installed" also when
    the driver manager itself was missing. Now only added when
    `OdbcDriverManagerAvailable`, the error already says "Please install
    unixODBC".
  - Fixed: LCL's emulated task dialog (Linux, macOS) showed the "Find some
    help on this error" link and URLs in the text as raw `<a href=...>`
    HTML. `MessageDialog` adds links on Windows only now.
  - Fixed: the search URL ended in `...founduery`, because the
    `WebSearchBaseUrl` presets contain `%query`, but `%q` was replaced
    (also on Windows). Windows retested 2026-11-23, see "macOS test".

### macOS test (2026-11-23)

Notarized CI bundle of 1b9e42e5 (12.22, run 45206980312) against 21c XE
in the LAN, Instant Client 23.26 registered in `~/.odbcinst.ini`:
- Connect, error dialog (no raw `<a href>`, search URL ok) and query
  cancelling work.
- "Use own client time zone": `SESSIONTIMEZONE` is `+01:00`, as intended
  (offset on Oracle, IANA name only on Snowflake).
- Missing driver hint: not testable in the GUI, the library field is a
  dropdown of installed drivers. Covered by the `--library` test on Linux.
- Fixed: grid editing failed on every UPDATE/INSERT/DELETE with "Operation
  cannot be performed on an active dataset". FPC 3.2.4 sets
  `sqCommitEndsPrepared`/`sqRollbackEndsPrepared` for all SQLDB connections
  (3.2.2 has neither), so the commit after the statement unprepared all
  prepared queries of the transaction, including the open grid result.
  `TFixedODBCConnection.Create` removes both options (by name via RTTI, so
  it compiles with 3.2.2). Affects the macOS build (FPC 3.2.4) and probably
  Linux ARM64 (3.2.3), not Windows (3.2.2). Tested with a locally built
  executable in the CI bundle: UPDATE, INSERT and DELETE work. Windows
  retested 2026-11-23 (clean build, FPC 3.2.2): grid UPDATE/INSERT/DELETE
  on Oracle and Snowflake, Snowflake BINARY, error dialog link and search
  URL work.
- Snowflake (driver 4.2.0, set up as in "Packaging", trial account), with
  the fixed executable: connect, tree, umlauts, long VARCHAR, TIMESTAMP_TZ/LTZ
  and grid UPDATE/INSERT/DELETE work. "Use own client time zone" gives
  `TIMEZONE = Europe/Berlin` at SESSION level (`SHOW PARAMETERS`), so the
  `/etc/localtime` symlink detection works on macOS. `NUMBER(38,0)` exact,
  BINARY shown and edited via `x'...'` (also kept when only another column
  of the row is edited), confirming the binary display change.
- Oracle reconnect after a network interruption: no crash, grid editing
  works afterwards.

## Crash pass before release (2026-11-23)

All menus, dialogs and tabs tested with Oracle 21c XE and Snowflake. No crash
except the reconnect below; fixed:
- Reconnect after a network failure crashed: freeing the old
  `TSQLTransaction` rolled back on the dead connection (ORA-03114).
  `TOdbcConnection.FreeHandles` rolls back in try/except and marks the
  transaction inactive (`CloseTrans`).
- Cancelling a query: `CancelQuery` calls `SQLCancel` (not declared by FPC,
  loaded in `OdbcCancel`) on the running statement handle, which `Query`
  publishes via `SetRunningStmt` under a critical section. Oracle answers
  ORA-01013, Snowflake "Operation was cancelled". No helper connection.
- `Query` skips comment-only statements, e.g. `/*!40101 ... */` from the SQL
  export, which Oracle and Snowflake reject as empty statements. The export's
  status SELECT gets `FROM dual` on Oracle.
- Find text: Oracle (`LIKE`, `LOWER`, `REGEXP_LIKE`, BLOB/spatial columns
  skipped) and Snowflake (`TO_VARCHAR() LIKE/ILIKE`, `REGEXP_INSTR`). Routines
  skipped with a note.
- Maintenance and bulk table edit: skipped with a note per object (MySQL
  commands only).
- Generate data on Snowflake: `INSERT ... SELECT` instead of `VALUES` (only
  constants allowed there), `PARSE_JSON` / `TO_OBJECT` / `TO_ARRAY` for
  VARIANT/OBJECT/ARRAY, values for TIMESTAMP_TZ/LTZ (`dbdtDatetimeOffset`,
  before an empty string, also on MSSQL).

Left as is: empty host tabs (variables, status, processes), user manager and
create/alter/drop database disabled or "not supported", virtual columns not
in grid export INSERTs (as on MySQL).

Later: test "Generate data" with all data types on Oracle and Snowflake.
Text values must fit the column length, and generated rows must not violate
foreign keys or unique keys. On Oracle, date strings rely on implicit
conversion via `NLS_DATE_FORMAT`.

## Oracle: slow dictionary queries

Measured 2026-11-23 on 21c XE (LAN, idle server): the key, index and foreign
key queries on `ALL_CONSTRAINTS`/`ALL_INDEXES`/`ALL_CONS_COLUMNS` took 1-3 s
each, together 4.6-6.2 s per newly selected table. Nearly all of it is the hard
parse, as the table name is a literal: the same text again took 0.01-0.9 s.
`DBMS_METADATA.GET_DDL` itself takes about 0.2 s per table (13 s for the first
call on a cold server).

Fix: `ALTER SESSION SET CURSOR_SHARING = FORCE` after connect. Then only the
first table pays the parse, the next ones took 0.02 s for all three queries.
Side effects on user queries: literals become binds, so plans for skewed data
may be worse, and literals in the select list get bind-sized types.
Alternative for later: real bind variables in the internal dictionary
queries (needs `SQLBindParameter` support in `TOdbcConnection.Query`).

Server side, `DBMS_STATS.GATHER_DICTIONARY_STATS` and
`GATHER_FIXED_OBJECTS_STATS` (as SYS) brought `GET_DDL` over 18 tables from
15.5 s to 2.6 s.

## Grid editing

Uses the generic `TDBQuery` code (`SaveModifications`, `DeleteRow`,
`GetWhereClause`), with these ODBC specifics:
- Only tables with a primary key, or a unique key on NOT NULL columns
  (`CheckEditable`). No fallback to matching all columns: Oracle cannot compare
  CLOB/BLOB in WHERE, Snowflake has no `UPDATE ... LIMIT 1`. Oracle ROWID as
  a key for tables without one: later, it needs an extra column in the grid
  SELECT.
- WHERE uses the plain key columns, without `CastAsText` (Snowflake's
  `TO_VARCHAR`).
- Oracle: `ALTER SESSION SET NLS_DATE_FORMAT`, `NLS_TIMESTAMP_FORMAT` and
  `NLS_TIMESTAMP_TZ_FORMAT` to ISO formats after connect, so date strings in
  UPDATE/INSERT convert implicitly. Text longer than 1000 bytes is sent as
  concatenated `TO_CLOB('...')` chunks (string literals hold 4000 bytes).
  Binary via `HEXTORAW('...')`, limited to RAW's 2000 bytes.
- Snowflake: VARIANT/OBJECT/ARRAY via `PARSE_JSON` / `TO_OBJECT` / `TO_ARRAY`.
  INSERT uses `INSERT ... SELECT`, as `VALUES` takes only constants.

## Help page: Oracle setup for users

How to explain it, learned from testing on macOS:

- Lead with the one sentence users miss: HeidiSQL ships the ODBC driver
  manager, but not Oracle's driver. The user installs Oracle's Instant Client
  (free, no Oracle account needed for the macOS downloads) and tells the
  driver manager where it is. Nothing else, no Homebrew, no unixODBC.
- One section per OS with copy-paste commands, not prose. The macOS steps
  are short, but every one of them is a hard stop if skipped.
- Then one section for the session settings, which are the same everywhere.
- End with a troubleshooting table: error text as the user sees it, cause,
  fix. Users search for the error text.
- Mark the whole thing experimental, and list what does not work yet
  (routine editor, copy table), see "Not done / open". Grid editing works
  since 2026-11-23.
- Snowflake needs its own section: macOS setup (driver `.pkg`, `sf.odbc.ini`
  with UTF-16, `~/.odbcinst.ini` entry) is in "Packaging"; Windows uses the
  official `.msi`, which registers the driver.

Draft:

### Install the Oracle ODBC driver

**macOS (Apple Silicon)**

1. Download "Basic Light Package (DMG)" (or "Basic") and "ODBC Package
   (DMG)" from Oracle's "Instant Client for macOS (ARM64)" page. Same version
   for both.
2. Open both DMGs, then in Terminal run the installer from one of them. It
   copies all mounted packages to `~/Downloads/instantclient_23_26`, no admin
   rights needed:
   `sh /Volumes/instantclient-basiclite-macos.arm64-23.26.3.0.0/install_ic.sh`
   Eject the DMGs afterwards. The folder can be moved elsewhere, adjust the
   path in the next step then.
3. Register the driver: create the file `~/.odbcinst.ini` with this content
   (replace `<you>` with your user name, and check the file name of
   `libsqora` in the folder):
   ```
   [Oracle 23 ODBC driver]
   Description = Oracle ODBC driver
   Driver = /Users/<you>/Downloads/instantclient_23_26/libsqora.dylib.23.1
   ```
   Do not use Oracle's `odbc_update_ini.sh`: it writes the Linux file name
   `libsqora.so...` and expects a unixODBC installation directory.
4. Intel Macs: Oracle's last Instant Client for macOS Intel is 19.x; the
   HeidiSQL macOS build is ARM64 only anyway.

**Windows**

1. Download "Basic Package" (or "Basic Light") and "ODBC Package" from
   Oracle's "Instant Client for Microsoft Windows (x64)" page, same version,
   and unzip both into the same folder, e.g. `C:\oracle\instantclient_23_x`.
2. Run `odbc_install.exe` from that folder in a command prompt with admin
   rights. It registers a driver named like "Oracle in instantclient_23_x".
3. If HeidiSQL cannot load the driver, add the folder to `PATH`.
   (Windows steps not tested by me, taken from Oracle's instructions.)

**Linux**

1. HeidiSQL's packages pull in unixODBC (`libodbc2`/`libodbc1`, rpm
   `unixODBC`). Install Oracle's Instant Client "Basic" + "ODBC" packages
   (rpm or zip).
2. Register the driver in `/etc/odbcinst.ini` (system wide, with sudo) or
   `~/.odbcinst.ini` (only for you), same format as on macOS, with
   `Driver = /path/to/libsqora.so.23.1`.
3. With the zip packages, the Oracle libraries must be found by the dynamic
   linker: `sudo sh -c "echo /opt/oracle/instantclient_23_x >
   /etc/ld.so.conf.d/oracle-instantclient.conf" && sudo ldconfig`.
   (Not tested here.)
4. The snap package most likely cannot use Oracle at all, see "Packaging".

### Session settings

- Network type: "Oracle (ODBC, experimental)"
- Hostname / IP and Port: the database server, port usually 1521
- User and Password: database user
- Service name: the service of the database (PDB), e.g. `XEPDB1` for
  Oracle XE 21c, `FREEPDB1` for Oracle Database Free 23ai. Not the SID.
- ODBC driver: pick the installed Oracle driver from the list. Only drivers
  with "oracle" in their name are listed. Empty list: the driver is not
  registered, see above.
- After connecting, the schemas (users) appear as databases in the tree.
- `NLS_LANG` is set to `.AL32UTF8` by HeidiSQL if unset. A user who sets
  it to a non-UTF-8 character set gets broken non-ASCII text (HeidiSQL logs
  a warning).

### Troubleshooting

| Message | Cause | Fix |
|---|---|---|
| "No ODBC driver selected" / empty driver list | Driver not registered | Check `~/.odbcinst.ini` (macOS/Linux) or run `odbc_install.exe` (Windows) |
| "Can not load the ODBC driver manager. Please install unixODBC" | Linux only: unixODBC missing (tar.gz/AppImage builds) | Install `libodbc2` / `unixODBC` |
| ORA-12543 "TNS:destination host unreachable" (macOS, server in the LAN, works from Terminal) | macOS "Local Network" privacy blocks HeidiSQL | System Settings → Privacy & Security → Local Network: enable HeidiSQL (after an update: toggle off and on) |
| ORA-12541 "no listener" | Wrong host or port, listener down, firewall | Check host/port, `lsnrctl status` on the server |
| ORA-12514 "Service ... is not registered with the listener" | Wrong service name | Use the PDB service name, e.g. `XEPDB1`; `lsnrctl services` on the server lists them |
| ORA-01017 "invalid username/password" | Wrong credentials, or connected to the wrong service (user lives in a PDB, not in the CDB) | Check user and service name |
| Driver loads on Windows only after setting PATH | Oracle DLLs not found | Add the Instant Client folder to `PATH` |

## Testing

- Oracle: Oracle Database Free, on Windows or in Docker, plus Instant Client
  ODBC.
- Snowflake: 30-day trial account, cloud only.
- Without a server: MS SQL ODBC and MySQL ODBC cover the `odbcconn` fixes
  (see above). MS Access (ACE driver) would add 32/64-bit and codepage cases.
