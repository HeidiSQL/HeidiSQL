# SQL function lists

The `functions-*.ini` files feed HeidiSQL's SQL function lists: completion proposal, function menus and
editor hints. HeidiSQL loads one file per server type from its resources directory:

| Server type        | File                       |
|--------------------|----------------------------|
| MariaDB            | `functions-mariadb.ini`    |
| MySQL              | `functions-mysql.ini`      |
| PostgreSQL         | `functions-postgresql.ini` |
| Redshift           | `functions-redshift.ini`   |
| MS SQL, Azure SQL  | `functions-mssql.ini`      |
| SQLite             | `functions-sqlite.ini`     |
| Firebird           | `functions-firebird.ini`   |
| Interbase          | `functions-interbase.ini`  |
| Oracle (ODBC)      | `functions-oracle.ini`     |

All files are UTF-8 without BOM, with CRLF line endings. The Delphi checkout (branch `master`) uses the same
files in its `out/` folder.

## File format

```ini
[JSON_TABLE]
declaration=json_doc, context_path COLUMNS (column_list)
category=JSON Functions
since=10.6.0
description=Line 1\nLine 2\n\nURL: https://...
```

- Overloaded functions get numbered sections (`[SUBSTRING1]`, `[SUBSTRING2]`) plus a `name=` key.
- `\n` in `description` becomes a line break.
- `since`: first server version having the function. HeidiSQL hides it on older servers.
- `until`: first server version *without* the function. HeidiSQL hides it on this and newer servers.
- Both are ignored for Interbase (no version reported) and Azure SQL (reports version 12, but has all
  functions). Azure-only MS SQL functions have `since=99`, so they are hidden on boxed SQL Server.

## Sources

`generate-functions.php` creates all files except Redshift and Interbase, which are maintained by hand.

| File       | Function names, descriptions                              | since / until                                          |
|------------|-----------------------------------------------------------|--------------------------------------------------------|
| mariadb    | [mariadb-docs](https://github.com/mariadb-corporation/mariadb-docs) repository (Markdown) | Function registry in the [server sources](https://github.com/MariaDB/server), all release tags |
| mysql      | Help tables of several running servers (5.5 ... 9.x)      | Function registry in the [server sources](https://github.com/mysql/mysql-server), all release tags |
| postgresql | Online documentation of all versions in `--versions`      | First documentation version mentioning a function      |
| sqlite     | Online documentation                                      | Release history, corrected in `supplements/`           |
| mssql      | [sql-docs](https://github.com/MicrosoftDocs/sql-docs) repository (Markdown) | `monikerRange` of each page               |
| firebird   | Online language references of all versions in `--versions` | First reference version mentioning a function         |
| oracle     | Online SQL Language Reference, function pages and text syntax diagrams of the newest version in `--versions` | First reference version listing a function page; documentation 26 means server 23 (26ai reports 23.x) |

The function registry is read from `sql/item_create.cc`, `sql/lex.h` and a few more files at each release
tag, via raw.githubusercontent.com. Per major.minor series only the first and last tag are read, plus tags found
by bisecting where these differ. Keywords count for `since` only, as for example `PASSWORD` stays a keyword after
`PASSWORD()` was removed.

`supplements/functions-<x>.ini` is merged into the generated result. Use it for functions missing in the
sources and for wrong versions. Keys as above; a value `none` clears a generated value; an entry without
`declaration` applies to all overloads of that name; `remove=1` drops a function; `replace=1` replaces all
generated overloads by this entry, keeping their category, description and since where the entry has none (for
wrong declarations). Functions skipped by the oracle target for a missing or broken syntax diagram get these values
from the function page as well, so their supplement entry only needs a declaration.

## One-time setup

- PHP 8.4 or newer, with the `mysqli` and `openssl` extensions.
- Sparse checkouts of the two documentation repositories, by default expected in `3rdparty/` of the
  workspace (next to the Lazarus and Delphi checkouts):

  ```bash
  git clone --depth 1 --filter=blob:none --sparse https://github.com/mariadb-corporation/mariadb-docs
  git -C mariadb-docs sparse-checkout set server/reference/sql-functions server/reference/sql-statements/geometry-constructors server/reference/sql-structure/operators/comparison-operators server/reference/sql-structure/sequences/sequence-functions server/server-usage/storage-engines/spider/spider-functions
  git clone --depth 1 --filter=blob:none --sparse https://github.com/MicrosoftDocs/sql-docs
  git -C sql-docs sparse-checkout set docs/t-sql/functions
  ```

- Running MySQL servers of several versions, for the help texts. More versions give better descriptions of
  removed functions; since/until come from the sources anyway. The newest server should be the newest
  release, as functions only in newer versions get no description otherwise.

## Updating

1. Regenerate everything. This clears the download cache, pulls the documentation checkouts and runs the
   generator for all targets:

   ```bash
   php update-functions.php --mysql-servers=<port>,<port>:<password>,...
   ```

   Ports in ascending server version order. `--only=mariadb,postgresql` limits the targets, see the header of
   `update-functions.php` for all options. Single targets can also be run directly, see the header of
   `generate-functions.php`.

2. Review the changes:

   ```bash
   php check-functions.php diff
   ```

   Lists removed and added functions plus changed since/until values per file, against the last commit.
   Many removed functions usually mean a changed documentation format, which the generator does not parse
   anymore.

3. Check the version filter against real servers, if available:

   ```bash
   php check-functions.php mysql --port=<port> [--pass=...]
   php check-functions.php mysql --port=<port> --file=functions-mariadb.ini
   php check-functions.php postgresql --psql=<path to psql> --port=5432 --pass=...
   ```

   "Hidden by since/until, but on server" lists point to wrong versions. Fix them in `supplements/`.
   Expected exceptions: `MD5` and `SHA1` on MySQL 9.6+, which are no built-in functions anymore, but still
   in the help tables frozen at 9.1. On old servers, "shown, but not in server help" is long, as their help
   tables lack many functions.

4. Build, test in the app, commit.

5. Copy the files to the `out/` folder of the Delphi checkout, or use `--trunk-out=<dir>` in step 1.

## When to update

Before each HeidiSQL release, and at least every three months, as MariaDB and MySQL publish new releases
about quarterly. Additionally after:

| Event                                   | Action                                                              |
|-----------------------------------------|---------------------------------------------------------------------|
| New PostgreSQL major (yearly, autumn)   | Append the version to `--pg-versions`, or update the default in `generate-functions.php` |
| New Firebird major                      | Append the version to `--fb-versions`, or update the default        |
| New Oracle documentation release        | Append the version to `--ora-versions`, or update the default. If its number differs from the version the server reports, extend the mapping in `genOracle()` |
| New MySQL release                       | Nothing for since/until. For descriptions of new functions, add a server of that version to `--mysql-servers`; the help tables of MySQL 9.x are frozen at 9.1, so supplements may be needed |
| New MariaDB, SQLite, SQL Server release | Nothing, just regenerate                                            |

## Known limitations

- The MySQL help tables in the server sources are placeholders before 8.0.11 and frozen since 9.1, and the
  MariaDB help tables are copied between branches. Therefore they are not used for versions.
- Grammar-only functions like `IF()` or `REPLACE()` are keywords in the sources and get no `until`.
- The SQL highlighter of SynEdit has its own, outdated built-in function names. HeidiSQL's editor hints use the
  function lists independently of it. With Lazarus 5, the function names are also added to the highlighter.
- MS SQL `@@` variables and functions without parentheses like `CURRENT_USER` are not listed, as HeidiSQL
  inserts functions with parentheses. Same for Oracle's `SYSDATE`, `USER` etc., and for Oracle pages whose
  syntax diagram does not start with the function name, like `FIRST`/`LAST` (`... KEEP (DENSE_RANK FIRST ...)`).
