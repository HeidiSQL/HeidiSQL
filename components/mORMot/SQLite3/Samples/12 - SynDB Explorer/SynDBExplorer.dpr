program SynDBExplorer;

(*

  Synopse mORMot framework

  Sample 12 - SynDB explorer
    purpose of this sample is to show SynDB classes at work
    resulting speed is quite amazing, and available features make it useful

  Version 1.15 - July 12, 2011
  - Initial Release, handling OleDB, Oracle/OCI, and SQLite3 databases

  Version 1.16
  - SynDbExplorer now executes selected text statement (very convenient)
  - will try to reconnect to the server in case of error triggerred
  - added advanced Query Builder
  - now accepts a SQLite3 database file as command line parameter
  - fix error ORA-00932 at OCI client level
  - added UTF-8 BOM to CSV or TXT exports
  - now direct-to-file fast export feature (into CSV, TXT, SQLite3,
    Synopse BigTable records or two JSON flavors)
  - now multi tables direct export into SQLite3 DB files (e.g. for support)
  - SQLite3 3.7.12.1 including (beta) private encryption methods

  Version 1.17
  - added Jet / MSAccess direct support (via OleDB provider)
  - now accepts a Jet / MSAccess database file as command line parameter
  - added ODBC providers direct support
  - added log history of SQL requests (in SynDBExplorer.history file)
  - SQLite3 engine updated to revision 3.7.13
  - changed .config file format from binary to JSON (with Base64+Zip if needed)

  first line of uses clause must be  {$I SynDprUses.inc}  to enable FastMM4
  conditional define should contain INCLUDE_FTS3 to handle FTS3/FTS4 in SQLite3 

*)

uses
  {$I SynDprUses.inc}
  Forms,
  SynDBExplorerMain in 'SynDBExplorerMain.pas' {DbExplorerMain},
  SynDBExplorerClasses in 'SynDBExplorerClasses.pas',
  SynDBExplorerFrame in 'SynDBExplorerFrame.pas' {DBExplorerFrame: TFrame},
  SynDBExplorerQueryBuilder in 'SynDBExplorerQueryBuilder.pas' {DBQueryBuilderForm},
  SynDBExplorerExportTables in 'SynDBExplorerExportTables.pas' {DBExportTablesForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDbExplorerMain, DbExplorerMain);
  Application.Run;
end.
