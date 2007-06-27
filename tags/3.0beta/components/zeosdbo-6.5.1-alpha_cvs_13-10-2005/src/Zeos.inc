// Compilation directives for Lazarus
{$IFDEF FPC}
{$DEFINE VER130BELOW}
{$DEFINE VER140BELOW}
{$ENDIF}

// Compilation directives for Delphi 5
{$IFDEF VER130}
{$DEFINE VER130BELOW}
{$DEFINE VER140BELOW}
{$ENDIF}

{$IFDEF VER140}
{$DEFINE VER140BELOW}
{$ENDIF}

{$IFDEF FREEBSD}
  {$DEFINE UNIX}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
{$ENDIF}

// Debug/release compiler options
{$D+}

{$IFOPT D-}
{$DEFINE BINARY_RELEASE}
{$ENDIF}

{$IFDEF BINARY_RELEASE}

// Settings for Release mode
{$C-}
{$I-}
{$R-}
{$L-}
{$Q-}
{$IFNDEF FPC}
{$O+}
{$W-}
{$ENDIF}

{$ELSE}

// Settings for Debug mode
{$C+}
{$I+}
{$R+}
{$L+}
{$Q+}
{$IFNDEF FPC}
{$O-}
{$W+}
{$ENDIF}

{$ENDIF}

// Disables checking code.
// Use Range checking option to turn on/off optimization
{$IFOPT R-}
{$DEFINE DISABLE_CHECKING}
{$ENDIF}


// Enables MySQL support in TZConnection/TZDataset
{$DEFINE ENABLE_MYSQL}

// Enables PostgreSQL support in TZConnection/TZDataset
{$DEFINE ENABLE_POSTGRESQL}

// Enables Sybase/MSSQL support in TZConnection/TZDataset
{$DEFINE ENABLE_DBLIB}

// Enables ADO support in TZConnection/TZDataset
{$IFNDEF FPC}
{$DEFINE ENABLE_ADO}
{$ENDIF}

// Enables Interbase/Firebird support in TZConnection/TZDataset
{$DEFINE ENABLE_INTERBASE}

// Enables SQLite support in TZConnection/TZDataset
{$DEFINE ENABLE_SQLITE}

// Enables Oracle support in TZConnection/TZDataset
{$DEFINE ENABLE_ORACLE}

// Enables ASA support in TZConnection/TZDataset
{$DEFINE ENABLE_ASA}

// Supported language. Now available languages:
// ENGLISH, GERMAN, PORTUGUESE, DUTCH, SPANISH
{$DEFINE ENGLISH}

// Prevents loading default libmysql.dll
{$DEFINE MYSQL_STRICT_DLL_LOADING}

// Prevents loading default libpq.dll
{$DEFINE POSTGRESQL_STRICT_DLL_LOADING}

//Allows to see SQL exceptions as strings 
{$DEFINE INTERBASE_EXTENDED_MESSAGES}

// Loads libcrypt.so before Firebird client library.
// It fixes error "Undefined symbol: crypt".
{$DEFINE INTERBASE_CRYPT}

// Excludes old ZeosDBO from the performance tests
{$DEFINE EXCLUDE_OLD_ZEOS_TEST}

// Excludes DBExpress from the performance tests
{$DEFINE EXCLUDE_DBX_TEST}

// Excludes IBX from the performance tests
{$DEFINE EXCLUDE_IBX_TEST}

// Excludes BDE from the performance tests
{$DEFINE EXCLUDE_BDE_TEST}

// Registers property editors for the components.
{$DEFINE WITH_PROPERTY_EDITOR}

// Turn on IProviderSupport interface
{$IFNDEF FPC}
{$DEFINE WITH_IPROVIDER}
{$ENDIF}

// Turn on multithreading
{$DEFINE MULTI_THREADED}

// In Version 6.1.5 there are several bugs with the TZSQLMetadata-Component
// For Version 6.5.0 you should uncomment the following line
{$DEFINE USE_METADATA}

// A large database may have many tables, colums and/or procedures!!!
// Therefore there is the ability to show a warning
// before retrieving a list of these database objects
// {$DEFINE SHOW_WARNING}

// Use libfbclient.so under linux
// and not libfbembed.so when Firebird 1.5.2 is used under linux
{$DEFINE USELIBFBCLIENTSO}

// Use SynEdit for the SQL Editors.
// Uncomment it when you will use SynEdit instead of a MemoEdit.
// The SynEdit component must be installed on your system.
// You can get SynEdit at http://synedit.sourceforge.net/
//{$DEFINE USE_SYNEDIT}

// Compile test applications with a graphical user interface (GUI)
// Comment this definition if you want to compile console test applications
//{$DEFINE TESTGUI}
