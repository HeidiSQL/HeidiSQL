unit SynDBExplorerClasses;

interface

uses
  SysUtils, Classes,
  SynCommons, SQLite3Commons;

type
  TExpConnectionType = (
    ctOracleDirectOCI, ctOracleOLEDB, ctOracleMSOLEDB, ctMSSQL, ctGenericOLEDB,
    ctSqlite3, ctJet_mdbOLEDB, ctODBC);

  TSQLConnection = class(TSQLRecord)
  private
    fUserName: RawUTF8;
    fIdent: RawUTF8;
    fPassword: RawUTF8;
    fServer: RawUTF8;
    fDataBase: RawUTF8;
    fConnection: TExpConnectionType;
    fForeignKeys: TSQLRawBlob;
  public
    fTableNames: TRawUTF8DynArray;
  published
    property Ident: RawUTF8 read fIdent write fIdent;
    property Connection: TExpConnectionType read fConnection write fConnection;
    property Server: RawUTF8 read fServer write fServer;
    property Database: RawUTF8 read fDataBase write fDataBase;
    property UserName: RawUTF8 read fUserName write fUserName;
    property Password: RawUTF8 read fPassword write fPassword;
    property TableNames: TRawUTF8DynArray read fTableNames write fTableNames;
    property ForeignKeys: TSQLRawBlob read fForeignKeys write fForeignKeys;
  end;

implementation



end.
