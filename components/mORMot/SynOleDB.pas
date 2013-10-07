/// fast OleDB direct access classes
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynOleDB;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Several implementation notes about Oracle and OleDB:
  - Oracle OleDB provider by Microsoft do not handle BLOBs. Period. :(
  - Oracle OleDB provider by Oracle will handle only 3/4 BLOBs. :(
    See http://stackoverflow.com/questions/6147274/is-oraoledb-provider-in-net-unreliable-on-clob-fields/6640101#6640101
  - Oracle OleDB provider by Oracle or Microsoft could trigger some ORA-80040e4B
    error when accessing column data with very low dates value (like 0001-01-01)
  - in all cases, that's why we wrote the SynDBOracle unit, for direct OCI
    access - and it is from 2 to 10 times faster than OleDB, with no setup issue
  - or take a look at latest patches from Oracle support, and pray it's fixed ;)
    http://stackoverflow.com/questions/6147274/is-oraoledb-provider-in-net-unreliable-on-clob-fields/6661058#6661058


  Version 1.14
  - first public release, corresponding to SQLite3 Framework 1.14

  Version 1.15
  - SynDB unit extracted from previous SynOleDB.pas
  - several fixes and speed enhancements
  - made the code compatible with Delphi 5
  - TOleDBStatement class now follows the prepared statement pattern introduced
    with its parent TSQLDBStatement
  - now able to retrieve table names and column information from OleDB metadata
    including GetTableNames, GetFields, GetFieldDefinitions and GetForeignKey
    methods - able to use faster direct SQL retrieval (e.g. for Oracle / MS SQL)

  Version 1.16
  - add some reference to http://synopse.info/fossil/tktview?name=213544b2f5
    in case of wrong implementation of multi-thread connection (within the
    THttpServerGeneric mORMot server, for instance)

  Version 1.17
  - fix issue (depending on OleDB Provider) about ordinal binding error
  - fix issue about void string parameter binding
  - fix issue in TOleDBStatement.BindCurrency() method
  - fix retrieval of table and field metadata, for tables without schema
    (e.g. Jet/MSAccess database)
  - added TOleDBJetConnectionProperties kind of connection to direct
    access of Microsoft Jet databases (.mdb files)
  - added corresponding IsJetFile() function
  - added FieldSize optional parameter to TOleDBStatement.ColumnType()
    method (used e.g. by SynDBVCL to provide the expected field size on TDataSet)
  - added TOleDBConnectionProperties.CreateDatabase able to create a database
    from the supplied connection string (used e.g. to initialize .mdb files)
  - code refactoring, especially about error handling and ODBC integration
  - CoInit and CoUninit made public for user convenience, e.g. when using
    custom COM objects in a mORMot multi-thread servers

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  {$ifdef ISDELPHIXE2}System.Win.ComObj,{$else}ComObj,{$endif}
  ActiveX,
  SysUtils,
{$ifndef DELPHI5OROLDER}
  Variants,
{$endif}
  Classes,
  Contnrs,
  SynCommons,
  SynDB;


{ -------------- OleDB interfaces, constants and types
  (OleDB.pas is not provided e.g. in Delphi 7 Personal) }


const
  IID_IAccessor: TGUID = '{0C733A8C-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IRowset: TGUID = '{0C733A7C-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IOpenRowset: TGUID = '{0C733A69-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IDataInitialize: TGUID = '{2206CCB1-19C1-11D1-89E0-00C04FD7A829}';
  IID_IDBInitialize: TGUID = '{0C733A8B-2A1C-11CE-ADE5-00AA0044773D}';
  IID_ICommandText: TGUID = '{0C733A27-2A1C-11CE-ADE5-00AA0044773D}';
  IID_ITransactionLocal: TGUID = '{0C733A5F-2A1C-11CE-ADE5-00AA0044773D}';
  IID_IDBPromptInitialize: TGUID = '{2206CCB0-19C1-11D1-89E0-00C04FD7A829}';
  CLSID_DATALINKS: TGUID = '{2206CDB2-19C1-11D1-89E0-00C04FD7A829}';
  CLSID_MSDAINITIALIZE: TGUID = '{2206CDB0-19C1-11D1-89E0-00C04FD7A829}';
  DB_NULLGUID: TGuid = '{00000000-0000-0000-0000-000000000000}';
  DBGUID_DEFAULT: TGUID = '{C8B521FB-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_TABLES: TGUID = '{C8B52229-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_COLUMNS: TGUID = '{C8B52214-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_INDEXES: TGUID = '{C8B5221E-5CF3-11CE-ADE5-00AA0044773D}';
  DBSCHEMA_FOREIGN_KEYS: TGUID = '{C8B522C4-5CF3-11CE-ADE5-00AA0044773D}';

  DBTYPE_EMPTY = $00000000;
  DBTYPE_NULL = $00000001;
  DBTYPE_I2 = $00000002;
  DBTYPE_I4 = $00000003;
  DBTYPE_R4 = $00000004;
  DBTYPE_R8 = $00000005;
  DBTYPE_CY = $00000006;
  DBTYPE_DATE = $00000007;
  DBTYPE_BSTR = $00000008;
  DBTYPE_IDISPATCH = $00000009;
  DBTYPE_ERROR = $0000000A;
  DBTYPE_BOOL = $0000000B;
  DBTYPE_VARIANT = $0000000C;
  DBTYPE_IUNKNOWN = $0000000D;
  DBTYPE_DECIMAL = $0000000E;
  DBTYPE_UI1 = $00000011;
  DBTYPE_ARRAY = $00002000;
  DBTYPE_BYREF = $00004000;
  DBTYPE_I1 = $00000010;
  DBTYPE_UI2 = $00000012;
  DBTYPE_UI4 = $00000013;
  DBTYPE_I8 = $00000014;
  DBTYPE_UI8 = $00000015;
  DBTYPE_GUID = $00000048;
  DBTYPE_VECTOR = $00001000;
  DBTYPE_RESERVED = $00008000;
  DBTYPE_BYTES = $00000080;
  DBTYPE_STR = $00000081;
  DBTYPE_WSTR = $00000082;
  DBTYPE_NUMERIC = $00000083;
  DBTYPE_UDT = $00000084;
  DBTYPE_DBDATE = $00000085;
  DBTYPE_DBTIME = $00000086;
  DBTYPE_DBTIMESTAMP = $00000087;
  DBTYPE_FILETIME = $00000040;
  DBTYPE_DBFILETIME = $00000089;
  DBTYPE_PROPVARIANT = $0000008A;
  DBTYPE_VARNUMERIC = $0000008B;

  DBPARAMIO_NOTPARAM = $00000000;
  DBPARAMIO_INPUT = $00000001;
  DBPARAMIO_OUTPUT = $00000002;

  DBPART_VALUE = $00000001;
  DBPART_LENGTH = $00000002;
  DBPART_STATUS = $00000004;

  DBMEMOWNER_CLIENTOWNED = $00000000;
  DBMEMOWNER_PROVIDEROWNED = $00000001;

  DBACCESSOR_ROWDATA = $00000002;
  DBACCESSOR_PARAMETERDATA = $00000004;
  DBACCESSOR_OPTIMIZED = $00000008;

  DB_E_CANCELED = HResult($80040E4E);
  DB_E_NOTSUPPORTED = HResult($80040E53);
  DBCOLUMNFLAGS_MAYBENULL = $00000040;
  ISOLATIONLEVEL_READCOMMITTED = $00001000;
  DBPROMPTOPTIONS_PROPERTYSHEET = $2;
  DB_NULL_HCHAPTER = $00;
  DB_S_ENDOFROWSET = $00040EC6;
  XACTTC_SYNC = $00000002;

  MAXBOUND = 65535; { High bound for arrays }

type
  /// indicates whether the data value or some other value, such as a NULL,
  // is to be used as the value of the column or parameter
  // - see http://msdn.microsoft.com/en-us/library/ms722617(VS.85).aspx
  TOleDBStatus = (
    stOK, stBadAccessor, stCanNotConvertValue, stIsNull, stTruncated,
    stSignMismatch, stDataoverFlow, stCanNotCreateValue, stUnavailable,
    stPermissionDenied, stIntegrityViolation, stBadStatus, stDefault);
  /// binding status of a given column
  // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms720969
  TOleDBBindStatus = (
    bsOK, bsBadOrdinal, bsUnsupportedConversion, bsBadBindInfo,
    bsBadStorageFlags, bsNoInterface, bsMultipleStorage);

  PIUnknown = ^IUnknown;
  HACCESSOR = UINT;
  HCHAPTER = UINT;
  HROW = UINT;
  DBPART = UINT;
  DBMEMOWNER = UINT;
  DBPARAMIO = UINT;
  DBPROPSTATUS = UINT;
  DBPROPID = UINT;
  DBPROPOPTIONS = UINT;
  DBCOLUMNFLAGS = UINT;
  DBKIND = UINT;
  DBSTATUS = DWORD;
  DBTYPE = Word;
  PBoid = ^TBoid;
  TBoid = packed record
    rgb_: array[0..15] of Byte;
  end;
  TXactOpt = packed record
    ulTimeout: UINT;
    szDescription: array[0..39] of Shortint;
  end;
  TXactTransInfo = packed record
    uow: PBoid;
    isoLevel: Integer;
    isoFlags: UINT;
    grfTCSupported: UINT;
    grfRMSupported: UINT;
    grfTCSupportedRetaining: UINT;
    grfRMSupportedRetaining: UINT;
  end;
  PErrorInfo = ^TErrorInfo;
  TErrorInfo = packed record
    hrError: HResult;
    dwMinor: UINT;
    clsid: TGUID;
    iid: TGUID;
    dispid: Integer;
  end;
  PDBParams = ^TDBParams;
  TDBParams = packed record
    pData: Pointer;
    cParamSets: UINT;
    HACCESSOR: HACCESSOR;
  end;
  PDBObject = ^TDBObject;
  TDBObject = packed record
    dwFlags: UINT;
    iid: TGUID;
  end;
  PDBBindExt = ^TDBBindExt;
  TDBBindExt = packed record
    pExtension: PByte;
    ulExtension: UINT;
  end;
  PDBBinding = ^TDBBinding;
  TDBBinding = packed record
    iOrdinal: UINT;
    obValue: UINT;
    obLength: UINT;
    obStatus: UINT;
    pTypeInfo: ITypeInfo;
    pObject: PDBObject;
    pBindExt: PDBBindExt;
    dwPart: DBPART;
    dwMemOwner: DBMEMOWNER;
    eParamIO: DBPARAMIO;
    cbMaxLen: UINT;
    dwFlags: UINT;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
  end;
  PDBBindingArray = ^TDBBindingArray;
  TDBBindingArray = array[0..MAXBOUND] of TDBBinding;
  TDBBindingDynArray = array of TDBBinding;
  DBIDGUID = record
    case Integer of
      0: (guid: TGUID);
      1: (pguid: ^TGUID);
  end;
  DBIDNAME = record
    case Integer of
      0: (pwszName: PWideChar);
      1: (ulPropid: UINT);
  end;
  DBID = packed record
    uGuid: DBIDGUID;
    eKind: DBKIND;
    uName: DBIDNAME;
  end;
  PDBIDArray = ^TDBIDArray;
  TDBIDArray = array[0..MAXBOUND] of DBID;
  PDBColumnInfo = ^TDBColumnInfo;
  TDBColumnInfo = packed record
    pwszName: PWideChar;
    pTypeInfo: ITypeInfo;
    iOrdinal: UINT;
    dwFlags: DBCOLUMNFLAGS;
    ulColumnSize: UINT;
    wType: DBTYPE;
    bPrecision: Byte;
    bScale: Byte;
    columnid: DBID;
  end;
  DBSOURCETYPE = DWORD;
  PDBSOURCETYPE = ^DBSOURCETYPE;
  TDBProp = packed record
    dwPropertyID: DBPROPID;
    dwOptions: DBPROPOPTIONS;
    dwStatus: DBPROPSTATUS;
    colid: DBID;
    vValue: OleVariant;
  end;
  PDBPropArray = ^TDBPropArray;
  TDBPropArray = array[0..MAXBOUND] of TDBProp;
  TDBPropSet = packed record
    rgProperties: PDBPropArray;
    cProperties: UINT;
    guidPropertySet: TGUID;
  end;
  PDBPropSet = ^TDBPropSet;
  TDBSchemaRec = record
    SchemaGuid: TGuid;
    SupportedRestrictions: Integer;
  end;

  /// initialize and uninitialize OleDB data source objects and enumerators
  IDBInitialize = interface(IUnknown)
    ['{0C733A8B-2A1C-11CE-ADE5-00AA0044773D}']
    function Initialize: HResult; stdcall;
    function Uninitialize: HResult; stdcall;
  end;
  /// create an OleDB data source object using a connection string
  IDataInitialize = interface(IUnknown)
    ['{2206CCB1-19C1-11D1-89E0-00C04FD7A829}']
    function GetDataSource(const pUnkOuter: IUnknown; dwClsCtx: DWORD;
      pwszInitializationString: POleStr; const riid: TIID;
      var DataSource: IUnknown): HResult; stdcall;
    function GetInitializationString(const DataSource: IUnknown;
      fIncludePassword: Boolean; out pwszInitString: POleStr): HResult; stdcall;
    function CreateDBInstance(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; dwClsCtx: DWORD; pwszReserved: POleStr;
      riid: TIID; var DataSource: IUnknown): HResult; stdcall;
    function CreateDBInstanceEx(const clsidProvider: TGUID;
      const pUnkOuter: IUnknown; dwClsCtx: DWORD; pwszReserved: POleStr;
      pServerInfo: PCoServerInfo; cmq: ULONG; rgmqResults: PMultiQI): HResult; stdcall;
    function LoadStringFromStorage(pwszFileName: POleStr;
      out pwszInitializationString: POleStr): HResult; stdcall;
    function WriteStringToStorage(pwszFileName, pwszInitializationString: POleStr;
      dwCreationDisposition: DWORD): HResult; stdcall;
  end;
  /// obtain a new session to a given OleDB data source
  IDBCreateSession = interface(IUnknown)
    ['{0C733A5D-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateSession(const punkOuter: IUnknown; const riid: TGUID;
      out ppDBSession: IUnknown): HResult; stdcall;
  end;
  /// commit, abort, and obtain status information about OleDB transactions
  ITransaction = interface(IUnknown)
    ['{0FB15084-AF41-11CE-BD2B-204C4F4F5020}']
    function Commit(fRetaining: BOOL; grfTC: UINT; grfRM: UINT): HResult; stdcall;
    function Abort(pboidReason: PBOID; fRetaining: BOOL; fAsync: BOOL): HResult; stdcall;
    function GetTransactionInfo(out pinfo: TXactTransInfo): HResult; stdcall;
  end;
  /// gets and sets a suite of options associated with an OleDB transaction
  ITransactionOptions = interface(IUnknown)
    ['{3A6AD9E0-23B9-11CF-AD60-00AA00A74CCD}']
    function SetOptions(var pOptions: TXactOpt): HResult; stdcall;
    function GetOptions(var pOptions: TXactOpt): HResult; stdcall;
  end;
  /// optional interface on OleDB sessions, used to start, commit, and abort
  // transactions on the session
  ITransactionLocal = interface(ITransaction)
    ['{0C733A5F-2A1C-11CE-ADE5-00AA0044773D}']
    function GetOptionsObject(out ppOptions: ITransactionOptions): HResult; stdcall;
    function StartTransaction(isoLevel: Integer; isoFlags: UINT;
      const pOtherOptions: ITransactionOptions; pulTransactionLevel: PUINT): HResult; stdcall;
  end;
  /// provide methods to execute commands
  ICommand = interface(IUnknown)
    ['{0C733A63-2A1C-11CE-ADE5-00AA0044773D}']
    function Cancel: HResult; stdcall;
    function Execute(const punkOuter: IUnknown; const riid: TGUID; var pParams: TDBParams;
      pcRowsAffected: PInteger; ppRowset: PIUnknown): HResult; stdcall;
    function GetDBSession(const riid: TGUID; out ppSession: IUnknown): HResult; stdcall;
  end;
  /// methods to access the ICommand text to be executed
  ICommandText = interface(ICommand)
    ['{0C733A27-2A1C-11CE-ADE5-00AA0044773D}']
    function GetCommandText(var pguidDialect: TGUID;
      out ppwszCommand: PWideChar): HResult; stdcall;
    function SetCommandText(const guidDialect: TGUID;
      pwszCommand: PWideChar): HResult; stdcall;
  end;
  /// provides methods for fetching rows sequentially, getting the data from
  // those rows, and managing rows
  IRowset = interface(IUnknown)
    ['{0C733A7C-2A1C-11CE-ADE5-00AA0044773D}']
    /// Adds a reference count to an existing row handle
    function AddRefRows(cRows: UINT; rghRows: PCardinalArray; rgRefCounts: PCardinalArray;
      rgRowStatus: PCardinalArray): HResult; stdcall;
    /// Retrieves data from the rowset's copy of the row
    function GetData(HROW: HROW; HACCESSOR: HACCESSOR; pData: Pointer): HResult; stdcall;
    /// Fetches rows sequentially, remembering the previous position
    // - this method has been modified from original OleDB.pas to allow direct
    // typecast of prghRows parameter to pointer(fRowStepHandles)
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: Integer; cRows: Integer;
      out pcRowsObtained: UINT; var prghRows: pointer): HResult; stdcall;
    /// Releases rows
    function ReleaseRows(cRows: UINT; rghRows: PCardinalArray; rgRowOptions,
      rgRefCounts, rgRowStatus: PCardinalArray): HResult; stdcall;
    /// Repositions the next fetch position to its initial position
    // - that is, its position when the rowset was first created
    function RestartPosition(hReserved: HCHAPTER): HResult; stdcall;
  end;
  /// interface used to retrieve enhanced custom error information
  IErrorRecords = interface(IUnknown)
    ['{0c733a67-2a1c-11ce-ade5-00aa0044773d}']
    function AddErrorRecord(pErrorInfo: PErrorInfo; dwLookupID: UINT;
      pDispParams: pointer; punkCustomError: IUnknown;
      dwDynamicErrorID: UINT): HResult; stdcall;
    function GetBasicErrorInfo(ulRecordNum: UINT;
      pErrorInfo: PErrorInfo): HResult; stdcall;
    function GetCustomErrorObject(ulRecordNum: UINT;
      const riid: TGUID; var ppObject: IUnknown): HResult; stdcall;
    function GetErrorInfo(ulRecordNum: UINT; lcid: UINT;
      var ppErrorInfo: IErrorInfo): HResult; stdcall;
    function GetErrorParameters(ulRecordNum: UINT;
      pDispParams: pointer): HResult; stdcall;
    function GetRecordCount(var pcRecords: UINT): HResult; stdcall;
  end;
  /// used on an OleDB session to obtain a new command
  IDBCreateCommand = interface(IUnknown)
    ['{0C733A1D-2A1C-11CE-ADE5-00AA0044773D}']
    function CreateCommand(const punkOuter: IUnknown; const riid: TGUID;
      out ppCommand: ICommand): HResult; stdcall;
  end;
  /// provides methods for accessor management, to access OleDB data
  // - An accessor is a data structure created by the consumer that describes
  // how row or parameter data from the data store is to be laid out in the
  // consumer's data buffer.
  // - For each column in a row (or parameter in a set of parameters), the
  // accessor contains a binding. A binding is a DBBinding data structure that
  // holds information about a column or parameter value, such as its ordinal
  // value, data type, and destination in the consumer's buffer.
  IAccessor = interface(IUnknown)
    ['{0C733A8C-2A1C-11CE-ADE5-00AA0044773D}']
    function AddRefAccessor(HACCESSOR: HACCESSOR; pcRefCount: PUINT): HResult; stdcall;
    function CreateAccessor(dwAccessorFlags: UINT; cBindings: UINT; rgBindings: PDBBindingArray;
      cbRowSize: UINT; var phAccessor: HACCESSOR; rgStatus: PCardinalArray): HResult; stdcall;
    function GetBindings(HACCESSOR: HACCESSOR; pdwAccessorFlags: PUINT; var pcBindings: UINT;
      out prgBindings: PDBBinding): HResult; stdcall;
    function ReleaseAccessor(HACCESSOR: HACCESSOR; pcRefCount: PUINT): HResult; stdcall;
  end;
  /// expose information about columns of an OleDB rowset or prepared command
  IColumnsInfo = interface(IUnknown)
    ['{0C733A11-2A1C-11CE-ADE5-00AA0044773D}']
    function GetColumnInfo(var pcColumns: UINT; out prgInfo: PDBColumnInfo;
      out ppStringsBuffer: PWideChar): HResult; stdcall;
    function MapColumnIDs(cColumnIDs: UINT; rgColumnIDs: PDBIDArray;
      rgColumns: PCardinalArray): HResult; stdcall;
  end;
  /// allows the display of the data link dialog boxes programmatically
  IDBPromptInitialize = interface(IUnknown)
    ['{2206CCB0-19C1-11D1-89E0-00C04FD7A829}']
    function PromptDataSource(const pUnkOuter: IUnknown; hWndParent: HWND;
      dwPromptOptions: UINT; cSourceTypeFilter: ULONG;
      rgSourceTypeFilter: PDBSOURCETYPE; pszProviderFilter: POleStr;
      const riid: TIID; var DataSource: IUnknown): HResult; stdcall;
    function PromptFileName(hWndParent: HWND; dwPromptOptions: UINT;
      pwszInitialDirectory, pwszInitialFile: POleStr;
      var ppwszSelectedFile: POleStr): HResult; stdcall;
  end;
  /// used to retrieve the database metadata (e.g. tables and fields layout)
  IDBSchemaRowset = interface(IUnknown)
    ['{0c733a7b-2a1c-11ce-ade5-00aa0044773d}']
    function GetRowset(pUnkOuter: IUnknown; const rguidSchema: TGUID;
      cRestrictions: Integer; rgRestrictions: pointer;
      const riid: TIID; cPropertySets: Integer; rgPropertySets: PDBPROPSET;
      var ppRowset: IRowset): HResult; stdcall;
    function GetSchemas(var pcSchemas: Integer; var prgSchemas: PGUID;
      var prgRestrictionSupport: PInteger): HResult; stdcall;
  end;


{ -------------- TOleDB* OleDB classes and types }
type
  /// generic Exception type, generated for OleDB connection
  EOleDBException = class(ESQLDBException);

  TOleDBConnection = class;

  TOleDBOnCustomError = function(Connection: TOleDBConnection;
      ErrorRecords: IErrorRecords; RecordNum: UINT): boolean of object;

  /// will implement properties shared by OleDB connections
  TOleDBConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    fProviderName: RawUTF8;
    fConnectionString: SynUnicode;
    fOnCustomError: TOleDBOnCustomError;
    fSchemaRec: array of TDBSchemaRec;
    function GetSchema(const aUID: TGUID; const Fields: array of RawUTF8;
      var aResult: IRowSet): boolean; 
    /// will create the generic fConnectionString from supplied parameters
    procedure SetInternalProperties; override;
    /// convert a textual column data type, as retrieved e.g. from SQLGetField,
    // into our internal primitive types
    function ColumnTypeNativeToDB(const aNativeType: RawUTF8; aScale: integer): TSQLDBFieldType; override;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - used by GetForeignKey method
    procedure GetForeignKeys; override;
    /// create the database
    // - shall be called only if necessary (e.g. for file-based database, if
    // the file does not exist yet)
    function CreateDatabase: boolean; virtual;
  public
    /// create a new connection
    // - call this method if the shared MainConnection is not enough (e.g. for
    // multi-thread access)
    // - the caller is responsible of freeing this instance
    // - this overriden method will create an TOleDBConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// display the OleDB/ADO Connection Settings dialog to customize the
    // OleDB connection string
    // - returns TRUE if the connection string has been modified
    // - Parent is an optional GDI Window Handle for modal display
    function ConnectionStringDialogExecute(Parent: HWND=0): boolean;
    /// get all table names
    // - will retrieve the corresponding metadata from OleDB interfaces if SQL
    // direct access was not defined
    procedure GetTableNames(var Tables: TRawUTF8DynArray); override;
    /// retrieve the column/field layout of a specified table
    // - will retrieve the corresponding metadata from OleDB interfaces if SQL
    // direct access was not defined
    // - if GetForeignKey is TRUE, will retrieve ColumnForeignKey* properties,
    // but will be much slower
    procedure GetFields(const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray); override;
    /// the associated OleDB connection string
    // - is set by the Create() constructor most of the time from the supplied
    // server name, user id and password, according to the database provider
    // corresponding to the class
    // - you may want to customize it via the ConnectionStringDialogExecute
    // method, or to provide some additional parameters
    property ConnectionString: SynUnicode read fConnectionString write fConnectionString;
    /// custom Error handler for OleDB COM objects
    // - returns TRUE if specific error was retrieved and has updated
    // ErrorMessage and InfoMessage
    // - default implementation just returns false
    property OnCustomError: TOleDBOnCustomError read fOnCustomError write fOnCustomError;
  published { to be loggged as JSON }
    /// the associated OleDB provider name, as set for each class
    property ProviderName: RawUTF8 read fProviderName;
  end;

  /// OleDB connection properties to an Oracle database using Oracle's Provider
  // - this will use the native OleDB provider supplied by Oracle
  // see @http://download.oracle.com/docs/cd/E11882_01/win.112/e17726/toc.htm
  TOleDBOracleConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'OraOLEDB.Oracle.1'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to an Oracle database using Microsoft's Provider
  // - this will use the generic (older) OleDB provider supplied by Microsoft
  // which would not be used any more:
  // "This feature will be removed in a future version of Windows. Avoid
  // using this feature in new development work, and plan to modify applications
  // that currently use this feature. Instead, use Oracle’s OLE DB provider."
  // see http://msdn.microsoft.com/en-us/library/ms675851(v=VS.85).aspx
  TOleDBMSOracleConnectionProperties = class(TOleDBOracleConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'MSDAORA'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to Microsoft SQL Server
  // - this will use the native OleDB provider supplied by Microsoft
  // see http://msdn.microsoft.com/en-us/library/ms677227(v=VS.85).aspx
  // - is aUserID='' at Create, it will use Windows Integrated Security
  // for the connection
  TOleDBMSSQLConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'SQLOLEDB'
    procedure SetInternalProperties; override;
    /// custom Error handler for OleDB COM objects
    // - will handle Microsoft SQL Server error messages (if any)
    function MSOnCustomError(Connection: TOleDBConnection;
      ErrorRecords: IErrorRecords; RecordNum: UINT): boolean;
  public
  end;

  /// OleDB connection properties to MySQL Server
  TOleDBMySQLConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'MySQLProv'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to Jet/MSAccess .mdb files
  // - the server name should be the .mdb file name
  TOleDBJetConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'Microsoft.Jet.OLEDB.4.0'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties to IBM AS/400
  TOleDBAS400ConnectionProperties = class(TOleDBConnectionProperties)
  protected
    /// will set the appropriate provider name, i.e. 'IBMDA400.DataSource.1'
    procedure SetInternalProperties; override;
  end;

  /// OleDB connection properties via Microsoft Provider for ODBC
  // - this will use the ODBC provider supplied by Microsoft
  // see http://msdn.microsoft.com/en-us/library/ms675326(v=VS.85).aspx
  // - an ODBC Driver should be specified at creation
  // - you should better use direct connection classes, like
  // TOleDBMSSQLConnectionProperties or TOleDBOracleConnectionProperties
  TOleDBODBCSQLConnectionProperties = class(TOleDBConnectionProperties)
  protected
    fDriver: RawUTF8;
    /// will set the appropriate provider name, i.e. 'MSDASQL'
    procedure SetInternalProperties; override;
  public
    /// initialize the properties
    // - an additional parameter is available to set the ODBC driver to use
    // - you may also set aDriver='' and modify the connection string directly,
    // e.g. adding '{ DSN=name | FileDSN=filename };'
    constructor Create(const aDriver, aServerName, aDatabaseName,
      aUserID, aPassWord: RawUTF8); reintroduce;
  published { to be logged as JSON }
    /// the associated ODBC Driver name, as specified at creation
    property Driver: RawUTF8 read fDriver;
  end;

  /// implements an OleDB connection
  // - will retrieve the remote DataBase behavior from a supplied
  // TSQLDBConnectionProperties class, shared among connections
  TOleDBConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fMalloc: IMalloc;
    fDBInitialize: IDBInitialize;
    fTransaction: ITransactionLocal;
    fSession: IUnknown;
    fOleDBProperties: TOleDBConnectionProperties;
    /// Error handler for OleDB COM objects
    // - will update ErrorMessage and InfoMessage
    procedure OleDBCheck(aResult: HRESULT; const aStatus: TCardinalDynArray=nil); virtual;
    /// called just after fDBInitialize.Initialized: could add parameters
    procedure OnDBInitialized; virtual;
  public
    /// connect to a specified OleDB database
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release all associated memory and OleDB COM objects
    destructor Destroy; override;
    /// initialize a new SQL query statement for the given connection
    // - the caller should free the instance after use
    function NewStatement: TSQLDBStatement; override;
    /// connect to the specified database
    // - should raise an EOleDBException on error
    procedure Connect; override;
    /// stop connection to the specified database
    // - should raise an EOleDBException on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// begin a Transaction for this connection
    // - be aware that not all OleDB provider support nested transactions
    // see http://msdn.microsoft.com/en-us/library/ms716985(v=vs.85).aspx
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// the associated OleDB database properties
    property OleDBProperties: TOleDBConnectionProperties read fOleDBProperties;
  end;

  /// used to store properties and value about one TOleDBStatement Param
  // - we don't use a Variant, not the standard TSQLDBParam record type,
  // but manual storage for better performance
  // - whole memory block of a TOleDBStatementParamDynArray will be used as the
  // source Data for the OleDB parameters - so we should align data carefully
  TOleDBStatementParam = packed record
    /// storage used for BLOB (ftBlob) values
    // - will be refered as DBTYPE_BYREF when sent as OleDB parameters, to
    // avoid unnecessary memory copy
    VBlob: RawByteString;
    /// storage used for TEXT (ftUTF8) values
    // - we store TEXT here as WideString, and not RawUTF8, since OleDB
    // expects the text to be provided with Unicode encoding
    // - for some providers (like Microsoft SQL Server 2008 R2, AFAIK), using
    // DBTYPE_WSTR value (i.e. what the doc. says) will raise an OLEDB Error
    // 80040E1D (DB_E_UNSUPPORTEDCONVERSION, i.e. 'Requested conversion is not
    // supported'): we found out that only DBTYPE_BSTR type (i.e. OLE WideString)
    // does work... so we'll use it here! Shame on Microsoft!
    // - what's fine with DBTYPE_BSTR is that it can be resized by the provider
    // in case of VInOut in [paramOut, paramInOut] - so let it be
    VText: WideString;
    /// storage used for ftInt64, ftDouble, ftDate and ftCurrency value
    VInt64: Int64;
    /// the column/parameter Value type
    VType: TSQLDBFieldType;
    /// define if parameter can be retrieved after a stored procedure execution
    VInOut: TSQLDBParamInOutType;
    // so that VInt64 will be 8 bytes aligned
    VFill: array[sizeof(TSQLDBFieldType)+sizeof(TSQLDBParamInOutType)..SizeOf(Int64)-1] of byte;
  end;
  POleDBStatementParam = ^TOleDBStatementParam;

  /// used to store properties about TOleDBStatement Parameters
  // - whole memory block of a TOleDBStatementParamDynArray will be used as the
  // source Data for the OleDB parameters
  TOleDBStatementParamDynArray = array of TOleDBStatementParam;

  /// implements an OleDB SQL query statement
  // - this statement won't retrieve all rows of data, but will allow direct
  // per-row access using the Step() and Column*() methods
  TOleDBStatement = class(TSQLDBStatement)
  protected
    fParams: TOleDBStatementParamDynArray;
    fColumns: TSQLDBColumnPropertyDynArray;
    fParam: TDynArray;
    fColumn: TDynArrayHashed;
    fCommand: ICommandText;
    fRowSet: IRowSet;
    fRowSetAccessor: HACCESSOR;
    fRowSize: integer;
    fRowStepResult: HRESULT;
    fRowStepHandleRetrieved: cardinal;
    fRowStepHandleCurrent: cardinal;
    fRowStepHandles: TCardinalDynArray;
    fRowSetData: array of byte;
    fParamBindings: TDBBindingDynArray;
    fColumnBindings: TDBBindingDynArray;
    fHasColumnValueInlined: boolean;
    fOleDBConnection: TOleDBConnection;
    fDBParams: TDBParams;
    fRowBufferSize: integer;
    fAlignBuffer: boolean;
    procedure SetRowBufferSize(Value: integer);
    /// resize fParams[] if necessary, set the VType and return pointer to
    // the corresponding entry in fParams[]
    // - first parameter has Param=1
    function CheckParam(Param: Integer; NewType: TSQLDBFieldType;
      IO: TSQLDBParamInOutType): POleDBStatementParam;
    /// raise an exception if Col is incorrect or no IRowSet is available
    // - set Column to the corresponding fColumns[] item
    // - return a pointer to status-data[-length] in fRowSetData[], or
    // nil if status states this column is NULL
    function GetCol(Col: integer; out Column: PSQLDBColumnProperty): pointer;
    procedure GetCol64(Col: integer; DestType: TSQLDBFieldType; var Dest);
      {$ifdef HASINLINE}inline;{$endif}
    procedure FlushRowSetData;
    procedure ReleaseRows;
    procedure CloseRowSet;
    ///  retrieve column information, and initialize Bindings[]
    // - add the high-level column information in Column[], initializes
    // OleDB Bindings array and returns the row size (in bytes)
    function BindColumns(ColumnInfo: IColumnsInfo; var Column: TDynArrayHashed;
      out Bindings: TDBBindingDynArray): integer;
    procedure LogStatusError(Status: integer; Column: PSQLDBColumnProperty);
  public
    {{ create an OleDB statement instance, from an OleDB connection
     - the Execute method can be called only once per TOleDBStatement instance
     - if the supplied connection is not of TOleDBConnection type, will raise
       an exception }
    constructor Create(aConnection: TSQLDBConnection); override;
    {{ release all associated memory and COM objects }
    destructor Destroy; override;
    {{ retrieve column information from a supplied IRowSet
    - is used e.g. by TOleDBStatement.Execute or to retrieve metadata columns
    - raise an exception on error }
    procedure FromRowSet(RowSet: IRowSet);

    {{ bind a NULL value to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindNull(Param: Integer; IO: TSQLDBParamInOutType=paramIn); override;
    {{ bind an integer value to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure Bind(Param: Integer; Value: Int64;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a double value to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure Bind(Param: Integer; Value: double;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a TDateTime value to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindDateTime(Param: Integer; Value: TDateTime;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a currency value to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindCurrency(Param: Integer; Value: currency;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a UTF-8 encoded string to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindTextU(Param: Integer; const Value: RawUTF8;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a UTF-8 encoded buffer text (#0 ended) to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindTextP(Param: Integer; Value: PUTF8Char;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a VCL string to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindTextS(Param: Integer; const Value: string;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind an OLE WideString to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindTextW(Param: Integer; const Value: WideString;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a Blob buffer to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindBlob(Param: Integer; Data: pointer; Size: integer;
      IO: TSQLDBParamInOutType=paramIn); overload; override;
    {{ bind a Blob buffer to a parameter
     - the leftmost SQL parameter has an index of 1
     - raise an EOleDBException on any error }
    procedure BindBlob(Param: Integer; const Data: RawByteString;
      IO: TSQLDBParamInOutType=paramIn); overload; override;

    {{ Execute an UTF-8 encoded SQL statement
     - parameters marked as ? should have been already bound with Bind*()
       functions above
     - raise an EOleDBException on any error }
    procedure ExecutePrepared; override;
    {{ retrieve the parameter content, after SQL execution
     - the leftmost SQL parameter has an index of 1 
     - to be used e.g. with stored procedures
     - any TEXT parameter will be retrieved as WideString Variant (i.e. as
       stored in TOleDBStatementParam) }
    function ParamToVariant(Param: Integer; var Value: Variant;
      CheckIsOutParameter: boolean=true): TSQLDBFieldType; override;

    {/ After a statement has been prepared via Prepare() + ExecutePrepared() or
       Execute(), this method must be called one or more times to evaluate it
     - you shall call this method before calling any Column*() methods
     - return TRUE on success, with data ready to be retrieved by Column*()
     - return FALSE if no more row is available (e.g. if the SQL statement
      is not a SELECT but an UPDATE or INSERT command)
     - access the first or next row of data from the SQL Statement result:
       if SeekFirst is TRUE, will put the cursor on the first row of results,
       otherwise, it will fetch one row of data, to be called within a loop
     - raise an ESQLEOleDBException on any error }
    function Step(SeekFirst: boolean=false): boolean; override;
    {{ retrieve a column name of the current Row
     - Columns numeration (i.e. Col value) starts with 0
     - it's up to the implementation to ensure than all column names are unique }
    function ColumnName(Col: integer): RawUTF8; override;
    {{ returns the Column index of a given Column name
     - Columns numeration (i.e. Col value) starts with 0
     - returns -1 if the Column name is not found (via case insensitive search) }
    function ColumnIndex(const aColumnName: RawUTF8): integer; override;
    {{ the Column type of the current Row
     - ftCurrency type should be handled specificaly, for faster process and
     avoid any rounding issue, since currency is a standard OleDB type
     - FieldSize can be set to store the size in chars of a ftUTF8 column
       (0 means BLOB kind of TEXT column) }
    function ColumnType(Col: integer; FieldSize: PInteger=nil): TSQLDBFieldType; override;
    {{ returns TRUE if the column contains NULL }
    function ColumnNull(Col: integer): boolean; override;
    {{ return a Column integer value of the current Row, first Col is 0 }
    function ColumnInt(Col: integer): Int64; override;
    {{ return a Column floating point value of the current Row, first Col is 0 }
    function ColumnDouble(Col: integer): double; override;
    {{ return a Column date and time value of the current Row, first Col is 0 }
    function ColumnDateTime(Col: integer): TDateTime; override;
    {{ return a Column currency value of the current Row, first Col is 0
     - should retrieve directly the 64 bit Currency content, to avoid
     any rounding/conversion error from floating-point types }
    function ColumnCurrency(Col: integer): currency; override;
    {{ return a Column UTF-8 encoded text value of the current Row, first Col is 0 }
    function ColumnUTF8(Col: integer): RawUTF8; override;
    {{ return a Column text generic VCL string value of the current Row, first Col is 0 }
    function ColumnString(Col: integer): string; override;
    {{ return a Column as a blob value of the current Row, first Col is 0
    - ColumnBlob() will return the binary content of the field is was not ftBlob,
      e.g. a 8 bytes RawByteString for a vtInt64/vtDouble/vtDate/vtCurrency,
      or a direct mapping of the RawUnicode  }
    function ColumnBlob(Col: integer): RawByteString; override;
    {{ append all columns values of the current Row to a JSON stream
     - will use WR.Expand to guess the expected output format
     - fast overriden implementation with no temporary variable 
     - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
       format and contains true BLOB data }
    procedure ColumnsToJSON(WR: TJSONWriter); override;
    {{ return a Column as a variant
     - this implementation will retrieve the data with no temporary variable
       (since TQuery calls this method a lot, we tried to optimize it)
     - a ftUTF8 content will be mapped into a generic WideString variant
       for pre-Unicode version of Delphi, and a generic UnicodeString (=string)
       since Delphi 2009: you may not loose any data during charset conversion
     - a ftBlob content will be mapped into a TBlobData AnsiString variant }
    function ColumnToVariant(Col: integer; var Value: Variant): TSQLDBFieldType; override;
    /// just map the original Collection into a TOleDBConnection class
    property OleDBConnection: TOleDBConnection read fOleDBConnection;
    /// if TRUE, the data will be 8 bytes aligned in OleDB internal buffers
    // - it's recommended by official OleDB documentation for faster process
    // - is enabled by default, and should not be modified in most cases
    property AlignDataInternalBuffer: boolean read fAlignBuffer write fAlignBuffer;
    /// size in bytes of the internal OleDB buffer used to fetch rows
    // - several rows are retrieved at once into the internal buffer
    // - default value is 16384 bytes, minimal allowed size is 8192
    property RowBufferSize: integer read fRowBufferSize write SetRowBufferSize;
  end;

/// check from the file beginning if sounds like a valid Jet / MSAccess file
function IsJetFile(const FileName: TFileName): boolean;

/// this global procedure should be called for each thread needing to use OLE
// - it is already called by TOleDBConnection.Create when an OleDb connection
// is instantiated for a new thread
// - every call of CoInit shall be followed by a call to CoUninit
// - implementation will maintain some global counting, to call the CoInitialize
// API only once per thread
// - only made public for user convenience, e.g. when using custom COM objects
procedure CoInit;

/// this global procedure should be called at thread termination
// - it is already called by TOleDBConnection.Destroy, e.g. when thread associated
// to an OleDb connection is terminated
// - every call of CoInit shall be followed by a call to CoUninit
// - only made public for user convenience, e.g. when using custom COM objects
procedure CoUninit;


implementation

function IsJetFile(const FileName: TFileName): boolean;
var F: THandle;
    Header: array[0..31] of AnsiChar;
begin
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if F=INVALID_HANDLE_VALUE then
    result := false else begin
    result := (FileRead(F,Header,sizeof(Header))=SizeOf(Header)) and
      IdemPChar(@Header[4],'STANDARD JET');
    FileClose(F);
  end;
end;

{ TOleDBStatement }

procedure TOleDBStatement.BindTextU(Param: Integer; const Value: RawUTF8;
  IO: TSQLDBParamInOutType=paramIn);
begin
  UTF8ToWideString(Value,CheckParam(Param,ftUTF8,IO)^.VText);
end;

procedure TOleDBStatement.BindTextP(Param: Integer; Value: PUTF8Char;
  IO: TSQLDBParamInOutType);
begin
  UTF8ToWideString(Value,StrLen(Value),CheckParam(Param,ftUTF8,IO)^.VText);
end;

procedure TOleDBStatement.BindTextS(Param: Integer; const Value: string;
  IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftUTF8,IO)^.VText := Value; // let Delphi do the work
end;

procedure TOleDBStatement.BindTextW(Param: Integer;
  const Value: WideString; IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftUTF8,IO)^.VText := Value;
end;

procedure TOleDBStatement.BindBlob(Param: Integer;
  const Data: RawByteString; IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftBlob,IO)^.VBlob := Data;
end;

procedure TOleDBStatement.BindBlob(Param: Integer; Data: pointer; Size: integer;
  IO: TSQLDBParamInOutType=paramIn);
begin
  SetString(CheckParam(Param,ftBlob,IO)^.VBlob,PAnsiChar(Data),Size);
end;

procedure TOleDBStatement.Bind(Param: Integer; Value: double;
  IO: TSQLDBParamInOutType=paramIn);
begin
  CheckParam(Param,ftDouble,IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TOleDBStatement.Bind(Param: Integer; Value: Int64;
  IO: TSQLDBParamInOutType=paramIn);
begin
  CheckParam(Param,ftInt64,IO)^.VInt64 := Value;
end;

procedure TOleDBStatement.BindCurrency(Param: Integer; Value: currency;
  IO: TSQLDBParamInOutType=paramIn);
begin
  CheckParam(Param,ftCurrency,IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TOleDBStatement.BindDateTime(Param: Integer; Value: TDateTime;
  IO: TSQLDBParamInOutType=paramIn);
begin
  CheckParam(Param,ftDate,IO)^.VInt64 := PInt64(@Value)^;
end;

procedure TOleDBStatement.BindNull(Param: Integer;
  IO: TSQLDBParamInOutType);
begin
  CheckParam(Param,ftNull,IO);
end;

function TOleDBStatement.CheckParam(Param: Integer; NewType: TSQLDBFieldType;
  IO: TSQLDBParamInOutType): POleDBStatementParam;
begin
  if (self=nil) or Assigned(fCommand) then
    raise EOleDBException.Create('Bind*() called after Execute');
  if Param<=0 then
    raise EOleDBException.CreateFmt('Bind*() called with Param=%d should be >= 1',[Param]);
  if Param>fParamCount then
    fParam.Count := Param; // resize fParams[] dynamic array if necessary
  result := @fParams[Param-1];
  result^.VType := NewType;
  result^.VInOut := IO;
end;

constructor TOleDBStatement.Create(aConnection: TSQLDBConnection);
begin
  if not aConnection.InheritsFrom(TOleDBConnection) then
    raise EOleDBException.CreateFmt('%s.Create expects a TOleDBConnection',[ClassName]);
  inherited Create(aConnection);
  fOleDBConnection := TOleDBConnection(aConnection);
  fParam.Init(TypeInfo(TOleDBStatementParamDynArray),fParams,@fParamCount);
  fColumn.Init(TypeInfo(TSQLDBColumnPropertyDynArray),fColumns,nil,nil,nil,@fColumnCount,True);
  fRowBufferSize := 16384;
  fAlignBuffer := true;
end;

type
  TColumnValue = packed record
    Status: cardinal;
    Length: integer; // ignored for alignment
    case integer of
    0: (Int64: Int64);
    1: (Double: double);
    2: (case integer of
        0: (VData: array[0..0] of byte);
        1: (VWideChar: PWideChar);
        2: (VAnsiChar: PAnsiChar));
  end;
  PColumnValue = ^TColumnValue;

procedure TOleDBStatement.LogStatusError(Status: integer; Column: PSQLDBColumnProperty);
var msg: RawUTF8;
begin
{$ifndef PUREPASCAL}
  if cardinal(Status)<=cardinal(ord(high(TOleDBStatus))) then
    msg := UnCamelCase(TrimLeftLowerCase(GetEnumName(TypeInfo(TOleDBStatus),Status))) else
{$else}
    msg := Int32ToUtf8(Status);
{$endif}
  SynDBLog.Add.Log(sllError,
  {$ifdef DELPHI5OROLDER}
    msg+' for column "'+Column^.ColumnName+'" at row '+Int32ToUTF8(fCurrentRow)+
    ' for '+fSQL);
  {$else}
    'Invalid "%" status for column "%" at row % for %',[msg,Column^.ColumnName,fCurrentRow,fSQL],self);
  {$endif}
end;

function TOleDBStatement.GetCol(Col: integer; out Column: PSQLDBColumnProperty): pointer;
begin
  CheckCol(Col); // check Col value
  if not Assigned(fRowSet) or (fColumnCount=0) then
    raise EOleDBException.Create('TOleDBStatement.Column*() with no prior Execute');
  if CurrentRow<=0 then
    raise EOleDBException.Create('TOleDBStatement.Column*() with no prior Step');
  Column := @fColumns[Col];
  result := @fRowSetData[Column^.ColumnAttr];
  case TOleDBStatus(PColumnValue(result)^.Status) of
    stOk:
      exit; // valid content
    stIsNull:
      result := nil;
    stTruncated:
      LogTruncatedColumn(Column^);
    else
      LogStatusError(PColumnValue(result)^.Status,Column);
  end;
end;

procedure TOleDBStatement.GetCol64(Col: integer;
  DestType: TSQLDBFieldType; var Dest);
var C: PSQLDBColumnProperty;
    V: PColumnValue;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    Int64(Dest) := 0 else
    if C^.ColumnType=DestType then
      // types match -> fast direct retrieval
      Int64(Dest) := V^.Int64 else
      // need conversion to destination type
      ColumnToTypedValue(Col,DestType,Dest);
end;

function TOleDBStatement.ColumnBlob(Col: integer): RawByteString;
// ColumnBlob will return the binary content of the field
var C: PSQLDBColumnProperty;
    V: PColumnValue;
    P: PAnsiChar;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := '' else
    case C^.ColumnType of
    ftBlob: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VAnsiChar;
      SetString(Result,P,V^.Length);
    end;
    ftUTF8:
      if V^.Length=0 then
        result := '' else begin
        if C^.ColumnValueInlined then
          P := @V^.VData else
          P := V^.VAnsiChar;
        // +1 below for trailing WideChar(#0) in the resulting RawUnicode
        SetString(Result,P,V^.Length+1);
      end;
     else SetString(result,PAnsiChar(@V^.Int64),sizeof(Int64));
    end;
end;

function TOleDBStatement.ColumnCurrency(Col: integer): currency;
begin
  GetCol64(Col,ftCurrency,Result);
end;

function TOleDBStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  GetCol64(Col,ftDate,Result);
end;

function TOleDBStatement.ColumnDouble(Col: integer): double;
begin
  GetCol64(Col,ftDouble,Result);
end;

function TOleDBStatement.ColumnIndex(const aColumnName: RawUTF8): integer;
begin
  result := fColumn.FindHashed(aColumnName);
end;

function TOleDBStatement.ColumnNull(Col: integer): boolean;
var C: PSQLDBColumnProperty;
begin
  result := GetCol(Col,C)=nil;
end;

function TOleDBStatement.ColumnInt(Col: integer): Int64;
begin
  GetCol64(Col,ftInt64,Result);
end;

function TOleDBStatement.ColumnName(Col: integer): RawUTF8;
begin
  CheckCol(Col);
  result := fColumns[Col].ColumnName;
end;

function TOleDBStatement.ColumnType(Col: integer; FieldSize: PInteger=nil): TSQLDBFieldType;
begin
  CheckCol(Col);
  with fColumns[Col] do begin
    result := ColumnType;
    if FieldSize<>nil then
      if ColumnValueInlined then
        FieldSize^ := ColumnValueDBSize else
        FieldSize^ := 0;
  end;
end;

function TOleDBStatement.ColumnUTF8(Col: integer): RawUTF8;
var C: PSQLDBColumnProperty;
    V: PColumnValue;
    P: pointer;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := '' else
    case C^.ColumnType of // fast direct conversion from OleDB buffer
    ftInt64: result := Int64ToUtf8(V^.Int64);
    ftDate:  result := DateTimeToIso8601Text(V^.Double);
    ftUTF8: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VWideChar;
      result := RawUnicodeToUtf8(P,V^.Length shr 1);
    end;
    ftBlob: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VAnsiChar;
      result := BinToBase64WithMagic(P,V^.Length);
    end;
    ftCurrency: result := Curr64ToStr(V^.Int64);
    ftDouble:
      if V^.Int64=0 then
        result := '0' else
        result := DoubleToStr(V^.Double);
    end;
end;

function TOleDBStatement.ColumnString(Col: integer): string;
var C: PSQLDBColumnProperty;
    V: PColumnValue;
    P: pointer;
begin
  V := GetCol(Col,C);
  if V=nil then // column is NULL
    result := '' else
    case C^.ColumnType of // fast direct conversion from OleDB buffer
    ftInt64: result := IntToString(V^.Int64);
    ftDouble:
      if V^.Int64=0 then
        result := '0' else
        result := DoubleToString(V^.Double);
    ftCurrency: result := Curr64ToString(V^.Int64);
    ftDate:  result := Ansi7ToString(DateTimeToIso8601Text(V^.Double));
    ftUTF8: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VWideChar;
      result := RawUnicodeToString(P,V^.Length shr 1);
    end;
    ftBlob: begin
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VAnsiChar;
      result := Ansi7ToString(BinToBase64WithMagic(P,V^.Length));
    end;
    end;
end;

function TOleDBStatement.ColumnToVariant(Col: integer;
  var Value: Variant): TSQLDBFieldType;
const FIELDTYPE2VARTYPE: array[TSQLDBFieldType] of Word = (
  varEmpty, varNull, varInt64, varDouble, varCurrency, varDate,
  {$ifdef UNICODE}varUString{$else}varOleStr{$endif}, varString);
// ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob
var C: PSQLDBColumnProperty;
    V: PColumnValue;
    P: pointer;
    Val: TVarData absolute Value;
begin
  V := GetCol(Col,C);
  if V=nil then
    result := ftNull else
    result := C^.ColumnType;
  VarClear(Value);
  Val.VType := FIELDTYPE2VARTYPE[result];
  case result of
    ftInt64, ftDouble, ftCurrency, ftDate:
      Val.VInt64 := V^.Int64; // copy 64 bit content
    ftUTF8: begin
      Val.VAny := nil;
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VAnsiChar;
      {$ifndef UNICODE}
      if not Connection.Properties.VariantStringAsWideString then begin
        Val.VType := varString;
        RawUnicodeToString(P,V^.Length shr 1,AnsiString(Val.VAny));
      end else
      {$endif}
        SetString(SynUnicode(Val.VAny),PWideChar(P),V^.Length shr 1);
    end;
    ftBlob: begin
      Val.VAny := nil;
      if C^.ColumnValueInlined then
        P := @V^.VData else
        P := V^.VAnsiChar;
      SetString(RawByteString(Val.VAny),PAnsiChar(P),V^.Length);
    end;
    end;
end;

procedure TOleDBStatement.ColumnsToJSON(WR: TJSONWriter);
var col: integer;
    V: PColumnValue;
    P: Pointer;
label Write;
begin
  if CurrentRow<=0 then
    raise EOleDBException.Create('TOleDBStatement.ColumnsToJSON() with no prior Step');
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount-1 do // fast direct conversion from OleDB buffer
  with fColumns[col] do begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    V := @fRowSetData[ColumnAttr];
    case TOleDBStatus(V^.Status) of
      stOK:
Write:case ColumnType of
        ftInt64:    WR.Add(V^.Int64);
        ftDouble:   WR.Add(V^.Double);
        ftCurrency: WR.AddCurr64(V^.Int64);
        ftDate: begin
          WR.Add('"');
          WR.AddDateTime(@V^.Double);
          WR.Add('"');
        end;
        ftUTF8: begin
          WR.Add('"');
          if ColumnValueInlined then
            P := @V^.VData else
            P := V^.VWideChar;
          WR.AddJSONEscapeW(P,V^.Length shr 1);
          WR.Add('"');
        end;
        ftBlob:begin
          if ColumnValueInlined then
            P := @V^.VData else
            P := V^.VAnsiChar;
          WR.WrBase64(P,V^.Length,true); // withMagic=true
        end;
        else WR.AddShort('null');
      end;
      stIsNull:
        WR.AddShort('null');
      stTruncated: begin
        LogTruncatedColumn(fColumns[col]);
        goto Write;
      end;
      else begin
        WR.AddShort('null');
        LogStatusError(V^.Status,@fColumns[col]);
      end;
    end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

function TOleDBStatement.ParamToVariant(Param: Integer; var Value: Variant;
  CheckIsOutParameter: boolean=true): TSQLDBFieldType;
begin
  inherited ParamToVariant(Param,Value); // raise exception if Param incorrect
  dec(Param); // start at #1
  if CheckIsOutParameter and (fParams[Param].VInOut=paramIn) then
    raise EOleDBException.CreateFmt('%s.ParamToVariant expects an [In]Out parameter',[ClassName]);
  // OleDB provider should have already modified the parameter in-place, i.e.
  // in our fParams[] buffer, especialy for TEXT parameters (OleStr/WideString)
  // -> we have nothing to do but return the current value :)
  with fParams[Param] do begin
    result := VType;
    case VType of
      ftInt64:     Value := {$ifdef DELPHI5OROLDER}integer{$endif}(VInt64);
      ftDouble:    Value := PDouble(@VInt64)^;
      ftCurrency:  Value := PCurrency(@VInt64)^;
      ftDate:      Value := PDateTime(@VInt64)^;
      ftUTF8:      Value := VText; // returned as WideString/OleStr variant
      ftBlob:      Value := VBlob;
      else         Value := Null;
    end;
  end;
end;

const
  PARAMTYPE2OLEDB: array[TSQLDBParamInOutType] of DBPARAMIO = (
    DBPARAMIO_INPUT, DBPARAMIO_OUTPUT, DBPARAMIO_INPUT or DBPARAMIO_OUTPUT);
  FIELDTYPE2OLEDB: array[TSQLDBFieldType] of DBTYPE = (
    DBTYPE_EMPTY, DBTYPE_NULL, DBTYPE_I8, DBTYPE_R8, DBTYPE_CY, DBTYPE_DATE,
    DBTYPE_WSTR or DBTYPE_BYREF, DBTYPE_BYTES or DBTYPE_BYREF);
// ftUnknown, ftNull, ftInt64, ftDouble, ftCurrency, ftDate, ftUTF8, ftBlob

procedure TOleDBStatement.ExecutePrepared;
var i, L: integer;
    P: POleDBStatementParam;
    B: PDBBinding;
    ParamsStatus: TCardinalDynArray;
    SQLW: RawUnicode;
    RowSet: IRowSet;
    Log: ISynLog;
begin
  Log := SynDBLog.Enter(self);
  try
    if Assigned(fCommand) or Assigned(fRowSet) or (fColumnCount>0) or
       (fColumnBindings<>nil) or (fParamBindings<>nil) then
      raise EOleDBException.CreateFmt('%s.Execute should be called only once',[ClassName]);
    // 1. prepare command
    with Log.Instance do
    if sllSQL in Family.Level then
      Log(sllSQL,SQLWithInlinedParams,self);
    with OleDBConnection do begin
      if not IsConnected then
        Connect;
      OleDBCheck((fSession as IDBCreateCommand).
        CreateCommand(nil,IID_ICommandText,ICommand(fCommand)));
    end;
    L := Length(fSQL);
    while (L>0) and (fSQL[L] in [#1..' ',';']) do
      dec(L); // trim ' ' or ';' right (last ';' could be found incorrect)
    SetLength(SQLW,L*2+1);
    UTF8ToWideChar(pointer(SQLW),pointer(fSQL),L);
    fCommand.SetCommandText(DBGUID_DEFAULT,pointer(SQLW));
    // 2. bind parameters
    if fParamCount=0 then
      // no parameter to bind
      fDBParams.cParamSets := 0 else begin
      // bind supplied parameters, with direct mapping to fParams[]
      for i := 0 to fParamCount-1 do
        case fParams[i].VType of
          ftUnknown: raise EOleDBException.CreateFmt(
            '%s.Execute called with a missing #%d bound parameter for "%s"',
            [ClassName,i+1,fSQL]);
        end;
      P := pointer(fParams);
      SetLength(fParamBindings,fParamCount);
      B := pointer(fParamBindings);
      for i := 1 to fParamCount do begin
        B^.iOrdinal := i; // parameter index (starting at 1)
        B^.eParamIO := PARAMTYPE2OLEDB[P^.VInOut]; // parameter direction
        B^.wType := FIELDTYPE2OLEDB[P^.VType];     // parameter data type
        B^.dwPart := DBPART_VALUE;
        // set additional fields
        case P^.VType of
        ftNull: begin
          // bind a NULL parameter
          B^.dwPart := DBPART_STATUS;
          B^.obStatus := PAnsiChar(@P^.VInt64)-pointer(fParams);
          P^.VInt64 := Ord(stIsNull);
        end;
        ftInt64, ftDouble, ftCurrency, ftDate: begin
          // those types match the VInt64 binary representation :)
          B^.cbMaxLen := sizeof(Int64);
          B^.obValue := PAnsiChar(@P^.VInt64)-pointer(fParams);
        end;
        ftBlob: begin
          // sent as DBTYPE_BYREF mapping directly RawByteString VBlob content
          B^.dwPart := DBPART_VALUE or DBPART_LENGTH;
          B^.obValue := PAnsiChar(@P^.VBlob)-pointer(fParams);
          B^.cbMaxLen := sizeof(Pointer);
          P^.VInt64 := length(P^.VBlob); // store length in unused VInt64 property
          B^.obLength := PAnsiChar(@P^.VInt64)-pointer(fParams);
        end;
        ftUTF8: begin
          B^.obValue := PAnsiChar(@P^.VText)-pointer(fParams);
          if P^.VText='' then begin
            B^.wType := DBTYPE_WSTR; // '' -> bind one #0 wide char
            B^.cbMaxLen := sizeof(WideChar);
          end else begin
            // mapping directly the WideString VText content
            B^.wType := DBTYPE_BSTR; // DBTYPE_WSTR just doesn't work :(
            B^.cbMaxLen := sizeof(Pointer);
          end;
        end;
        end;
        inc(P);
        inc(B);
      end;
      SetLength(ParamsStatus,fParamCount);
      OleDBConnection.OleDBCheck((fCommand as IAccessor).CreateAccessor(
        DBACCESSOR_PARAMETERDATA,fParamCount,Pointer(fParamBindings),0,
        fDBParams.HACCESSOR,pointer(ParamsStatus)));
      fDBParams.cParamSets := 1;
      fDBParams.pData := pointer(fParams);
    end;
    // 3. Execute SQL
    if fExpectResults then
    try
      // 3.1 SELECT will allow access to resulting rows data from fRowSet
      OleDBConnection.OleDBCheck(fCommand.Execute(nil,IID_IRowset,fDBParams,nil,@RowSet),ParamsStatus);
      FromRowSet(RowSet);
    except
      on E: Exception do begin
        CloseRowSet; // force fRowSet=nil
        raise;
      end;
    end else
      // 3.2 ExpectResults=false (e.g. SQL UPDATE) -> leave fRowSet=nil
      OleDBConnection.OleDBCheck(fCommand.Execute(nil,DB_NULLGUID,fDBParams,nil,nil));
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      raise;
    end;
  end;
end;

procedure TOleDBStatement.FromRowSet(RowSet: IRowSet);
begin
  if fRowSet<>nil then
    EOleDBException.Create('TOleDBStatement.FromRowSet twice');
  if not Assigned(RowSet) then
    exit; // no row returned
  fRowSet := RowSet;
  fRowSize := BindColumns(fRowSet as IColumnsInfo,fColumn,fColumnBindings);
  SetLength(fRowSetData,fRowSize);
  if fRowSize>RowBufferSize then
    RowBufferSize := fRowSize; // enforce at least one row in OleDB buffer
  SetLength(fRowStepHandles,RowBufferSize div fRowSize);
end;

procedure TOleDBStatement.FlushRowSetData;
var c: integer;
begin
  if fHasColumnValueInlined then
    for c := 0 to fColumnCount-1 do
      with fColumns[c] do
      if not ColumnValueInlined then // release DBTYPE_BYREF memory
      with PColumnValue(@fRowSetData[ColumnAttr])^ do
        if VWideChar<>nil then
          OleDBConnection.fMalloc.Free(VWideChar);
  fillchar(fRowSetData[0],fRowSize,0);
end;

function TOleDBStatement.Step(SeekFirst: boolean): boolean;
var Status: TCardinalDynArray;
    sav: integer;
begin
{  if not Assigned(fCommand) then
    raise EOleDBException.CreateFmt('%s.Execute should be called before Step',[ClassName]); }
  result := false;
  sav := fCurrentRow;
  fCurrentRow := 0;
  if not Assigned(fRowSet) or (fColumnCount=0) then 
    exit; // no row available at all (e.g. for SQL UPDATE) -> return false
  if fRowSetAccessor=0 then begin
    // first time called -> need to init accessor from fColumnBindings[]
    {$ifndef DELPHI5OROLDER}
    SynDBLog.Enter(self,'CreateAccessor');
    {$endif}
    SetLength(Status,fColumnCount);
    OleDBConnection.OleDBCheck((fRowSet as IAccessor).CreateAccessor(
      DBACCESSOR_ROWDATA or DBACCESSOR_OPTIMIZED,fColumnCount,
      pointer(fColumnBindings),fRowSize,fRowSetAccessor,pointer(Status)),Status);
    fRowStepHandleRetrieved := 0;
    fRowStepHandleCurrent := 0;
    fRowStepResult := 0;
  end else
  if SeekFirst then begin
    // rewind to first row
    ReleaseRows;
    OleDBConnection.OleDBCheck(fRowSet.RestartPosition(DB_NULL_HCHAPTER));
  end else
    FlushRowSetData;
  if fRowStepHandleCurrent>=fRowStepHandleRetrieved then begin
    ReleaseRows;
    if fRowStepResult=DB_S_ENDOFROWSET then
      exit; // no more row available -> return false
    fRowStepResult := fRowSet.GetNextRows(DB_NULL_HCHAPTER,0,length(fRowStepHandles),
      fRowStepHandleRetrieved,pointer(fRowStepHandles));
    OleDBConnection.OleDBCheck(fRowStepResult);
    fRowStepHandleCurrent := 0;
    if fRowStepHandleRetrieved=0 then
      exit; // no more row available
  end;
  // here we have always fRowStepHandleCurrent<fRowStepHandleRetrieved
  OleDBConnection.OleDBCheck(fRowSet.GetData(fRowStepHandles[fRowStepHandleCurrent],
    fRowSetAccessor,pointer(fRowSetData)));
  inc(fRowStepHandleCurrent);
  fCurrentRow := sav+1;
  inc(fTotalRowsRetrieved);
  result := true; // mark data available in fRowSetData
end;

destructor TOleDBStatement.Destroy;
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self);
  try
    {$ifndef DELPHI5OROLDER}
    Log.Log(sllDB,'Total rows = %',[TotalRowsRetrieved],self);
    {$endif}
    CloseRowSet;
  finally
    fCommand := nil;
    inherited;
  end;
end;

procedure TOleDBStatement.SetRowBufferSize(Value: integer);
begin
  if Value<4096 then
    Value := 4096;
  fRowBufferSize := Value;
end;

procedure TOleDBStatement.ReleaseRows;
begin
  FlushRowSetData;
  if fRowStepHandleRetrieved<>0 then begin
    fRowSet.ReleaseRows(fRowStepHandleRetrieved,Pointer(fRowStepHandles),nil,nil,nil);
    fRowStepHandleRetrieved := 0;
  end;
  fCurrentRow := 0;
end;

procedure TOleDBStatement.CloseRowSet;
begin
  if not Assigned(fRowSet) then
    exit;
  ReleaseRows;
  if fRowSetAccessor<>0 then begin
    (fRowSet as IAccessor).ReleaseAccessor(fRowSetAccessor,nil);
    fRowSetAccessor := 0;
  end;
  fRowSet := nil;
end;

function OleDBColumnToFieldType(wType: DBTYPE; bScale: byte): TSQLDBFieldType;
begin
  case wType of
  DBTYPE_EMPTY:
    result := ftUnknown;
  DBTYPE_NULL:
    result := ftNull;
  DBTYPE_I1, DBTYPE_I2, DBTYPE_I4, DBTYPE_I8,
  DBTYPE_UI1, DBTYPE_UI2, DBTYPE_UI4, DBTYPE_UI8, DBTYPE_BOOL:
    result := ftInt64;
  DBTYPE_CY:
    result := ftCurrency;
  DBTYPE_R4, DBTYPE_R8:
    result := ftDouble;
  DBTYPE_DECIMAL, DBTYPE_NUMERIC, DBTYPE_VARNUMERIC:
    case bScale of // number of digits to the right of the decimal point
         0: result := ftInt64;
      1..4: result := ftCurrency;
    else    result := ftDouble;
    end;
  DBTYPE_DATE, DBTYPE_DBDATE, DBTYPE_DBTIME, DBTYPE_FILETIME, DBTYPE_DBTIMESTAMP:
    result := ftDate;
  DBTYPE_BYTES, DBTYPE_UDT:
    result := ftBlob;
  else // all other types will be converted to text
    result := ftUtf8;
  end;
end;


function TOleDBStatement.BindColumns(ColumnInfo: IColumnsInfo;
  var Column: TDynArrayHashed; out Bindings: TDBBindingDynArray): integer;
const
  // column content is inlined up to 4 KB, otherwise will be stored as DBTYPE_BYREF
  MAXCOLUMNSIZE = 4000;
var i, len: integer;
    B: PDBBinding;
    Cols, nfo: PDBColumnInfo;
    Col: PSQLDBColumnProperty;
    nCols: cardinal;
    ColsNames: PWideChar;
    aName: RawUTF8;
begin
  OleDBConnection.OleDBCheck(ColumnInfo.GetColumnInfo(nCols,Cols,ColsNames));
  try
    nfo := Cols;
    SetLength(fColumnBindings,nCols);
    B := pointer(fColumnBindings);
    result := 0;
    for i := 1 to nCols do begin
      if nfo^.pwszName=nil then
        aName := 'col_'+Int32ToUTF8(i) else
        aName := RawUnicodeToUtf8(nfo^.pwszName,StrLenW(nfo^.pwszName));
      Col := fColumn.AddAndMakeUniqueName(aName); // set ColumnName := aName
      Col^.ColumnType := OleDBColumnToFieldType(nfo^.wType,nfo^.bScale);
      Col^.ColumnNonNullable := nfo^.dwFlags and DBCOLUMNFLAGS_MAYBENULL=0;
      Col^.ColumnAttr := result; // offset of status[-length]-value in fRowSetData[]
      Col^.ColumnValueInlined := true;
      B^.iOrdinal := nfo^.iOrdinal;
      B^.eParamIO := DBPARAMIO_NOTPARAM;
      B^.obStatus := result;
      inc(result,4);
      B^.wType := FIELDTYPE2OLEDB[Col^.ColumnType];
      case Col^.ColumnType of
      ftInt64, ftDouble, ftCurrency, ftDate: begin
        inc(result,4); // always force 8 bytes alignment = TColumnValue layout
        B^.dwPart := DBPART_STATUS or DBPART_VALUE;
        B^.obValue := result;
        B^.cbMaxLen := sizeof(Int64);
        inc(result,sizeof(Int64));
      end;
      ftUTF8, ftBlob: begin
        B^.dwPart := DBPART_STATUS or DBPART_VALUE or DBPART_LENGTH;
        B^.obLength := result; // need -length field in fRowSetData[]
        inc(result,4);
        B^.obValue := result;
        if cardinal(nfo^.ulColumnSize)<MAXCOLUMNSIZE then begin // inline up to 4 KB
          B^.wType :=  B^.wType and not DBTYPE_BYREF;
          Len := nfo^.ulColumnSize;
          Col^.ColumnValueDBSize := Len;
          if Col^.ColumnType=ftUTF8 then begin
            case nfo^.wType of         
            DBTYPE_STR, DBTYPE_BSTR, DBTYPE_WSTR:
              Len := Len*2; // ulColumnSize = WideChar count
            DBTYPE_GUID: Len := 78;
            else Len := 62; // 31 widechars will fit any type converted
            end;
            inc(Len,2); // reserve memory for trailing WideChar(#0)
          end;
          if AlignDataInternalBuffer then // 8 bytes alignment
            Len := ((Len-1) shr 3+1)shl 3;
          inc(result,Len);
          B^.cbMaxLen := Len;
        end else begin // get huge content by pointer (includes DBTYPE_BYREF)
          fHasColumnValueInlined := true;
          Col^.ColumnValueInlined := false;
          B^.cbMaxLen := sizeof(Pointer); // value=pointer in fRowSetData[]
          if AlignDataInternalBuffer then
            inc(result,8) else
            inc(result,sizeof(Pointer));
        end;
      end;
      else raise EOleDBException.CreateFmt(
             '%s.Execute: wrong column "%s" for "%s"',[ClassName,aName,fSQL]);
      end;
      inc(nfo);
      inc(B);
    end;
    assert((not AlignDataInternalBuffer) or (result and 7=0));
    assert(fColumnCount=integer(nCols));
  finally
    OleDBConnection.fMalloc.Free(Cols);
    OleDBConnection.fMalloc.Free(ColsNames);
  end;
end;


{ TOleDBConnection }

threadvar
  OleDBCoinitialized: integer;

procedure CoInit;
begin
  inc(OleDBCoInitialized);
  if OleDBCoInitialized=1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  assert(OleDBCoinitialized>0,'You should call TOleDBConnection.Free from the same '+
    'thread which called its Create: i.e. call MyProps.EndCurrentThread from an '+
    'THttpServerGeneric.OnHttpThreadTerminate event - see ticket 213544b2f5');
  dec(OleDBCoinitialized);
  if OleDBCoinitialized=0 then
    CoUninitialize;
end;

procedure TOleDBConnection.Connect;
var DataInitialize : IDataInitialize;
    unknown: IUnknown;
    Log: ISynLog;
begin
  Log := SynDBLog.Enter(self);
  // check context
  if Connected then
    Disconnect;
  if OleDBProperties.ConnectionString='' then
    raise EOleDBException.CreateFmt('%s.Connect excepts a ConnectionString',[ClassName]);
  try
    // retrieve initialization parameters from connection string
    OleCheck(CoCreateInstance(CLSID_MSDAINITIALIZE, nil, CLSCTX_INPROC_SERVER,
      IID_IDataInitialize, DataInitialize));
    OleCheck(DataInitialize.GetDataSource(nil,CLSCTX_INPROC_SERVER,
      pointer(OleDBProperties.ConnectionString),
      IID_IDBInitialize,IUnknown(fDBInitialize)));
    DataInitialize := nil;
    // open the connection to the DB
    OleDBCheck(fDBInitialize.Initialize);
    OnDBInitialized; // optionaly set parameters
    OleDBCheck((fDBInitialize as IDBCreateSession).CreateSession(nil,IID_IOpenRowset,fSession));
    // check if DB handle transactions
    if fSession.QueryInterface(IID_ITransactionLocal,unknown)=S_OK then
      fTransaction := unknown as ITransactionLocal else
      fTransaction := nil;
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      fSession := nil; // mark not connected
      fDBInitialize := nil;
      DataInitialize := nil;
      raise;
    end;
  end;
end;

constructor TOleDBConnection.Create(aProperties: TSQLDBConnectionProperties);
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self);
  if not aProperties.InheritsFrom(TOleDBConnectionProperties) then
    raise EOleDBException.CreateFmt('Invalid %s.Create',[ClassName]);
  Log.Log(sllInfo,aProperties);
  fOleDBProperties := TOleDBConnectionProperties(aProperties);
  inherited;
  CoInit;
  OleCheck(CoGetMalloc(1,fMalloc));
end;

destructor TOleDBConnection.Destroy;
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(self);
  try
    inherited Destroy; // call Disconnect;
    fMalloc := nil;
    CoUninit;
  except
    on E: Exception do
      Log.Log(sllError,E);
  end;
end;

procedure TOleDBConnection.Disconnect;
begin
  SynDBLog.Enter(self);
  if not Connected then
    exit;
  fTransaction := nil;
  fSession := nil;
  OleDBCheck(fDBInitialize.Uninitialize);
  fDBInitialize := nil;
end;

function TOleDBConnection.IsConnected: boolean;
begin
  result := (self<>nil) and (fSession<>nil);
end;

function TOleDBConnection.NewStatement: TSQLDBStatement;
begin
  result := TOleDBStatement.Create(self);
end;

procedure TOleDBConnection.OleDBCheck(aResult: HRESULT; const aStatus: TCardinalDynArray);
procedure EnhancedTest;
var ErrorInfo, ErrorInfoDetails: IErrorInfo;
    ErrorRecords: IErrorRecords;
    i: integer;
    Desc: WideString;
    ErrorCount: UINT;
    E: Exception;
    s: string;
begin
    // get OleDB specific error information
  GetErrorInfo(0,ErrorInfo);
  if Assigned(ErrorInfo) then begin
    ErrorRecords := ErrorInfo as IErrorRecords;
    ErrorRecords.GetRecordCount(ErrorCount);
    for i := 0 to ErrorCount-1 do
      if not Assigned(OleDBProperties.OnCustomError) or
         not OleDBProperties.OnCustomError(self,ErrorRecords,i) then begin
        // retrieve generic error info if OnCustomError() didn't handle it
        OleCheck(ErrorRecords.GetErrorInfo(i,GetSystemDefaultLCID,ErrorInfoDetails));
        OleCheck(ErrorInfoDetails.GetDescription(Desc));
        if fErrorMessage<>'' then
          fErrorMessage := fErrorMessage+'  ';
        fErrorMessage := fErrorMessage+string(Desc);
        ErrorInfoDetails := nil;
      end;
  end;
  // get generic HRESULT error
  if not Succeeded(aResult) or (fErrorMessage<>'') then begin
    s := SysErrorMessage(aResult);
    if s='' then
      s := 'OLEDB Error '+IntToHex(aResult,8);
    fErrorMessage := s+' - '+fErrorMessage;
  end;
  if fErrorMessage='' then
    exit;
  // retrieve binding information from Status[]
  s := '';
  for i := 0 to high(aStatus) do
    if TOleDBBindStatus(aStatus[i])<>bsOK then begin
      if aStatus[i]<=cardinal(high(TOleDBBindStatus)) then
        s := s+GetCaptionFromEnum(TypeInfo(TOleDBBindStatus),aStatus[i]) else
        s := s+IntToStr(aStatus[i]);
      s := Format(' Status[%d]=%s',[i,s]);
    end;
  if s<>'' then
    fErrorMessage :=  fErrorMessage+s;
  // raise exception
  E := EOleDBException.Create(fErrorMessage);
  SynDBLog.Add.Log(sllError,E);
  raise E;
end;
begin
  fErrorMessage := '';
  if not Succeeded(aResult) or Assigned(OleDBProperties.OnCustomError) then
    EnhancedTest;
end;

procedure TOleDBConnection.OnDBInitialized;
begin // do nothing by default
end;

procedure TOleDBConnection.Commit;
begin
  SynDBLog.Enter(self);
  if assigned(fTransaction) then begin
    inherited Commit;
    OleDbCheck(fTransaction.Commit(False,XACTTC_SYNC,0));
  end;
end;

procedure TOleDBConnection.Rollback;
begin
  SynDBLog.Enter(self);
  if assigned(fTransaction) then begin
    inherited Rollback;
    OleDbCheck(fTransaction.Abort(nil,False,False));
  end;
end;

procedure TOleDBConnection.StartTransaction;
begin
  SynDBLog.Enter(self);
  if assigned(fTransaction) then begin
    inherited StartTransaction;   
    OleDbCheck(fTransaction.StartTransaction(ISOLATIONLEVEL_READCOMMITTED,0,nil,nil));
  end;
end;


{ TOleDBConnectionProperties }

function TOleDBConnectionProperties.ConnectionStringDialogExecute(Parent: HWND): boolean;
var DataInitialize: IDataInitialize;
    DBPromptInitialize: IDBPromptInitialize;
    DBInitialize: IUnknown;
    res: HRESULT;
    tmp: PWideChar;
    Log: ISynLog;
begin
  Log := SynDBLog.Enter(self);
  result := false;
  if self<>nil then
  try
    CoInit; // if not already done
    try
      OleCheck(CoCreateInstance(CLSID_DATALINKS, nil, CLSCTX_INPROC_SERVER,
        IID_IDBPromptInitialize, DBPromptInitialize));
      OleCheck(CoCreateInstance(CLSID_MSDAINITIALIZE, nil, CLSCTX_INPROC_SERVER,
        IID_IDataInitialize, DataInitialize));
      if fConnectionString<>'' then
        DataInitialize.GetDataSource(nil,CLSCTX_INPROC_SERVER,Pointer(fConnectionString),
          IID_IDBInitialize,DBInitialize) else
        DBInitialize := nil;
      res := DBPromptInitialize.PromptDataSource(nil,Parent,DBPROMPTOPTIONS_PROPERTYSHEET,
        0,nil,nil,IID_IDBInitialize,DBInitialize);
      case res of
      S_OK: begin
        OleCheck(DataInitialize.GetInitializationString(DBInitialize,True,tmp));
        fConnectionString := tmp;
        if tmp<>nil then
          CoTaskMemFree(tmp);
        Log.Log(sllDB,'New connection settings set',self);
        result := true;
      end;
      DB_E_CANCELED:
        Log.Log(sllDB,'Canceled',self);
      else OleCheck(res);
      end;
    finally
      CoUninit;
    end;
  except
    on E: Exception do
      Log.Log(sllError,E);
  end;
end;

const
  CLASS_Catalog: TGUID = '{00000602-0000-0010-8000-00AA006D2EA4}';
  IID__Catalog: TGUID = '{00000603-0000-0010-8000-00AA006D2EA4}';

type
  _Catalog = interface(IDispatch)
    ['{00000603-0000-0010-8000-00AA006D2EA4}']
    function Get_Tables: OleVariant; safecall;
    function Get_ActiveConnection: OleVariant; safecall;
    procedure Set_ActiveConnection(pVal: OleVariant); safecall;
    procedure _Set_ActiveConnection(const pVal: IDispatch); safecall;
    function Get_Procedures: OleVariant; safecall;
    function Get_Views: OleVariant; safecall;
    function Get_Groups: OleVariant; safecall;
    function Get_Users: OleVariant; safecall;
    function Create(const ConnectString: WideString): OleVariant; safecall;
    function GetObjectOwner(const ObjectName: WideString; ObjectType: OleVariant;
                            ObjectTypeId: OleVariant): WideString; safecall;
    procedure SetObjectOwner(const ObjectName: WideString; ObjectType: OleVariant;
                             const UserName: WideString; ObjectTypeId: OleVariant); safecall;
  end;

function TOleDBConnectionProperties.CreateDatabase: boolean;
var Catalog: _Catalog;
    DB: OleVariant;
begin
  result := false;
  if ConnectionString<>'' then
  try
    CoInit;
    if Succeeded(CoCreateInstance(CLASS_Catalog, nil, CLSCTX_INPROC_SERVER,
      IID__Catalog, Catalog)) then
      try
        DB := Catalog.Create(ConnectionString);
        result := true;
      except
      end;
    {$ifdef DELPHI5OROLDER}
    SynDBLog.Add.Log(sllDB,'CreateDatabase returned '+Int32ToUTF8(ord(result)));
    {$else}
    SynDBLog.Add.Log(sllDB,'CreateDatabase for "%" returned %',[ConnectionString,ord(result)]);
    {$endif}
  finally
    DB := null;
    Catalog := nil;
    CoUninit;
  end;
end;

procedure TOleDBConnectionProperties.GetTableNames(var Tables: TRawUTF8DynArray);
var Rows: IRowset;
    count, schemaCol, nameCol: integer;
    schema, tablename: RawUTF8;
begin
  inherited; // first try from SQL, if any (faster)
  if Tables<>nil then
    exit; // already retrieved directly from engine
  try
    // see http://msdn.microsoft.com/en-us/library/ms716980(v=VS.85).aspx
    // Restriction columns: TABLE_CATALOG, TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE
    if GetSchema(DBSCHEMA_TABLES,['','','','TABLE'],Rows) then
    with TOleDBStatement.Create(MainConnection) do
    try
      FromRowSet(Rows);
      count := 0;
      schemaCol := ColumnIndex('TABLE_SCHEMA');
      nameCol := ColumnIndex('TABLE_NAME');
      if (schemaCol>=0) and (nameCol>=0) then
        while Step do begin
          schema := ColumnUTF8(schemaCol);
          tablename := ColumnUTF8(nameCol);
          if schema<>'' then
            tablename := schema+'.'+tablename;
          AddSortedRawUTF8(Tables,count,tableName);
        end;
      SetLength(Tables,count);
    finally
      Free;
    end;
  except
    on Exception do
      SetLength(Tables,0);
  end;
end;

procedure TOleDBConnectionProperties.GetFields(const aTableName: RawUTF8;
  var Fields: TSQLDBColumnDefineDynArray);
var Owner, Table, Column: RawUTF8;
    Rows: IRowset;
    n, i: integer;
    F: TSQLDBColumnDefine;
    FA: TDynArray;
const DBTYPE_DISPLAY: array[TSQLDBFieldType] of RawUTF8 = (
  '???','null','int','double','currency','date','nvarchar','blob');
begin
  inherited; // first try from SQL, if any (faster)
  if Fields<>nil then
    exit; // already retrieved directly from engine
  try
    Split(aTableName,'.',Owner,Table);
    if Table='' then begin
      Table := Owner;
      Owner := '';
    end;
    // see http://msdn.microsoft.com/en-us/library/ms723052(v=VS.85).aspx
    if GetSchema(DBSCHEMA_COLUMNS,['',Owner,Table,''],Rows) then
      // Restriction columns: TABLE_CATALOG,TABLE_SCHEMA,TABLE_NAME,COLUMN_NAME
      with TOleDBStatement.Create(MainConnection) do
      try
        FromRowSet(Rows);
        FA.Init(TypeInfo(TSQLDBColumnDefineDynArray),Fields,@n);
        while Step do begin
          F.ColumnName := ColumnUTF8('COLUMN_NAME');
          F.ColumnLength := ColumnInt('CHARACTER_MAXIMUM_LENGTH');
          F.ColumnPrecision := ColumnInt('NUMERIC_PRECISION');
          F.ColumnScale := ColumnInt('NUMERIC_SCALE');
          F.ColumnType:= OleDBColumnToFieldType(ColumnInt('DATA_TYPE'),F.ColumnScale);
          F.ColumnTypeNative := DBTYPE_DISPLAY[F.ColumnType];
          FA.Add(F);
        end;
        SetLength(Fields,n);
      finally
        Free;
      end;
  // now we have Fields[] with the column information -> get indexes and foreign keys
  if GetSchema(DBSCHEMA_INDEXES,['',Owner,'','',Table],Rows) then
    // Restriction columns: TABLE_CATALOG,TABLE_SCHEMA,INDEX_NAME,TYPE,TABLE_NAME
    with TOleDBStatement.Create(MainConnection) do
    try
      FromRowSet(Rows);
      while Step do begin
        Column := ColumnUTF8('COLUMN_NAME');
        for i := 0 to high(Fields) do
        with Fields[i] do
        if IdemPropNameU(ColumnName,Column) then begin
          ColumnIndexed := true;
          break;
        end;
      end;
    finally
      Free;
    end;
  except
    on Exception do
      SetLength(Fields,0);
  end;
end;

procedure TOleDBConnectionProperties.GetForeignKeys;
var Rows: IRowset;
begin // retrieve all foreign keys into fForeignKeys list
  try
    if GetSchema(DBSCHEMA_FOREIGN_KEYS,['','','','','',''],Rows) then
    // PK_TABLE_CATALOG,PK_TABLE_SCHEMA,PK_TABLE_NAME,FK_TABLE_CATALOG,FK_TABLE_SCHEMA,FK_TABLE_NAME
    with TOleDBStatement.Create(MainConnection) do
    try
      FromRowSet(Rows);
      while Step do
        fForeignKeys.Add(
          ColumnUTF8('FK_TABLE_SCHEMA')+'.'+ColumnUTF8('FK_TABLE_NAME')+'.'+ColumnUTF8('FK_COLUMN_NAME'),
          ColumnUTF8('PK_TABLE_SCHEMA')+'.'+ColumnUTF8('PK_TABLE_NAME')+'.'+ColumnUTF8('PK_COLUMN_NAME'));
    finally
      Free;
    end;
  except
    on Exception do ; // just ignore errors here
  end;
end;

function TOleDBConnectionProperties.GetSchema(const aUID: TGUID;
  const Fields: array of RawUTF8; var aResult: IRowset): boolean;
var i, res, n: integer;
    C: TOleDBConnection;
    SRS: IDBSchemaRowset;
    PG, OG: PGUID;
    PI, OI: PInteger;
    Args: array of Variant;
begin
  result := false;
  if (self=nil) or (high(Fields)<0) then
    exit;
  C := MainConnection as TOleDBConnection;
  if C.fSession=nil then
    C.Connect;
  C.fSession.QueryInterface(IDBSchemaRowset,SRS);
  if not Assigned(SRS) then
    exit; // provider do not support this interface
  if fSchemaRec=nil then begin
    SRS.GetSchemas(n,OG,OI);
    if n>0 then
    try
      SetLength(fSchemaRec,n);
      PG := OG;
      PI := OI;
      for i := 0 to n-1 do
      with fSchemaRec[i] do begin
        SchemaGuid := PG^;
        SupportedRestrictions := PI^;
        inc(PG);
        inc(PI);
      end;
    finally
      C.fMalloc.Free(OG);
      C.fMalloc.Free(OI);
    end;
  end;
  res := 0;
  for i := 0 to high(fSchemaRec) do
    if IsEqualGuid(fSchemaRec[i].SchemaGuid,aUID) then begin
      res := fSchemaRec[i].SupportedRestrictions;
      break;
    end;
  if res=0 then
    exit;
  SetLength(Args,length(Fields));
  for i := 0 to high(Fields) do
  if res and (1 shl i)<>0 then
    if Fields[i]<>'' then // '' will leave VT_EMPTY parameter = no restriction
      Args[i] := UTF8ToWideString(Fields[i]); // expect parameter as BSTR
  aResult := nil;
  try
    C.OleDBCheck(SRS.GetRowset(nil,aUID,length(Args),Args,IID_IRowset,0,nil,aResult));
    result := aResult<>nil; // mark some rows retrieved
  except
    on E: Exception do
      result := false;
  end;
end;

function TOleDBConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TOleDBConnection.Create(self);
end;

procedure TOleDBConnectionProperties.SetInternalProperties;
var tmp: RawUTF8;
begin
  if fProviderName<>'' then
    tmp := 'Provider='+fProviderName+';';
  if fServerName<>'' then
    tmp := tmp+'Data Source='+fServerName+';';
  if fDatabaseName<>'' then
    tmp := tmp+'Initial Catalog='+fDatabaseName+';';
  fConnectionString := UTF8ToSynUnicode(tmp+'User Id='+fUserID+';Password='+fPassWord+';');
end;

function TOleDBConnectionProperties.ColumnTypeNativeToDB(
  const aNativeType: RawUTF8; aScale: integer): TSQLDBFieldType;
begin
  result := OleDBColumnToFieldType(GetInteger(pointer(aNativeType)),aScale)
end;


{ TOleDBOracleConnectionProperties }

procedure TOleDBOracleConnectionProperties.SetInternalProperties;
begin
  if fProviderName='' then
    fProviderName := 'OraOLEDB.Oracle.1';
  fDBMS := dOracle;
  inherited SetInternalProperties;
end;


{ TOleDBMSOracleConnectionProperties }

procedure TOleDBMSOracleConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MSDAORA';
  fDBMS := dOracle;
  inherited SetInternalProperties;
end;

{ TOleDBMSSQLConnectionProperties }

type
  /// to retrieve enhanced Microsoft SQL Server error information
  PSSERRORINFO = ^SSERRORINFO;
  SSERRORINFO = packed record
    pwszMessage: PWideChar;
    pwszServer: PWideChar;
    pwszProcedure: PWideChar;
    lNative: cardinal;
    bState: byte;
    bClass: byte;
    wLineNumber: word;
  end;
  ISQLServerErrorInfo = interface(IUnknown)
    ['{5CF4CA12-EF21-11d0-97E7-00C04FC2AD98}']
    function GetErrorInfo(out ppErrorInfo: PSSERRORINFO;
                          out Error: PWideChar): HResult; stdcall;
  end;

const
  IID_ISQLServerErrorInfo: TGUID = '{5CF4CA12-EF21-11d0-97E7-00C04FC2AD98}';

function TOleDBMSSQLConnectionProperties.MSOnCustomError(Connection: TOleDBConnection;
  ErrorRecords: IErrorRecords; RecordNum: UINT): boolean;
var SQLServerErrorInfo: ISQLServerErrorInfo;
    SSErrorInfo: PSSERRORINFO;
    SSErrorMsg: PWideChar;
    msg, tmp: string;
begin
  result := False;
  if (self=nil) or (Connection=nil) then
    exit;
  ErrorRecords.GetCustomErrorObject(RecordNum,IID_ISQLServerErrorInfo,
    IUnknown(SQLServerErrorInfo));
  if Assigned(SQLServerErrorInfo) then
  try
    if (SQLServerErrorInfo.GetErrorInfo(SSErrorInfo,SSErrorMsg)=S_OK) and
       (SSErrorInfo<>nil) then
    with SSErrorInfo^ do
    try
      msg := UnicodeBufferToString(pwszMessage)+#13#10;
      if bClass<=10 then begin
        SynDBLog.Add.Log(sllDB,RawUTF8(msg),self);
        Connection.fInfoMessage := Connection.fInfoMessage+msg;
      end else begin
        if pwszProcedure<>nil then
          tmp := UnicodeBufferToString(pwszProcedure) else
          tmp := 'Error '+IntToStr(lNative);
        Connection.fErrorMessage := Connection.fErrorMessage+
          tmp+' (line '+IntToStr(wLineNumber)+'): '+msg;
      end;
    finally
      Connection.fMalloc.Free(SSErrorInfo);
      Connection.fMalloc.Free(SSErrorMsg);
    end;
    result := true;
  finally
    SQLServerErrorInfo := nil;
  end;
end;

procedure TOleDBMSSQLConnectionProperties.SetInternalProperties;
begin
  OnCustomError := MSOnCustomError;
  fProviderName := 'SQLOLEDB';
  fDBMS := dMSSQL;
  inherited SetInternalProperties;
  if fUserID='' then
    fConnectionString := fConnectionString+
      'Integrated Security=SSPI;Persist Security Info=False;';
end;


{ TOleDBODBCSQLConnectionProperties }

constructor TOleDBODBCSQLConnectionProperties.Create(const aDriver,
  aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
begin
  fDriver := aDriver;
  inherited Create(aServerName,aDatabaseName,aUserID,aPassWord);
end;

procedure TOleDBODBCSQLConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MSDASQL'; // we could have left it void - never mind
  inherited SetInternalProperties;
  if fDriver<>'' then
    fConnectionString := UTF8ToSynUnicode('Driver='+fDriver+';')+fConnectionString;
end;

{ TOleDBMySQLConnectionProperties }

procedure TOleDBMySQLConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'MYSQLPROV';
  fDBMS := dMySQL;
  inherited;
end;

{ TOleDBAS400ConnectionProperties }

procedure TOleDBAS400ConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'IBMDA400.DataSource.1';
  inherited SetInternalProperties;
end;

{ TOleDBJetConnectionProperties }

procedure TOleDBJetConnectionProperties.SetInternalProperties;
begin
  fProviderName := 'Microsoft.Jet.OLEDB.4.0';
  fDBMS := dJet;
  inherited SetInternalProperties;
  if not FileExists(UTF8ToString(ServerName)) then
    CreateDatabase;
end;


initialization
  assert(sizeof(TOleDBStatementParam)=sizeof(PTrUInt)*4+sizeof(Int64));

finalization
  {$ifndef DELPHI5OROLDER}
  if OleDBCoinitialized<>0 then
    SynDBLog.Add.Log(sllError,'Missing TOleDBConnection.Destroy call = %',
      OleDBCoInitialized);
  {$endif}
end.
