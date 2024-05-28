unit dbstructures;

// Column structures, dll loading
// For server constants, variables and data types see dbstructures.XXX.pas

interface

uses
  gnugettext, Vcl.Graphics, Winapi.Windows, System.SysUtils;


type

  // Column types
  TDBDatatypeIndex = (dbdtTinyint, dbdtSmallint, dbdtMediumint, dbdtInt, dbdtUint, dbdtBigint, dbdtSerial, dbdtBigSerial,
    dbdtFloat, dbdtDouble, dbdtDecimal, dbdtNumeric, dbdtReal, dbdtDoublePrecision, dbdtMoney, dbdtSmallmoney,
    dbdtDate, dbdtTime, dbdtYear, dbdtDatetime, dbdtDatetime2, dbdtDatetimeOffset, dbdtSmalldatetime, dbdtTimestamp, dbdtInterval,
    dbdtChar, dbdtNchar, dbdtVarchar, dbdtNvarchar, dbdtTinytext, dbdtText, dbdtCiText, dbdtNtext, dbdtMediumtext, dbdtLongtext,
    dbdtJson, dbdtJsonB, dbdtCidr, dbdtInet, dbdtMacaddr,
    dbdtBinary, dbdtVarbinary, dbdtTinyblob, dbdtBlob, dbdtMediumblob, dbdtLongblob, dbdtImage,
    dbdtEnum, dbdtSet, dbdtBit, dbdtVarBit, dbdtBool, dbdtRegClass, dbdtRegProc, dbdtUnknown,
    dbdtCursor, dbdtSqlvariant, dbdtTable, dbdtUniqueidentifier, dbdtHierarchyid, dbdtXML,
    dbdtPoint, dbdtLinestring, dbdtLineSegment, dbdtPolygon, dbdtGeometry, dbdtBox, dbdtPath, dbdtCircle, dbdtMultipoint, dbdtMultilinestring, dbdtMultipolygon, dbdtGeometrycollection
    );

  // Column type categorization
  TDBDatatypeCategoryIndex = (dtcInteger, dtcReal, dtcText, dtcBinary, dtcTemporal, dtcSpatial, dtcOther);

  // Column type structure
  TDBDatatype = record
    Index:           TDBDatatypeIndex;
    NativeType:      Integer; // MySQL column type constant (e.g. 1 = TINYINT). See include/mysql.h.pp.
    NativeTypes:     String;  // Same as above, but for multiple ids (e.g. PostgreSQL oids). Prefer over NativeType. See GetDatatypeByNativeType.
    Name:            String;
    Names:           String;
    Description:     String;
    HasLength:       Boolean; // Can have Length- or Set-attribute?
    RequiresLength:  Boolean; // Must have a Length- or Set-attribute?
    MaxSize:         Int64;
    DefaultSize:     Int64;   // TEXT and BLOB allow custom length, but we want to leave the default max length away from ALTER TABLE's
    HasBinary:       Boolean; // Can be binary?
    HasDefault:      Boolean; // Can have a default value?
    LoadPart:        Boolean; // Select per SUBSTR() or LEFT()
    DefLengthSet:    String;  // Should be set for types which require a length/set
    Format:          String;  // Used for date/time values when displaying and generating queries
    ValueMustMatch:  String;
    Category:        TDBDatatypeCategoryIndex;
  end;

  // Column type category structure
  TDBDatatypeCategory = record
    Index:           TDBDatatypeCategoryIndex;
    Name:            String;
    Color:           TColor;
    NullColor:       TColor;
  end;

  // Server variables
  TVarScope = (vsGlobal, vsSession, vsBoth);
  TServerVariable = record
    Name: String;
    IsDynamic: Boolean;
    VarScope: TVarScope;
    EnumValues: String;
  end;

  // Custom exception class for any connection or database related error
  EDbError = class(Exception)
    private
      FErrorCode: Cardinal;
      FHint: String;
    public
      property ErrorCode: Cardinal read FErrorCode;
      property Hint: String read FHint;
      constructor Create(const Msg: string; const ErrorCode: Cardinal=0; const Hint: String='');
  end;

  // DLL loading
  TDbLib = class(TObject)
    const
      LIB_PROC_ERROR: Cardinal = 1000;
    private
      FDllFile: String;
      FHandle: HMODULE;
    protected
      procedure AssignProc(var Proc: FARPROC; Name: PAnsiChar; Mandantory: Boolean=True);
      procedure AssignProcedures; virtual; abstract;
    public
      property Handle: HMODULE read FHandle;
      property DllFile: String read FDllFile;
      constructor Create(DllFile, DefaultDll: String); virtual;
      destructor Destroy; override;
  end;


var

  // Column type categories
  DatatypeCategories: array[TDBDatatypeCategoryIndex] of TDBDatatypeCategory = (
    (
      Index:           dtcInteger;
      Name:            'Integer'
    ),
    (
      Index:           dtcReal;
      Name:            'Real'
    ),
    (
      Index:           dtcText;
      Name:            'Text'
    ),
    (
      Index:           dtcBinary;
      Name:            'Binary'
    ),
    (
      Index:           dtcTemporal;
      Name:            'Temporal (time)'
    ),
    (
      Index:           dtcSpatial;
      Name:            'Spatial (geometry)'
    ),
    (
      Index:           dtcOther;
      Name:            'Other'
    )
  );



implementation

uses apphelpers;



{ EDbError }

constructor EDbError.Create(const Msg: string; const ErrorCode: Cardinal=0; const Hint: String='');
begin
  FErrorCode := ErrorCode;
  FHint := Hint;
  inherited Create(Msg);
end;



{ TDbLib }

constructor TDbLib.Create(DllFile, DefaultDll: String);
var
  msg, ErrorHint: String;
begin
  // Load DLL as is (with or without path)
  inherited Create;
  FDllFile := DllFile;
  if not FileExists(FDllFile) then begin
    msg := f_('File does not exist: %s', [FDllFile]) +
      sLineBreak + sLineBreak +
      f_('Please launch %s from the directory where you have installed it. Or just reinstall %s.', [APPNAME, APPNAME]
      );
    Raise EdbError.Create(msg);
  end;

  FHandle := LoadLibrary(PWideChar(FDllFile));
  if FHandle = 0 then begin
    msg := f_('Library %s could not be loaded. Please select a different one.',
      [ExtractFileName(FDllFile)]
      );
    if GetLastError <> 0 then begin
      msg := msg + sLineBreak + sLineBreak + f_('Internal error %d: %s', [GetLastError, SysErrorMessage(GetLastError)]);
    end;
    if (DefaultDll <> '') and (ExtractFileName(FDllFile) <> DefaultDll) then begin
      ErrorHint := f_('You could try the default library %s in your session settings. (Current: %s)',
        [DefaultDll, ExtractFileName(FDllFile)]
        );
    end else begin
      ErrorHint := '';
    end;
    Raise EDbError.Create(msg, GetLastError, ErrorHint);
  end;

  // Dll was loaded, now initialize required procedures
  AssignProcedures;
end;


destructor TDbLib.Destroy;
begin
  if FHandle <> 0 then begin
    FreeLibrary(FHandle);
    FHandle := 0;
  end;
  inherited;
end;


procedure TDbLib.AssignProc(var Proc: FARPROC; Name: PAnsiChar; Mandantory: Boolean=True);
var
  msg: String;
begin
  // Map library procedure to internal procedure
  Proc := GetProcAddress(FHandle, Name);
  if Proc = nil then begin
    if Mandantory then begin
      msg := f_('Library error in %s: Could not find procedure address for "%s"',
        [ExtractFileName(FDllFile), Name]
        );
      if GetLastError <> 0 then
        msg := msg + sLineBreak + sLineBreak + f_('Internal error %d: %s', [GetLastError, SysErrorMessage(GetLastError)]);
      Raise EDbError.Create(msg, LIB_PROC_ERROR);
    end;
  end;
end;


end.
