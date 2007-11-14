{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Unidatabase UpdateSQL component             }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZSqlUpdate;

interface

{$I ZComponent.inc}

uses
  SysUtils, Classes, DB, ZDbcIntfs, ZDbcCachedResultSet, ZDbcCache, ZSqlStrings;

type
  {ADDED BY fduenas}
  TZBeforeSQLStatementEvent = procedure(const Sender: TObject;
    StatementIndex: Integer; out Execute: Boolean ) of object;

  TZAfterSQLStatementEvent = procedure(const Sender: TObject;
    StatementIndex: Integer) of object;

  TZAfterInsertSQLStatementEvent = procedure(const Sender: TObject;
    StatementIndex: Integer; out UpdateAutoIncFields: Boolean ) of object;

  {**
    Implements an object which manages SQL DML statements to update TDatasets.
  }
  TZUpdateSQL = class(TComponent, IZCachedResolver)
  private
    FDataSet: TDataSet;

    FDeleteSQL: TZSQLStrings;
    FInsertSQL: TZSQLStrings;
    FModifySQL: TZSQLStrings;
    //FOSPATCH
    FRefreshSQL: TZSQLStrings;
    //FOSPATCH

    FParamCheck: Boolean;
    FParams: TParams;
    FMultiStatements: Boolean;
    FBeforeDeleteSQL: TNotifyEvent;
    FBeforeInsertSQL: TNotifyEvent;
    FBeforeModifySQL: TNotifyEvent;
    FAfterDeleteSQL: TNotifyEvent;
    FAfterInsertSQL: TNotifyEvent;
    FAfterModifySQL: TNotifyEvent;
    FUseSequenceFieldForRefreshSQL: Boolean;
    {New Statement Events added by Fduenas}
    FBeforeDeleteSQLStatement: TZBeforeSQLStatementEvent;
    FAfterDeleteSQLStatement: TZAfterSQLStatementEvent;
    FBeforeInsertSQLStatement: TZBeforeSQLStatementEvent;
    FAfterInsertSQLStatement: TZAfterInsertSQLStatementEvent;
    FBeforeModifySQLStatement: TZBeforeSQLStatementEvent;
    FAfterModifySQLStatement: TZAfterSQLStatementEvent;

    procedure SetUseSequenceFieldForRefreshSQL(const Value: Boolean);
    procedure SetDataset(Value: TDataset);
    function GetSQL(UpdateKind: TUpdateKind): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
    function GetParamsCount: Word;
    procedure SetParamsList(Value: TParams);
    procedure SetParamCheck(Value: Boolean);
    procedure SetMultiStatements(Value: Boolean);

    function GetDeleteSQL: TStrings;
    procedure SetDeleteSQL(Value: TStrings);
    function GetInsertSQL: TStrings;
    procedure SetInsertSQL(Value: TStrings);
    function GetModifySQL: TStrings;
    procedure SetModifySQL(Value: TStrings);

    //FOSPATCH
    function GetRefreshSQL: TStrings;
    procedure SetRefreshSQL(Value: TStrings);
    //FOSPATCH

    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);

  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure CalculateDefaults(Sender: IZCachedResultSet;
      RowAccessor: TZRowAccessor);
    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor);
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver);
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure Rebuild(SQLStrings: TZSQLStrings);
    procedure RebuildAll;
    procedure FillStatement(ResultSet: IZCachedResultSet;
      Statement: IZPreparedStatement; Config: TZSQLStatement;
      OldRowAccessor, NewRowAccessor: TZRowAccessor);
    procedure UpdateParams(Sender: TObject);

    procedure DoBeforeDeleteSQL;
    procedure DoBeforeInsertSQL;
    procedure DoBeforeModifySQL;
    procedure DoAfterDeleteSQL;
    procedure DoAfterInsertSQL;
    procedure DoAfterModifySQL;

    procedure DoBeforeDeleteSQLStatement(const Sender: TObject;
      StatementIndex: Integer; out Execute: Boolean);
    procedure DoBeforeInsertSQLStatement(const Sender: TObject;
      StatementIndex: Integer; out Execute: Boolean);
    procedure DoBeforeModifySQLStatement(const Sender: TObject;
      StatementIndex: Integer; out Execute: Boolean);
    procedure DoAfterDeleteSQLStatement(const Sender: TObject;
      StatementIndex: Integer);
    procedure DoAfterInsertSQLStatement(const Sender: TObject;
      StatementIndex: Integer; out UpdateAutoIncFields: Boolean) ;
    procedure DoAfterModifySQLStatement(const Sender: TObject;
      StatementIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
    property ParamCount: Word read GetParamsCount;
    property DataSet: TDataSet read FDataSet write SetDataSet;

  published
    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;
    //FOSPATCH
    property RefreshSQL: TStrings read GetRefreshSQL write SetRefreshSQL;
    //FOSPATCH
    property UseSequenceFieldForRefreshSQL:Boolean read FUseSequenceFieldForRefreshSQL write SetUseSequenceFieldForRefreshSQL;


    property Params: TParams read FParams write SetParamsList stored False;
    property ParamCheck: Boolean read FParamCheck write SetParamCheck default True;
    property MultiStatements: Boolean read FMultiStatements write SetMultiStatements default True;

    property BeforeDeleteSQL: TNotifyEvent
      read FBeforeDeleteSQL write FBeforeDeleteSQL;
    property BeforeInsertSQL: TNotifyEvent
      read FBeforeInsertSQL write FBeforeInsertSQL;
    property BeforeModifySQL: TNotifyEvent
      read FBeforeModifySQL write FBeforeModifySQL;
    property AfterDeleteSQL: TNotifyEvent
      read FAfterDeleteSQL write FAfterDeleteSQL;
    property AfterInsertSQL: TNotifyEvent
      read FAfterInsertSQL write FAfterInsertSQL;
    property AfterModifySQL: TNotifyEvent
      read FAfterModifySQL write FAfterModifySQL;

    {New Events Fired by executed Statement}
    property BeforeDeleteSQLStatement: TZBeforeSQLStatementEvent
      read FBeforeDeleteSQLStatement write FBeforeDeleteSQLStatement;
    property BeforeInsertSQLStatement: TZBeforeSQLStatementEvent
      read FBeforeInsertSQLStatement write FBeforeInsertSQLStatement;
    property BeforeModifySQLStatement: TZBeforeSQLStatementEvent
      read FBeforeModifySQLStatement write FBeforeModifySQLStatement;
    property AfterDeleteSQLStatement: TZAfterSQLStatementEvent
      read FAfterDeleteSQLStatement write FAfterDeleteSQLStatement;
    property AfterInsertSQLStatement: TZAfterInsertSQLStatementEvent
      read FAfterInsertSQLStatement write FAfterInsertSQLStatement;
    property AfterModifySQLStatement: TZAfterSQLStatementEvent
      read FAfterModifySQLStatement write FAfterModifySQLStatement;
  end;

implementation

uses ZTokenizer, ZGenericSqlToken, ZDatasetUtils, ZAbstractRODataset,ZAbstractDataset,
  ZSysUtils, ZDbcUtils,ZMessages;

{ TZUpdateSQL }

{**
  Constructs this object and assignes main properties.
  @param AOwner a component owner.
}
constructor TZUpdateSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDeleteSQL := TZSQLStrings.Create;
  FDeleteSQL.OnChange := UpdateParams;
  FInsertSQL := TZSQLStrings.Create;
  FInsertSQL.OnChange := UpdateParams;
  FModifySQL := TZSQLStrings.Create;
  FModifySQL.OnChange := UpdateParams;

//FOSPATCH
  FRefreshSQL := TZSQLStrings.Create;
  FRefreshSQL.OnChange:= UpdateParams;
//FOSPATCH

  FParams := TParams.Create(Self);
  FParamCheck := True;
  FMultiStatements := True;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZUpdateSQL.Destroy;
begin
  FParams.Free;
  FDeleteSQL.Free;
  FInsertSQL.Free;
  FModifySQL.Free;
  FRefreshSQL.Free;
  
  inherited Destroy;
end;

{**
  Store the related dataset object for update sql editor
}
procedure TZUpdateSQL.SetDataset(Value: TDataset);
begin
  FDataSet := Value;
  FDeleteSQL.Dataset := Value;
  FInsertSQL.Dataset := Value;
  FModifySQL.Dataset := Value;
end;

{**
  Gets a DML statements for specified action.
  @param UpdateKind a type of the DML statements.
  @return a stored DML statement.
}
function TZUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  case UpdateKind of
    ukModify: Result := FModifySQL;
    ukInsert: Result := FInsertSQL;
    else Result := FDeleteSQL;
  end;
end;

{**
  Sets a DML statements for specified action.
  @param UpdateKind a type of the DML statements.
  @param Value a DML statements to be set.
}
procedure TZUpdateSQL.SetSQL(UpdateKind: TUpdateKind; Value: TStrings);
begin
  case UpdateKind of
    ukModify: FModifySQL.Assign(Value);
    ukInsert: FInsertSQL.Assign(Value);
    ukDelete: FDeleteSQL.Assign(Value);
  end;
end;

{**
  Get parameters count.
  @return a parameters count.
}
function TZUpdateSQL.GetParamsCount: Word;
begin
  Result := FParams.Count;
end;

function TZUpdateSQL.GetRefreshSQL: TStrings;
begin
  Result := FRefreshSQL;
end;

{**
  Sets parameters checking flag.
  @param Value a new parameters checking flag.
}
procedure TZUpdateSQL.SetParamCheck(Value: Boolean);
begin
  if FParamCheck <> Value then
  begin
    FParamCheck := Value;
    FModifySQL.ParamCheck := Value;
    FInsertSQL.ParamCheck := Value;
    FDeleteSQL.ParamCheck := Value;
    RebuildAll;
  end;
end;

{**
  Sets multiple statements flag.
  @param Value a new multiple statements flag.
}
procedure TZUpdateSQL.SetMultiStatements(Value: Boolean);
begin
  if FMultiStatements <> Value then
  begin
    FMultiStatements := Value;
    FModifySQL.MultiStatements := Value;
    FInsertSQL.MultiStatements := Value;
    FDeleteSQL.MultiStatements := Value;
    RebuildAll;
  end;
end;

{**
  Set a new list of SQL parameters.
  @param Value a new list of SQL parameters.
}
procedure TZUpdateSQL.SetParamsList(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

procedure TZUpdateSQL.SetRefreshSQL(Value: TStrings);
begin
  FRefreshSQL.Assign(Value);
end;

procedure TZUpdateSQL.SetUseSequenceFieldForRefreshSQL(const Value: Boolean);
begin
  FUseSequenceFieldForRefreshSQL := Value;
end;

{**
  Defines a persistent dataset properties.
  @param Filer a persistent manager object.
}
procedure TZUpdateSQL.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TZUpdateSQL(Filer.Ancestor).FParams)
    else Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

{**
  Reads parameter data from persistent storage.
  @param Reader an input data stream.
}
procedure TZUpdateSQL.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

{**
  Writes parameter data from persistent storage.
  @param Writer an output data stream.
}
procedure TZUpdateSQL.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{**
  Gets strings with Delete statements.
  @return strings with Delete statements.
}
function TZUpdateSQL.GetDeleteSQL: TStrings;
begin
  Result := FDeleteSQL;
end;

{**
  Sets a new Delete SQL statement.
  @param Value a new Delete SQL statement.
}
procedure TZUpdateSQL.SetDeleteSQL(Value: TStrings);
begin
  FDeleteSQL.Assign(Value);
end;

{**
  Gets strings with Insert statements.
  @return strings with Insert statements.
}
function TZUpdateSQL.GetInsertSQL: TStrings;
begin
  Result := FInsertSQL;
end;

{**
  Sets a new Insert SQL statement.
  @param Value a new Insert SQL statement.
}
procedure TZUpdateSQL.SetInsertSQL(Value: TStrings);
begin
  FInsertSQL.Assign(Value);
end;

{**
  Gets strings with Modify statements.
  @return strings with Modify statements.
}
function TZUpdateSQL.GetModifySQL: TStrings;
begin
  Result := FModifySQL;
end;

{**
  Sets a new  Modify SQL statement.
  @param Value a new Modify SQL statement.
}
procedure TZUpdateSQL.SetModifySQL(Value: TStrings);
begin
  FModifySQL.Assign(Value);
end;

{**
  Updates all parameters.
  @param Sender an event sender object.
}
procedure TZUpdateSQL.UpdateParams(Sender: TObject);
begin
  RebuildAll;
end;

{**
  Rebuilds parameters and inserts a new one from specified sql statements.
  @param SQLStrings a strings with SQL statements.
}
procedure TZUpdateSQL.Rebuild(SQLStrings: TZSQLStrings);
var
  I: Integer;
begin
  for I := 0 to SQLStrings.ParamCount - 1 do
  begin
    if FParams.FindParam(SQLStrings.ParamNames[I]) = nil then
      FParams.CreateParam(ftUnknown, SQLStrings.ParamNames[I], ptUnknown);
  end;
end;

{**
  Rebuilds all internal structures including parameters from SQL statements.
}
procedure TZUpdateSQL.RebuildAll;
var
  OldParams: TParams;
begin
  OldParams := TParams.Create;
  OldParams.Assign(FParams);
  FParams.Clear;
  try
    Rebuild(FModifySQL);
    Rebuild(FInsertSQL);
    Rebuild(FDeleteSQL);
//FOSPATCH
    Rebuild(FRefreshSQL);
//FOSPATCH
    FParams.AssignValues(OldParams);
  finally
    OldParams.Free;
  end;
end;

{**
  Fills the specified statement with stored or given parameters.
  @param ResultSet a source result set object.
  @param Statement a DBC statement object.
  @param Config a SQLStatement configuration.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZUpdateSQL.FillStatement(ResultSet: IZCachedResultSet;
  Statement: IZPreparedStatement; Config: TZSQLStatement;
  OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  I, ColumnIndex: Integer;
  ParamValue: TParam;
  ParamName: string;
  OldParam: Boolean;
//  SeqParam: Boolean;
  WasNull: Boolean;
  RowAccessor: TZRowAccessor;
  Stream: TStream;
  TempBlob: IZBlob;
begin
  for I := 0 to Config.ParamCount - 1 do begin
    ParamValue := Params.FindParam(Config.ParamNames[I]);
    ParamName := Config.ParamNames[I];
    OldParam := False;{Seqparam:=False;}
    if StrLIComp(PChar(ParamName), 'NEW_', 4) = 0 then begin
      ParamName := Copy(ParamName, 5, Length(ParamName) - 4)
    end else
    if StrLIComp(PChar(ParamName), 'OLD_', 4) = 0 then begin
      ParamName := Copy(ParamName, 5, Length(ParamName) - 4);
      OldParam := True;
    end;

    ColumnIndex := ResultSet.FindColumn(ParamName);
    if ColumnIndex > 0 then begin
      if OldParam then
        RowAccessor := OldRowAccessor
      else RowAccessor := NewRowAccessor;

      if StrToBoolEx(DefineStatementParameter(
        ResultSet.GetStatement, 'defaults', 'true')) then
        Statement.SetDefaultValue(I + 1,
          ResultSet.GetMetadata.GetDefaultValue(ColumnIndex));

      case ResultSet.GetMetadata.GetColumnType(ColumnIndex) of
        stBoolean:
          Statement.SetBoolean(I + 1,
            RowAccessor.GetBoolean(ColumnIndex, WasNull));
        stByte:
          Statement.SetByte(I + 1, RowAccessor.GetByte(ColumnIndex, WasNull));
        stShort:
          Statement.SetShort(I + 1, RowAccessor.GetShort(ColumnIndex, WasNull));
        stInteger:
          Statement.SetInt(I + 1, RowAccessor.GetInt(ColumnIndex, WasNull));
        stLong:
          Statement.SetLong(I + 1, RowAccessor.GetLong(ColumnIndex, WasNull));
        stFloat:
          Statement.SetFloat(I + 1, RowAccessor.GetFloat(ColumnIndex, WasNull));
        stDouble:
          Statement.SetDouble(I + 1, RowAccessor.GetDouble(ColumnIndex, WasNull));
        stBigDecimal:
          Statement.SetBigDecimal(I + 1,
            RowAccessor.GetBigDecimal(ColumnIndex, WasNull));
        stString, stUnicodeString:
          Statement.SetString(I + 1, RowAccessor.GetString(ColumnIndex, WasNull));
        stBytes:
          Statement.SetBytes(I + 1, RowAccessor.GetBytes(ColumnIndex, WasNull));
        stDate:
          Statement.SetDate(I + 1, RowAccessor.GetDate(ColumnIndex, WasNull));
        stTime:
          Statement.SetTime(I + 1, RowAccessor.GetTime(ColumnIndex, WasNull));
        stTimestamp:
          Statement.SetTimestamp(I + 1,
            RowAccessor.GetTimestamp(ColumnIndex, WasNull));
        stAsciiStream:
          begin
            TempBlob := RowAccessor.GetBlob(ColumnIndex, WasNull);
            if not TempBlob.IsEmpty then
              Statement.SetBlob(I + 1, stAsciiStream, TempBlob)
            else
              Statement.SetNull(I + 1, stAsciiStream);
          end;
        stUnicodeStream:
          begin
            TempBlob := RowAccessor.GetBlob(ColumnIndex, WasNull);
            if not TempBlob.IsEmpty then
              Statement.SetBlob(I + 1, stUnicodeStream, TempBlob)
            else
              Statement.SetNull(I + 1, stUnicodeStream);
          end;
        stBinaryStream:
          begin
            TempBlob := RowAccessor.GetBlob(ColumnIndex, WasNull);
            if not TempBlob.IsEmpty then
              Statement.SetBlob(I + 1, stBinaryStream, TempBlob)
            else
              Statement.SetNull(I + 1, stBinaryStream);
          end;
      end;
      if WasNull then
      begin
        Statement.SetNull(I + 1,
          ResultSet.GetMetadata.GetColumnType(ColumnIndex))
      end;
    end else begin
      if ParamValue.IsNull then
        Statement.SetNull(I + 1, ConvertDatasetToDbcType(ParamValue.DataType))
      else begin
        case ParamValue.DataType of
          ftBoolean:
            Statement.SetBoolean(I + 1, ParamValue.AsBoolean);
          ftSmallInt:
            Statement.SetShort(I + 1, ParamValue.AsSmallInt);
          ftInteger, ftAutoInc:
            Statement.SetInt(I + 1, ParamValue.AsInteger);
          ftFloat:
            Statement.SetFloat(I + 1, ParamValue.AsFloat);
          ftLargeInt:
            Statement.SetInt(I + 1, ParamValue.AsInteger);
          ftString:
            Statement.SetString(I + 1, ParamValue.AsString);
          ftBytes:
            Statement.SetString(I + 1, ParamValue.AsString);
          ftDate:
            Statement.SetDate(I + 1, ParamValue.AsDate);
          ftTime:
            Statement.SetTime(I + 1, ParamValue.AsTime);
          ftDateTime:
            Statement.SetTimestamp(I + 1, ParamValue.AsDateTime);
          ftMemo:
            begin
              Stream := TStringStream.Create(ParamValue.AsMemo);
              try
                Statement.SetAsciiStream(I + 1, Stream);
              finally
                Stream.Free;
              end;
            end;
          ftBlob, ftGraphic:
            begin
              Stream := TStringStream.Create(ParamValue.AsBlob);
              try
                Statement.SetBinaryStream(I + 1, Stream);
              finally
                Stream.Free;
              end;
            end;
        end;
      end;
    end;
  end;
end;

{**
  Calculate default values for the fields.
  @param Sender a cached result set object.
  @param RowAccessor an accessor object to column values.
}
procedure TZUpdateSQL.CalculateDefaults(Sender: IZCachedResultSet;
  RowAccessor: TZRowAccessor);
begin
 {BEGIN PATCH [1214009] TZUpdateSQL - implemented feature to Calculate default values}
 Sender.GetNativeResolver.CalculateDefaults(Sender, RowAccessor);
 {END PATCH [1214009] TZUpdateSQL - implemented feature to Calculate default values}
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZUpdateSQL.PostUpdates(Sender: IZCachedResultSet;
 UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var I: Integer;
    Statement: IZPreparedStatement;
    Config: TZSQLStrings;
    CalcDefaultValues,
    ExecuteStatement,
    UpdateAutoIncFields: Boolean;

    RefreshResultSet: IZResultSet;
    RefreshRowAccessor: TZRowAccessor;
    RefreshColumnIndex:integer;
    RefreshColumnName:String;
    Refresh_OldSQL:String;
    RefreshColumnType:TZSQLType;

  procedure Apply_RefreshResultSet;
  var
    I: Integer;
  begin
    if Assigned(RefreshResultSet) then begin
      if not RefreshResultSet.First then begin
        raise EZDatabaseError.Create(SUpdateSQLNoResult);
      end;
      for I := 1 to RefreshResultSet.GetMetadata.GetColumnCount do begin
        RefreshColumnName:=RefreshResultSet.GetMetadata.GetColumnName(I); // What Column from Resultset should be updated
        RefreshColumnIndex := Sender.FindColumn(RefreshColumnName); // Is the Column available in the select ?
        if RefreshColumnIndex=0 then begin
          continue; // Column not found in Select from Dataset
        end;
        if RefreshResultSet.IsNull(I) then begin
          RefreshRowAccessor.SetNull(I);
        end else begin
          RefreshColumnType  := RefreshResultSet.GetMetadata.GetColumnType(RefreshColumnIndex); // Type of Column ?
          case RefreshColumnType of
            stBoolean: RefreshRowAccessor.SetBoolean(RefreshColumnIndex, RefreshResultSet.GetBoolean(I));
            stByte: RefreshRowAccessor.SetByte(RefreshColumnIndex, RefreshResultSet.GetByte(I));
            stShort: RefreshRowAccessor.SetShort(RefreshColumnIndex, RefreshResultSet.GetShort(I));
            stInteger: RefreshRowAccessor.SetInt(RefreshColumnIndex, RefreshResultSet.GetInt(I));
            stLong: RefreshRowAccessor.SetLong(RefreshColumnIndex, RefreshResultSet.GetLong(I));
            stFloat: RefreshRowAccessor.SetFloat(RefreshColumnIndex, RefreshResultSet.GetFloat(I));
            stDouble: RefreshRowAccessor.SetDouble(RefreshColumnIndex, RefreshResultSet.GetDouble(I));
            stBigDecimal: RefreshRowAccessor.SetBigDecimal(RefreshColumnIndex, RefreshResultSet.GetBigDecimal(I));
            stString: RefreshRowAccessor.SetPChar(RefreshColumnIndex, RefreshResultSet.GetPChar(I));
            stUnicodeString: RefreshRowAccessor.SetUnicodeString(RefreshColumnIndex, RefreshResultSet.GetUnicodeString(I));
            stBytes: RefreshRowAccessor.SetBytes(RefreshColumnIndex, RefreshResultSet.GetBytes(I));
            stDate: RefreshRowAccessor.SetDate(RefreshColumnIndex, RefreshResultSet.GetDate(I));
            stTime: RefreshRowAccessor.SetTime(RefreshColumnIndex, RefreshResultSet.GetTime(I));
            stTimestamp: RefreshRowAccessor.SetTimestamp(RefreshColumnIndex, RefreshResultSet.GetTimestamp(I));
            stAsciiStream, stUnicodeStream, stBinaryStream:RefreshRowAccessor.SetBlob(RefreshColumnIndex, RefreshResultSet.GetBlob(I));
          end;
        end;
      end;
    end;
  end;

begin
  if (UpdateType = utDeleted)
    and (OldRowAccessor.RowBuffer.UpdateType = utInserted) then
    Exit;

  case UpdateType of
    utInserted:
      Config := FInsertSQL;
    utDeleted:
      Config := FDeleteSQL;
    utModified:
      Config := FModifySQL;
    else
      Exit;
  end;

  case UpdateType of
    utInserted:
      DoBeforeInsertSQL;
    utDeleted:
      DoBeforeDeleteSQL;
    utModified:
      DoBeforeModifySQL;
  end;

  if Dataset is TZAbstractRODataset then
    (Dataset as TZAbstractRODataset).Connection.ShowSqlHourGlass;
  CalcDefaultValues :=
    ZSysUtils.StrToBoolEx(DefineStatementParameter(Sender.GetStatement,'defaults','true'));
  try
    for I := 0 to Config.StatementCount - 1 do
    begin
      Statement := Sender.GetStatement.GetConnection.
        PrepareStatement(Config.Statements[I].SQL);
      FillStatement(Sender, Statement, Config.Statements[I],
        OldRowAccessor, NewRowAccessor);
      {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
      {Update AutoInc Field Tasks will be only executed if the UpdateAutoIncFields
       in the AfterInsertSQLStatement event returns true
      }
      ExecuteStatement := true;
      UpdateAutoIncFields := false;
      case UpdateType of
           utDeleted:
             DoBeforeDeleteSQLStatement(Self, I, ExecuteStatement);
           utInserted:
             DoBeforeInsertSQLStatement(Self, I, ExecuteStatement);
           utModified:
             DoBeforeModifySQLStatement(Self, I, ExecuteStatement);
      end;
      if ExecuteStatement then
      begin
       Statement.ExecutePrepared;

       case UpdateType of
            utDeleted:
              DoAfterDeleteSQLStatement(Self, I);
            utInserted:
              begin
               DoAfterInsertSQLStatement(Self, I, UpdateAutoIncFields);
               if CalcDefaultValues and UpdateAutoIncFields then
                  UpdateAutoIncrementFields(Sender, UpdateType,
                                            OldRowAccessor, NewRowAccessor, Self);
              end;
            utModified:
              DoAfterModifySQLStatement(Self,I);
       end;
      end;
      {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    end;

    RefreshRowAccessor := NewRowAccessor;
    case UpdateType of
      utInserted,utModified: begin
       if FRefreshSql.Text<>'' then begin
        Refresh_OldSQL:=FRefreshSql.Text;
        try
         Config:=FRefreshSQL;
         if UpdateType=utInserted then begin
          if Dataset is TZAbstractDataset then begin
           if FUseSequenceFieldForRefreshSQL then begin
            if assigned(TZAbstractDataset(DataSet).Sequence) and (TZAbstractDataset(DataSet).SequenceField<>'') then begin
             Config.Text :=
               StringReplace(UpperCase(Config.Text),
                 ':OLD_'+UpperCase(TZAbstractDataset(DataSet).SequenceField),
                 TZAbstractDataset(DataSet).Sequence.GetCurrentValueSQL,[rfReplaceAll]);
            end;
           end;
          end;
         end;
         if CONFIG.StatementCount=1 then
         begin
          Statement := Sender.GetStatement.GetConnection.PrepareStatement(Config.Statements[0].SQL);
            FillStatement(Sender, Statement, Config.Statements[0],OldRowAccessor, NewRowAccessor);
          RefreshResultSet:=Statement.ExecuteQueryPrepared;
          Apply_RefreshResultSet;
         end;
       finally
        FRefreshSQL.Text:=Refresh_OldSQL;
       end;
      end;
    end;
  end;
//FOSPATCH

  finally
    if Dataset is TZAbstractRODataset then
      (Dataset as TZAbstractRODataset).Connection.HideSQLHourGlass;
  end;

  case UpdateType of
    utInserted:
      DoAfterInsertSQL;
    utDeleted:
      DoAfterDeleteSQL;
    utModified:
      DoAfterModifySQL;
  end;
end;

{**
  Fires an event before delete Statement
}
procedure TZUpdateSQL.DoBeforeDeleteSQL;
begin
  if Assigned(FBeforeDeleteSQL) then
    FBeforeDeleteSQL(Self);
end;

{**
  Fires an event before insert Statement
}
procedure TZUpdateSQL.DoBeforeInsertSQL;
begin
  if Assigned(BeforeInsertSQL) then
    FBeforeInsertSQL(Self);
end;

{**
  Fires an event before modify Statement
}
procedure TZUpdateSQL.DoBeforeModifySQL;
begin
  if Assigned(FBeforeModifySQL) then
    FBeforeModifySQL(Self);
end;

{**
  Fires an event after delete Statement
}
procedure TZUpdateSQL.DoAfterDeleteSQL;
begin
  if Assigned(FAfterDeleteSQL) then
    FAfterDeleteSQL(Self);
end;

{**
  Fires an event after insert Statement
}
procedure TZUpdateSQL.DoAfterInsertSQL;
begin
  if Assigned(FAfterInsertSQL) then
    FAfterInsertSQL(Self);
end;

{**
  Fires an event after modify Statement
}
procedure TZUpdateSQL.DoAfterModifySQL;
begin
  if Assigned(FAfterModifySQL) then
    FAfterModifySQL(Self);
end;

{BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
procedure TZUpdateSQL.UpdateAutoIncrementFields(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver);
begin
 with Sender.GetNativeResolver do
 begin
  UpdateAutoIncrementFields(Sender, UpdateType,
   OldRowAccessor, NewRowAccessor, Resolver);
 end;
end;
{END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }

{NEW Methods for Events to validate at Statement level }
procedure TZUpdateSQL.DoAfterDeleteSQLStatement(const Sender: TObject;
  StatementIndex: Integer);
begin
 if Assigned(FAfterDeleteSQLStatement) then
    FAfterDeleteSQLStatement(Self, StatementIndex);
end;

procedure TZUpdateSQL.DoAfterInsertSQLStatement(const Sender: TObject;
  StatementIndex: Integer; out UpdateAutoIncFields: Boolean);
begin
 if Assigned(FAfterInsertSQLStatement) then
    FAfterInsertSQLStatement(Self, StatementIndex, UpdateAutoIncFields);
end;

procedure TZUpdateSQL.DoAfterModifySQLStatement(const Sender: TObject;
  StatementIndex: Integer);
begin
 if Assigned(FAfterModifySQLStatement) then
    FAfterModifySQLStatement(Self, StatementIndex);
end;

procedure TZUpdateSQL.DoBeforeDeleteSQLStatement(const Sender: TObject;
  StatementIndex: Integer; out Execute: Boolean);
begin
 if Assigned(FBeforeDeleteSQLStatement) then
    FBeforeDeleteSQLStatement(Self, StatementIndex, Execute);
end;

procedure TZUpdateSQL.DoBeforeInsertSQLStatement(const Sender: TObject;
  StatementIndex: Integer; out Execute: Boolean);
begin
 if Assigned(FBeforeInsertSQLStatement) then
    FBeforeInsertSQLStatement(Self, StatementIndex, Execute);
end;

procedure TZUpdateSQL.DoBeforeModifySQLStatement(const Sender: TObject;
  StatementIndex: Integer; out Execute: Boolean);
begin
 if Assigned(FBeforeModifySQLStatement) then
    FBeforeModifySQLStatement(Self, StatementIndex, Execute);
end;

end.
