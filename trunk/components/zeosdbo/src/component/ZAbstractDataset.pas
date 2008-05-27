{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Read/Write Dataset component          }
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

unit ZAbstractDataset;

interface

{$I ZComponent.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
  Variants,
{$ENDIF}
  SysUtils, DB, Classes, ZConnection, ZSqlUpdate, ZDbcIntfs, ZVariant,
  ZDbcCache, ZDbcCachedResultSet, ZAbstractRODataset, ZDbcGenericResolver,
  ZCompatibility, ZSequence;

type
  {$IFDEF FPC}
  TUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied);
  {$ENDIF}

  {** Update Event type. }
  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
    var UpdateAction: TUpdateAction) of object;

  {** Defines update modes for the resultsets. }
  TZUpdateMode = (umUpdateChanged, umUpdateAll);

  {** Defines where form types for resultsets. }
  TZWhereMode = (wmWhereKeyOnly, wmWhereAll);

  {**
    Abstract dataset component which supports read/write access and
    cached updates.
  }
  TZAbstractDataset = class(TZAbstractRODataset)
  private
    FCachedUpdates: Boolean;
    FUpdateObject: TZUpdateSQL;
    FCachedResultSet: IZCachedResultSet;
    FCachedResolver: IZCachedResolver;
    FOnApplyUpdateError: TDataSetErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;
    FUpdateMode: TZUpdateMode;
    FWhereMode: TZWhereMode;
    FSequence: TZSequence;
    FSequenceField: string;

    FBeforeApplyUpdates: TNotifyEvent; {bangfauzan addition}
    FAfterApplyUpdates: TNotifyEvent; {bangfauzan addition}

  private
    function GetUpdatesPending: Boolean;
    procedure SetUpdateObject(Value: TZUpdateSQL);
    procedure SetCachedUpdates(Value: Boolean);
    procedure SetWhereMode(Value: TZWhereMode);
    procedure SetUpdateMode(Value: TZUpdateMode);

  protected
    property CachedResultSet: IZCachedResultSet read FCachedResultSet
      write FCachedResultSet;
    property CachedResolver: IZCachedResolver read FCachedResolver
      write FCachedResolver;
    property UpdateMode: TZUpdateMode read FUpdateMode write SetUpdateMode
      default umUpdateChanged;
    property WhereMode: TZWhereMode read FWhereMode write SetWhereMode
      default wmWhereKeyOnly;

    procedure InternalClose; override;
    procedure InternalEdit; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalPost; override;
    procedure InternalDelete; override;
    procedure InternalUpdate;
    procedure InternalCancel; override;

    procedure DOBeforeApplyUpdates; {bangfauzan addition}
    procedure DOAfterApplyUpdates; {bangfauzan addition}


    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; override;
    function CreateResultSet(const SQL: WideString; MaxRows: Integer):
      IZResultSet; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

  {$IFDEF WITH_IPROVIDER}
    function PSUpdateRecord(UpdateKind: TUpdateKind;
      Delta: TDataSet): Boolean; override;
  {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ApplyUpdates;
    procedure CommitUpdates;
    procedure CancelUpdates;
    procedure RevertRecord;

    procedure EmptyDataSet; {bangfauzan addition}

  public
    property UpdatesPending: Boolean read GetUpdatesPending;
    property Sequence: TZSequence read FSequence write FSequence;
    property SequenceField: string read FSequenceField write FSequenceField;

  published
    property UpdateObject: TZUpdateSQL read FUpdateObject write SetUpdateObject;
    property CachedUpdates: Boolean read FCachedUpdates write SetCachedUpdates
      default False;

    property OnApplyUpdateError: TDataSetErrorEvent read FOnApplyUpdateError
      write FOnApplyUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord
      write FOnUpdateRecord;

    property BeforeApplyUpdates: TNotifyEvent read FBeforeApplyUpdates
      write FBeforeApplyUpdates; {bangfauzan addition}
    property AfterApplyUpdates: TNotifyEvent read FAfterApplyUpdates
      write FAfterApplyUpdates; {bangfauzan addition}


  published
//    property Constraints;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property OnDeleteError;
    property OnEditError;
    property OnPostError;
    property OnNewRecord;
  end;

implementation

uses Math, ZMessages, ZDatasetUtils;

{ TZAbstractDataset }

{**
  Constructs this object and assignes the mail properties.
  @param AOwner a component owner.
}
constructor TZAbstractDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWhereMode := wmWhereKeyOnly;
  FUpdateMode := umUpdateChanged;
  RequestLive := True;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractDataset.Destroy;
begin
  inherited Destroy;
end;

{**
  Sets a new UpdateSQL object.
  @param Value a new UpdateSQL object.
}
procedure TZAbstractDataset.SetUpdateObject(Value: TZUpdateSQL);
begin
  if FUpdateObject <> Value then
  begin
    if Assigned(FUpdateObject) then
      FUpdateObject.RemoveFreeNotification(Self);
    FUpdateObject := Value;
    if Assigned(FUpdateObject) then
      FUpdateObject.FreeNotification(Self);
    if Assigned(FUpdateObject) then
      FUpdateObject.DataSet := Self;
    if Active and (CachedResultSet <> nil) then
    begin
      if FUpdateObject <> nil then
        CachedResultSet.SetResolver(FUpdateObject)
      else
        CachedResultSet.SetResolver(CachedResolver);
    end;
  end;
end;

{**
  Sets a new CachedUpdates property value.
  @param Value a new CachedUpdates value.
}
procedure TZAbstractDataset.SetCachedUpdates(Value: Boolean);
begin
  if FCachedUpdates <> Value then
  begin
    FCachedUpdates := Value;
    if Active and (CachedResultSet <> nil) then
      CachedResultSet.SetCachedUpdates(Value);
  end;
end;

{**
  Sets a new UpdateMode property value.
  @param Value a new UpdateMode value.
}
procedure TZAbstractDataset.SetUpdateMode(Value: TZUpdateMode);
begin
  if FUpdateMode <> Value then
  begin
    FUpdateMode := Value;
    if Active then Close;
  end;
end;

{**
  Sets a new WhereMode property value.
  @param Value a new WhereMode value.
}
procedure TZAbstractDataset.SetWhereMode(Value: TZWhereMode);
begin
  if FWhereMode <> Value then
  begin
    FWhereMode := Value;
    if Active then Close;
  end;
end;

{**
  Creates a DBC statement for the query.
  @param SQL an SQL query.
  @param Properties a statement specific properties.
  @returns a created DBC statement.
}
function TZAbstractDataset.CreateStatement(
  const SQL: string; Properties: TStrings): IZPreparedStatement;
var
  Temp: TStrings;
begin
  Temp := TStringList.Create;
  try
    Temp.AddStrings(Properties);

    { Sets update mode property.}
    case FUpdateMode of
      umUpdateAll: Temp.Values['update'] := 'all';
      umUpdateChanged: Temp.Values['update'] := 'changed';
    end;
    { Sets where mode property. }
    case FWhereMode of
      wmWhereAll: Temp.Values['where'] := 'all';
      wmWhereKeyOnly: Temp.Values['where'] := 'keyonly';
    end;

    Result := inherited CreateStatement(SQL, Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Creates a DBC resultset for the query.
  @param SQL an SQL query.
  @param MaxRows a maximum rows number (-1 for all).
  @returns a created DBC resultset.
}
function TZAbstractDataset.CreateResultSet(const SQL: WideString; MaxRows: Integer):
  IZResultSet;
begin
  Result := inherited CreateResultSet(SQL, MaxRows);

  if not Assigned(Result) then
    Exit;

  if Result.QueryInterface(IZCachedResultSet, FCachedResultSet) = 0 then
  begin
    CachedResultSet := Result as IZCachedResultSet;
    CachedResolver := CachedResultSet.GetResolver;
    CachedResultSet.SetCachedUpdates(CachedUpdates);
    if FUpdateObject <> nil then
      CachedResultSet.SetResolver(FUpdateObject);
  end;
end;

{**
  Performs internal query closing.
}
procedure TZAbstractDataset.InternalClose;
begin
  inherited InternalClose;

  if Assigned(CachedResultSet) then
    CachedResultSet.Close;
  CachedResultSet := nil;
  CachedResolver := nil;
end;

{**
  Performs an internal action before switch into edit mode.
}
procedure TZAbstractDataset.InternalEdit;
begin
end;

{**
  Performs an internal record updates.
}
procedure TZAbstractDataset.InternalUpdate;
var
  RowNo: Integer;
  RowBuffer: PZRowBuffer;
begin
  if (CachedResultSet <> nil) and GetActiveBuffer(RowBuffer) then
  begin
    RowNo := Integer(CurrentRows[CurrentRow - 1]);
    CachedResultSet.MoveAbsolute(RowNo);
    RowAccessor.RowBuffer := RowBuffer;
    PostToResultSet(CachedResultSet, FieldsLookupTable, Fields, RowAccessor);
    try
      CachedResultSet.UpdateRow;
    except on E: EZSQLThrowable do
      raise EZDatabaseError.CreateFromException(E);
    end;

    { Filters the row }
    if not FilterRow(RowNo) then
    begin
      CurrentRows.Delete(CurrentRow - 1);
      CurrentRow := Min(CurrentRows.Count, CurrentRow);
    end;
  end;
end;

{**
  Performs an internal adding a new record.
  @param Buffer a buffer of the new adding record.
  @param Append <code>True</code> if record should be added to the end
    of the result set.
}
procedure TZAbstractDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RowNo: Integer;
  RowBuffer: PZRowBuffer;
begin
  if not GetActiveBuffer(RowBuffer) or (RowBuffer <> Buffer) then
    raise EZDatabaseError.Create(SInternalError);

  if Append then FetchRows(0);

  if CachedResultSet <> nil then
  begin
    CachedResultSet.MoveToInsertRow;
    RowAccessor.RowBuffer := RowBuffer;
    PostToResultSet(CachedResultSet, FieldsLookupTable, Fields, RowAccessor);
    try
      CachedResultSet.InsertRow;
    except on E: EZSQLThrowable do
      raise EZDatabaseError.CreateFromException(E);
    end;
    RowNo := CachedResultSet.GetRow;
    FetchCount := FetchCount + 1;

    { Filters the row }
    if FilterRow(RowNo) then
    begin
      if Append then
      begin
        CurrentRows.Add(Pointer(RowNo));
        CurrentRow := CurrentRows.Count;
      end
      else
      begin
        CurrentRow := Max(CurrentRow, 1);
        CurrentRows.Insert(CurrentRow - 1, Pointer(RowNo));
      end;
    end;
  end;
end;

{**
  Performs an internal post updates.
}
procedure TZAbstractDataset.InternalPost;
var
  RowBuffer: PZRowBuffer;
  BM:TBookMarkStr;
begin
  if (FSequenceField <> '') and Assigned(FSequence) then
  begin
    if FieldByName(FSequenceField).IsNull then
    {$IFDEF VER130} //Delphi5 
      FieldByName(FSequenceField).Value := Integer(FSequence.GetNextValue);
    {$ELSE}
      FieldByName(FSequenceField).Value := FSequence.GetNextValue;
    {$ENDIF}
  end;

  inherited;

  if not GetActiveBuffer(RowBuffer) then
    raise EZDatabaseError.Create(SInternalError);

  Connection.ShowSqlHourGlass;
  try
    if State = dsInsert then
      InternalAddRecord(RowBuffer, False)
    else InternalUpdate;

    {BUG-FIX: bangfauzan addition}
    if (SortedFields<>'') then begin
      FreeFieldBuffers;
      SetState(dsBrowse);
      Resync([]);
      BM:=Bookmark;
      DisableControls;
      InternalSort;
      BookMark:=BM;
      EnableControls;
    end;
    {end of bangfauzan addition}
  finally
    Connection.HideSqlHourGlass;
  end;
end;

{**
  Performs an internal record removing.
}
procedure TZAbstractDataset.InternalDelete;
var
  RowNo: Integer;
  RowBuffer: PZRowBuffer;
begin
  if (CachedResultSet <> nil) and GetActiveBuffer(RowBuffer) then
  begin
    Connection.ShowSqlHourGlass;
    try
      RowNo := Integer(CurrentRows[CurrentRow - 1]);
      CachedResultSet.MoveAbsolute(RowNo);
      try
        CachedResultSet.DeleteRow;
      except on E: EZSQLThrowable do
        raise EZDatabaseError.CreateFromException(E);
      end;

      { Filters the row }
      if not FilterRow(RowNo) then
      begin
        CurrentRows.Delete(CurrentRow - 1);
        if not FetchRows(CurrentRow) then
          CurrentRow := Min(CurrentRows.Count, CurrentRow);
      end;
    finally
      Connection.HideSQLHourGlass;
    end;
  end;
end;

{**
  Performs an internal cancel updates.
}
procedure TZAbstractDataset.InternalCancel;
var
  RowNo: Integer;
  RowBuffer: PZRowBuffer;
begin
  if (CachedResultSet <> nil) and GetActiveBuffer(RowBuffer)
    and (CurrentRow > 0) and (State = dsEdit) then
  begin
    RowNo := Integer(CurrentRows[CurrentRow - 1]);
    CachedResultSet.MoveAbsolute(RowNo);
    RowAccessor.RowBuffer := RowBuffer;
    FetchFromResultSet(CachedResultSet, FieldsLookupTable, Fields, RowAccessor);
  end;
end;

{**
  Processes component notifications.
  @param AComponent a changed component object.
  @param Operation a component operation code.
}
procedure TZAbstractDataset.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent = FUpdateObject) then
    begin
      Close;
      FUpdateObject := nil;
    end;
    if (AComponent = FSequence) then
    begin
      FSequence := nil;
    end;
  end;
end;

{**
   Applies all cached updates stored in the resultset.
}
procedure TZAbstractDataset.ApplyUpdates;
begin
  if not Active then Exit;

  Connection.ShowSQLHourGlass;
  try
    if State in [dsEdit, dsInsert] then Post;

    DoBeforeApplyUpdates; {bangfauzan addition}

    if CachedResultSet <> nil then
      CachedResultSet.PostUpdates;

    if not (State in [dsInactive]) then
      Resync([]);

  DOAfterApplyUpdates; {bangfauzan addition}

  finally
    Connection.HideSqlHourGlass;
  end;
end;

{**
  Clears cached updates buffer.
}
procedure TZAbstractDataset.CommitUpdates;
begin
  CheckBrowseMode;

  if CachedResultSet <> nil then
    CachedResultSet.CancelUpdates;
end;

{**
  Cancels all cached updates and clears the buffer.
}
procedure TZAbstractDataset.CancelUpdates;
begin
  if State in [dsEdit, dsInsert] then Cancel;

  if CachedResultSet <> nil then
    CachedResultSet.CancelUpdates;

  if not (State in [dsInactive]) then
    RereadRows;
end;

{**
  Reverts the previous status for the current row.
}
procedure TZAbstractDataset.RevertRecord;
begin
  if State in [dsInsert] then
  begin
    Cancel;
    Exit;
  end;
  if State in [dsEdit] then Cancel;

  if CachedResultSet <> nil then
    CachedResultSet.RevertRecord;

  if not (State in [dsInactive]) then Resync([]);
end;

{**
  Checks is there cached updates pending in the buffer.
  @return <code>True</code> if there some pending cached updates.
}
function TZAbstractDataset.GetUpdatesPending: Boolean;
begin
  if State = dsInactive then
    Result := False
  else if (CachedResultSet <> nil) and CachedResultSet.IsPendingUpdates then
    Result := True
  else if (State in [dsInsert, dsEdit]) then
    Result := Modified
  else Result := False;
end;

{$IFDEF WITH_IPROVIDER}

{**
  Applies a single update to the underlying database table or tables.
  @param UpdateKind an update type.
  @param Delta a dataset where the current position shows the row to update.
  @returns <code>True</code> if updates were successfully applied.
}
function TZAbstractDataset.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;

var
  Bookmark: TBookmark;
  ActiveMode: Boolean;
  UpdateMode: Boolean;

  function LocateRecord: Boolean;
  var
    I: Integer;
    KeyFields: string;
    Temp: Variant;
    SrcField: TField;
    KeyValues: Variant;
    FieldRefs: TObjectDynArray;
    OnlyDataFields: Boolean;
  begin
    if Properties.Values['KeyFields'] <> '' then
      KeyFields := Properties.Values['KeyFields']
    else
      KeyFields := DefineKeyFields(Fields);
    FieldRefs := DefineFields(Self, KeyFields, OnlyDataFields);
    Temp := VarArrayCreate([0, Length(FieldRefs) - 1], varVariant);

    for I := 0 to Length(FieldRefs) - 1 do
    begin
      SrcField := Delta.FieldByName(TField(FieldRefs[I]).FieldName);
      if SrcField <> nil then
      begin
        if (SrcField.DataType = ftLargeInt)
          and not VarIsNull(SrcField.OldValue) then
          Temp[I] := Integer(SrcField.OldValue)
        else Temp[I] := SrcField.OldValue;
      end else
        Temp[I] := Null;
    end;

    if Length(FieldRefs) = 1 then
      KeyValues := Temp[0]
    else KeyValues := Temp;

    if KeyFields <> '' then
      Result := Locate(KeyFields, KeyValues, [])
    else Result := False;
  end;

  procedure CopyRecord(SrcDataset: TDataset; DestDataset: TDataset);
  var
    I: Integer;
    SrcField: TField;
    DestField: TField;
    SrcStream: TStream;
    DestStream: TStream;
  begin
    for I := 0 to DestDataset.FieldCount - 1 do
    begin
      DestField := DestDataset.Fields[I];
      SrcField := SrcDataset.FieldByName(DestField.FieldName);
      if (SrcField = nil) or VarIsEmpty(SrcField.NewValue) then
        Continue;

      if SrcField.IsNull then
      begin
        DestField.Clear;
        Continue;
      end;

      if SrcField.DataType = ftMemo then
        raise Exception.Create('Internal error: How did this happen in Unicode mode?');

      case DestField.DataType of
        ftLargeInt:
          begin
            if SrcField.DataType = ftLargeInt then
            begin
              TLargeIntField(DestField).AsLargeInt :=
                TLargeIntField(SrcField).AsLargeInt;
            end else
              DestField.AsInteger := SrcField.AsInteger;
          end;
        ftMemo:
          raise Exception.Create('Internal error: How did this happen in Unicode mode?');
        ftBlob, ftWideMemo:
          begin
            if SrcField.DataType in [ftBlob, ftWideMemo] then
            begin
              SrcStream := SrcDataset.CreateBlobStream(SrcField, bmRead);
              try
                DestStream := DestDataset.CreateBlobStream(DestField, bmWrite);
                try
                  DestStream.CopyFrom(SrcStream, 0);
                finally
                  DestStream.Free;
                end;
              finally
                SrcStream.Free;
              end;
            end else
              DestField.AsVariant := SrcField.AsVariant;
          end;
        else
          DestField.AsVariant := SrcField.AsVariant;
      end;
    end;
  end;

begin
  Result := False;
  ActiveMode := Self.Active;
  UpdateMode := Self.RequestLive;

  if Self.RequestLive = False then
    Self.RequestLive := True;
  if Self.Active = False then
    Self.Open;

  CheckBrowseMode;
  try
    Self.DisableControls;

    { Saves the current position. }
    Bookmark := Self.GetBookmark;

    { Applies updates. }
    try
      case UpdateKind of
        ukModify:
          begin
            if LocateRecord then
            begin
              Self.Edit;
              CopyRecord(Delta, Self);
              Self.Post;
              Result := True;
            end;
          end;
        ukInsert:
          begin
            Self.Append;
            CopyRecord(Delta, Self);
            Self.Post;
            Result := True;
          end;
        ukDelete:
          begin
            if LocateRecord then
            begin
              Self.Delete;
              Result := True;
            end;
          end;
      end;
    except
      Result := False;
    end;

    { Restores the previous position. }
    try
      Self.GotoBookmark(Bookmark);
    except
      Self.First;
    end;
    Self.FreeBookmark(Bookmark);
  finally
    EnableControls;
    Self.RequestLive := UpdateMode;
    Self.Active := ActiveMode;
  end;
end;

{$ENDIF}

{============================bangfauzan addition===================}

procedure TZAbstractDataset.DOBeforeApplyUpdates;
begin
  if assigned(BeforeApplyUpdates) then
    FBeforeApplyUpdates(Self);
end;

procedure TZAbstractDataset.DOAfterApplyUpdates;
begin
  if assigned(AfterApplyUpdates) then
    FAfterApplyUpdates(Self);
end;

procedure TZAbstractDataset.EmptyDataSet;
begin
  if Active then
  begin
    Self.CancelUpdates;
    Self.CurrentRows.Clear;
    Self.CurrentRow:=0;
    Resync([]);
    InitRecord(ActiveBuffer);
  end;
end;

{========================end of bangfauzan addition================}


end.
