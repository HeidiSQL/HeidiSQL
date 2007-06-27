{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{              Abstract StoredProc component              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                            & Janos Fegyverneki          }
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

unit ZStoredProcedure;

interface

{$I ZComponent.inc}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  SysUtils, DB, Classes, ZConnection, ZDbcIntfs,
  ZAbstractDataset, ZCompatibility;

type

  {**
    Abstract dataset to access to stored procedures.
  }
  TZStoredProc = class(TZAbstractDataset)
  private
    procedure RetrieveParamValues;
    function GetStoredProcName: string;
    procedure SetStoredProcName(const Value: string);
  protected
{    property CallableResultSet: IZCallableStatement read FCallableStatement
      write FCallableStatement;}

    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; override;
    procedure SetStatementParams(Statement: IZPreparedStatement;
      ParamNames: TStringDynArray; Params: TParams;
      DataLink: TDataLink); override;
    procedure InternalOpen; override;
    procedure InternalClose; override;

  protected
  {$IFDEF WITH_IPROVIDER}
    function PSIsSQLBased: Boolean; override;
    procedure PSExecute; override;
    function PSGetTableNameW: WideString; override;
    procedure PSSetCommandText(const ACommandText: string); override;
  {$ENDIF}

  public
    procedure ExecProc; virtual;

  published
    property Active;
    property ParamCheck;
    property Params;
    property ShowRecordTypes;
    property Options;
    property StoredProcName: string read GetStoredProcName
      write SetStoredProcName;
  end;

implementation

uses
  ZAbstractRODataset, ZMessages, ZDatasetUtils;

{ TZStoredProc }

{**
  Creates a DBC statement for the query.
  @param SQL an SQL query.
  @param Properties a statement specific properties.
  @returns a created DBC statement.
}
function TZStoredProc.CreateStatement(const SQL: string; Properties: TStrings):
  IZPreparedStatement;
var
  I: Integer;
  CallableStatement: IZCallableStatement;
begin
  CallableStatement := Connection.DbcConnection.PrepareCallWithParams(
    Trim(SQL), Properties);

  for I := 0 to Params.Count - 1 do
  begin
    if Params[I].ParamType in [ptResult, ptOutput, ptInputOutput] then
      CallableStatement.RegisterOutParameter(I + 1,
        Ord(ConvertDatasetToDbcType(Params[I].DataType)));
  end;
  Result := CallableStatement;
end;

{**
  Fill prepared statement with parameters.
  @param Statement a prepared SQL statement.
  @param ParamNames an array of parameter names.
  @param Params a collection of SQL parameters.
  @param DataLink a datalink to get parameters.
}
procedure TZStoredProc.SetStatementParams(Statement: IZPreparedStatement;
  ParamNames: TStringDynArray; Params: TParams; DataLink: TDataLink);
var
  I: Integer;
  Param: TParam;
  Stream: TStream;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];

    if Params[I].ParamType in [ptResult, ptOutput] then
     Continue;

    if Param.IsNull then
      Statement.SetNull(I+1, ConvertDatasetToDbcType(Param.DataType))
    else begin
      case Param.DataType of
        ftBoolean:
          Statement.SetBoolean(I+1, Param.AsBoolean);
        ftSmallInt:
          Statement.SetShort(I+1, Param.AsSmallInt);
        ftInteger, ftAutoInc:
          Statement.SetInt(I+1, Param.AsInteger);
        ftFloat:
          Statement.SetDouble(I+1, Param.AsFloat);
        ftLargeInt:
          Statement.SetLong(I+1, StrToInt64(Param.AsString));
        ftString , ftFixedChar:
          Statement.SetString(I+1, Param.AsString);
        ftBytes:
          Statement.SetString(I+1, Param.AsString);
        ftDate:
          Statement.SetDate(I+1, Param.AsDate);
        ftTime:
          Statement.SetTime(I+1, Param.AsTime);
        ftDateTime{$IFNDEF VER130}, ftTimestamp{$ENDIF}:
          Statement.SetTimestamp(I+1, Param.AsDateTime);
        ftMemo:
          begin
            Stream := TStringStream.Create(Param.AsMemo);
            try
              Statement.SetAsciiStream(I+1, Stream);
            finally
              Stream.Free;
            end;
          end;
        ftBlob:
          begin
            Stream := TStringStream.Create(Param.AsBlob);
            try
              Statement.SetBinaryStream(I+1, Stream);
            finally
              Stream.Free;
            end;
          end;
        else
          raise EZDatabaseError.Create(SUnKnownParamDataType);
      end;
    end;
  end;
end;

{**
  Retrieves parameter values from callable statement.
}
procedure TZStoredProc.RetrieveParamValues;
var
  I: Integer;
  Param: TParam;
  FCallableStatement: IZCallableStatement;
begin
  if not Assigned(FCallableStatement) then
  begin
    if Assigned(Statement) then
      Statement.QueryInterface(IZCallableStatement, FCallableStatement);
    if not Assigned(FCallableStatement) then
      Exit;
  end;

  for I := 0 to Params.Count - 1 do
  begin
    Param := Params[I];
    if not (Param.ParamType in [ptResult, ptOutput, ptInputOutput]) then
      Continue;

    if FCallableStatement.IsNull(I + 1) then
      Param.Clear
    else
      case Param.DataType of
        ftBoolean:
          Param.AsBoolean := FCallableStatement.GetBoolean(I + 1);
        ftSmallInt:
          Param.AsSmallInt := FCallableStatement.GetShort(I + 1);
        ftInteger, ftAutoInc:
          Param.AsInteger := FCallableStatement.GetInt(I + 1);
        ftFloat:
          Param.AsFloat := FCallableStatement.GetDouble(I + 1);
        ftLargeInt:
  {$IFNDEF VER130BELOW}
          Param.Value := FCallableStatement.GetLong(I + 1);
  {$ELSE}
          Param.AsInteger := FCallableStatement.GetLong(I + 1);
  {$ENDIF}
        ftString:
          Param.AsString := FCallableStatement.GetString(I + 1);
        ftBytes:
          Param.AsString := FCallableStatement.GetString(I + 1);
        ftDate:
          Param.AsDate := FCallableStatement.GetDate(I + 1);
        ftTime:
          Param.AsTime := FCallableStatement.GetTime(I + 1);
        ftDateTime:
          Param.AsDateTime := FCallableStatement.GetTimestamp(I + 1);
        else
           raise EZDatabaseError.Create(SUnKnownParamDataType);
      end;
  end;
end;

{**
  Performs internal query opening.
}
procedure TZStoredProc.InternalOpen;
begin
  inherited InternalOpen;

  RetrieveParamValues;
end;

{**
  Performs internal query closing.
}
procedure TZStoredProc.InternalClose;
begin
  inherited InternalClose;
end;

function TZStoredProc.GetStoredProcName: string;
begin
  Result := Trim(SQL.Text);
end;

procedure TZStoredProc.SetStoredProcName(const Value: string);
var
  ResultSet: IZResultSet;
  OldParams: TParams;
  Catalog,
  Schema,
  ObjectName: string;
begin
  if AnsiCompareText(Trim(SQL.Text), Trim(Value)) <> 0 then
  begin
    SQL.Text := Value;
    if ParamCheck and (Value <> '') and not (csLoading in ComponentState) and Assigned(Connection) then
    begin
      Connection.ShowSQLHourGlass;
      try
        SplitQualifiedObjectName(Value, Catalog, Schema, ObjectName);
        ResultSet := Connection.DbcConnection.GetMetadata.GetProcedureColumns(Catalog, Schema, ObjectName, '');
        OldParams := TParams.Create;
        try
          OldParams.Assign(Params);
          Params.Clear;
          while ResultSet.Next do
            if ResultSet.GetInt(5) >= 0 then //-1 is result column
              Params.CreateParam(ConvertDbcToDatasetType(TZSqlType(ResultSet.GetInt(6))),
                ResultSet.GetString(4), TParamType(ResultSet.GetInt(5)));
          Params.AssignValues(OldParams);
        finally
          OldParams.Free;
        end;
      finally
        Connection.HideSQLHourGlass;
      end;
    end;
  end;
end;

procedure TZStoredProc.ExecProc;
begin
  Connection.ShowSQLHourGlass;
  try
    if Active then
      Close;
    ExecSQL;
    RetrieveParamValues;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{$IFDEF WITH_IPROVIDER}

{**
  Checks if dataset can execute SQL queries?
  @returns <code>True</code> if the query can execute SQL.
}
function TZStoredProc.PSIsSQLBased: Boolean;
begin
  Result := False;
end;

{**
  Gets the name of the stored procedure.
  @returns the name of this stored procedure.
}
function TZStoredProc.PSGetTableNameW: WideString;
begin
  Result := StoredProcName;
end;

{**
  Executes this stored procedure.
}
procedure TZStoredProc.PSExecute;
begin
  ExecProc;
end;

{**
  Assignes a new name for this stored procedure.
  @param ACommandText a new name for this stored procedure.
}
procedure TZStoredProc.PSSetCommandText(const ACommandText: string);
begin
  StoredProcName := ACommandText;
end;

{$ENDIF}

end.
