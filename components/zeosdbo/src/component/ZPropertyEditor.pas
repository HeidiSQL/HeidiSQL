{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Component Property Editors                }
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

unit ZPropertyEditor;

interface

{$I ZComponent.inc}

{$IFDEF WITH_PROPERTY_EDITOR}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Classes, ZClasses, ZCompatibility, ZDbcIntfs,
{$IFNDEF VER130BELOW}
  DesignIntf, DesignEditors;
{$ELSE}
  {$IFDEF FPC}
    PropEdits;
  {$ELSE}
    DsgnIntf;
  {$ENDIF}
{$ENDIF}

type

  {** Implements the basic methods of the property editor. }
  TZStringProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  {** Shows all Fields received from FieldDefs. }
  TZDataFieldPropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Fields received from IndexDefs - not used yet. }
  TZIndexFieldPropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Fields received from the MasterSource's DataSet.FieldDefs. }
  TZMasterFieldPropertyEditor = class(TZStringProperty)
  public
{    procedure GetValueList(List: TStrings); override;}
  end;

  {** Shows all Tables received from Connection.IZDatabaseMetadata. }
  TZTableNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Procedures received from Connection.IZDatabaseMetadata. }
  TZProcedureNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Sequences received from Connection.IZDatabaseMetadata. }
  TZSequenceNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Implements a property editor for ZConnection.Protocol property. }
  TZProtocolPropertyEditor = class(TZStringProperty)
  public
    function  GetValue: string; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  {** Implements a property editor for ZConnection.Database property. }
  TZDatabasePropertyEditor = class(TZStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  {** Implements a property editor for ZConnection.Catalog property. }
  TZCatalogPropertyEditor = class(TZStringProperty)
  public
    function  GetValue: string; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

{$IFDEF USE_METADATA}
  {** Shows all Catalogs received from Connection.IZDatabaseMetadata. }
  TZCatalogProperty = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Columns received from Connection.IZDatabaseMetadata. }
  TZColumnNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Schemes received from Connection.IZDatabaseMetadata. }
  TZSchemaPropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Types received from Connection.IZDatabaseMetadata. }
  TZTypeNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;
{$ENDIF}

const
  CRLF = #13#10;

{$ENDIF}

implementation

{$IFDEF WITH_PROPERTY_EDITOR}

uses SysUtils, Forms, Dialogs, Controls, DB, TypInfo,
  ZConnection, ZDataSet, ZStoredProcedure, ZMessages
{$IFDEF USE_METADATA}
  , ZSqlMetadata
{$ENDIF}
{$IFNDEF UNIX}
  {$IFNDEF FPC}
  {$IFDEF ENABLE_ADO}
, ZDbcAdoUtils
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
;

{$IFDEF FPC}
procedure GetItemNames(FieldDefs: TFieldDefs; List: TStrings); overload;
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to Pred(FieldDefs.Count) do
    List.Append(FieldDefs[i].Name);
end;

procedure GetItemNames(IndexDefs: TIndexDefs; List: TStrings); overload;
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to Pred(IndexDefs.Count) do
    List.Append(IndexDefs[i].Name);
end;
{$ENDIF}

{ Returns the IndexDefs from Dataset }
function GetIndexDefs(Component: TPersistent): TIndexDefs;
var
  DataSet: TDataSet;
begin
  DataSet := Component as TDataSet;
  Result := GetObjectProp(DataSet, 'IndexDefs') as TIndexDefs;
  if Assigned(Result) then
  begin
    Result.Updated := False;
    Result.Update;
  end;
end;

function IsEmpty(const s: string): Boolean;
begin
  Result := Trim(s) = '';
end;

{ TZStringProperty }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TZStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

{**
  Processes a list of list items.
  @param Proc a procedure to process the list items.
}
procedure TZStringProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for i := 0 to Pred(Values.Count) do
      Proc(Values[i]);
  finally
    Values.Free;
  end;
end;

{ TZDataFieldPropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZDataFieldPropertyEditor.GetValueList(List: TStrings);
begin
  try
    with (GetComponent(0) as TDataSet) do
    begin
      // Update the FieldDefs and return the Fieldnames
      FieldDefs.Updated := False;
      FieldDefs.Update;
      {$IFNDEF FPC}
      FieldDefs.GetItemNames(List);
      {$ELSE}
      GetItemNames(FieldDefs, List);
      {$ENDIF}
    end;
  except
  end;
end;

{ TZIndexFieldPropertyEditor }

procedure TZIndexFieldPropertyEditor.GetValueList(List: TStrings);
begin
  {$IFNDEF FPC}
  GetIndexDefs(GetComponent(0)).GetItemNames(List);
  {$ELSE}
  GetItemNames(GetIndexDefs(GetComponent(0)), List);
  {$ENDIF}
end;

{ TZMasterFieldPropertyEditor }

{procedure TZMasterFieldPropertyEditor.GetValueList(List: TStrings);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetComponent(0), 'MasterSource') as TDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.GetFieldNames(List);
end;}

{ TZTableNamePropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZTableNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
{$IFDEF USE_METADATA}
  Catalog, Schema: string;
{$ENDIF}
begin
  Connection := GetObjectProp(GetComponent(0), 'Connection') as TZConnection;
  if Assigned(Connection) and Connection.Connected then
  begin
{$IFDEF USE_METADATA}
    if GetComponent(0) is TZSqlMetadata then
    begin
      Catalog := GetStrProp(GetComponent(0), 'Catalog');
      Schema := GetStrProp(GetComponent(0), 'Schema');
{$IFDEF SHOW_WARNING}
      if not (IsEmpty(Catalog) and IsEmpty(Schema)) or
       (MessageDlg(SPropertyQuery + CRLF + SPropertyTables + CRLF +
        SPropertyExecute, mtWarning, [mbYes,mbNo], 0) = mrYes) then
{$ENDIF}
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Tables of the defined Catalog and Schema
        ResultSet := Metadata.GetTables(Catalog, Schema, '', nil);
        while ResultSet.Next do
          List.Add(ResultSet.GetStringByName('TABLE_NAME'));
      finally
        ResultSet.Close;
      end;
    end
    else
{$ENDIF}
    begin
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Tables of the defined Catalog and Schema
        ResultSet := Metadata.GetTables(Connection.Catalog, '', '', nil);
        while ResultSet.Next do
          List.Add(ResultSet.GetStringByName('TABLE_NAME'));
      finally
        ResultSet.Close;
      end;
    end;
  end;
end;

{ TZProcedureNamePropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZProcedureNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
{$IFDEF USE_METADATA}
  Catalog, Schema: string;
{$ENDIF}
begin
  Connection := GetObjectProp(GetComponent(0), 'Connection') as TZConnection;
  if Assigned(Connection) and Connection.Connected then
  begin
{$IFDEF USE_METADATA}
    if GetComponent(0) is TZSqlMetadata then
    begin
      Catalog := GetStrProp(GetComponent(0), 'Catalog');
      Schema := GetStrProp(GetComponent(0), 'Schema');
{$IFDEF SHOW_WARNING}
      if not (IsEmpty(Catalog) and IsEmpty(Schema)) or
       (MessageDlg(SPropertyQuery + CRLF + SPropertyProcedures + CRLF +
        SPropertyExecute, mtWarning, [mbYes,mbNo], 0) = mrYes) then
{$ENDIF}
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Procedures of the defined Catalog and Schema
        ResultSet := Metadata.GetProcedures(Catalog, Schema, '');
        while ResultSet.Next do
          List.Add(ResultSet.GetStringByName('PROCEDURE_NAME'));
      finally
        ResultSet.Close;
      end;
    end
    else
{$ENDIF}
    begin
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Procedures
        ResultSet := Metadata.GetProcedures(Connection.Catalog, '', '');
        while ResultSet.Next do
          List.Add(ResultSet.GetStringByName('PROCEDURE_NAME'));
      finally
        ResultSet.Close;
      end;
    end;
  end;
end;

{ TZSequenceNamePropertyEditor }

{**
  Gets a selected string value.
  @return a selected string value.
}
procedure TZSequenceNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
{$IFDEF USE_METADATA}
  Catalog, Schema: string;
{$ENDIF}
begin
  Connection := GetObjectProp(GetComponent(0), 'Connection') as TZConnection;
  if Assigned(Connection) and Connection.Connected then
  begin
{$IFDEF USE_METADATA}
    if GetComponent(0) is TZSqlMetadata then
    begin
      Catalog := GetStrProp(GetComponent(0), 'Catalog');
      Schema := GetStrProp(GetComponent(0), 'Schema');
{$IFDEF SHOW_WARNING}
      if not (IsEmpty(Catalog) and IsEmpty(Schema)) or
       (MessageDlg(SPropertyQuery + CRLF + SPropertySequences + CRLF +
        SPropertyExecute, mtWarning, [mbYes,mbNo], 0) = mrYes) then
{$ENDIF}
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Procedures of the defined Catalog and Schema
        ResultSet := Metadata.GetSequences(Catalog, Schema, '');
        while ResultSet.Next do
          List.Add(ResultSet.GetStringByName('SEQUENCE_NAME'));
      finally
        ResultSet.Close;
      end;
    end
    else
{$ENDIF}
    begin
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Procedures
        ResultSet := Metadata.GetSequences(Connection.Catalog, '', '');
        while ResultSet.Next do
          List.Add(ResultSet.GetStringByName('SEQUENCE_NAME'));
      finally
        ResultSet.Close;
      end;
    end;
  end;
end;

{ TZProtocolPropertyEditor }

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZProtocolPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZProtocolPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  if GetComponent(0) is TZConnection then
    (GetComponent(0) as TZConnection).Connected := False;
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZProtocolPropertyEditor.GetValueList(List: TStrings);
var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  Drivers := DriverManager.GetDrivers;
  Protocols := nil;
  for I := 0 to Drivers.Count - 1 do
  begin
    Protocols := (Drivers[I] as IZDriver).GetSupportedProtocols;
    for J := Low(Protocols) to High(Protocols) do
      List.Append(Protocols[J]);
  end;
end;

{ TZDatabasePropertyEditor }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TZDatabasePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  if GetComponent(0) is TZConnection then
  begin
    if ((GetComponent(0) as TZConnection).Protocol = 'mssql') or
    ((GetComponent(0) as TZConnection).Protocol = 'sybase') then
      Result := inherited GetAttributes
    else
      Result := [paDialog];
  end;
end;

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZDatabasePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZDatabasePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  if GetComponent(0) is TZConnection then
    (GetComponent(0) as TZConnection).Connected := False;
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZDatabasePropertyEditor.GetValueList(List: TStrings);
var
  DbcConnection: IZConnection;
  Url: string;
begin
  if GetComponent(0) is TZConnection then
  try
    if (GetComponent(0) as TZConnection).Port = 0 then
      Url := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s', [
        (GetComponent(0) as TZConnection).Protocol,
        (GetComponent(0) as TZConnection).HostName,
        '',
        (GetComponent(0) as TZConnection).User,
        (GetComponent(0) as TZConnection).Password])
    else
      Url := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s', [
        (GetComponent(0) as TZConnection).Protocol,
        (GetComponent(0) as TZConnection).HostName,
        (GetComponent(0) as TZConnection).Port,
        '',
        (GetComponent(0) as TZConnection).User,
        (GetComponent(0) as TZConnection).Password]);

    (GetComponent(0) as TZConnection).ShowSqlHourGlass;
    try
      DbcConnection := DriverManager.GetConnectionWithParams(Url,
        (GetComponent(0) as TZConnection).Properties);

      with DbcConnection.GetMetadata.GetCatalogs do
      try
        while Next do
          List.Append(GetStringByName('TABLE_CAT'));
      finally
        Close;
      end;

    finally
      (GetComponent(0) as TZConnection).HideSqlHourGlass;
    end;
  except
//    raise;
  end;
end;

{**
  Brings up the proper database property editor dialog.
}
procedure TZDatabasePropertyEditor.Edit;
var
  OD: TOpenDialog;
begin
  if GetComponent(0) is TZConnection then
  begin
    if ((GetComponent(0) as TZConnection).Protocol = 'mssql') or
    ((GetComponent(0) as TZConnection).Protocol = 'sybase') then
      inherited
{$IFNDEF UNIX}
{$IFNDEF FPC}
{$IFDEF ENABLE_ADO}
    else
    if ((GetComponent(0) as TZConnection).Protocol = 'ado') then
      (GetComponent(0) as TZConnection).Database := PromptDataSource(Application.Handle,
        (GetComponent(0) as TZConnection).Database)
{$ENDIF}
{$ENDIF}
{$ENDIF}
    else
    begin
      OD := TOpenDialog.Create(nil);
      try
        OD.InitialDir := ExtractFilePath((GetComponent(0) as TZConnection).Database);
        if OD.Execute then
          (GetComponent(0) as TZConnection).Database := OD.FileName;
      finally
        OD.Free;
      end;
    end;
  end
  else
    inherited;
end;

{ TZCatalogPropertyEditor }

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZCatalogPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZCatalogPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZCatalogPropertyEditor.GetValueList(List: TStrings);
var
  DbcConnection: IZConnection;
  Url: string;
begin
  if GetComponent(0) is TZConnection then
  try
    if (GetComponent(0) as TZConnection).Port = 0 then
      Url := Format('zdbc:%s://%s/%s?UID=%s;PWD=%s', [
        (GetComponent(0) as TZConnection).Protocol,
        (GetComponent(0) as TZConnection).HostName,
        '',
        (GetComponent(0) as TZConnection).User,
        (GetComponent(0) as TZConnection).Password])
    else
      Url := Format('zdbc:%s://%s:%d/%s?UID=%s;PWD=%s', [
        (GetComponent(0) as TZConnection).Protocol,
        (GetComponent(0) as TZConnection).HostName,
        (GetComponent(0) as TZConnection).Port,
        '',
        (GetComponent(0) as TZConnection).User,
        (GetComponent(0) as TZConnection).Password]);

    (GetComponent(0) as TZConnection).ShowSqlHourGlass;
    try
      DbcConnection := DriverManager.GetConnectionWithParams(Url,
        (GetComponent(0) as TZConnection).Properties);

      with DbcConnection.GetMetadata.GetCatalogs do
      try
        while Next do
          List.Append(GetStringByName('TABLE_CAT'));
      finally
        Close;
      end;

    finally
      (GetComponent(0) as TZConnection).HideSqlHourGlass;
    end;
  except
//    raise;
  end;
end;

{$IFDEF USE_METADATA}

{ TZCatalogProperty }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZCatalogProperty.GetValueList(List: TStrings);
var
  Connection: TZConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  Connection := GetObjectProp(GetComponent(0), 'Connection') as TZConnection;
  if Assigned(Connection) and Connection.Connected then
  try
    Metadata := Connection.DbcConnection.GetMetadata;
    ResultSet := Metadata.GetCatalogs;
    while ResultSet.Next do
      List.Add(ResultSet.GetStringByName('TABLE_CAT'));
  finally
    ResultSet.Close;
  end;
end;

{ TZColumnNamePropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZColumnNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
  Catalog, Schema, TableName: string;
begin
  Connection := GetObjectProp(GetComponent(0), 'Connection') as TZConnection;
  if Assigned(Connection) and Connection.Connected then
  begin
    Catalog := GetStrProp(GetComponent(0), 'Catalog');
    Schema := GetStrProp(GetComponent(0), 'Schema');
    TableName := GetStrProp(GetComponent(0), 'TableName');
{$IFDEF SHOW_WARNING}
    if not IsEmpty(TableName) or not (IsEmpty(Schema) and IsEmpty(Schema)) or
     (MessageDlg(SPropertyQuery + CRLF + SPropertyTables + CRLF +
      SPropertyExecute, mtWarning, [mbYes,mbNo], 0) = mrYes) then
{$ENDIF}
    try
      Metadata := Connection.DbcConnection.GetMetadata;
      // Look for the Columns of the defined Catalog, Schema and TableName
      ResultSet := Metadata.GetColumns(Catalog, Schema, TableName, '');
      while ResultSet.Next do
        if List.IndexOf(ResultSet.GetStringByName('COLUMN_NAME')) = -1 then
          List.Add(ResultSet.GetStringByName('COLUMN_NAME'));
    finally
      ResultSet.Close;
    end;
  end;
end;

{ TZSchemaPropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZSchemaPropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  Connection := GetObjectProp(GetComponent(0), 'Connection') as TZConnection;
  if Assigned(Connection) and Connection.Connected then
  try
    Metadata := Connection.DbcConnection.GetMetadata;
    ResultSet := Metadata.GetSchemas;
    while ResultSet.Next do
      List.Add(ResultSet.GetStringByName('TABLE_SCHEM'));
  finally
    ResultSet.Close;
  end;
end;

{ TZTypeNamePropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZTypeNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  Connection := GetObjectProp(GetComponent(0), 'Connection') as TZConnection;
  if Assigned(Connection) and Connection.Connected then
  try
    Metadata := Connection.DbcConnection.GetMetadata;
    ResultSet := Metadata.GetTypeInfo;
    while ResultSet.Next do
      List.Add(ResultSet.GetStringByName('TYPE_NAME'));
  finally
    ResultSet.Close;
  end;
end;
{$ENDIF}

{$ENDIF}

end.


