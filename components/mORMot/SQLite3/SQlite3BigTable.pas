/// Virtual tables for Synopse Big Table access for the mORMot framework
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.15
unit SQLite3BigTable;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
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

  Version 1.15
  - first public release, corresponding to mORMot Framework 1.15

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  SysUtils,
  Classes,
  SynCommons,
  SQLite3Commons,
  SynBigTable;

type
  /// REST server with direct access to a Synopse Big Table external database
  // - handle all REST commands via direct TSynBigTableMetaData or
  // TSynBigTableRecord call (using a TSynBigTableTable instance)
  // - is used by TSQLRestServer.URI for faster RESTful direct access
  // - for JOINed SQL statements, the external database is also defined as
  // a SQLite3 virtual table, via the TSQLVirtualTableBigTable[Cursor] classes
  TSQLRestServerStaticBigTable = class(TSQLRestServerStaticRecordBased)
  protected
    /// the associated Big Table instance
    // - either a TSynBigTableMetaData or a TSynBigTableRecord
    fBig: TSynBigTableTable;
    // overriden methods calling the fBig instance
    function EngineRetrieve(TableModelIndex: integer; ID: integer): RawUTF8; override;
    function EngineLockedNextID: Integer;
    function EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false): RawUTF8; override;
    // BLOBs should be access directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(Table: TSQLRecordClass; aID: integer;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(Table: TSQLRecordClass; aID: integer;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    function EngineSearchField(const FieldName: ShortString;
      const FieldValue: array of const; var ResultID: TIntegerDynArray): boolean;
  public
    /// initialize the BigTable instance
    // - aFilename parameter will be used to specify the corresponding file name
    // - aBinaryFile should be TRUE for TSynBigTableMetaData, or FALSE for a
    // TSynBigTableRecord
    constructor Create(aClass: TSQLRecordClass; aServer: TSQLRestServer;
      const aFileName: TFileName = ''; aBinaryFile: boolean=false); override;
    /// release used memory
    // - i.e. the internal TSynBigTableMetaData / TSynBigTableRecord instance
    destructor Destroy; override;
    /// delete a row, calling the current BigTable instance
    // - made public since a TSQLRestServerStatic instance may be created
    // stand-alone, i.e. without any associated Model/TSQLRestServer
    function EngineDelete(Table: TSQLRecordClass; ID: integer): boolean; override;
    /// search for a numerical field value
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName: ShortString; const FieldValue: Integer;
      var ResultID: TIntegerDynArray): boolean; overload; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName: ShortString; FieldValue: RawUTF8;
      var ResultID: TIntegerDynArray): boolean; overload; override;
    /// update an individual record field INTEGER value from a specified ID or Value
    // - if ByID=true, search for RowID=Where, otherwise search for FieldName=Where 
    // - return true on success
    // - this method call RecordCanBeUpdated() to check if update is possible,
    // then call EngineExecute() with the corresponding SQL statement
    function UpdateField(Table: TSQLRecordClass; Where: integer;
      const FieldName: shortstring; FieldValue: integer; ByID: boolean): boolean; override;
    /// create one index for all specific FieldNames at once
    // - this method should better be called before 
    function CreateSQLMultiIndex(Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8=''): boolean; override;
    /// the associated Big Table instance
    // - either a TSynBigTableMetaData or a TSynBigTableRecord
    property BigTable: TSynBigTableTable read fBig;
  end;

  
implementation


{ TSQLRestServerStaticBigTable }

constructor TSQLRestServerStaticBigTable.Create(aClass: TSQLRecordClass;
  aServer: TSQLRestServer; const aFileName: TFileName;
  aBinaryFile: boolean);
begin
  inherited Create(aClass,aServer,aFileName,aBinaryFile);
  if fStoredClassProps.Kind in INSERT_WITH_ID then
    raise EModelException.CreateFmt('%s: %s virtual table can''t be static',
      [fStoredClassProps.SQLTableName,aClass.ClassName]);
  if aBinaryFile then
    fBig := TSynBigTableMetaData.Create(aFileName,fStoredClassProps.SQLTableName) else
    fBig := TSynBigTableRecord.Create(aFileName,fStoredClassProps.SQLTableName);
end;

function TSQLRestServerStaticBigTable.CreateSQLMultiIndex(
  Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
  Unique: boolean; IndexName: RawUTF8): boolean;
begin
  result := false;
  if (self=nil) or (fBig=nil) then
    exit;

end;

destructor TSQLRestServerStaticBigTable.Destroy;
begin
  FreeAndNil(fBig);
  inherited Destroy;
end;

function TSQLRestServerStaticBigTable.EngineDelete(Table: TSQLRecordClass;
  ID: integer): boolean;
begin

end;

function TSQLRestServerStaticBigTable.EngineList(const SQL: RawUTF8;
  ForceAJAX: Boolean): RawUTF8;
begin

end;

function TSQLRestServerStaticBigTable.EngineLockedNextID: Integer;
begin

end;

function TSQLRestServerStaticBigTable.EngineRetrieve(TableModelIndex,
  ID: integer): RawUTF8;
begin

end;

function TSQLRestServerStaticBigTable.EngineRetrieveBlob(
  Table: TSQLRecordClass; aID: integer; BlobField: PPropInfo;
  out BlobData: TSQLRawBlob): boolean;
begin

end;

function TSQLRestServerStaticBigTable.EngineSearchField(
  const FieldName: ShortString; const FieldValue: array of const;
  var ResultID: TIntegerDynArray): boolean;
begin

end;

function TSQLRestServerStaticBigTable.EngineUpdateBlob(
  Table: TSQLRecordClass; aID: integer; BlobField: PPropInfo;
  const BlobData: TSQLRawBlob): boolean;
begin

end;

function TSQLRestServerStaticBigTable.SearchField(
  const FieldName: ShortString; const FieldValue: Integer;
  var ResultID: TIntegerDynArray): boolean;
begin

end;

function TSQLRestServerStaticBigTable.SearchField(
  const FieldName: ShortString; FieldValue: RawUTF8;
  var ResultID: TIntegerDynArray): boolean;
begin

end;

function TSQLRestServerStaticBigTable.UpdateField(Table: TSQLRecordClass;
  Where: integer; const FieldName: shortstring; FieldValue: integer;
  ByID: boolean): boolean;
begin

end;

end.