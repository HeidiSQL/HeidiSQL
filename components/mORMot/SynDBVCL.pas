/// DB VCL dataset using SynDB data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynDBVCL;

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

  Version 1.17
  - first public release, corresponding to Synopse mORMOT Framework 1.17


}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
  Contnrs,
{$ifndef DELPHI5OROLDER}
  Variants,
  MidasLib,
{$endif}
  SynCommons,
  SynDB,
  DB, DBClient;

/// fetch a SynDB TQuery result into a VCL DataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TClientDataSet instance, created from
// the supplied TSQLTable content (a more optimized version may appear later)
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function QueryToDataSet(aOwner: TComponent; aStatement: SynDB.TQuery;
  aMaxRowCount: integer=0): TDataSet;

/// fetch a SynDB TSQLDBStatement result into a VCL DataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TClientDataSet instance, created from
// the supplied TSQLTable content (a more optimized version may appear later)
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function StatementToDataSet(aOwner: TComponent; aStatement: TSQLDBStatement;
  aMaxRowCount: integer=0): TDataSet;


implementation

var
  GlobalDataSetCount: integer;

type
  TQueryWrapper = class(SynDB.TQuery);

function QueryToDataSet(aOwner: TComponent; aStatement: SynDB.TQuery;
  aMaxRowCount: integer): TDataSet;
begin
  if aStatement=nil then
    result := nil else
    result := StatementToDataSet(aOwner,TQueryWrapper(aStatement).fPrepared,aMaxRowCount);
end;

function StatementToDataSet(aOwner: TComponent; aStatement: TSQLDBStatement;
  aMaxRowCount: integer): TDataSet;
var T: TFieldType;
    S,Siz,F,i: integer;
    IsBlob: TSQLFieldBits;
begin
  result := TClientDataSet.Create(aOwner);
  try
    result.Name := 'DS'+IntToStr(GlobalDataSetCount); // unique name
    inc(GlobalDataSetCount);
    if aStatement=nil then
      exit;
    fillchar(IsBlob,sizeof(IsBlob),0);
    for F := 0 to aStatement.ColumnCount-1 do begin
      S := 0;
      case aStatement.ColumnType(F,@Siz) of
      ftDouble, SynDB.ftCurrency: T := ftFloat;
      ftInt64: T := ftLargeint;
      SynDB.ftDate: T := ftDateTime;
      SynDB.ftBlob: begin
        T := ftBlob;
        include(IsBlob,F);
      end;
      ftUTF8:
        if Siz=0 then
          T := ftMemo else begin
          S := Siz;
          {$ifndef UNICODE}
          if aStatement.Connection.Properties.VariantStringAsWideString then
            T := ftWideString else
          {$endif}
            T := ftString;
        end;
      else T := ftString;
      end;
      result.FieldDefs.Add(UTF8ToString(aStatement.ColumnName(F)),T,S);
    end;
    TClientDataSet(result).CreateDataSet;
    TClientDataSet(result).LogChanges := false; // speed up
    i := 0;
    if aStatement.CurrentRow>0 then // append if data pending
      repeat
        result.Append;
        for F := 0 to result.FieldCount-1 do
          if F in IsBlob then
            {$ifdef UNICODE}
            result.Fields[F].AsBytes := aStatement.ColumnBlobBytes(F) else
            {$else}
            result.Fields[F].AsString := aStatement.ColumnBlob(F) else
            {$endif}
            result.Fields[F].AsVariant := aStatement.ColumnVariant(F);
        result.Post;
        inc(i);
        if (aMaxRowCount>0) and (i>aMaxRowCount) then
          break;
      until not aStatement.Step;
    result.First;
  except
    on Exception do
      FreeAndNil(result);
  end;
end;

end.

