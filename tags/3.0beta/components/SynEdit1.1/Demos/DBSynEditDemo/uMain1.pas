{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uMain1.pas, released 2000-06-23.

The Original Code is part of the DBEditDemo1 project, written by Michael Hieke
for the SynEdit component suite.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: uMain1.pas,v 1.2 2000/11/22 08:34:13 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit uMain1;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, DBCtrls, SynEdit, SynDBEdit, Db, DBTables, ExtCtrls,
  SynEditHighlighter, SynHighlighterPas, SynHighlighterPerl;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    DataSource1: TDataSource;
    Table1: TTable;
    DBSynEdit1: TDBSynEdit;
    DBNavigator1: TDBNavigator;
    DBEdit1: TDBEdit;
    SynPasSyn1: TSynPasSyn;
    SynPerlSyn1: TSynPerlSyn;
    procedure FormCreate(Sender: TObject);
    procedure UpdateHighlighterNeeded(DataSet: TDataSet);
    procedure Table1UpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    fUpdateHL: boolean;
    procedure IdleAction(Sender: TObject; var Done: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const
  SourceTableName = 'DemoSrc.dbf';
  HighlighterClassField = 'HLCLASS';
  DemoSourceField = 'SRC';

procedure TForm1.FormCreate(Sender: TObject);
var
  TableName: string;
  ASQL: TQuery;
begin
  Application.OnIdle := IdleAction;
  fUpdateHL := TRUE;
  TableName := ExtractFilePath(Application.ExeName) + SourceTableName;

  if not FileExists(TableName) then begin
    ASQL := TQuery.Create(Self);
    try
      ASQL.SQL.Text :=
        'CREATE TABLE "' + TableName + '" ('#13#10 +
        '  ' + HighlighterClassField + ' CHAR(20),'#13#10 +
        '  ' + DemoSourceField + ' CHAR(250)'#13#10 +
        ')';

      ASQL.ExecSQL;
    finally
      ASQL.Free;
    end;
  end;
  Table1.TableName := TableName;
  Table1.Active := TRUE;
  DBSynEdit1.DataField := DemoSourceField;
  DBEdit1.DataField := HighlighterClassField;
end;

procedure TForm1.UpdateHighlighterNeeded(DataSet: TDataSet);
begin
  fUpdateHL := TRUE;
end;

procedure TForm1.Table1UpdateRecord(DataSet: TDataSet; UpdateKind: TUpdateKind;
  var UpdateAction: TUpdateAction);
begin
  fUpdateHL := TRUE;
end;

procedure TForm1.IdleAction(Sender: TObject; var Done: Boolean);
var
  HLName: string;
  i: integer;
  HL: TSynCustomHighlighter;
begin
  if fUpdateHL then begin
    fUpdateHL := FALSE;
    HLName := DBEdit1.Text;
    HL := nil;
    for i := 0 to ComponentCount - 1 do
      if CompareText(Components[i].ClassName, HLName) = 0 then begin
        if Components[i] is TSynCustomHighlighter then begin
          HL := Components[i] as TSynCustomHighlighter;
          break;
        end;
      end;
    DBSynEdit1.Highlighter := HL;
  end;
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  fUpdateHL := TRUE;
end;

end.

