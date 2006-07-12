{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Simple Example Application                }
{                                                         }
{    Copyright (c) 1999-2003 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZSimpleMain;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF VER140} Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DBCtrls, ExtCtrls, StdCtrls, ZConnection, ZDataset,
  ZDbcCache, ZAbstractRODataset, ZDbcMySQL, ZDbcPostgreSQL, DB, ZSqlUpdate, 
  ComCtrls, ZDbcInterbase6;

type
  {** Implements the main application form. }
  TfrmMain = class(TForm)
    pnMain: TPanel;
    splMain: TSplitter;
    navMain: TDBNavigator;
    gdMain: TDBGrid;
    pnControl: TPanel;
    lblProtocol: TLabel;
    cbxProtocol: TComboBox;
    lblHostName: TLabel;
    edtHostName: TEdit;
    lblDatabase: TLabel;
    edtDatabase: TEdit;
    lblUserName: TLabel;
    edtUserName: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnOpen: TButton;
    btnClose: TButton;
    btnExecute: TButton;
    dsMain: TDataSource;
    memLog: TMemo;
    splLog: TSplitter;
    btnPrint: TButton;
    btnFilter: TButton;
    btnLocate: TButton;
    btnApplyUpdates: TButton;
    btnCancelUpdates: TButton;
    pcStatements: TPageControl;
    tshQuery: TTabSheet;
    memQuery: TMemo;
    tshInsert: TTabSheet;
    tshUpdate: TTabSheet;
    tshDelete: TTabSheet;
    memInsert: TMemo;
    memUpdate: TMemo;
    memDelete: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure DataSetCalcFields(DataSet: TDataSet);
    procedure DataSetFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure btnFilterClick(Sender: TObject);
    procedure btnLocateClick(Sender: TObject);
    procedure btnApplyUpdatesClick(Sender: TObject);
    procedure btnCancelUpdatesClick(Sender: TObject);
    procedure PropertiesChange(Sender: TObject);
  private
    FConnection: TZConnection;
    FDataset: TZQuery;
    FUpdateSQL: TZUpdateSQL;
  public
    property Connection: TZConnection read FConnection write FConnection;
    property Dataset: TZQuery read FDataset write FDataset;
    property UpdateSQL: TZUpdateSQL read FUpdateSQL write FUpdateSQL;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{**
  Initializes this form properties.
  @param Sender an event sender object reference.
}
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Connection := TZConnection.Create(Self);
  Dataset := TZQuery.Create(Self);
  Dataset.Connection := FConnection;
  Dataset.RequestLive := True;
  Dataset.CachedUpdates := True;
//  Dataset.IsUniDirectional := True;

  Dataset.OnCalcFields := DataSetCalcFields;
  Dataset.OnFilterRecord := DataSetFilterRecord;
//  Dataset.Filtered := True;
  dsMain.Dataset := Dataset;

  UpdateSQL := TZUpdateSQL.Create(Self);
  Dataset.UpdateObject := UpdateSQL;

  PropertiesChange(Self);
end;

{**
  Reads different properties from form controls.
  @param Sender an event sender object reference.
}
procedure TfrmMain.PropertiesChange(Sender: TObject);
begin
  Connection.Protocol := cbxProtocol.Text;
  Connection.HostName := edtHostName.Text;
  Connection.Database := edtDatabase.Text;
  Connection.User := edtUserName.Text;
  Connection.Password := edtPassword.Text;

  Dataset.SQL := memQuery.Lines;
  Dataset.Fields.Clear;

  UpdateSQL.InsertSQL := memInsert.Lines;
  UpdateSQL.ModifySQL := memUpdate.Lines;
  UpdateSQL.DeleteSQL := memDelete.Lines;
end;

{**
  Establishes a connection to SQL server.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnConnectClick(Sender: TObject);
begin
  Connection.Connect;
end;

{**
  Closes a connection to SQL server.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnDisconnectClick(Sender: TObject);
begin
  Connection.Disconnect;
end;

{**
  Opens a SQL query and read data from server.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnOpenClick(Sender: TObject);
var
  I: Integer;
  FieldDefs: TFieldDefs;
  CalcField: TStringField;
begin
  Dataset.Close;
  FieldDefs := Dataset.FieldDefs;
  FieldDefs.Update;

  if Dataset.FindField('Calculated') = nil then
  begin
    for I := 0 to FieldDefs.Count - 1 do
      FieldDefs[I].CreateField(Dataset).DataSet := Dataset;

    CalcField := TStringField.Create(nil);
    CalcField.Size := 10;
    CalcField.FieldName := 'Calculated';
    CalcField.FieldKind := fkCalculated;
    CalcField.Visible := True;
    CalcField.DataSet := Dataset;
  end;

  Dataset.Open;
end;

{**
  Closes the SQL query.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Dataset.Close;
end;

{**
  Executes the SQL query.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnExecuteClick(Sender: TObject);
begin
  Dataset.ExecSQL;
end;

{**
  Prints SQL query field values.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnPrintClick(Sender: TObject);
var
  I: Integer;
  Temp: string;
begin
  Temp := '';
  for I := 0 to Dataset.FieldCount - 1 do
  begin
    if Temp <> '' then Temp := Temp + ',';
    Temp := Temp + '"' + Dataset.Fields[I].DisplayName + '"';
  end;
  memLog.Lines.Append(Temp);
  memLog.Lines.Append('------------------------------------------');

  Dataset.First;
  while not Dataset.EOF do
  begin
    Temp := '';
    for I := 0 to Dataset.FieldCount - 1 do
    begin
      if Temp <> '' then Temp := Temp + ',';
      Temp := Temp + '"' + Dataset.Fields[I].AsString + '"';
    end;
    memLog.Lines.Append(Temp);

    Dataset.Next;
  end;
  memLog.Lines.Append('');
end;

{**
  Fills a dataset calculated fields.
  @param Dataset a dataset sender object reference.
}
procedure TfrmMain.DataSetCalcFields(DataSet: TDataSet);
begin
  Dataset.FieldByName('Calculated').AsString :=
    Copy('### ' + Dataset.Fields[0].AsString + ' ###', 1, 10);
end;

{**
  Filters the dataset records.
  @param Dataset a dataset sender object reference.
  @param Accept <code>True</code> to accept the field
    and <code>False</code> to hide it.
}
procedure TfrmMain.DataSetFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept := not ((Dataset.Fields[0].AsInteger mod 3) = 0);
end;

{**
  Sets a new filter for SQL query.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnFilterClick(Sender: TObject);
begin
//  Dataset.Filtered := not Dataset.Filtered;
  if Dataset.ShowRecordTypes = [utModified, utInserted, utUnmodified] then
    Dataset.ShowRecordTypes := [utModified, utDeleted]
  else Dataset.ShowRecordTypes := [utModified, utInserted, utUnmodified];
end;

{**
  Locates a record in SQL query.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnLocateClick(Sender: TObject);
begin
//  Dataset.Locate('0, fld,"Calculated"',
//    VarArrayOf([7,'Mango','### 7 ###']), []);
//  Dataset.Locate('ID, 1', VarArrayOf([7,'MAN']),
//    [loCaseInsensitive, loPartialKey]);
//  Dataset.Locate('"Id", "FLD"', VarArrayOf([7,'MANGO']), [loCaseInsensitive]);

////  Dataset.Lookup('id, fld', VarArrayOf([7,'Mango']), '"Calculated"')
end;

{**
  Posts query updates to SQL server.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnApplyUpdatesClick(Sender: TObject);
begin
  Dataset.ApplyUpdates;
  Dataset.CommitUpdates;
end;

{**
  Cancels all previously made update in SQL query.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnCancelUpdatesClick(Sender: TObject);
begin
  Dataset.CancelUpdates;
end;

end.

