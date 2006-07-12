{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Blobs Example Application                 }
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

unit ZBlobsMain;

interface

uses

  SysUtils, {$IFDEF VER140}Types, {$ENDIF}Classes, DB,
{$IFDEF LINUX}
  QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QDBCtrls, QGrids, QDBGrids,
{$ELSE}
  Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids,
{$ENDIF}
  ZConnection, ZDataset,
  ZDbcMySQL, ZDbcPostgreSQL, ZDbcInterbase6, ZDbcCache, ZSqlUpdate;

type
  {** Defines the main form of the application. }
  TfrmMain = class(TForm)
    pnControl: TPanel;
    pnMain: TPanel;
    navMain: TDBNavigator;
    gdMain: TDBGrid;
    pnDetail: TPanel;
    splMain: TSplitter;
    splDetail: TSplitter;
    memText: TDBMemo;
    lblProtocol: TLabel;
    cbxProtocol: TComboBox;
    lblHostName: TLabel;
    edtHostName: TEdit;
    lblDatabase: TLabel;
    edtDatabase: TEdit;
    lblUserName: TLabel;
    edtUserName: TEdit;
    edtPassword: TEdit;
    lblPassword: TLabel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnOpen: TButton;
    btnClose: TButton;
    btnApplyUpdates: TButton;
    btnCancelUpdates: TButton;
    dsMain: TDataSource;
    imgBlob: TDBImage;
    lblTableName: TLabel;
    edtTableName: TEdit;
    lblMemoColumn: TLabel;
    edtMemoColumn: TEdit;
    lblBlobColumn: TLabel;
    edtBlobColumn: TEdit;
    btnLoadImage: TButton;
    dlgOpenFile: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnApplyUpdatesClick(Sender: TObject);
    procedure btnCancelUpdatesClick(Sender: TObject);
    procedure PropertiesChange(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
  private
    FConnection: TZConnection;
    FDataset: TZQuery;
  public
    property Connection: TZConnection read FConnection write FConnection;
    property Dataset: TZQuery read FDataset write FDataset;
  end;

var
  frmMain: TfrmMain;

implementation

{$IFDEF VER140}
  {$R *.xfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

{**
  Initializes this form properties.
  @param Sender an event sender object reference.
}
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Connection := TZConnection.Create(Self);

  Dataset := TZQuery.Create(Self);
  Dataset.Connection := Connection;
  Dataset.RequestLive := True;
  Dataset.CachedUpdates := True;
//  Dataset.IsUniDirectional := True;

  dsMain.Dataset := Dataset;
  PropertiesChange(Self);
end;

{**
  Updates database properties.
  @param Sender an event sender object reference.
}
procedure TfrmMain.PropertiesChange(Sender: TObject);
begin
  Connection.Protocol := cbxProtocol.Text;
  Connection.HostName := edtHostName.Text;
  Connection.Database := edtDatabase.Text;
  Connection.User := edtUserName.Text;
  Connection.Password := edtPassword.Text;

  Dataset.SQL.Text := 'SELECT * FROM ' + edtTableName.Text;
  memText.DataField := edtMemoColumn.Text;
  imgBlob.DataField := edtBlobColumn.Text;
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
  Opens a SQL query object.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnOpenClick(Sender: TObject);
begin
  Dataset.Open;
end;

{**
  Closes a SQL query object.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnCloseClick(Sender: TObject);
begin
  Dataset.Close;
end;

{**
  Posts cached update to database from SQL query.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnApplyUpdatesClick(Sender: TObject);
begin
  Dataset.ApplyUpdates;
  Dataset.CommitUpdates;
end;

{**
  Cancel all previously made update in SQL query.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnCancelUpdatesClick(Sender: TObject);
begin
  Dataset.CancelUpdates;
end;

{**
  Load an image from file and stores it into the blob column.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnLoadImageClick(Sender: TObject);
var
  BlobStream: TStream;
  FileStream: TStream;
begin
  if Dataset.Active and dlgOpenFile.Execute then
  begin
    if Dataset.State <> dsEdit then
      Dataset.Edit;

    BlobStream := Dataset.CreateBlobStream(
      Dataset.FieldByName(edtBlobColumn.Text), bmWrite);
    try
      FileStream := TFileStream.Create(dlgOpenFile.FileName, fmOpenRead);
      try
        BlobStream.CopyFrom(FileStream, FileStream.Size);
      finally
        FileStream.Free;
      end;
    finally
      BlobStream.Free;
    end;

    Dataset.Post;
  end;
end;

end.


