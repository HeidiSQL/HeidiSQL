{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Blobs Example Application                 }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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
  ZDbcMySQL, ZDbcPostgreSQL, ZDbcInterbase6, ZDbcCache, ZSqlUpdate, ZSqlMetadata,
  ZDbcIntfs;

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
    cbxOidAsBlob: TCheckBox;
    procedure cbxOidAsBlobClick(Sender: TObject);
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
    FMetadata: TZSqlMetadata;
  public
    property Connection: TZConnection read FConnection write FConnection;
    property Metadata:TZSqlMetadata read FMetadata write FMetadata;
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
  Dataset.ReadOnly := False;
  // Dataset.CachedUpdates := True;

  dsMain.Dataset := Dataset;
  PropertiesChange(Self);
end;

{**
  Updates database properties.
  @param Sender an event sender object reference.
}
procedure TfrmMain.PropertiesChange(Sender: TObject);
Var iIndex: Integer;
begin
  Connection.Protocol := cbxProtocol.Text;
  Connection.HostName := edtHostName.Text;
  Connection.Database := edtDatabase.Text;
  Connection.User := edtUserName.Text;
  Connection.Password := edtPassword.Text;

  Dataset.SQL.Text := 'SELECT * FROM ' + edtTableName.Text;
  memText.DataField := edtMemoColumn.Text;
  imgBlob.DataField := edtBlobColumn.Text;

  cbxOidAsBlob.Enabled := (pos('postgre', cbxProtocol.Text) <> 0);

  if cbxOidAsBlob.Enabled then
     cbxOidAsBlobClick(Sender)
  else
     Connection.Properties.Clear;

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
var sTyp: String;
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
  Dataset.Post;
  // Dataset.CommitUpdates;
end;

{**
  Cancel all previously made update in SQL query.
  @param Sender an event sender object reference.
}
procedure TfrmMain.btnCancelUpdatesClick(Sender: TObject);
begin
  // Dataset.CancelUpdates;
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
  if Dataset.Active then begin
    dlgOpenFile.Filter := 'Bitmap files (*.bmp)|*.BMP';
    if dlgOpenFile.Execute then begin
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
  end;  
end;

procedure TfrmMain.cbxOidAsBlobClick(Sender: TObject);
begin
  if cbxOidAsBlob.Checked then
    Connection.Properties.Add('oidasblob=true')
  else
    Connection.Properties.Clear;
end;

end.


