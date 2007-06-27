{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Design Time Test Application              }
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

unit ZDesignTimeMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ZSqlUpdate, DB, ZAbstractRODataset, ZAbstractDataset, ZDataset,
  ZConnection, Grids, DBGrids, ExtCtrls, DBCtrls, ZDbcMySql, StdCtrls;

type
  TfrmMain = class(TForm)
    conMain: TZConnection;
    qrMain: TZQuery;
    dsMain: TDataSource;
    navMain: TDBNavigator;
    gdMain: TDBGrid;
    upMain: TZUpdateSQL;
    procedure qrMainCalcFields(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{**
  Calculates dataset calculated fields.
  @param DataSet a dataset object.
}
procedure TfrmMain.qrMainCalcFields(DataSet: TDataSet);
begin
  DataSet.FieldByName('dep_calc').AsString := 'AbCdEfGhI';
end;

end.

