{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 SQL Monitor component                   }
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

unit ZSqlTestForm;

{$I ZComponent.inc}

interface

uses

{$IFDEF WIN32}
  Windows, Messages,
{$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DBGrids, Buttons, DB, ZDataset, ZMessages;

type

  { TZeosSQLEditorTestForm }

  TZeosSQLEditorTestForm = class(TForm)
  private
    { private declarations }
    Button1: TButton;
    Datasource1: TDatasource;
    dbGrid1: TdbGrid;
    Panel1: TPanel;
  public
    { public declarations }
    ZeosSQL: TZReadOnlyQuery;
    constructor Create(AOwner: TComponent);
  end;

var
  ZeosSQLEditorTestForm: TZeosSQLEditorTestForm;

implementation

constructor TZeosSQLEditorTestForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := SFormTest;
  ClientHeight := 300;
  ClientWidth := 683;
  Height := 300;
  Left := 291;
  Top := 323;
  Width := 683;
  ZeosSQL := TZReadOnlyQuery.Create(self);
  Datasource1 := TDataSource.Create(self);
  Datasource1.DataSet := ZeosSQL;
  Panel1 := TPanel.Create(self);
  with Panel1 do
  begin
    Parent := self;
    Anchors := [akTop,akLeft,akRight];
    Align := alTop;
    Left :=0;
    Height := 42;
    Top := 0;
    Width := 683;
    TabStop := False;
  end;
  Button1 := TButton.Create(self);
  with Button1 do
  begin
    Parent := Panel1;
    Anchors := [akTop,akLeft];
    Cancel := True;
    Default := True;
    ModalResult := mrOk;
    Caption := SButtonClose;
    Left := 600;
    Height := 25;
    Top := 8;
    Width := 75;
    TabOrder := 0;
    TabStop := True;
  end;
  dbGrid1 := TdbGrid.Create(self);
  with dbGrid1 do
  begin
    Parent := self;
    Anchors := [akTop,akLeft,akRight,akBottom];
    DataSource := Datasource1;
    Options := [dgTitles,dgIndicator,dgColumnResize,dgColLines,dgRowLines,
                dgTabs,dgAlwaysShowSelection,dgConfirmDelete,dgCancelOnExit];
    ReadOnly := True;
    Align := alClient;
    DefaultRowHeight := 24;
    Left := 0;
    Height := 258;
    TabOrder := 1;
    TabStop := True;
    Top := 42;
    Width := 683;
  end;
end;

end.

