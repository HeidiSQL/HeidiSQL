{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 SQL Monitor component                   }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
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

