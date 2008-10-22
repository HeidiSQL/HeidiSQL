
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntDBGrids_Design;

{$INCLUDE compilers.inc}

interface

uses
  DesignEditors, DesignIntf; 

type
  TTntDBGridEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string{TNT-ALLOW string}; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  TntDBGrids, DsnDBCst, TntDesignEditors_Design;

procedure Register;
begin
  RegisterComponentEditor(TTntDBGrid, TTntDBGridEditor);
end;

{ TTntDBGridEditor }

function TTntDBGridEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TTntDBGridEditor.GetVerb(Index: Integer): string{TNT-ALLOW string};
begin
  Result := DsnDBCst.SDBGridColEditor;
end;

procedure TTntDBGridEditor.ExecuteVerb(Index: Integer);
begin
  EditPropertyWithDialog(Component, 'Columns', Designer);
end;

end.
