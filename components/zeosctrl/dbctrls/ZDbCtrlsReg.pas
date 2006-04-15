{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Dbware controls registration              }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZDbCtrlsReg;

interface

{$INCLUDE ..\Zeos.inc}

procedure Register;

implementation

uses Classes, ZFilterDlg, ZFindDlg, ZGrid, ZHexEdit, ZImage;

{ Register component in a component pallette }
procedure Register;
begin
  RegisterComponents(ZEOS_PALETTE, [TZFilterDialog]);
  RegisterComponents(ZEOS_PALETTE, [TZFindDialog]);
  RegisterComponents(ZEOS_PALETTE, [TZDbGrid]);
  RegisterComponents(ZEOS_PALETTE, [TZDbHexEdit]);
  RegisterComponents(ZEOS_PALETTE, [TZDbImage]);
end;

end.
