{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Extra component registration              }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZUtilsReg;

interface

{$INCLUDE ..\Zeos.Inc}

{ Component registration }
procedure Register;

implementation

uses Classes, ZParser, ZHexEdit, ZDataGrid;

{ Register component in component palette }
procedure Register;
begin
  RegisterComponents(ZEOS_PALETTE, [TZParser]);
  RegisterComponents(ZEOS_PALETTE, [TZHexEdit]);
  RegisterComponents(ZEOS_PALETTE, [TZDataGrid]);
end;

end.
