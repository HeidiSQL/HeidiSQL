{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Variant Processing Classes                }
{                                                         }
{            Originally written by Sergey Seroukhov       }
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

unit ZVariant;

interface

{$I ZCore.inc}

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, ZCompatibility, ZClasses, ZSysUtils;

const
  {** Precision for float values comparison }
  FLOAT_COMPARE_PRECISION = 1.e-5;

type
  {** Defines variant types. }
  TZVariantType = (vtNull, vtBoolean, vtInteger, vtFloat, vtString,
    vtUnicodeString, vtDateTime, vtPointer, vtInterface);

  {** Defines a variant structure. }
  TZVariant = packed record
    VType: TZVariantType;
    VBoolean: Boolean;
    VInteger: Int64;
    VFloat: Extended;
    VString: AnsiString;
    VUnicodeString: WideString;
    VDateTime: TDateTime;
    VPointer: Pointer;
    VInterface: IZInterface;
  end;

  {** Defines an array of variants. }
  TZVariantDynArray = array of TZVariant;

  {** Defines a variant processing exception. }
  EZVariantException = class (Exception);

  {** Defines an interface for variant data. }
  {** Defines a Variant Manager interface. }
  IZVariantManager = interface (IZInterface)
    ['{DAA373D9-1A98-4AA8-B65E-4C23167EE83F}']

    function IsNull(const Value: TZVariant): Boolean;
    procedure SetNull(var Value: TZVariant);

    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
    procedure Assign(const SrcValue: TZVariant; var DstValue: TZVariant);
    function Clone(const Value: TZVariant): TZVariant;
    function Compare(const Value1, Value2: TZVariant): Integer;

    function GetAsBoolean(const Value: TZVariant): Boolean;
    function GetAsInteger(const Value: TZVariant): Int64;
    function GetAsFloat(const Value: TZVariant): Extended;
    function GetAsString(const Value: TZVariant): AnsiString;
    function GetAsUnicodeString(const Value: TZVariant): WideString;
    function GetAsDateTime(const Value: TZVariant): TDateTime;
    function GetAsPointer(const Value: TZVariant): Pointer;
    function GetAsInterface(const Value: TZVariant): IZInterface;

    procedure SetAsBoolean(var Value: TZVariant; Data: Boolean);
    procedure SetAsInteger(var Value: TZVariant; Data: Int64);
    procedure SetAsFloat(var Value: TZVariant; Data: Extended);
    procedure SetAsString(var Value: TZVariant; const Data: AnsiString);
    procedure SetAsUnicodeString(var Value: TZVariant; const Data: WideString);
    procedure SetAsDateTime(var Value: TZVariant; Data: TDateTime);
    procedure SetAsPointer(var Value: TZVariant; Data: Pointer);
    procedure SetAsInterface(var Value: TZVariant; Data: IZInterface);

    function OpAdd(const Value1, Value2: TZVariant): TZVariant;
    function OpSub(const Value1, Value2: TZVariant): TZVariant;
    function OpMul(const Value1, Value2: TZVariant): TZVariant;
    function OpDiv(const Value1, Value2: TZVariant): TZVariant;
    function OpMod(const Value1, Value2: TZVariant): TZVariant;
    function OpPow(const Value1, Value2: TZVariant): TZVariant;
    function OpAnd(const Value1, Value2: TZVariant): TZVariant;
    function OpOr(const Value1, Value2: TZVariant): TZVariant;
    function OpXor(const Value1, Value2: TZVariant): TZVariant;
    function OpNot(const Value: TZVariant): TZVariant;
    function OpNegative(const Value: TZVariant): TZVariant;
    function OpEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpNotEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpMore(const Value1, Value2: TZVariant): TZVariant;
    function OpLess(const Value1, Value2: TZVariant): TZVariant;
    function OpMoreEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpLessEqual(const Value1, Value2: TZVariant): TZVariant;
  end;

  {** Implements a variant manager with strict convertion rules. }
  TZDefaultVariantManager = class (TInterfacedObject, IZVariantManager)
  protected
    procedure RaiseTypeMismatchError;
    procedure RaiseUnsupportedOperation;
  public
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
      virtual;
    procedure Assign(const SrcValue: TZVariant; var DstValue: TZVariant);
    function Clone(const Value: TZVariant): TZVariant;
    function Compare(const Value1, Value2: TZVariant): Integer;

    function IsNull(const Value: TZVariant): Boolean;
    procedure SetNull(var Value: TZVariant);

    function GetAsBoolean(const Value: TZVariant): Boolean;
    function GetAsInteger(const Value: TZVariant): Int64;
    function GetAsFloat(const Value: TZVariant): Extended;
    function GetAsString(const Value: TZVariant): AnsiString;
    function GetAsUnicodeString(const Value: TZVariant): WideString;
    function GetAsDateTime(const Value: TZVariant): TDateTime;
    function GetAsPointer(const Value: TZVariant): Pointer;
    function GetAsInterface(const Value: TZVariant): IZInterface;

    procedure SetAsBoolean(var Value: TZVariant; Data: Boolean);
    procedure SetAsInteger(var Value: TZVariant; Data: Int64);
    procedure SetAsFloat(var Value: TZVariant; Data: Extended);
    procedure SetAsString(var Value: TZVariant; const Data: AnsiString);
    procedure SetAsUnicodeString(var Value: TZVariant; const Data: WideString);
    procedure SetAsDateTime(var Value: TZVariant; Data: TDateTime);
    procedure SetAsPointer(var Value: TZVariant; Data: Pointer);
    procedure SetAsInterface(var Value: TZVariant; Data: IZInterface);

    function OpAdd(const Value1, Value2: TZVariant): TZVariant;
    function OpSub(const Value1, Value2: TZVariant): TZVariant;
    function OpMul(const Value1, Value2: TZVariant): TZVariant;
    function OpDiv(const Value1, Value2: TZVariant): TZVariant;
    function OpMod(const Value1, Value2: TZVariant): TZVariant;
    function OpPow(const Value1, Value2: TZVariant): TZVariant;
    function OpAnd(const Value1, Value2: TZVariant): TZVariant;
    function OpOr(const Value1, Value2: TZVariant): TZVariant;
    function OpXor(const Value1, Value2: TZVariant): TZVariant;
    function OpNot(const Value: TZVariant): TZVariant;
    function OpNegative(const Value: TZVariant): TZVariant;
    function OpEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpNotEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpMore(const Value1, Value2: TZVariant): TZVariant;
    function OpLess(const Value1, Value2: TZVariant): TZVariant;
    function OpMoreEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpLessEqual(const Value1, Value2: TZVariant): TZVariant;
  end;

  {** Implements a variant manager with soft convertion rules. }
  TZSoftVariantManager = class (TZDefaultVariantManager)
  public
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
      override;
  end;

type

  {** Represents any value interface. }
  IZAnyValue = interface (IZClonnable)
    ['{E81988B3-FD0E-4524-B658-B309B02F0B6A}']

    function IsNull: Boolean;
    function GetValue: TZVariant;

    function GetBoolean: Boolean;
    function GetInteger: Int64;
    function GetFloat: Extended;
    function GetString: AnsiString;
    function GetUnicodeString: WideString;
    function GetDateTime: TDateTime;
  end;

  {** Implements an any value object. }
  TZAnyValue = class(TZAbstractObject, IZAnyValue, IZComparable)
  private
    FValue: TZVariant;
  public
    constructor Create(const Value: TZVariant);
    constructor CreateWithBoolean(Value: Boolean);
    constructor CreateWithInteger(Value: Int64);
    constructor CreateWithFloat(Value: Extended);
    constructor CreateWithString(const Value: AnsiString);
    constructor CreateWithUnicodeString(const Value: WideString);
    constructor CreateWithDateTime(Value: TDateTime);

    function IsNull: Boolean;
    function GetValue: TZVariant;

    function GetBoolean: Boolean;
    function GetInteger: Int64;
    function GetFloat: Extended;
    function GetString: AnsiString;
    function GetUnicodeString: WideString;
    function GetDateTime: TDateTime;

    function Equals(const Value: IZInterface): Boolean; override;
    function Clone: IZInterface; override;
    function ToString: string; override;
  end;

{**
  Encodes a custom variant value into standard variant.
  @param Value a custom variant value to be encoded.
  @returns an encoded standard variant.
}
function EncodeVariant(const Value: TZVariant): Variant;

{**
  Encodes an array of custom variant values into array of standard variants.
  @param Value an array of custom variant values to be encoded.
  @returns an encoded array of standard variants.
}
function EncodeVariantArray(const Value: TZVariantDynArray): Variant;

{**
  Decodes a standard variant value into custom variant.
  @param Value a standard variant value to be decoded.
  @returns an decoded custom variant.
}
function DecodeVariant(const Value: Variant): TZVariant;

{**
  Decodes an array of standard variant values into array of custom variants.
  @param Value an array of standard variant values to be decoded.
  @returns an decoded array of custom variants.
}
function DecodeVariantArray(const Value: Variant): TZVariantDynArray;

var
  {** Declares a default variant manager with strict convertion rules. }
  DefVarManager: IZVariantManager;

  {** Declares a variant manager with soft convertion rules. }
  SoftVarManager: IZVariantManager;

  {** A NULL Variant Value. }
  NullVariant: TZVariant;

implementation

uses
{$IFNDEF VER130BELOW}
  Variants,
{$ELSE}
  {$IFDEF FPC}
    Variants,
  {$ENDIF}
{$ENDIF}
  Math, ZMessages;

{ TZDefaultVariantManager }

{**
  Assignes one variant value to another one.
  @param SrcValue a source variant value.
  @param DstValue a destination variant value.
}
procedure TZDefaultVariantManager.Assign(const SrcValue: TZVariant;
  var DstValue: TZVariant);
begin
  DstValue.VType := SrcValue.VType;
  case SrcValue.VType of
    vtBoolean: DstValue.VBoolean := SrcValue.VBoolean;
    vtInteger: DstValue.VInteger := SrcValue.VInteger;
    vtFloat: DstValue.VFloat := SrcValue.VFloat;
    vtString: DstValue.VString := SrcValue.VString;
    vtUnicodeString: DstValue.VUnicodeString := SrcValue.VUnicodeString;
    vtDateTime: DstValue.VDateTime := SrcValue.VDateTime;
    vtPointer: DstValue.VPointer := SrcValue.VPointer;
    vtInterface: DstValue.VInterface := SrcValue.VInterface;
  end;
end;

{**
  Clones a variant value.
  @param Value a source variant value.
  @returns a clonned variant value.
}
function TZDefaultVariantManager.Clone(const Value: TZVariant): TZVariant;
begin
  Assign(Value, Result);
end;

{**
  Raises a type mismatch exception.
}
procedure TZDefaultVariantManager.RaiseTypeMismatchError;
begin
  raise EZVariantException.Create(STypesMismatch);
end;

{**
  Raises an unsupported operation exception.
}
procedure TZDefaultVariantManager.RaiseUnsupportedOperation;
begin
  raise EZVariantException.Create(SUnsupportedOperation);
end;

{**
  Converts a specified variant value to a new type.
  @param Value a variant value to be converted.
  @param NewType a type of the result variant value.
  @returns a converted variant value.
}
function TZDefaultVariantManager.Convert(const Value: TZVariant;
  NewType: TZVariantType): TZVariant;
begin
  Result.VType := NewType;
  case NewType of
    vtBoolean:
      case Value.VType of
        vtNull:
          Result.VBoolean := False;
        vtBoolean:
          Result.VBoolean := Value.VBoolean;
        else
          RaiseTypeMismatchError;
      end;
    vtInteger:
      case Value.VType of
        vtNull:
          Result.VInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VInteger := 1
          else Result.VInteger := 0;
        vtInteger:
          Result.VInteger := Value.VInteger;
        else
          RaiseTypeMismatchError;
      end;
    vtFloat:
      case Value.VType of
        vtNull:
          Result.VFloat := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VFloat := 1
          else Result.VFloat := 0;
        vtInteger:
          Result.VFloat := Value.VInteger;
        vtFloat:
          Result.VFloat := Value.VFloat;
        else
          RaiseTypeMismatchError;
      end;
    vtString:
      case Value.VType of
        vtNull:
          Result.VString := '';
        vtString:
          Result.VString := Value.VString;
        vtUnicodeString:
          Result.VString := Value.VUnicodeString;
        else
          RaiseTypeMismatchError;
      end;
    vtUnicodeString:
      case Value.VType of
        vtNull:
          Result.VUnicodeString := '';
        vtString:
          Result.VUnicodeString := Value.VString;
        vtUnicodeString:
          Result.VUnicodeString := Value.VUnicodeString;
        else
          RaiseTypeMismatchError;
      end;
    vtDateTime:
      case Value.VType of
        vtNull:
          Result.VDateTime := 0;
        vtDateTime:
          Result.VDateTime := Value.VDateTime;
        else
          RaiseTypeMismatchError;
      end;
    vtPointer:
      case Value.VType of
        vtNull:
          Result.VPointer := nil;
        vtPointer:
          Result.VPointer := Value.VPointer;
        else
          RaiseTypeMismatchError;
      end;
    vtInterface:
      case Value.VType of
        vtNull:
          Result.VInterface := nil;
        vtInterface:
          Result.VInterface := Value.VInterface;
        else
          RaiseTypeMismatchError;
      end;
  end;
end;

{**
  Compares two variant values.
  @param Value1 the first variant value.
  @param Value2 the second variant value.
  @return <0 if Value1 < Value 2, =0 if Value1 = Value2, >0 if Value1 > Value2
}
function TZDefaultVariantManager.Compare(const Value1,
  Value2: TZVariant): Integer;
var
  TempFloat: Extended;
  TempDateTime: TDateTime;
begin
  case Value1.VType of
    vtNull:
      begin
        if IsNull(Value2) then
          Result := 0
        else Result := -1;
      end;
    vtBoolean:
      begin
        if GetAsBoolean(Value2) then
        begin
          if Value1.VBoolean then
            Result := 0
          else Result := -1;
        end
        else
        begin
          if Value1.VBoolean then
            Result := 1
          else Result := 0;
        end;
      end;
    vtInteger:
      Result := Value1.VInteger - GetAsInteger(Value2);
    vtFloat:
      begin
        TempFloat := GetAsFloat(Value2);
        if Value1.VFloat - TempFloat < -FLOAT_COMPARE_PRECISION then
          Result := -1
        else if Value1.VFloat - TempFloat > FLOAT_COMPARE_PRECISION then
          Result := 1
        else Result := 0;
      end;
    vtString:
      Result := AnsiCompareStr(Value1.VString, GetAsString(Value2));
    vtUnicodeString:
{$IFNDEF VER130BELOW}
      Result := WideCompareStr(Value1.VUnicodeString, GetAsUnicodeString(Value2));
{$ELSE}
      Result := AnsiCompareStr(Value1.VUnicodeString, GetAsString(Value2));
{$ENDIF}
    vtDateTime:
      begin
        TempDateTime := GetAsDateTime(Value2);
        if Value1.VDateTime < TempDateTime then
          Result := -1
        else if Value1.VDateTime > TempDateTime then
          Result := 1
        else Result := 0;
      end;
    vtPointer:
      Result := LongInt(Value1.VPointer) - GetAsInteger(Value2);
    else
      Result := 0;
  end;
end;

{**
  Checks is the specified value NULL.
  @param Value a value to be checked.
  @returns <code>True</code> if variant has NULL value.
}
function TZDefaultVariantManager.IsNull(const Value: TZVariant): Boolean;
begin
  Result := Value.VType = vtNull;
end;

{**
  Sets the NULL value to specified variant.
  @param Value variant value to be set to NULL.
}
procedure TZDefaultVariantManager.SetNull(var Value: TZVariant);
begin
  Value.VType := vtNull;
end;

{**
  Gets a variant to boolean value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsBoolean(
  const Value: TZVariant): Boolean;
begin
  Result := Convert(Value, vtBoolean).VBoolean;
end;

{**
  Gets a variant to integer value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsInteger(
  const Value: TZVariant): Int64;
begin
  Result := Convert(Value, vtInteger).VInteger;
end;

{**
  Gets a variant to float value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsFloat(
  const Value: TZVariant): Extended;
begin
  Result := Convert(Value, vtFloat).VFloat;
end;

{**
  Gets a variant to string value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsString(
  const Value: TZVariant): AnsiString;
begin
  Result := Convert(Value, vtString).VString;
end;

{**
  Gets a variant to unicode string value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsUnicodeString(
  const Value: TZVariant): WideString;
begin
  Result := Convert(Value, vtUnicodeString).VUnicodeString;
end;

{**
  Gets a variant to date and time value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsDateTime(
  const Value: TZVariant): TDateTime;
begin
  Result := Convert(Value, vtDateTime).VDateTime;
end;

{**
  Gets a variant to pointer value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsPointer(
  const Value: TZVariant): Pointer;
begin
  Result := Convert(Value, vtPointer).VPointer;
end;

{**
  Gets a variant to interface value.
  @param Value a variant to be converted.
  @param a result value.
}
function TZDefaultVariantManager.GetAsInterface(
  const Value: TZVariant): IZInterface;
begin
  Result := Convert(Value, vtInterface).VInterface;
end;

{**
  Assignes a boolean value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsBoolean(var Value: TZVariant;
  Data: Boolean);
begin
  Value.VType := vtBoolean;
  Value.VBoolean := Data;
end;

{**
  Assignes an integer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsInteger(var Value: TZVariant;
  Data: Int64);
begin
  Value.VType := vtInteger;
  Value.VInteger := Data;
end;

{**
  Assignes a float value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsFloat(var Value: TZVariant;
  Data: Extended);
begin
  Value.VType := vtFloat;
  Value.VFloat := Data;
end;

{**
  Assignes a string value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsString(var Value: TZVariant;
  const Data: AnsiString);
begin
  Value.VType := vtString;
  Value.VString := Data;
end;

{**
  Assignes a unicode string value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsUnicodeString(var Value: TZVariant;
  const Data: WideString);
begin
  Value.VType := vtUnicodeString;
  Value.VUnicodeString := Data;
end;

{**
  Assignes a datetime value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsDateTime(var Value: TZVariant;
  Data: TDateTime);
begin
  Value.VType := vtDateTime;
  Value.VDateTime := Data;
end;

{**
  Assignes a pointer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsPointer(var Value: TZVariant;
  Data: Pointer);
begin
  Value.VType := vtPointer;
  Value.VPointer := Data;
end;

{**
  Assignes a interface value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure TZDefaultVariantManager.SetAsInterface(var Value: TZVariant;
  Data: IZInterface);
begin
  Value.VType := vtInterface;
  Value.VInterface := Data;
end;

{**
  Performs '+' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpAdd(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: RaiseUnsupportedOperation;
    vtInteger: SetAsInteger(Result, Value1.VInteger + GetAsInteger(Value2));
    vtFloat: SetAsFloat(Result, Value1.VFloat + GetAsFloat(Value2));
    vtString: SetAsString(Result, Value1.VString + GetAsString(Value2));
    vtUnicodeString: SetAsUnicodeString(Result,
      Value1.VUnicodeString + GetAsUnicodeString(Value2));
    vtDateTime: SetAsDateTime(Result, Value1.VDateTime + GetAsDateTime(Value2));
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '&' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpAnd(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: SetAsBoolean(Result, Value1.VBoolean and GetAsBoolean(Value2));
    vtInteger: SetAsInteger(Result, Value1.VInteger and GetAsInteger(Value2));
    vtFloat: RaiseUnsupportedOperation;
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '/' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpDiv(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: RaiseUnsupportedOperation;
    vtInteger: SetAsInteger(Result, Value1.VInteger div GetAsInteger(Value2));
    vtFloat: SetAsFloat(Result, Value1.VFloat / GetAsFloat(Value2));
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  SetAsBoolean(Result, Compare(Value1, Value2) = 0);
end;

{**
  Performs '<' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpLess(const Value1,
  Value2: TZVariant): TZVariant;
begin
  SetAsBoolean(Result, Compare(Value1, Value2) < 0);
end;

{**
  Performs '<=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpLessEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  SetAsBoolean(Result, Compare(Value1, Value2) <= 0);
end;

{**
  Performs '%' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpMod(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: RaiseUnsupportedOperation;
    vtInteger: SetAsInteger(Result, Value1.VInteger mod GetAsInteger(Value2));
    vtFloat: RaiseUnsupportedOperation;
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '>' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpMore(const Value1,
  Value2: TZVariant): TZVariant;
begin
  SetAsBoolean(Result, Compare(Value1, Value2) > 0);
end;

{**
  Performs '>=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpMoreEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  SetAsBoolean(Result, Compare(Value1, Value2) >= 0);
end;

{**
  Performs '*' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpMul(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: RaiseUnsupportedOperation;
    vtInteger: SetAsInteger(Result, Value1.VInteger * GetAsInteger(Value2));
    vtFloat: SetAsFloat(Result, Value1.VFloat * GetAsFloat(Value2));
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs unary '-' operation.
  @param Value the variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpNegative(const Value: TZVariant): TZVariant;
begin
  case Value.VType of
    vtNull: SetNull(Result);
    vtBoolean: RaiseUnsupportedOperation;
    vtInteger: SetAsInteger(Result, -Value.VInteger);
    vtFloat: SetAsFloat(Result, -Value.VFloat);
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '~' operation.
  @param Value the variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpNot(const Value: TZVariant): TZVariant;
begin
  case Value.VType of
    vtNull: SetNull(Result);
    vtBoolean: SetAsBoolean(Result, not Value.VBoolean);
    vtInteger: SetAsInteger(Result, not Value.VInteger);
    vtFloat: RaiseUnsupportedOperation;
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '<>' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpNotEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  SetAsBoolean(Result, Compare(Value1, Value2) <> 0);
end;

{**
  Performs '|' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpOr(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: SetAsBoolean(Result, Value1.VBoolean or GetAsBoolean(Value2));
    vtInteger: SetAsInteger(Result, Value1.VInteger or GetAsInteger(Value2));
    vtFloat: RaiseUnsupportedOperation;
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '^' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpPow(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: RaiseUnsupportedOperation;
    vtInteger: SetAsFloat(Result, Power(Value1.VInteger, GetAsInteger(Value2)));
    vtFloat: SetAsFloat(Result, Power(Value1.VFloat, GetAsFloat(Value2)));
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '-' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpSub(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean: RaiseUnsupportedOperation;
    vtInteger: SetAsInteger(Result, Value1.VInteger - GetAsInteger(Value2));
    vtFloat: SetAsFloat(Result, Value1.VFloat - GetAsFloat(Value2));
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '^' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function TZDefaultVariantManager.OpXor(const Value1,
  Value2: TZVariant): TZVariant;
var
  TempBool1, TempBool2: Boolean;
  TempInteger1, TempInteger2: Int64;
begin
  case Value1.VType of
    vtNull: SetNull(Result);
    vtBoolean:
      begin
        TempBool1 := Value1.VBoolean;
        TempBool2 := GetAsBoolean(Value2);
        SetAsBoolean(Result, (TempBool1 and not TempBool2)
          or (not TempBool1 and TempBool2));
      end;
    vtInteger:
      begin
        TempInteger1 := Value1.VInteger;
        TempInteger2 := GetAsInteger(Value2);
        SetAsInteger(Result, (TempInteger1 and not TempInteger2)
          or (not TempInteger1 and TempInteger2));
      end;
    vtFloat: RaiseUnsupportedOperation;
    vtString: RaiseUnsupportedOperation;
    vtUnicodeString: RaiseUnsupportedOperation;
    vtDateTime: RaiseUnsupportedOperation;
    vtPointer: RaiseUnsupportedOperation;
    vtInterface: RaiseUnsupportedOperation;
  end;
end;

{ TZSoftVariantManager }

{**
  Converts a specified variant value to a new type.
  @param Value a variant value to be converted.
  @param NewType a type of the result variant value.
  @returns a converted variant value.
}
function TZSoftVariantManager.Convert(const Value: TZVariant;
  NewType: TZVariantType): TZVariant;
begin
  Result.VType := NewType;
  case NewType of
    vtBoolean:
      case Value.VType of
        vtNull:
          Result.VBoolean := False;
        vtBoolean:
          Result.VBoolean := Value.VBoolean;
        vtInteger:
          Result.VBoolean := Value.VInteger <> 0;
        vtFloat:
          Result.VBoolean := Value.VFloat <> 0;
        vtString:
          Result.VBoolean := StrToBoolEx(Value.VString);
        vtUnicodeString:
          Result.VBoolean := StrToBoolEx(Value.VUnicodeString);
        vtDateTime:
          Result.VBoolean := Value.VDateTime <> 0;
        vtPointer:
          RaiseTypeMismatchError;
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtInteger:
      case Value.VType of
        vtNull:
          Result.VInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VInteger := 1
          else Result.VInteger := 0;
        vtInteger:
          Result.VInteger := Value.VInteger;
        vtFloat:
          Result.VInteger := Trunc(Value.VFloat);
        vtString:
          Result.VInteger := StrToIntDef(Value.VString, 0);
        vtUnicodeString:
          Result.VInteger := StrToIntDef(Value.VUnicodeString, 0);
        vtDateTime:
          Result.VInteger := Trunc(Value.VDateTime);
        vtPointer:
          Result.VInteger := Integer(Value.VPointer);
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtFloat:
      case Value.VType of
        vtNull:
          Result.VFloat := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VFloat := 1
          else Result.VFloat := 0;
        vtInteger:
          Result.VFloat := Value.VInteger;
        vtFloat:
          Result.VFloat := Value.VFloat;
        vtString:
          Result.VFloat := SqlStrToFloatDef(Value.VString, 0);
        vtUnicodeString:
          Result.VFloat := SqlStrToFloatDef(Value.VUnicodeString, 0);
        vtDateTime:
          Result.VFloat := Value.VDateTime;
        vtPointer:
          RaiseTypeMismatchError;
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtString:
      case Value.VType of
        vtNull:
          Result.VString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VString := 'TRUE'
          else Result.VString := 'FALSE';
        vtInteger:
          Result.VString := IntToStr(Value.VInteger);
        vtFloat:
          Result.VString := FloatToSqlStr(Value.VFloat);
        vtString:
          Result.VString := Value.VString;
        vtUnicodeString:
          Result.VString := Value.VUnicodeString;
        vtDateTime:
          Result.VString := DateTimeToAnsiSQLDate(Value.VDateTime);
        vtPointer:
          RaiseTypeMismatchError;
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtUnicodeString:
      case Value.VType of
        vtNull:
          Result.VUnicodeString := '';
        vtBoolean:
          if Value.VBoolean then
            Result.VUnicodeString := 'True'
          else Result.VUnicodeString := 'False';
        vtInteger:
          Result.VUnicodeString := IntToStr(Value.VInteger);
        vtFloat:
          Result.VUnicodeString := FloatToSqlStr(Value.VFloat);
        vtString:
          Result.VUnicodeString := Value.VString;
        vtUnicodeString:
          Result.VUnicodeString := Value.VUnicodeString;
        vtDateTime:
          Result.VUnicodeString := DateTimeToAnsiSQLDate(Value.VDateTime);
        vtPointer:
          RaiseTypeMismatchError;
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtDateTime:
      case Value.VType of
        vtNull:
          Result.VDateTime := 0;
        vtBoolean:
          RaiseTypeMismatchError;
        vtInteger:
          Result.VDateTime := Value.VInteger;
        vtFloat:
          Result.VDateTime := Value.VFloat;
        vtString:
          Result.VDateTime := AnsiSQLDateToDateTime(Value.VString);
        vtUnicodeString:
          Result.VDateTime := AnsiSQLDateToDateTime(Value.VUnicodeString);
        vtDateTime:
          Result.VDateTime := Value.VDateTime;
        vtPointer:
          RaiseTypeMismatchError;
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtPointer:
      case Value.VType of
        vtNull:
          Result.VPointer := nil;
        vtBoolean:
          RaiseTypeMismatchError;
        vtInteger:
          Result.VPointer := Pointer(Value.VInteger);
        vtFloat:
          RaiseTypeMismatchError;
        vtString:
          RaiseTypeMismatchError;
        vtUnicodeString:
          RaiseTypeMismatchError;
        vtDateTime:
          RaiseTypeMismatchError;
        vtPointer:
          Result.VPointer := Value.VPointer;
        vtInterface:
          RaiseTypeMismatchError;
      end;
    vtInterface:
      case Value.VType of
        vtNull:
          Result.VInterface := nil;
        vtBoolean:
          RaiseTypeMismatchError;
        vtInteger:
          RaiseTypeMismatchError;
        vtFloat:
          RaiseTypeMismatchError;
        vtString:
          RaiseTypeMismatchError;
        vtUnicodeString:
          RaiseTypeMismatchError;
        vtDateTime:
          RaiseTypeMismatchError;
        vtPointer:
          RaiseTypeMismatchError;
        vtInterface:
          Result.VInterface := Value.VInterface;
      end;
  end;
end;

{ TZAnyValue }

{**
  Constructs this object and assignes the main properties.
  @param Value an any value.
}
constructor TZAnyValue.Create(const Value: TZVariant);
begin
  FValue := Value;
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a boolean value.
}
constructor TZAnyValue.CreateWithBoolean(Value: Boolean);
begin
  DefVarManager.SetAsBoolean(FValue, Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a datetime value.
}
constructor TZAnyValue.CreateWithDateTime(Value: TDateTime);
begin
  DefVarManager.SetAsDateTime(FValue, Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a float value.
}
constructor TZAnyValue.CreateWithFloat(Value: Extended);
begin
  DefVarManager.SetAsFloat(FValue, Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a integer value.
}
constructor TZAnyValue.CreateWithInteger(Value: Int64);
begin
  DefVarManager.SetAsInteger(FValue, Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a string value.
}
constructor TZAnyValue.CreateWithString(const Value: AnsiString);
begin
  DefVarManager.SetAsString(FValue, Value);
end;

{**
  Constructs this object and assignes the main properties.
  @param Value a unicode string value.
}
constructor TZAnyValue.CreateWithUnicodeString(const Value: WideString);
begin
  DefVarManager.SetAsUnicodeString(FValue, Value);
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZAnyValue.Clone: IZInterface;
begin
  Result := TZAnyValue.Create(FValue);
end;

{**
  Compares this and another property.
  @return <code>True</code> is properties are equal.
}
function TZAnyValue.Equals(const Value: IZInterface): Boolean;
var
  Temp: IZAnyValue;
begin
  if Value <> nil then
  begin
    if Value.QueryInterface(IZAnyValue, Temp) = 0 then
    begin
      Result := SoftVarManager.Compare(FValue, Temp.GetValue) = 0;
      Temp := nil;
    end else
      Result := inherited Equals(Value);
  end else
    Result := False;
end;

{**
  Gets a stored any value.
  @return a stored any value.
}
function TZAnyValue.GetValue: TZVariant;
begin
  Result := FValue;
end;

{**
  Converts this object into the string representation.
  @return a string representation for this object.
}
function TZAnyValue.ToString: string;
begin
  Result := GetString;
end;

{**
  Checks is the stored value contains NULL.
  @returns <code>True</code> if NULL is stored.
}
function TZAnyValue.IsNull: Boolean;
begin
  Result := SoftVarManager.IsNull(FValue);
end;

{**
  Gets a stored value converted to double.
  @return a stored value converted to double.
}
function TZAnyValue.GetFloat: Extended;
begin
  Result := SoftVarManager.GetAsFloat(FValue);
end;

{**
  Gets a stored value converted to integer.
  @return a stored value converted to integer.
}
function TZAnyValue.GetInteger: Int64;
begin
  Result := SoftVarManager.GetAsInteger(FValue);
end;

{**
  Gets a stored value converted to string.
  @return a stored value converted to string.
}
function TZAnyValue.GetString: AnsiString;
begin
  Result := SoftVarManager.GetAsString(FValue);
end;

{**
  Gets a stored value converted to boolean.
  @return a stored value converted to boolean.
}
function TZAnyValue.GetBoolean: Boolean;
begin
  Result := SoftVarManager.GetAsBoolean(FValue);
end;

{**
  Gets a stored value converted to unicode string.
  @return a stored value converted to unicode string.
}
function TZAnyValue.GetUnicodeString: WideString;
begin
  Result := SoftVarManager.GetAsUnicodeString(FValue);
end;

{**
  Gets a stored value converted to datetime.
  @return a stored value converted to datetime.
}
function TZAnyValue.GetDateTime: TDateTime;
begin
  Result := SoftVarManager.GetAsDateTime(FValue);
end;

{**
  Encodes a custom variant value into standard variant.
  @param Value a custom variant value to be encoded.
  @returns an encoded standard variant.
}
function EncodeVariant(const Value: TZVariant): Variant;
begin
  case Value.VType of
    vtBoolean: Result := Value.VBoolean;
    vtInteger:
      if (Value.VInteger > -MaxInt) and (Value.VInteger < MaxInt) then
        Result := Integer(Value.VInteger)
      else Result := IntToStr(Value.VInteger);
    vtFloat: Result := Value.VFloat;
    vtString: Result := Value.VString;
    vtUnicodeString: Result := Value.VUnicodeString;
    vtDateTime: Result := Value.VDateTime;
    vtPointer: Result := LongInt(Value.VPointer);
    vtInterface: Result := Value.VInterface;
    else Result := Null;
  end;
end;

{**
  Encodes an array of custom variant values into array of standard variants.
  @param Value an array of custom variant values to be encoded.
  @returns an encoded array of standard variants.
}
function EncodeVariantArray(const Value: TZVariantDynArray): Variant;
var
  I, L: Integer;
begin
  L := Length(Value);
  Result := VarArrayCreate([0, L - 1], varVariant);
  for I := 0 to L - 1 do
    Result[I] := EncodeVariant(Value[I]);
end;

{**
  Decodes a standard variant value into custom variant.
  @param Value a standard variant value to be decoded.
  @returns an decoded custom variant.
}
function DecodeVariant(const Value: Variant): TZVariant;
begin
  case VarType(Value) of
    varSmallint, varInteger, varByte:
      DefVarManager.SetAsInteger(Result, Integer(Value));
    varBoolean: DefVarManager.SetAsBoolean(Result, Value);
    varString:
      DefVarManager.SetAsString(Result, Value);
    varSingle, varDouble, varCurrency:
      DefVarManager.SetAsFloat(Result, Value);
    varUnknown: DefVarManager.SetAsInterface(Result, Value);
    varOleStr:
      DefVarManager.SetAsUnicodeString(Result, Value);
    varDate: DefVarManager.SetAsDateTime(Result, Value);
{$IFNDEF VER130BELOW}
    varShortInt, varWord, varLongWord:
      DefVarManager.SetAsInteger(Result, Value);
    varInt64: DefVarManager.SetAsInteger(Result, Value);
{$ENDIF}
    else DefVarManager.SetNull(Result);
  end;
end;

{**
  Decodes an array of standard variant values into array of custom variants.
  @param Value an array of standard variant values to be decoded.
  @returns an decoded array of custom variants.
}
function DecodeVariantArray(const Value: Variant): TZVariantDynArray;
var
  I, L, H: Integer;
begin
  if VarIsArray(Value) then
  begin
    L := VarArrayLowBound(Value, 1);
    H := VarArrayHighBound(Value, 1);
    SetLength(Result, H - L + 1);
    for I := L to H do
      Result[I - L] := DecodeVariant(Value[I]);
  end
  else
  begin
    SetLength(Result, 1);
    Result[0] := DecodeVariant(Value);
  end;
end;

initialization
  DefVarManager := TZDefaultVariantManager.Create;
  SoftVarManager := TZSoftVariantManager.Create;
  DefVarManager.SetNull(NullVariant);
finalization
  DefVarManager := nil;
  SoftVarManager := nil;
end.


