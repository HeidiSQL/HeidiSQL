{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Core classes and interfaces                 }
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

unit ZClasses;

interface

{$I ZCore.inc}

uses
  SysUtils, Classes;

const
  ZEOS_VERSION = '6.6.0 - beta';

type
  { Lazarus/FreePascal Support }
  {$IFDEF FPC}
  PDateTime = ^TDateTime;

  TAggregatedObject = class(TObject)
  private
    FController: Pointer;
    function GetController: IInterface;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(const Controller: IInterface);
    property Controller: IInterface read GetController;
  end;

  TContainedObject = class(TAggregatedObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  end;
  {$ENDIF}

  {** Replacement for generic interface type. }
  IZInterface = IUnknown;

  {** Represents an interface for all abstract object. }
  IZObject = interface(IZInterface)
    ['{EF46E5F7-00CF-4DDA-BED0-057D6686AEE0}']
    function Equals(const Value: IZInterface): Boolean;
    function Hash: LongInt;
    function Clone: IZInterface;
    function ToString: string;
    function InstanceOf(const IId: TGUID): Boolean;
  end;

  {** Represents a fake interface for coparable objects. }
  IZComparable = interface(IZObject)
    ['{04112081-F07B-4BBF-A757-817816EB67C1}']
  end;

  {** Represents an interface to clone objects. }
  IZClonnable = interface(IZObject)
    ['{ECB7F3A4-7B2E-4130-BA66-54A2D43C0149}']
  end;

  {** Represents a generic collection iterator interface. }
  IZIterator = interface(IZObject)
    ['{D964DDD0-2308-4D9B-BD36-5810632512F7}']
    function HasNext: Boolean;
    function Next: IZInterface;
  end;

  {** Represents a collection of object interfaces. }
  IZCollection = interface(IZClonnable)
    ['{51417C87-F992-4CAD-BC53-CF3925DD6E4C}']

    function Get(Index: Integer): IZInterface;
    procedure Put(Index: Integer; const Item: IZInterface);
    function IndexOf(const Item: IZInterface): Integer;
    function GetCount: Integer;
    function GetIterator: IZIterator;

    function First: IZInterface;
    function Last: IZInterface;

    function Add(const Item: IZInterface): Integer;
    procedure Insert(Index: Integer; const Item: IZInterface);
    function Remove(const Item: IZInterface): Integer;

    procedure Exchange(Index1, Index2: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;

    function Contains(const Item: IZInterface): Boolean;
    function ContainsAll(const Col: IZCollection): Boolean;
    function AddAll(const Col: IZCollection): Boolean;
    function RemoveAll(const Col: IZCollection): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IZInterface read Get write Put; default;
  end;

  {** Represents a hash map interface. }
  IZHashMap = interface(IZClonnable)
    ['{782C64F4-AD09-4F56-AF2B-E4193A05BBCE}']

    function Get(const Key: IZInterface): IZInterface;
    procedure Put(const Key: IZInterface; const Value: IZInterface);
    function GetKeys: IZCollection;
    function GetValues: IZCollection;
    function GetCount: Integer;

    function Remove(Key: IZInterface): Boolean;
    procedure Clear;

    property Count: Integer read GetCount;
    property Keys: IZCollection read GetKeys;
    property Values: IZCollection read GetValues;
  end;

  {** Represents a stack interface. }
  IZStack = interface(IZClonnable)
    ['{8FEA0B3F-0C02-4E70-BD8D-FB0F42D4497B}']

    function Peek: IZInterface;
    function Pop: IZInterface;
    procedure Push(Value: IZInterface);
    function GetCount: Integer;

    property Count: Integer read GetCount;
  end;

  {** Implements an abstract interfaced object. }
  TZAbstractObject = class(TInterfacedObject, IZObject)
  public
    function Equals(const Value: IZInterface): Boolean; virtual;
    function Hash: LongInt;
    function Clone: IZInterface; virtual;
    function ToString: string; virtual;
    function InstanceOf(const IId: TGUID): Boolean;
  end;

implementation

uses ZMessages, ZCompatibility;

{$IFDEF FPC}

{ TAggregatedObject }

constructor TAggregatedObject.Create(const Controller: IInterface);
begin
  FController := Pointer(Controller);
end;

function TAggregatedObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

function TAggregatedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := IInterface(FController).QueryInterface(IID, Obj);
end;

function TAggregatedObject._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TAggregatedObject._Release: Integer; stdcall;
begin
  Result := IInterface(FController)._Release;
end;

{ TContainedObject }

function TContainedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{$ENDIF}

{ TZAbstractObject }

{**
  Checks is the specified value equals to this object.
  @param Value an interface to some object.
  @return <code>True</code> if the objects are identical.
}
function TZAbstractObject.Equals(const Value: IZInterface): Boolean;
begin
  if Value <> nil then
  begin
    Result := (IZInterface(Self) = Value)
      or ((Self as IZInterface) = (Value as IZInterface));
  end else
   Result := False;
end;

{**
  Gets a unique hash for this object.
  @return a unique hash for this object.
}
function TZAbstractObject.Hash: LongInt;
begin
  Result := LongInt(Self);
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZAbstractObject.Clone: IZInterface;
begin
  raise Exception.Create(SClonningIsNotSupported);
end;

{**
  Checks is this object implements a specified interface.
  @param IId an interface id.
  @return <code>True</code> if this object support the interface.
}
function TZAbstractObject.InstanceOf(const IId: TGUID): Boolean;
begin
  Result := GetInterfaceEntry(IId) <> nil;
end;

{**
  Converts this object into the string representation.
  @return a string representation for this object.
}
function TZAbstractObject.ToString: string;
begin
  Result := Format('%s <%p>', [ClassName, Pointer(Self)])
end;

end.

