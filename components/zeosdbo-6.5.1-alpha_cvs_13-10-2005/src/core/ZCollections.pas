{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Core collection and map classes               }
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

unit ZCollections;

interface

{$I ZCore.inc}

uses Classes, ZClasses;

type

  {** Implements an iterator for regular TZCollection collection. }
  TZIterator = class (TZAbstractObject, IZIterator)
  private
    FCollection: IZCollection;
    FCurrentIndex: Integer;
  public
    constructor Create(const Col: IZCollection);

    function HasNext: Boolean;
    function Next: IZInterface;
  end;

  {** Interface list types. }
  TZInterfaceList = array[0..MaxListSize - 1] of IZInterface;
  PZInterfaceList = ^TZInterfaceList;

  {** Implenments a collection of interfaces. }
  TZCollection = class(TZAbstractObject, IZCollection, IZClonnable)
  private
    FList: PZInterfaceList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    class procedure Error(const Msg: string; Data: Integer);
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function Clone: IZInterface; override;
    function ToString: string; override;

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

  {** Implements an unmodifiable collection of interfaces. }
  TZUnmodifiableCollection = class(TZAbstractObject, IZCollection, IZClonnable)
  private
    FCollection: IZCollection;
  private
    procedure RaiseException;
  public
    constructor Create(Collection: IZCollection);
    destructor Destroy; override;

    function Clone: IZInterface; override;
    function ToString: string; override;

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

  {** Implements a hash map of interfaces. }
  TZHashMap = class(TZAbstractObject, IZHashMap, IZClonnable)
  private
    FKeys: IZCollection;
    FReadOnlyKeys: IZCollection;
    FValues: IZCollection;
    FReadOnlyValues: IZCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function Clone: IZInterface; override;

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

  {** Implements a stack of interfaces. }
  TZStack = class(TZAbstractObject, IZStack, IZClonnable)
  private
    FValues: IZCollection;
  public
    constructor Create;
    destructor Destroy; override;

    function Clone: IZInterface; override;
    function ToString: string; override;

    function Peek: IZInterface;
    function Pop: IZInterface;
    procedure Push(Value: IZInterface);
    function GetCount: Integer;

    property Count: Integer read GetCount;
  end;

implementation

uses SysUtils, ZMessages;

{ TZIterator }

{**
  Creates this iterator for the specified interface list.
  @param List a list of interfaces.
}
constructor TZIterator.Create(const Col: IZCollection);
begin
  FCollection := Col;
  FCurrentIndex := 0;
end;

{**
  Checks has the iterated collection more elements.
  @return <code>True</code> if iterated collection has more elements.
}
function TZIterator.HasNext: Boolean;
begin
  Result := FCurrentIndex < FCollection.Count;
end;

{**
  Gets a next iterated element from the collection.
  @return a next iterated element from the collection or <code>null</code>
    if no more elements.
}
function TZIterator.Next: IZInterface;
begin
  if FCurrentIndex < FCollection.Count then
  begin
    Result := FCollection[FCurrentIndex];
    Inc(FCurrentIndex);
  end else
    Result := nil;
end;

{ TZCollection }

{**
  Creates this collection and assignes main properties.
}
constructor TZCollection.Create;
begin
end;

{**
  Destroys this object and frees the memory.
}
destructor TZCollection.Destroy;
begin
  Clear;
end;

{**
  Raises a collection error.
  @param Msg an error message.
  @param Data a integer value to describe an error.
}
class procedure TZCollection.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

{**
  Increases an element count.
}
procedure TZCollection.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
  begin
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  end;
  SetCapacity(FCapacity + Delta);
end;

{**
  Sets a new list capacity.
  @param NewCapacity a new list capacity.
}
procedure TZCollection.SetCapacity(NewCapacity: Integer);
begin
{$IFOPT R+}
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SListCapacityError, NewCapacity);
{$ENDIF}
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(IZInterface));
    if NewCapacity > FCapacity then
      FillChar(FList^[FCount], (NewCapacity - FCapacity) * SizeOf(IZInterface), 0);
    FCapacity := NewCapacity;
  end;
end;

{**
  Sets a new element count.
  @param NewCount a new element count.
}
procedure TZCollection.SetCount(NewCount: Integer);
var
  I: Integer;
begin
{$IFOPT R+}
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SListCountError, NewCount);
{$ENDIF}
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount < FCount then
  begin
    for I := FCount - 1 downto NewCount do
      FList^[I] := nil;
  end;
  FCount := NewCount;
end;

{**
  Clones the instance of this object.
  @return a reference to the clonned object.
}
function TZCollection.Clone: IZInterface;
var
  I: Integer;
  Collection: IZCollection;
  Clonnable: IZClonnable;
begin
  Collection := TZCollection.Create;
  for I := 0 to FCount - 1 do
  begin
    if FList^[I].QueryInterface(IZClonnable, Clonnable) = 0 then
      Collection.Add(Clonnable.Clone)
    else Collection.Add(FList^[I]);
  end;
  Result := Collection;
end;

{**
  Adds a new object at the and of this collection.
  @param Item an object to be added.
  @return a position of the added object.
}
function TZCollection.Add(const Item: IZInterface): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item as IZInterface;
  Inc(FCount);
end;

{**
  Adds all elements from the specified collection into this collection.
  @param Col a collection of objects to be added.
  @return <code>True</code> is the collection was changed.
}
function TZCollection.AddAll(const Col: IZCollection): Boolean;
var
  I: Integer;
begin
  Result := Col.Count > 0;
  for I := 0 to Col.Count - 1 do
    Add(Col[I]);
end;

{**
  Clears the content of this collection.
}
procedure TZCollection.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

{**
  Checks is the specified object is stored in this collection.
  @return <code>True</code> if the object was found in the collection.
}
function TZCollection.Contains(const Item: IZInterface): Boolean;
begin
  Result := IndexOf(Item) >= 0;
end;

{**
  Checks are all the object in this collection.
  @param Col a collection of objects to be checked.
  @return <code>True</code> if all objects are in this collection.
}
function TZCollection.ContainsAll(const Col: IZCollection): Boolean;
var
  I: Integer;
begin
  Result := Col.Count > 0;
  for I := 0 to Col.Count - 1 do
  begin
    if IndexOf(Col[I]) < 0 then
    begin
      Result := False;
      Break;
    end;
  end;
end;

{**
  Deletes an object from the specified position.
}
procedure TZCollection.Delete(Index: Integer);
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  FList^[Index] := nil;
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(IZInterface));
  end;
end;

{**
  Exchanges two element in the collection.
  @param Index1 an index of the first element.
  @param Index2 an index of the second element.
}
procedure TZCollection.Exchange(Index1, Index2: Integer);
var
  Item: IZInterface;
begin
{$IFOPT R+}
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SListIndexError, Index2);
{$ENDIF}
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

{**
  Gets the first element from this collection.
  @return the first element.
}
function TZCollection.First: IZInterface;
begin
  Result := Get(0);
end;

{**
  Gets a collection element from the specified position.
  @param Index a position index of the element.
  @return a requested element.
}
function TZCollection.Get(Index: Integer): IZInterface;
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  Result := FList^[Index];
end;

{**
  Gets a number of the stored element in this collection.
  @return a number of stored elements.
}
function TZCollection.GetCount: Integer;
begin
  Result := FCount;
end;

{**
  Gets a created iterator for this collection.
  @return a created iterator for this collection.
}
function TZCollection.GetIterator: IZIterator;
begin
  Result := TZIterator.Create(Self);
end;

{**
  Defines an index of the specified object inside this colleciton.
  @param Item an object to be found.
  @return an object position index or -1 if it was not found.
}
function TZCollection.IndexOf(const Item: IZInterface): Integer;
var
  I: Integer;
  Comparable: IZComparable;
  Unknown: IZInterface;
begin
  Result := -1;
  if (FCount = 0) or (Item = nil) then
    Exit;

  { Find IComparable objects }
  if Item.QueryInterface(IZComparable, Comparable) = 0 then
  begin
    for I := 0 to FCount - 1 do
    begin
      if Comparable.Equals(FList^[I]) then
      begin
        Result := I;
        Break;
      end;
    end;
    Comparable := nil;
  end
  { Find ordinary objects }
  else
  begin
    Unknown := Item as IZInterface;
    for I := 0 to FCount - 1 do
    begin
      if Unknown = FList^[I] then
      begin
        Result := I;
        Break;
      end;
    end;
    Unknown := nil;
  end;
end;

{**
  Inserts an object into specified position.
  @param Index a position index.
  @param Item an object to be inserted.
}
procedure TZCollection.Insert(Index: Integer; const Item: IZInterface);
begin
{$IFOPT R+}
  if (Index < 0) or (Index > FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
  begin
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(IZInterface));
  end;
  FList^[Index] := Item as IZInterface;
  Inc(FCount);
end;

{**
  Gets the last object from this collection.
  @return the last object.
}
function TZCollection.Last: IZInterface;
begin
  Result := Get(FCount - 1);
end;

{**
  Puts a specified object into defined position.
  @param Index a position index.
  @param Items ab object to be put.
}
procedure TZCollection.Put(Index: Integer; const Item: IZInterface);
begin
{$IFOPT R+}
  if (Index < 0) or (Index >= FCount) then
    Error(SListIndexError, Index);
{$ENDIF}
  FList^[Index] := Item as IZInterface;
end;

{**
  Removes an existed object which equals to the specified one.
  @param Item an object to be removed.
  @return an index of the removed object.
}
function TZCollection.Remove(const Item: IZInterface): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

{**
  Removes all the elements from the specified collection.
  @param Col a collection of object to be removed.
  @return <code>True</code> if this collection was changed.
}
function TZCollection.RemoveAll(const Col: IZCollection): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Col.Count - 1 do
    Result := (Remove(Col[I]) >= 0) or Result;
end;

{**
  Gets a string representation for this object.
}
function TZCollection.ToString: string;
var
  I: Integer;
  TempObject: IZObject;
begin
  Result := '';
  for I := 0 to FCount - 1 do
  begin
    if I > 0 then
      Result := Result + ',';
    if FList^[I].QueryInterface(IZObject, TempObject) = 0 then
      Result := Result + TempObject.ToString
    else Result := Result + Format('<%p>', [Pointer(FList^[I])]);
  end;
  Result := '[' + Result + ']';
end;

{ TZUnmodifiableCollection }

{**
  Constructs this object and assignes main properties.
  @param Collection an initial modifiable list of interfaces.
}
constructor TZUnmodifiableCollection.Create(Collection: IZCollection);
begin
  inherited Create;
  FCollection := Collection;
end;

{**
  Destroys this object and frees the memory.
}
destructor TZUnmodifiableCollection.Destroy;
begin
  FCollection := nil;
  inherited Destroy;
end;

{**
  Clones the instance of this object.
  @return a reference to the clonned object.
}
function TZUnmodifiableCollection.Clone: IZInterface;
begin
  Result := TZUnmodifiableCollection.Create(FCollection);
end;

{**
  Raises invalid operation exception.
}
procedure TZUnmodifiableCollection.RaiseException;
begin
  raise EInvalidOperation.Create(SImmutableOpIsNotAllowed);
end;

{**
  Adds a new object at the and of this collection.
  @param Item an object to be added.
  @return a position of the added object.
}
function TZUnmodifiableCollection.Add(const Item: IZInterface): Integer;
begin
  Result := -1;
  RaiseException;
end;

{**
  Adds all elements from the specified collection into this collection.
  @param Col a collection of objects to be added.
  @return <code>True</code> is the collection was changed.
}
function TZUnmodifiableCollection.AddAll(const Col: IZCollection): Boolean;
begin
  Result := False;
  RaiseException;
end;

{**
  Clears the content of this collection.
}
procedure TZUnmodifiableCollection.Clear;
begin
  RaiseException;
end;

{**
  Checks is the specified object is stored in this collection.
  @return <code>True</code> if the object was found in the collection.
}
function TZUnmodifiableCollection.Contains(const Item: IZInterface): Boolean;
begin
  Result := FCollection.Contains(Item);
end;

{**
  Checks are all the object in this collection.
  @param Col a collection of objects to be checked.
  @return <code>True</code> if all objects are in this collection.
}
function TZUnmodifiableCollection.ContainsAll(const Col: IZCollection): Boolean;
begin
  Result := FCollection.ContainsAll(Col);
end;

{**
  Deletes an object from the specified position.
}
procedure TZUnmodifiableCollection.Delete(Index: Integer);
begin
  RaiseException;
end;

{**
  Exchanges two element in the collection.
  @param Index1 an index of the first element.
  @param Index2 an index of the second element.
}
procedure TZUnmodifiableCollection.Exchange(Index1, Index2: Integer);
begin
  RaiseException;
end;

{**
  Gets the first element from this collection.
  @return the first element.
}
function TZUnmodifiableCollection.First: IZInterface;
begin
  Result := FCollection.First;
end;

{**
  Gets a collection element from the specified position.
  @param Index a position index of the element.
  @return a requested element.
}
function TZUnmodifiableCollection.Get(Index: Integer): IZInterface;
begin
  Result := FCollection[Index];
end;

{**
  Gets a number of the stored element in this collection.
  @return a number of stored elements.
}
function TZUnmodifiableCollection.GetCount: Integer;
begin
  Result := FCollection.Count;
end;

{**
  Gets a created iterator for this collection.
  @return a created iterator for this collection.
}
function TZUnmodifiableCollection.GetIterator: IZIterator;
begin
  Result := TZIterator.Create(Self);
end;

{**
  Defines an index of the specified object inside this colleciton.
  @param Item an object to be found.
  @return an object position index or -1 if it was not found.
}
function TZUnmodifiableCollection.IndexOf(const Item: IZInterface): Integer;
begin
  Result := FCollection.IndexOf(Item);
end;

{**
  Inserts an object into specified position.
  @param Index a position index.
  @param Item an object to be inserted.
}
procedure TZUnmodifiableCollection.Insert(Index: Integer; const Item: IZInterface);
begin
  RaiseException;
end;

{**
  Gets the last object from this collection.
  @return the last object.
}
function TZUnmodifiableCollection.Last: IZInterface;
begin
  Result := FCollection.Last;
end;

{**
  Puts a specified object into defined position.
  @param Index a position index.
  @param Items ab object to be put.
}
procedure TZUnmodifiableCollection.Put(Index: Integer; const Item: IZInterface);
begin
  RaiseException;
end;

{**
  Removes an existed object which equals to the specified one.
  @param Item an object to be removed.
  @return an index of the removed object.
}
function TZUnmodifiableCollection.Remove(const Item: IZInterface): Integer;
begin
  Result := -1;
  RaiseException;
end;

{**
  Removes all the elements from the specified collection.
  @param Col a collection of object to be removed.
  @return <code>True</code> if this collection was changed.
}
function TZUnmodifiableCollection.RemoveAll(const Col: IZCollection): Boolean;
begin
  Result := False;
  RaiseException;
end;

{**
  Gets a string representation for this object.
}
function TZUnmodifiableCollection.ToString: string;
begin
  Result := FCollection.ToString;
end;

{ TZHashMap }

{**
  Creates this hash map and assignes main properties.
}
constructor TZHashMap.Create;
begin
  inherited Create;
  FKeys := TZCollection.Create;
  FValues := TZCollection.Create;
  FReadOnlyKeys := TZUnmodifiableCollection.Create(FKeys);
  FReadOnlyValues := TZUnmodifiableCollection.Create(FValues);
end;

{**
  Destroys this object and frees the memory.
}
destructor TZHashMap.Destroy;
begin
  FReadOnlyKeys := nil;
  FReadOnlyValues := nil;
  FKeys := nil;
  FValues := nil;
  inherited Destroy;
end;

{**
  Clones the instance of this object.
  @return a reference to the clonned object.
}
function TZHashMap.Clone: IZInterface;
var
  HashMap: TZHashMap;
begin
  HashMap := TZHashMap.Create;
  HashMap.FKeys := IZCollection(FKeys.Clone);
  HashMap.FReadOnlyKeys := IZCollection(FReadOnlyKeys.Clone);
  HashMap.FValues := IZCollection(FValues.Clone);
  HashMap.FReadOnlyValues := IZCollection(FReadOnlyValues.Clone);
  Result := HashMap;
end;

{**
  Gets a interface by it's key.
  @param Key a key interface.
  @return a found value interface or <code>nil</code> otherwise.
}
function TZHashMap.Get(const Key: IZInterface): IZInterface;
var
  Index: Integer;
begin
  Index := FKeys.IndexOf(Key);
  if Index >= 0 then
    Result := FValues[Index]
  else Result := nil;
end;

{**
  Put a new key/value pair interfaces.
  @param Key a key interface.
  @param Value a value interface.
}
procedure TZHashMap.Put(const Key: IZInterface; const Value: IZInterface);
var
 Index: Integer;
begin
  Index := FKeys.IndexOf(Key);
  if Index >= 0 then
    FValues[Index] := Value
  else
  begin
    FKeys.Add(Key);
    FValues.Add(Value);
  end;
end;

{**
  Gets a readonly collection of keys.
  @return a readonly collection of keys.
}
function TZHashMap.GetKeys: IZCollection;
begin
  Result := FReadOnlyKeys;
end;

{**
  Gets a readonly collection of values.
  @return a readonly collection of values.
}
function TZHashMap.GetValues: IZCollection;
begin
  Result := FReadOnlyValues;
end;

{**
  Gets a number of elements in this hash map.
  @return a number of elements in this hash map.
}
function TZHashMap.GetCount: Integer;
begin
  Result := FKeys.Count;
end;

{**
  Removes the element from the map by it's key.
  @param Key a key of the element.
  @return <code>true</code> of the hash map was changed.
}
function TZHashMap.Remove(Key: IZInterface): Boolean;
var
  Index: Integer;
begin
  Index := FKeys.IndexOf(Key);
  if Index >= 0 then
  begin
    FKeys.Delete(Index);
    FValues.Delete(Index);
    Result := True;
  end else
    Result := False;
end;

{**
  Clears this hash map and removes all elements.
}
procedure TZHashMap.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
end;

{ TZStack }

{**
  Constructs this object and assignes the main properties.
}
constructor TZStack.Create;
begin
  FValues := TZCollection.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZStack.Destroy;
begin
  FValues := nil;
  inherited Destroy;
end;

{**
  Clones the instance of this object.
  @return a reference to the clonned object.
}
function TZStack.Clone: IZInterface;
var
  Stack: TZStack;
begin
  Stack := TZStack.Create;
  Stack.FValues := IZCollection(FValues.Clone);
  Result := Stack;
end;

{**
  Gets a count of the stored elements.
  @return an elements count.
}
function TZStack.GetCount: Integer;
begin
  Result := FValues.Count;
end;

{**
  Gets an element from the top this stack without removing it.
  @return an element from the top of the stack.
}
function TZStack.Peek: IZInterface;
begin
  if FValues.Count > 0 then
    Result := FValues[FValues.Count - 1]
  else Result := nil;
end;

{**
  Gets an element from the top this stack and remove it.
  @return an element from the top of the stack.
}
function TZStack.Pop: IZInterface;
begin
  if FValues.Count > 0 then
  begin
    Result := FValues[FValues.Count - 1];
    FValues.Delete(FValues.Count - 1);
  end else
    Result := nil;
end;

{**
  Puts a new element to the top of this stack.
  @param Value a new element to be put.
}
procedure TZStack.Push(Value: IZInterface);
begin
  FValues.Add(Value);
end;

{**
  Gets a string representation for this object.
}
function TZStack.ToString: string;
begin
  Result := FValues.ToString;
end;

end.
