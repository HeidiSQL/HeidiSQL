{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                   Hash-table class                     }
{                                                        }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZHash;

interface

{$INCLUDE ..\ZeosDef.inc}

uses SysUtils, Classes {$IFDEF VERCLX}, Variants{$ENDIF};

{$INCLUDE ..\Zeos.inc}

const
{ Maximum values in hash-table item }
  MAX_HASH_ITEMS = 10;

type
{ Item of hash-table }
  PHashItem = ^THashItem;
  THashItem = record
    HashKey: string;
    Items:   array[0..MAX_HASH_ITEMS-1] of Variant;
  end;

{ Hash-table class }
  THash = class (TList)
  private
    function GetHashItem(Index: string): PHashItem;
  public
    destructor Destroy; override;

    procedure Add(KeyValue: string; ItemValues: array of Variant);
    procedure Delete(Index: Integer);
    procedure DeleteByKey(Index: string);
    procedure Clear; {$IFNDEF VER100} override; {$ENDIF}
    procedure Assign(Source: TObject); virtual;

    property HashItems[Index: string]: PHashItem read GetHashItem; default;
  end;

implementation

uses ZUtilsConst;

{******************* THash implementation ***********************}

{ Get hash item }
function THash.GetHashItem(Index: string): PHashItem;
var
  I: Integer;
  Item: PHashItem;
begin
  Result := nil;
  for I := 0 to Count-1 do
  begin
    Item := PHashItem(inherited Items[I]);
    if Item.HashKey = Index then
    begin
      Result := Item;
      Break;
    end;
  end;
  if Result = nil then
    raise Exception.CreateFmt(SHashNotFound,[Index]);
end;

{ Class destructor }
destructor THash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ Add new item }
procedure THash.Add(KeyValue: string; ItemValues: array of Variant);
var
  I: Integer;
  NewItem: PHashItem;
begin
  New(NewItem);
  if not Assigned(NewItem) then
    raise Exception.Create(SAllocError);
  NewItem.HashKey := KeyValue;

  for I := 0 to MAX_HASH_ITEMS-1 do
    NewItem.Items[I] := NULL;

  for I := 0 to High(ItemValues) do
    if I < MAX_HASH_ITEMS then
      NewItem.Items[I] := ItemValues[I]
    else
      raise Exception.Create(STooManyHash);

  inherited Add(NewItem);
end;

{ Delete item by key }
procedure THash.DeleteByKey(Index: string);
var
  I, N: Integer;
  Item: PHashItem;
begin
  N := -1;
  for I := 0 to Count-1 do
  begin
    Item := PHashItem(inherited Items[I]);
    if Item.HashKey = Index then
    begin
      N := I;
      Break;
    end;
  end;

  if N >= 0 then
    Delete(N)
  else
    raise Exception.CreateFmt(SHashNotFound,[Index]);
end;

{ Delete item }
procedure THash.Delete(Index: Integer);
var
  Item: PHashItem;
begin
  Item := PHashItem(inherited Items[Index]);
  if Item <> nil then
    Dispose(Item);
  inherited Delete(Index);
end;

{ Clear hash-table }
procedure THash.Clear;
var
  I: Integer;
  Item: PHashItem;
begin
  for I := 0 to Count-1 do
  begin
    Item := PHashItem(inherited Items[i]);
    if Item <> nil then
      Dispose(Item);
  end;
  inherited Clear;
end;

{ Assign another hash-table }
procedure THash.Assign(Source: TObject);
var
  I, J: Integer;
  Item1, Item2: PHashItem;
begin
  Clear;
  if Source is THash then
    for I := 0 to THash(Source).Count-1 do
    begin
      Item1 := PHashItem(THash(Source).Items[I]);
      Add(Item1.HashKey,['']);
      Item2 := PHashItem(Items[Count-1]);
      for J := 0 to MAX_HASH_ITEMS-1 do
        Item2.Items[J] := Item1.Items[J];
    end;
end;

end.
