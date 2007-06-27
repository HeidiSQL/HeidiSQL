{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                  Params list class                     }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZParams;

interface

uses Classes, SysUtils, ZToken;

type
  { Params list class }
  TParamList = class (TPersistent)
  private
    FKeys: TStringList;
    FValues: TStringList;

    function  GetKey(Index: Integer): string;
    procedure SetKey(Index: Integer; Value: string);
    function  GetValue(Index: Integer): string;
    procedure SetValue(Index: Integer; Value: string);
    function  GetText: string;
    procedure SetText(Value: string);
    function  GetValueByKey(Index: string): string;
    procedure SetValueByKey(Index, Value: string);
    function  GetCount: Integer;
  public
    constructor Create;
    constructor CreateNew(Params: string);
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Add(Key, Value: string); dynamic;
    procedure Delete(Index: Integer); dynamic;
    function  IndexOf(Key: string): Integer; dynamic;
    procedure Clear; dynamic;

    property Keys[Index: Integer]: string read GetKey write SetKey;
    property Values[Index: Integer]: string read GetValue write SetValue;
    property ValueByKey[Index: string]: string read GetValueByKey write SetValueByKey;
    property Text: string read GetText write SetText;
    property Count: Integer read GetCount;
  end;

implementation

{************** TParamList implementation **************}

{ Class constructor }
constructor TParamList.Create;
begin
  FKeys := TStringList.Create;
  FValues := TStringList.Create;
end;

{ Class constructor }
constructor TParamList.CreateNew(Params: string);
begin
  FKeys := TStringList.Create;
  FValues := TStringList.Create;
  SetText(Params);
end;

{ Class destructor }
destructor TParamList.Destroy;
begin
  FKeys.Free;
  FValues.Free;
end;

{ Get key by index }
function TParamList.GetKey(Index: Integer): string;
begin
  Result := FKeys[Index];
end;

{ Set key by index }
procedure TParamList.SetKey(Index: Integer; Value: string);
begin
  FKeys[Index] := Value
end;

{ Get value by index }
function TParamList.GetValue(Index: Integer): string;
begin
  Result := FValues[Index];
end;

{ Set value by index }
procedure TParamList.SetValue(Index: Integer; Value: string);
begin
  FValues[Index] := Value;
end;

{ Get parameters string }
function TParamList.GetText: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count-1 do
  begin
    if Result <> '' then
      Result := Result + ';';
    Result := Result + FKeys[I] + '="' + ConvStr(FValues[I]) + '"';
  end;
end;

{ Split param string and fill lists }
procedure TParamList.SetText(Value: string);
var
  I: Integer;
begin
  SplitParams(Value, FKeys, FValues);
  for I := 0 to FKeys.Count-1 do
  begin
    FKeys[I] := UpperCase(FKeys[I]);
    FValues[I] := UnconvStr(FValues[I]);
  end;
end;

{ Get value by key }
function TParamList.GetValueByKey(Index: string): string;
var
  N: Integer;
begin
  N := FKeys.IndexOf(UpperCase(Index));
  if N >= 0 then
    Result := FValues[N]
  else
    Result := '';
end;

{ Setvalue by index }
procedure TParamList.SetValueByKey(Index, Value: string);
var
  N: Integer;
begin
  N := FKeys.IndexOf(UpperCase(Index));
  if N >= 0 then
    FValues[N] := Value
  else begin
    FKeys.Add(UpperCase(Index));
    FValues.Add(Value);
  end;
end;

{ Get parameters count }
function TParamList.GetCount: Integer;
begin
  Result := FKeys.Count;
end;

{ Assign param list value }
procedure TParamList.Assign(Source: TPersistent);
begin
  if Source is TParamList then
  begin
    FKeys.Assign(TParamList(Source).FKeys);
    FValues.Assign(TParamList(Source).FValues);
  end
  else
    Clear;
end;

{ Add new pair key-value }
procedure TParamList.Add(Key, Value: string);
begin
  SetValueByKey(Key, Value);
end;

{ Delete pair key-value by index }
procedure TParamList.Delete(Index: Integer);
begin
  FKeys.Delete(Index);
  FValues.Delete(Index);
end;

{ Get index of key }
function TParamList.IndexOf(Key: string): Integer;
begin
  Result := FKeys.IndexOf(UpperCase(Key));
end;

{ Clear param list }
procedure TParamList.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
end;

end.
