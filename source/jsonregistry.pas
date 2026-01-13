unit jsonregistry;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, jsonConf, fpjson, ExtCtrls;

type

  { TJSONConfigExtended }

  TJSONConfigExtended = class(TJSONConfig)
    public
      function AddEndSlash(Path: String): String;
      function StripEndSlash(Path: String): String;
      procedure MoveKey(SourcePath: String; TargetPath: String; Delete: Boolean);
      function DataType(Path: String): TJSONtype;
  end;

  { TJSONRegistry }

  TJsonRegistry = class(TPersistent)
    private
      FJsConf: TJSONConfigExtended;
      FCurrentKeyPath: String;
      FAutoFlushTimer: TTimer;
      function GetAutoFlushMilliSeconds: Cardinal;
      procedure SetAutoFlushMilliSeconds(aValue: Cardinal);
      procedure AutoFlushOnTimer(Sender: TObject);
    public
      constructor Create(JsonFilePath: String);
      destructor Destroy; override;
      function FilePath: String;
      // Keys:
      function OpenKey(Path: String; CanCreate: Boolean): Boolean;
      procedure CloseKey;
      function DeleteKey(Path: String): Boolean;
      procedure MoveKey(sourcepath: String; targetpath: String; Delete: Boolean);
      procedure GetKeyNames(list: TStringList);
      function CurrentPath: String;
      function KeyExists(path: String): Boolean;
      function HasSubKeys: Boolean;
      // Values:
      function DeleteValue(name: String): Boolean;
      procedure GetValueNames(list: TStringList);
      function ValueExists(name: String): Boolean;
      function ReadInteger(name: String): Integer;
      function ReadBool(name: String): Boolean;
      function ReadString(name: String): String;
      procedure WriteInteger(name: String; value: Integer);
      procedure WriteBool(name: String; value: Boolean);
      procedure WriteString(name: String; value: String);
      function GetDataType(Path: String): TJSONtype;

      property AutoFlushMilliSeconds: Cardinal read GetAutoFlushMilliSeconds write SetAutoFlushMilliSeconds;
      procedure FlushToDisk;
  end;

implementation

{ TJSONConfigExtended }

function TJSONConfigExtended.AddEndSlash(Path: String): String;
begin
  Result := Path;
  if Result[Length(Result)] <> '/' then
    Result := Result + '/';
end;

function TJSONConfigExtended.StripEndSlash(Path: String): String;
begin
  Result := Path;
  if Result[Length(Result)] = '/' then
    Delete(Result, Length(Result), 1);
end;

procedure TJSONConfigExtended.MoveKey(SourcePath: String;
  TargetPath: String; Delete: Boolean);
var
  OldNode, NewNode, NewNodeParent: TJSONObject;
  NewNodeName: UnicodeString;
  TargetPathSlash, TargetPathNoSlash: String;
begin

  if Length(SourcePath) = 0 then
    Raise EJSONConfigError.Create('Cannot move from empty path');
  if Length(TargetPath) = 0 then
    Raise EJSONConfigError.Create('Cannot move to empty path');

  SourcePath := AddEndSlash(SourcePath);
  OldNode := FindObject(UTF8Decode(SourcePath), False);
  if not Assigned(OldNode) then
    raise EJSONConfigError.CreateFmt('Source path does not exist: %s', [SourcePath]);

  TargetPathSlash := AddEndSlash(TargetPath);
  TargetPathNoSlash := StripEndSlash(TargetPath);
  //showmessage('TargetPathSlash:"'+TargetPathSlash+'"'+sLineBreak+'TargetPathNoSlash:"'+TargetPathNoSlash+'"');

  // Error if target exists
  NewNode := FindObject(UTF8Decode(TargetPathSlash), False);
  if Assigned(NewNode) then
    Raise EJSONConfigError.CreateFmt('Target path already exists: %s', [TargetPathSlash]);

  // Create copied key
  NewNodeParent := FindObject(UTF8Decode(TargetPathNoSlash), True, NewNodeName);
  NewNodeParent.Add(UTF8Encode(NewNodeName), OldNode.Clone);

  if Delete then begin
    // Deleting source key. Note we have cloned this before.
    DeletePath(UTF8Decode(SourcePath));
  end;

  FModified:=True;
end;

function TJSONConfigExtended.DataType(Path: String): TJSONtype;
var
  e: TJSONData;
begin
  e := FindElement(UTF8Decode(Path), False, True);
  if Assigned(e) then
    Result := e.JSONType
  else
    Result := jtUnknown;
end;


{ TJsonRegistry }

constructor TJsonRegistry.Create(JsonFilePath: String);
begin
  FJsConf := TJSONConfigExtended.Create(nil);
  FJsConf.Formatted := True;
  FJsConf.Filename := JsonFilePath;
  FAutoFlushTimer := TTimer.Create(nil);
  FAutoFlushTimer.Enabled := False;
  FAutoFlushTimer.OnTimer := AutoFlushOnTimer;
  SetAutoFlushMilliSeconds(5000);
end;

destructor TJsonRegistry.Destroy;
begin
  FJsConf.Flush;
  FAutoFlushTimer.Free;
end;

function TJsonRegistry.GetAutoFlushMilliSeconds: Cardinal;
begin
  Result := FAutoFlushTimer.Interval;
end;

procedure TJsonRegistry.SetAutoFlushMilliSeconds(aValue: Cardinal);
begin
  FAutoFlushTimer.Enabled := False;
  FAutoFlushTimer.Interval := aValue;
  FAutoFlushTimer.Enabled := aValue > 0;
end;

procedure TJsonRegistry.AutoFlushOnTimer(Sender: TObject);
begin
  FlushToDisk;
end;

procedure TJsonRegistry.FlushToDisk;
begin
  FJsConf.Flush;
end;

function TJsonRegistry.FilePath: String;
begin
  Result := FJsConf.Filename;
end;

function TJsonRegistry.OpenKey(Path: String; CanCreate: Boolean): Boolean;
begin
  try
    FJsConf.OpenKey(UTF8Decode(Path), CanCreate);
    FCurrentKeyPath := Path;
    Result := True;
  except
    on EJSONConfigError do begin
      Result := False;
    end;
  end;
end;

procedure TJsonRegistry.CloseKey;
begin
  FJsConf.Flush;
  FJsConf.CloseKey;
end;

function TJsonRegistry.DeleteKey(Path: String): Boolean;
begin
  FJsConf.DeletePath(UTF8Decode(Path));
  Result := True;
end;

procedure TJsonRegistry.MoveKey(sourcepath: String; targetpath: String; Delete: Boolean);
begin
  FJsConf.MoveKey(sourcepath, targetpath, Delete);
end;

procedure TJsonRegistry.GetKeyNames(list: TStringList);
begin
  FJsConf.EnumSubKeys(UTF8Decode(FCurrentKeyPath), list);
end;

function TJsonRegistry.CurrentPath: String;
begin
  Result := FCurrentKeyPath;
end;

function TJsonRegistry.KeyExists(path: String): Boolean;
var
  SubKeys: TStringList;
  LastDelim: Integer;
  folder, name: String;
begin
  SubKeys := TStringList.Create;
  path := FJsConf.StripEndSlash(path);
  LastDelim := path.LastIndexOf('/');
  name := Copy(path, LastDelim+2);
  folder := Copy(path, 1, LastDelim);
  //showmessage('folder:'+folder+sLineBreak+'name:'+name);
  FJsConf.EnumSubKeys(UTF8Decode(folder), SubKeys);
  Result := SubKeys.IndexOf(name) > -1;
  SubKeys.Free;
end;

function TJsonRegistry.HasSubKeys: Boolean;
var
  SubKeys: TStringList;
begin
  SubKeys := TStringList.Create;
  GetKeyNames(SubKeys);
  Result := SubKeys.Count > 0;
  SubKeys.Free;
end;

function TJsonRegistry.DeleteValue(name: String): Boolean;
begin
  FJsConf.DeleteValue(UTF8Decode(name));
  Result := True;
end;

procedure TJsonRegistry.GetValueNames(list: TStringList);
begin
  FJsConf.EnumValues(UTF8Decode(FCurrentKeyPath), list);
end;

function TJsonRegistry.ValueExists(name: String): Boolean;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  GetValueNames(Values);
  Result := Values.IndexOf(name) > -1;
  Values.Free;
end;

function TJsonRegistry.ReadInteger(name: String): Integer;
begin
  Result := FJsConf.GetValue(name, 0);
end;

function TJsonRegistry.ReadBool(name: String): Boolean;
begin
  Result := FJsConf.GetValue(name, False);
end;

function TJsonRegistry.ReadString(name: String): String;
begin
  Result := UTF8Encode(FJsConf.GetValue(name, ''));
end;

procedure TJsonRegistry.WriteInteger(name: String; value: Integer);
begin
  FJsConf.SetValue(UTF8Decode(name), value);
end;

procedure TJsonRegistry.WriteBool(name: String; value: Boolean);
begin
  FJsConf.SetValue(UTF8Decode(name), value);
end;

procedure TJsonRegistry.WriteString(name: String; value: String);
begin
  FJsConf.SetValue(name, value);
end;

function TJsonRegistry.GetDataType(Path: String): TJSONtype;
begin
  Result := FJsConf.DataType(Path);
end;

end.

