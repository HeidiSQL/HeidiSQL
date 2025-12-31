unit jsonregistry;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, jsonConf, fpjson, ExtCtrls;

type

  { TJSONConfigExtended }

  TJSONConfigExtended = class(TJSONConfig)
    public
      function AddEndSlash(Path: UnicodeString): UnicodeString;
      function StripEndSlash(Path: UnicodeString): UnicodeString;
      procedure MoveKey(SourcePath: UnicodeString; TargetPath: UnicodeString; Delete: Boolean);
      function DataType(Path: UnicodeString): TJSONtype;
  end;

  { TJSONRegistry }

  TJsonRegistry = class(TPersistent)
    private
      FJsConf: TJSONConfigExtended;
      FCurrentKeyPath: UnicodeString;
      FAutoFlushTimer: TTimer;
      function GetAutoFlushMilliSeconds: Cardinal;
      procedure SetAutoFlushMilliSeconds(aValue: Cardinal);
      procedure AutoFlushOnTimer(Sender: TObject);
    public
      constructor Create(JsonFilePath: String);
      destructor Destroy; override;
      function FilePath: String;
      // Keys:
      function OpenKey(Path: UnicodeString; CanCreate: Boolean): Boolean;
      procedure CloseKey;
      function DeleteKey(Path: UnicodeString): Boolean;
      procedure MoveKey(sourcepath: UnicodeString; targetpath: UnicodeString; Delete: Boolean);
      procedure GetKeyNames(list: TStringList);
      function CurrentPath: UnicodeString;
      function KeyExists(path: UnicodeString): Boolean;
      function HasSubKeys: Boolean;
      // Values:
      function DeleteValue(name: UnicodeString): Boolean;
      procedure GetValueNames(list: TStringList);
      function ValueExists(name: UnicodeString): Boolean;
      function ReadInteger(name: UnicodeString): Integer;
      function ReadBool(name: UnicodeString): Boolean;
      function ReadString(name: UnicodeString): UnicodeString;
      procedure WriteInteger(name: UnicodeString; value: Integer);
      procedure WriteBool(name: UnicodeString; value: Boolean);
      procedure WriteString(name: UnicodeString; value: UnicodeString);
      function GetDataType(Path: UnicodeString): TJSONtype;

      property AutoFlushMilliSeconds: Cardinal read GetAutoFlushMilliSeconds write SetAutoFlushMilliSeconds;
      procedure FlushToDisk;
  end;

implementation

{ TJSONConfigExtended }

function TJSONConfigExtended.AddEndSlash(Path: UnicodeString): UnicodeString;
begin
  Result := Path;
  if Result[Length(Result)] <> '/' then
    Result := Result + '/';
end;

function TJSONConfigExtended.StripEndSlash(Path: UnicodeString): UnicodeString;
begin
  Result := Path;
  if Result[Length(Result)] = '/' then
    Delete(Result, Length(Result), 1);
end;

procedure TJSONConfigExtended.MoveKey(SourcePath: UnicodeString;
  TargetPath: UnicodeString; Delete: Boolean);
var
  OldNode, NewNode, NewNodeParent: TJSONObject;
  NewNodeName: UnicodeString;
  TargetPathSlash, TargetPathNoSlash: UnicodeString;
begin

  if Length(SourcePath) = 0 then
    Raise EJSONConfigError.Create('Cannot move from empty path');
  if Length(TargetPath) = 0 then
    Raise EJSONConfigError.Create('Cannot move to empty path');

  SourcePath := AddEndSlash(SourcePath);
  OldNode := FindObject(SourcePath, False);
  if not Assigned(OldNode) then
    raise EJSONConfigError.CreateFmt('Source path does not exist: %s', [SourcePath]);

  TargetPathSlash := AddEndSlash(TargetPath);
  TargetPathNoSlash := StripEndSlash(TargetPath);
  //showmessage('TargetPathSlash:"'+TargetPathSlash+'"'+sLineBreak+'TargetPathNoSlash:"'+TargetPathNoSlash+'"');

  // Error if target exists
  NewNode := FindObject(TargetPathSlash, False);
  if Assigned(NewNode) then
    Raise EJSONConfigError.CreateFmt('Target path already exists: %s', [TargetPathSlash]);

  // Create copied key
  NewNodeParent := FindObject(TargetPathNoSlash, True, NewNodeName);
  NewNodeParent.Add(NewNodeName, OldNode.Clone);

  if Delete then begin
    // Deleting source key. Note we have cloned this before.
    DeletePath(SourcePath);
  end;

  FModified:=True;
end;

function TJSONConfigExtended.DataType(Path: UnicodeString): TJSONtype;
var
  e: TJSONData;
begin
  e := FindElement(Path, False, True);
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

function TJsonRegistry.OpenKey(Path: UnicodeString; CanCreate: Boolean): Boolean;
begin
  try
    FJsConf.OpenKey(Path, CanCreate);
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

function TJsonRegistry.DeleteKey(Path: UnicodeString): Boolean;
begin
  FJsConf.DeletePath(Path);
  Result := True;
end;

procedure TJsonRegistry.MoveKey(sourcepath: UnicodeString; targetpath: UnicodeString; Delete: Boolean);
begin
  FJsConf.MoveKey(sourcepath, targetpath, Delete);
end;

procedure TJsonRegistry.GetKeyNames(list: TStringList);
begin
  FJsConf.EnumSubKeys(FCurrentKeyPath, list);
end;

function TJsonRegistry.CurrentPath: UnicodeString;
begin
  Result := FCurrentKeyPath;
end;

function TJsonRegistry.KeyExists(path: UnicodeString): Boolean;
var
  SubKeys: TStringList;
  LastDelim: Integer;
  folder, name: UnicodeString;
begin
  SubKeys := TStringList.Create;
  path := FJsConf.StripEndSlash(path);
  LastDelim := String(path).LastIndexOf('/');
  name := Copy(path, LastDelim+2);
  folder := Copy(path, 1, LastDelim);
  //showmessage('folder:'+folder+sLineBreak+'name:'+name);
  FJsConf.EnumSubKeys(folder, SubKeys);
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

function TJsonRegistry.DeleteValue(name: UnicodeString): Boolean;
begin
  FJsConf.DeleteValue(name);
  Result := True;
end;

procedure TJsonRegistry.GetValueNames(list: TStringList);
begin
  FJsConf.EnumValues(FCurrentKeyPath, list);
end;

function TJsonRegistry.ValueExists(name: UnicodeString): Boolean;
var
  Values: TStringList;
begin
  Values := TStringList.Create;
  GetValueNames(Values);
  Result := Values.IndexOf(name) > -1;
  Values.Free;
end;

function TJsonRegistry.ReadInteger(name: UnicodeString): Integer;
begin
  Result := FJsConf.GetValue(name, 0);
end;

function TJsonRegistry.ReadBool(name: UnicodeString): Boolean;
begin
  Result := FJsConf.GetValue(name, False);
end;

function TJsonRegistry.ReadString(name: UnicodeString): UnicodeString;
begin
  Result := FJsConf.GetValue(name, '');
end;

procedure TJsonRegistry.WriteInteger(name: UnicodeString; value: Integer);
begin
  FJsConf.SetValue(name, value);
end;

procedure TJsonRegistry.WriteBool(name: UnicodeString; value: Boolean);
begin
  FJsConf.SetValue(name, value);
end;

procedure TJsonRegistry.WriteString(name: UnicodeString; value: UnicodeString);
begin
  FJsConf.SetValue(name, value);
end;

function TJsonRegistry.GetDataType(Path: UnicodeString): TJSONtype;
begin
  Result := FJsConf.DataType(Path);
end;

end.

