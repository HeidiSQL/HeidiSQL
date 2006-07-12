{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynMacroRecorder.pas, released 2001-10-17.

Author of this file is Flávio Etrusco.
Portions created by Flávio Etrusco are Copyright 2001 Flávio Etrusco.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynMacroRecorder.pas,v 1.14 2002/04/08 08:38:14 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynMacroRecorder;

{$I SynEdit.inc}

interface

uses
  Classes,
  SynEdit,
  SynEditKeyCmds,  //js 06-04-2002 got consts out here and qconsts in
{$IFDEF SYN_CLX}
  QConsts,
  QStdCtrls,
  QControls,
  Qt,
  Types,
  QGraphics,
  QMenus,
{$ELSE}
  StdCtrls,
  Controls,
  Windows,
  Messages,
  Graphics,
  Menus,
{$ENDIF}
  SynEditPlugins;

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  sCannotRecord = 'Cannot record macro when recording';
  sCannotPlay = 'Cannot playback macro when recording';
  sCannotPause = 'Can only pause when recording';
  sCannotResume = 'Can only resume when paused';

type
  TSynMacroState = (msStopped, msRecording, msPlaying, msPaused);
  TSynMacroCommand = (mcRecord, mcPlayback);

  TSynMacroEvent = class(TObject)
  protected
    fRepeatCount: Byte;
    function GetAsString : string; virtual; abstract;
    procedure InitEventParameters(aStr : string); virtual; abstract;
  public
    constructor Create; virtual;
    procedure Initialize(aCmd: TSynEditorCommand; aChar: Char; aData: Pointer);
      virtual; abstract;
    { the CommandID must not be read inside LoadFromStream/SaveToStream. It's read by the
    MacroRecorder component to decide which MacroEvent class to instanciate }
    procedure LoadFromStream(aStream: TStream); virtual; abstract;
    procedure SaveToStream(aStream: TStream); virtual; abstract;
    procedure Playback(aEditor: TCustomSynEdit); virtual; abstract;
    property AsString : string read GetAsString;
    property RepeatCount : Byte read fRepeatCount write fRepeatCount;
  end;

  TSynBasicEvent = class(TSynMacroEvent)
  protected
    fCommand: TSynEditorCommand;
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: Char; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Command: TSynEditorCommand read fCommand write fCommand;
  end;

  TSynCharEvent = class(TSynMacroEvent)
  protected
    fKey: char;
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: Char; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Key: char read fKey write fKey;
  end;

  TSynStringEvent = class(TSynMacroEvent)
  protected
    fString: string;
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: Char; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Value: string read fString write fString;
  end;

  TSynPositionEvent = class(TSynBasicEvent)
  protected
    fPosition: TPoint;
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: Char; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Position: TPoint read fPosition write fPosition;
  end;

  TSynDataEvent = class(TSynBasicEvent)
  protected
    fData: Pointer;
  public
    procedure Initialize(aCmd: TSynEditorCommand; aChar: Char; aData: Pointer);
      override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  end;

  TCustomSynMacroRecorder = class;

  TSynUserCommandEvent = procedure (aSender: TCustomSynMacroRecorder;
    aCmd: TSynEditorCommand; var aEvent: TSynMacroEvent) of object;

  TCustomSynMacroRecorder = class(TAbstractSynHookerPlugin)
  private
    fShortCuts: array [TSynMacroCommand] of TShortCut;
    fOnStateChange: TNotifyEvent;
    fOnUserCommand: TSynUserCommandEvent;
    fMacroName: string;
    function GetEvent(aIndex: integer): TSynMacroEvent;
    function GetEventCount: integer;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  protected
    fCurrentEditor: TCustomSynEdit;
    fState: TSynMacroState;
    fEvents: TList;
    fCommandIDs: array [TSynMacroCommand] of TSynEditorCommand;
    procedure SetShortCut(const Index: Integer; const Value: TShortCut);
    function GetIsEmpty: boolean;
    procedure StateChanged;
    procedure DoAddEditor(aEditor: TCustomSynEdit); override;
    procedure DoRemoveEditor(aEditor: TCustomSynEdit); override;
    procedure OnCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var aChar: char;
      Data: pointer; HandlerData: pointer); override;
    function CreateMacroEvent(aCmd: TSynEditorCommand): TSynMacroEvent;
  protected
    property RecordCommandID: TSynEditorCommand read fCommandIDs[mcRecord];
    property PlaybackCommandID: TSynEditorCommand read fCommandIDs[mcPlayback];
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Error(const aMsg: String);
    procedure AddEditor(aEditor: TCustomSynEdit);
    procedure RemoveEditor(aEditor: TCustomSynEdit);
    procedure RecordMacro(aEditor: TCustomSynEdit);
    procedure PlaybackMacro(aEditor: TCustomSynEdit);
    procedure Stop;
    procedure Pause;
    procedure Resume;
    property IsEmpty: boolean read GetIsEmpty;
    property State: TSynMacroState read fState;
    procedure Clear;
    procedure AddEvent(aCmd: TSynEditorCommand; aChar: char; aData: pointer);
    procedure InsertEvent(aIndex: integer; aCmd: TSynEditorCommand; aChar: char;
      aData: pointer);
    procedure AddCustomEvent(aEvent: TSynMacroEvent);
    procedure InsertCustomEvent(aIndex: integer; aEvent: TSynMacroEvent);
    procedure DeleteEvent(aIndex: integer);
    procedure LoadFromStream(aSrc: TStream);
    procedure LoadFromStreamEx(aSrc: TStream; aClear: boolean);
    procedure SaveToStream(aDest: TStream);
    procedure LoadFromFile(aFilename : string);
    procedure SaveToFile(aFilename : string);
    property EventCount: integer read GetEventCount;
    property Events[aIndex: integer]: TSynMacroEvent read GetEvent;
    property RecordShortCut: TShortCut index Ord(mcRecord)
      read fShortCuts[mcRecord] write SetShortCut;
    property PlaybackShortCut: TShortCut index Ord(mcPlayback)
      read fShortCuts[mcPlayback] write SetShortCut;
    property AsString : string read GetAsString write SetAsString;
    property MacroName : string read fMacroName write fMacroName;
    { occurs when changing recorder state }
    property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
    property OnUserCommand: TSynUserCommandEvent read fOnUserCommand
      write fOnUserCommand;
  end;

  TSynMacroRecorder = class(TCustomSynMacroRecorder)
  published
    property RecordShortCut;
    property PlaybackShortCut;
    property OnStateChange;
    property OnUserCommand;
  end;

implementation

uses
  SynEditMiscProcs,
  SynEditTypes,
{$IFDEF SYN_CLX}
  QForms,
{$ELSE}
  Forms,
{$IFDEF SYN_DELPHI_6_UP}
  RTLConsts,
{$ENDIF}
{$ENDIF}
  SysUtils;

{ TSynDataEvent }

procedure TSynDataEvent.Initialize(aCmd: TSynEditorCommand; aChar: Char;
  aData: Pointer);
begin
  fCommand := aCmd;
  Assert( aChar = #0 );
  fData := aData;
end;

procedure TSynDataEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read( fData, SizeOf(fData) );
end;

procedure TSynDataEvent.Playback(aEditor: TCustomSynEdit);
begin
  aEditor.CommandProcessor( Command, #0, fData );
end;

procedure TSynDataEvent.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.Write( fData, SizeOf(fData) );
end;

{ TCustomSynMacroRecorder }

procedure TCustomSynMacroRecorder.AddCustomEvent(aEvent: TSynMacroEvent);
begin
  InsertCustomEvent( EventCount, aEvent );
end;

procedure TCustomSynMacroRecorder.AddEditor(aEditor: TCustomSynEdit);
begin
  inherited AddEditor(aEditor);
end;

procedure TCustomSynMacroRecorder.AddEvent(aCmd: TSynEditorCommand;
  aChar: char; aData: pointer);
begin
  InsertEvent( EventCount, aCmd, aChar, aData );
end;

procedure TCustomSynMacroRecorder.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  if Assigned(fEvents) then
  begin
    for I := fEvents.Count-1 downto 0 do
    begin
      Obj := fEvents[I];
      fEvents.Delete(I);
      Obj.Free;
    end;
    FreeAndNil( fEvents );
  end;
end;

constructor TCustomSynMacroRecorder.Create(aOwner: TComponent);
begin
  inherited;
  fMacroName := 'unnamed';
  fCommandIDs[mcRecord] := NewPluginCommand;
  fCommandIDs[mcPlayback] := NewPluginCommand;
  {$IFDEF SYN_CLX}  //js 06-04-2002 not only for linux, should also use qmenus when in clx for windows
  fShortCuts[mcRecord] := QMenus.ShortCut( Ord('R'), [ssCtrl, ssShift] );
  fShortCuts[mcPlayback] := QMenus.ShortCut( Ord('P'), [ssCtrl, ssShift] );
  {$ELSE}
  fShortCuts[mcRecord] := Menus.ShortCut( Ord('R'), [ssCtrl, ssShift] );
  fShortCuts[mcPlayback] := Menus.ShortCut( Ord('P'), [ssCtrl, ssShift] );
  {$ENDIF}
end;

function TCustomSynMacroRecorder.CreateMacroEvent(aCmd: TSynEditorCommand): TSynMacroEvent;

  function WantDefaultEvent(var aEvent: TSynMacroEvent): boolean;
  begin
    if Assigned( OnUserCommand ) then
      OnUserCommand( Self, aCmd, aEvent );
    Result := aEvent = nil;
  end;

begin
  case aCmd of
    ecGotoXY, ecSelGotoXY, ecSetMarker0..ecSetMarker9:
      begin
        Result := TSynPositionEvent.Create;
        TSynPositionEvent(Result).Command := aCmd;
      end;
    ecChar:
      Result := TSynCharEvent.Create;
    ecString:
      Result := TSynStringEvent.Create;
    else begin
      Result := nil;
      if (aCmd < ecUserFirst) or WantDefaultEvent( Result ) then
      begin
        Result := TSynBasicEvent.Create;
        TSynBasicEvent(Result).Command := aCmd;
      end;
    end;
  end;
end;

procedure TCustomSynMacroRecorder.DeleteEvent(aIndex: integer);
var
  iObj: Pointer;
begin
  {$IFDEF MSWINDOWS}
  if fEvents = nil then
    TList.Error( sListIndexError, aIndex );
  {$ENDIF}
  iObj := fEvents[ aIndex ];
  fEvents.Delete( aIndex );
  TObject( iObj ).Free;
end;

destructor TCustomSynMacroRecorder.Destroy;
begin
  Clear;
  inherited;
  ReleasePluginCommand( PlaybackCommandID );
  ReleasePluginCommand( RecordCommandID );
end;

procedure TCustomSynMacroRecorder.DoAddEditor(aEditor: TCustomSynEdit);
begin
  HookEditor( aEditor, RecordCommandID, 0, RecordShortCut );
  HookEditor( aEditor, PlaybackCommandID, 0, PlaybackShortCut );
end;

procedure TCustomSynMacroRecorder.DoRemoveEditor(aEditor: TCustomSynEdit);
begin
  UnHookEditor( aEditor, RecordCommandID, RecordShortCut );
  UnHookEditor( aEditor, PlaybackCommandID, PlaybackShortCut );
end;

procedure TCustomSynMacroRecorder.Error(const aMsg: String);
begin
  raise Exception.Create(aMsg);
end;

function TCustomSynMacroRecorder.GetEvent(aIndex: integer): TSynMacroEvent;
begin
  Result := TSynMacroEvent( fEvents[aIndex] );
end;

function TCustomSynMacroRecorder.GetEventCount: integer;
begin
  if fEvents = nil then
    Result := 0
  else
    Result := fEvents.Count;
end;

function TCustomSynMacroRecorder.GetIsEmpty: boolean;
begin
  Result := (fEvents = nil) or (fEvents.Count = 0);
end;

procedure TCustomSynMacroRecorder.InsertCustomEvent(aIndex: integer;
  aEvent: TSynMacroEvent);
begin
  if fEvents = nil then
    fEvents := TList.Create;
  fEvents.Insert( aIndex, aEvent );
end;

procedure TCustomSynMacroRecorder.InsertEvent(aIndex: integer;
  aCmd: TSynEditorCommand; aChar: char; aData: pointer);
var
  iEvent: TSynMacroEvent;
begin
  iEvent := CreateMacroEvent( aCmd );
  try
    iEvent.Initialize( aCmd, aChar, aData );
    InsertCustomEvent( aIndex, iEvent );
  except
    iEvent.Free;
    raise;
  end;
end;

procedure TCustomSynMacroRecorder.LoadFromStream(aSrc: TStream);
begin
  LoadFromStreamEx( aSrc, True );
end;

procedure TCustomSynMacroRecorder.LoadFromStreamEx(aSrc: TStream;
  aClear: boolean);
var
  iCommand : TSynEditorCommand;
  iEvent: TSynMacroEvent;
  cnt, i : Integer;
begin
  Stop;
  if aClear then
    Clear;
  fEvents := TList.Create;
  aSrc.Read(cnt, sizeof(cnt));
  i := 0;
  fEvents.Capacity := aSrc.Size div SizeOf( TSynEditorCommand );
  while (aSrc.Position < aSrc.Size) and (i < cnt) do
  begin
    aSrc.Read( iCommand, SizeOf(TSynEditorCommand) );
    iEvent := CreateMacroEvent( iCommand );
    iEvent.Initialize( iCommand, #0, nil );
    iEvent.LoadFromStream( aSrc );
    fEvents.Add( iEvent );
    Inc(i);
  end;
end;

procedure TCustomSynMacroRecorder.OnCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; var aChar: char; Data,
  HandlerData: pointer);
var
  iEvent: TSynMacroEvent;
begin
  if AfterProcessing then
  begin
    if fCurrentEditor <> Sender then
      Exit;
    if (State = msRecording) and (not Handled) then begin
      iEvent := CreateMacroEvent( Command );
      iEvent.Initialize( Command, aChar, Data );
      fEvents.Add( iEvent );
    end;
  end
  else begin
    {not AfterProcessing}
    case State of
      msStopped:
        if Command = RecordCommandID then
        begin
          RecordMacro( TCustomSynEdit( Sender ) );
          Handled := True;
        end
        else if Command = PlaybackCommandID then
        begin
          PlaybackMacro( TCustomSynEdit( Sender ) );
          Handled := True;
        end;
      msPlaying:
        ;
      msPaused:
        if Command = PlaybackCommandID then
        begin
          Resume;
          Handled := True;
        end;
      msRecording:
        if Command = PlaybackCommandID then
        begin
          Pause;
          Handled := True;
        end
        else if Command = RecordCommandID then
        begin
          Stop;
          Handled := True;
        end;
    end;
  end;
end;

procedure TCustomSynMacroRecorder.Pause;
begin
  if State <> msRecording then
    Error( sCannotPause );
  fState := msPaused;
  StateChanged;
end;

procedure TCustomSynMacroRecorder.PlaybackMacro(aEditor: TCustomSynEdit);
var
  cEvent: integer;
begin
  if State <> msStopped then
    Error( sCannotPlay );
  fState := msPlaying;
  try
    StateChanged;
    for cEvent := 0 to EventCount -1 do
      Events[ cEvent ].Playback( aEditor );
  finally
    fState := msStopped;
    StateChanged;
  end;
end;

procedure TCustomSynMacroRecorder.RecordMacro(aEditor: TCustomSynEdit);
begin
  if fState <> msStopped then
    Error( sCannotRecord );
  Clear;
  fEvents := TList.Create;
  fEvents.Capacity := 512;
  fState := msRecording;
  fCurrentEditor := aEditor;
  StateChanged;
end;

procedure TCustomSynMacroRecorder.RemoveEditor(aEditor: TCustomSynEdit);
begin
  inherited RemoveEditor( aEditor );
end;

procedure TCustomSynMacroRecorder.Resume;
begin
  if fState <> msPaused then
    Error( sCannotResume );
  fState := msRecording;
  StateChanged;
end;

procedure TCustomSynMacroRecorder.SaveToStream(aDest: TStream);
var
  cEvent, eCnt : integer;
begin
  eCnt := EventCount;
  aDest.Write(eCnt, sizeof(eCnt));
  for cEvent := 0 to eCnt -1 do
    Events[ cEvent ].SaveToStream( aDest );
end;

procedure TCustomSynMacroRecorder.SetShortCut(const Index: Integer;
  const Value: TShortCut);
var
  cEditor: integer;
begin
  if fShortCuts[TSynMacroCommand(Index)] <> Value then
  begin
    if Assigned(fEditors) then
      if Value <> 0 then
      begin
        for cEditor := 0 to fEditors.Count -1 do
          HookEditor( Editors[cEditor], fCommandIDs[TSynMacroCommand(Index)],
            fShortCuts[TSynMacroCommand(Index)], Value );
      end else
      begin
        for cEditor := 0 to fEditors.Count -1 do
          UnHookEditor( Editors[cEditor], fCommandIDs[TSynMacroCommand(Index)],
            fShortCuts[TSynMacroCommand(Index)] );
      end;
    fShortCuts[TSynMacroCommand(Index)] := Value;
  end;
end;

procedure TCustomSynMacroRecorder.StateChanged;
begin
  if Assigned( OnStateChange ) then
    OnStateChange( Self );
end;

procedure TCustomSynMacroRecorder.Stop;
begin
  if fState = msStopped then
    Exit;
  fState := msStopped;
  fCurrentEditor := nil;
  if fEvents.Count = 0 then
    FreeAndNil( fEvents );
  StateChanged;
end;

function TCustomSynMacroRecorder.GetAsString: string;
var
  i : integer;
  eStr : string;
begin
  Result := 'macro ' + MacroName + #13#10 + 'begin' + #13#10;
  if Assigned(fEvents) then
  begin
    for i := 0 to fEvents.Count -1 do
    begin
      eStr := Events[i].AsString;
      if eStr <> '' then
        Result := Result + '  '  + eStr + #13#10;
    end;
  end;
  Result := Result + 'end';
end;

procedure TCustomSynMacroRecorder.SetAsString(const Value: string);
var
  i, p, Cmd : Integer;
  S : TStrings;
  cmdStr : string;
  iEvent: TSynMacroEvent;
begin
  Stop;
  Clear;
  fEvents := TList.Create;
  // process file line by line and create events
  S := TStringList.Create;
  try
    S.Text := Value;
    for i := 0 to S.Count - 1 do
    begin
      cmdStr := Trim(S[i]);
      p := Pos(' ', cmdStr);
      if p = 0 then p := Length(cmdStr)+1;
      Cmd := ecNone;
      if IdentToEditorCommand(Copy(cmdStr, 1, p-1), Cmd) then
      begin
        Delete(cmdStr, 1, p);
        iEvent := CreateMacroEvent(Cmd);
        try
          fEvents.Add(iEvent);
          iEvent.InitEventParameters(cmdStr);
        except
          iEvent.Free;
        end;
      end;
    end;
  finally
    S.Free;
  end;
end;

procedure TCustomSynMacroRecorder.LoadFromFile(aFilename: string);
var
  F : TFileStream;
begin
  F := TFileStream.Create(aFilename, fmOpenRead);
  try
    LoadFromStream(F);
    MacroName := ChangeFileExt(ExtractFileName(aFilename), '');
  finally
    F.Free;
  end;
end;

procedure TCustomSynMacroRecorder.SaveToFile(aFilename: string);
var
  F : TFileStream;
begin
  F := TFileStream.Create(aFilename, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

{ TSynBasicEvent }

function TSynBasicEvent.GetAsString: string;
begin
  Result := '';
  EditorCommandToIdent(Command, Result);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynBasicEvent.InitEventParameters(aStr: string);
begin
  // basic events have no parameters but can contain an optional repeat count
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynBasicEvent.Initialize(aCmd: TSynEditorCommand; aChar: Char;
  aData: Pointer);
begin
  Command := aCmd;
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  if (aChar <> #0) or (aData <> nil) then
    raise Exception.Create('TSynBasicEvent cannot handle Char <> #0 or Data <> nil');
{$ENDIF}
end;

procedure TSynBasicEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read( fRepeatCount, SizeOf(fRepeatCount) );
end;

procedure TSynBasicEvent.Playback(aEditor: TCustomSynEdit);
var
  i : Integer;
begin
  for i := 1 to RepeatCount do
    aEditor.CommandProcessor( Command, #0, nil );
end;

procedure TSynBasicEvent.SaveToStream(aStream: TStream);
begin
  aStream.Write( Command, SizeOf(TSynEditorCommand) );
  aStream.Write( RepeatCount, SizeOf(RepeatCount) );
end;

{ TSynCharEvent }

function TSynCharEvent.GetAsString: string;
begin
  Result := '';
  EditorCommandToIdent(ecChar, Result);
  Result := Result + ' ' + Key;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynCharEvent.InitEventParameters(aStr: string);
begin
  // aStr should be a Key value one character in length
  // with an optional repeat count whitespace separated
  if Length(aStr) >= 1 then
    Key := aStr[1]
  else
    Key := ' ';
  Delete(aStr, 1, 1); // if possible delete the first character
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynCharEvent.Initialize(aCmd: TSynEditorCommand; aChar: Char;
  aData: Pointer);
begin
  Key := aChar;
  Assert( aData = nil );
end;

procedure TSynCharEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read( fKey, SizeOf(Key) );
  aStream.Read( fRepeatCount, SizeOf(fRepeatCount) );
end;

procedure TSynCharEvent.Playback(aEditor: TCustomSynEdit);
var
  i : Integer;
begin
  for i := 1 to RepeatCount do
    aEditor.CommandProcessor( ecChar, Key, nil );
end;

procedure TSynCharEvent.SaveToStream(aStream: TStream);
const
  iCharCommand: TSynEditorCommand = ecChar;
begin
  aStream.Write( iCharCommand, SizeOf(TSynEditorCommand) );
  aStream.Write( Key, SizeOf(Key) );
  aStream.Write( RepeatCount, SizeOf(RepeatCount) );
end;

{ TSynPositionEvent }

function TSynPositionEvent.GetAsString: string;
begin
  Result := inherited GetAsString;
  // add position data here
  Result := Result + Format(' (%d, %d)', [Position.x, Position.y]);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynPositionEvent.InitEventParameters(aStr: string);
var
  i, o, c, x, y : Integer;
  valStr : string;
begin
  inherited;
  // aStr should be (x, y) with optional repeat count whitespace separated
  aStr := Trim(aStr);
  i := Pos(',', aStr);
  o := Pos('(', aStr);
  c := Pos(')', aStr);
  if (not ((i = 0) or (o = 0) or (c = 0))) and
     ((i > o) and (i < c)) then
  begin
    valStr := Copy(aStr, o+1, i-o-1);
    x := StrToIntDef(valStr, 1);
    Delete(aStr, 1, i);
    aStr := Trim(aStr);
    c := Pos(')', aStr);
    valStr := Copy(aStr, 1, c-1);
    y := StrToIntDef(valStr, 1);
    Position := Point(x, y);
    Delete(aStr, 1, c);
    aStr := Trim(aStr);
    RepeatCount := StrToIntDef(aStr, 1);
  end;
end;

procedure TSynPositionEvent.Initialize(aCmd: TSynEditorCommand;
  aChar: Char; aData: Pointer);
begin
  inherited;
  if aData <> nil then
    Position := PPoint( aData )^;
end;

procedure TSynPositionEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read( fPosition, SizeOf(Position) );
end;

procedure TSynPositionEvent.Playback(aEditor: TCustomSynEdit);
begin
  aEditor.CommandProcessor( Command, #0, @Position );
end;

procedure TSynPositionEvent.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.Write( Position, SizeOf(Position) );
end;

{ TSynStringEvent }

function TSynStringEvent.GetAsString: string;
begin
  Result := '';
  EditorCommandToIdent(ecString, Result);
  Result := Result + ' ' + AnsiQuotedStr(Value, #39);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynStringEvent.InitEventParameters(aStr: string);
var
  o, c : Integer;
  valStr : string;
begin
  // aStr = 'test' with optional whitespace separated repeat count
  o := Pos('''', aStr);
  c := LastDelimiter('''', aStr);
  valStr := Copy(aStr, o+1, c-o-1);
  Value := StringReplace(valStr, '''''', '''', [rfReplaceAll]);
  Delete(aStr, 1, c);
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynStringEvent.Initialize(aCmd: TSynEditorCommand; aChar: Char;
  aData: Pointer);
begin
  Value := String(aData);
end;

procedure TSynStringEvent.LoadFromStream(aStream: TStream);
var
  l : Integer;
  Buff : PChar;
begin
  aStream.Read(l, SizeOf(l));
  GetMem(Buff, l);
  try
  {$IFNDEF SYN_CLX} //js 07-04-2002 changed from IFDEF WINDOWS
    FillMemory(Buff, l, 0);
  {$ENDIF}
    aStream.Read(Buff^, l);
    fString := Buff;
  finally
    FreeMem(Buff);
  end;
  aStream.Read( fRepeatCount, SizeOf(fRepeatCount) );
end;

procedure TSynStringEvent.Playback(aEditor: TCustomSynEdit);
var
  i, j : Integer;
begin
  for j := 1 to RepeatCount do
  begin
//    aEditor.CommandProcessor( ecString, #0, Pointer(Value) );
    // SynEdit doesn't actually support the ecString command so we convert
    // it into ecChar commands
    for i := 1 to Length(Value) do
      aEditor.CommandProcessor(ecChar, Value[i], nil);
  end;
end;

procedure TSynStringEvent.SaveToStream(aStream: TStream);
const
  StrCommand: TSynEditorCommand = ecString;
var
  l : Integer;
  Buff : PChar;
begin
  aStream.Write(StrCommand, SizeOf(StrCommand));
  l := Length(Value) + 1;
  aStream.Write(l, sizeof(l));
  GetMem(Buff, l);
  try
  {$IFNDEF SYN_CLX} //js 07-04-2002 changed from IFDEF WINDOWS
    FillMemory(Buff, l, 0);
  {$ENDIF}
    StrPCopy(Buff, Value);
    aStream.Write(Buff^, l);
  finally
    FreeMem(Buff);
  end;
  aStream.Write( RepeatCount, SizeOf(RepeatCount) );
end;



{ TSynMacroEvent }

constructor TSynMacroEvent.Create;
begin
  inherited Create;
  fRepeatCount := 1;
end;

end.
