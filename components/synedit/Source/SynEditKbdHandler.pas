{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmds.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmds.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditKbdHandler.pas,v 1.10.2.1 2004/08/31 12:55:17 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITKBDHANDLER}
unit SynEditKbdHandler;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  SynEditTypes,
  SysUtils,
  Classes;

type
  { This class provides a TWinControl-Object which supports only the
    needed Methods }
  TKeyboardControl = class(TWinControl)
  public
    property OnKeyDown;
    property OnKeyPress;
    property OnMouseDown;
  end;

  TMouseCursorEvent =  procedure(Sender: TObject; const aLineCharPos: TBufferCoord;
    var aCursor: TCursor) of object;

  TMethodList = class
  private
    FData: TList;
    function GetItem(Index: Integer): TMethod;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aHandler: TMethod);
    procedure Remove(aHandler: TMethod);
    property Items[Index: Integer]: TMethod read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TSynEditKbdHandler = class (TObject)
  private
    FKeyPressChain: TMethodList;
    FKeyDownChain: TMethodList;
    FKeyUpChain: TMethodList;
    FMouseDownChain: TMethodList;
    FMouseUpChain: TMethodList;
    FMouseCursorChain: TMethodList;
    { avoid infinite recursiveness }
    FInKeyPress: Boolean;
    FInKeyDown: Boolean;
    FInKeyUp: Boolean;
    FInMouseDown: Boolean;
    FInMouseUp: Boolean;
    FInMouseCursor: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ExecuteKeyPress(Sender: TObject; var Key: WideChar);
    procedure ExecuteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExecuteKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ExecuteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExecuteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExecuteMouseCursor(Sender: TObject; const aLineCharPos: TBufferCoord;
      var aCursor: TCursor);

    procedure AddKeyDownHandler(aHandler: TKeyEvent);
    procedure RemoveKeyDownHandler(aHandler: TKeyEvent);
    procedure AddKeyUpHandler(aHandler: TKeyEvent);
    procedure RemoveKeyUpHandler(aHandler: TKeyEvent);
    procedure AddKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
    procedure AddMouseDownHandler(aHandler: TMouseEvent);
    procedure RemoveMouseDownHandler(aHandler: TMouseEvent);
    procedure AddMouseUpHandler(aHandler: TMouseEvent);
    procedure RemoveMouseUpHandler(aHandler: TMouseEvent);
    procedure AddMouseCursorHandler(aHandler: TMouseCursorEvent);
    procedure RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
  end;


implementation

{ TSynEditKbdHandler }

procedure TSynEditKbdHandler.AddKeyDownHandler(aHandler: TKeyEvent);
begin
  FKeyDownChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddKeyUpHandler(aHandler: TKeyEvent);
begin
  FKeyUpChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  FKeyPressChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddMouseDownHandler(aHandler: TMouseEvent);
begin
  FMouseDownChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddMouseUpHandler(aHandler: TMouseEvent);
begin
  FMouseUpChain.Add(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.AddMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  FMouseCursorChain.Add(TMethod(aHandler));
end;

constructor TSynEditKbdHandler.Create;
begin
  inherited;

  { Elements to handle KeyDown-Events }
  FKeyDownChain := TMethodList.Create;

  { Elements to handle KeyUp-Events }
  FKeyUpChain := TMethodList.Create;

  { Elements to handle KeyPress-Events }
  FKeyPressChain := TMethodList.Create;

  { Elements to handle MouseDown Events }
  FMouseDownChain := TMethodList.Create;

  { Elements to handle MouseUp Events }
  FMouseUpChain := TMethodList.Create;

  { Elements to handle MouseCursor Events }
  FMouseCursorChain := TMethodList.Create;
end;

destructor TSynEditKbdHandler.Destroy;
begin
  FKeyPressChain.Free;
  FKeyDownChain.Free;
  FKeyUpChain.Free;
  FMouseDownChain.Free;
  FMouseUpChain.Free;
  FMouseCursorChain.Free;

  inherited Destroy;
end;

procedure TSynEditKbdHandler.ExecuteKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  idx: Integer;
begin
  if FInKeyDown then
    Exit;
  FInKeyDown := True;
  try
    with FKeyDownChain do
    begin
      for idx := Count - 1 downto 0 do
      begin
        TKeyEvent(Items[idx])(Sender, Key, Shift);
        if (Key = 0) then
        begin
          FInKeyDown := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyDown := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  idx: Integer;
begin
  if FInKeyUp then
    Exit;
  FInKeyUp := True;
  try
    with FKeyUpChain do
    begin
      for idx := Count - 1 downto 0 do
      begin
        TKeyEvent(Items[idx])(Sender,Key,Shift);
        if (Key = 0) then
        begin
          FInKeyUp := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyUp := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteKeyPress(Sender: TObject; var Key: WideChar);
var
  idx: Integer;
begin
  if FInKeyPress then
    Exit;
  FInKeyPress := True;
  try
    with FKeyPressChain do
    begin
      for idx := Count - 1 downto 0 do
      begin
        TKeyPressWEvent(Items[idx])(Sender, Key);
        if (Key = #0) then
        begin
          FInKeyPress := False;
          Exit;
        end;
      end;
    end;
  finally
    FInKeyPress := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cHandler: Integer;
begin
  if FInMouseDown then
    Exit;
  FInMouseDown := True;
  try
    for cHandler := FMouseDownChain.Count - 1 downto 0 do
      TMouseEvent(FMouseDownChain[cHandler])(Sender, Button, Shift, X, Y);
  finally
    FInMouseDown := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  cHandler: Integer;
begin
  if FInMouseUp then
    Exit;
  FInMouseUp := True;
  try
    for cHandler := FMouseUpChain.Count - 1 downto 0 do
      TMouseEvent(FMouseUpChain[cHandler])(Sender, Button, Shift, X, Y);
  finally
    FInMouseUp := False;
  end;
end;

procedure TSynEditKbdHandler.ExecuteMouseCursor(Sender: TObject;
  const aLineCharPos: TBufferCoord; var aCursor: TCursor);
var
  cHandler: Integer;
begin
  if FInMouseCursor then
    Exit;
  FInMouseCursor := True;
  try
    for cHandler := FMouseCursorChain.Count - 1 downto 0 do
      TMouseCursorEvent(FMouseCursorChain[cHandler])(Sender, aLineCharPos, aCursor);
  finally
    FInMouseCursor := False;
  end;
end;

procedure TSynEditKbdHandler.RemoveKeyDownHandler(aHandler: TKeyEvent);
begin
  FKeyDownChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveKeyUpHandler(aHandler: TKeyEvent);
begin
  FKeyUpChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveKeyPressHandler(aHandler: TKeyPressWEvent);
begin
  FKeyPressChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveMouseDownHandler(aHandler: TMouseEvent);
begin
  FMouseDownChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveMouseUpHandler(aHandler: TMouseEvent);
begin
  FMouseUpChain.Remove(TMethod(aHandler));
end;

procedure TSynEditKbdHandler.RemoveMouseCursorHandler(aHandler: TMouseCursorEvent);
begin
  FMouseCursorChain.Remove(TMethod(aHandler));
end;

{ TMethodList }

procedure TMethodList.Add(aHandler: TMethod);
begin
  FData.Add(aHandler.Data);
  FData.Add(aHandler.Code);
end;

constructor TMethodList.Create;
begin
  inherited;

  FData := TList.Create;
end;

destructor TMethodList.Destroy;
begin
  FData.Free;

  inherited;
end;

function TMethodList.GetCount: Integer;
begin
  Result := FData.Count div 2;
end;

function TMethodList.GetItem(Index: Integer): TMethod;
begin
  Index := Index * 2;
  Result.Data := FData[Index];
  Result.Code := FData[Index + 1];
end;

procedure TMethodList.Remove(aHandler: TMethod);
var
  cPos: Integer;
begin
  cPos := FData.Count - 2;
  while cPos >= 0 do
  begin
    if (FData.List[cPos] = aHandler.Data) and (FData.List[cPos + 1] = aHandler.Code) then
    begin
      FData.Delete(cPos);
      FData.Delete(cPos);
      Exit;
    end;
    Dec(cPos, 2);
  end;
end;

end.
