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

$Id: SynEditKbdHandler.pas,v 1.4 2002/06/05 10:23:00 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditKbdHandler;

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QControls,
  QForms,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
{$ENDIF}
  SysUtils,
  Classes;

type
  { This class provides a TWinControl-Object which supports only the
    needed Methods }
{$IFDEF SYN_CLX}
  TKeyboardControl = class (TWidgetControl)
{$ELSE}
  TKeyboardControl = class (TWinControl)
{$ENDIF}
    public
      property OnKeyDown;
      property OnKeyPress;

  end;


  TKeyDownProc = class (TObject)

    private
      fKeyDownProc : TKeyEvent;

    public
      constructor Create (aKeyDownProc : TKeyEvent);

      property OnKeyDown : TKeyEvent read fKeyDownProc write fKeyDownProc;
  end;


  TKeyPressProc = class (TObject)

    private
      fKeyPressProc : TKeyPressEvent;

    public
      constructor Create (aKeyPressProc : TKeyPressEvent);

      property OnKeyPress : TKeyPressEvent read fKeyPressProc write fKeyPressProc;
  end;


  TSynEditKbdHandler = class (TObject)

    private
      fControl       : TKeyboardControl;
      fKeyPressChain : TList;
      fOldKeyPress   : TKeyPressEvent;
      fInKeyPress    : Boolean;
      fKeyDownChain  : TList;
      fOldKeyDown    : TKeyEvent;
      fInKeyDown     : Boolean;

      procedure SetOnKeyPress (const Value: TKeyPressEvent);
      procedure SetOnKeyDown (const Value: TKeyEvent);

    protected
      procedure EditorKeyPress (Sender: TObject; var Key: Char);
      procedure EditorKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);

    public
    {$IFDEF SYN_CLX}
      constructor Create (aControl : TWidgetControl);
    {$ELSE}
      constructor Create (aControl : TWinControl);
    {$ENDIF}
      destructor Destroy; override;

      procedure ExecuteKeyPress (Sender: TObject; var Key: Char);
      procedure ExecuteKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);

      procedure AddKeyDownHandler (aHandler : TKeyDownProc);
      procedure RemoveKeyDownHandler (aHandler : TKeyDownProc);
      procedure AddKeyPressHandler (aHandler : TKeyPressProc);
      procedure RemoveKeyPressHandler (aHandler : TKeyPressProc);

      property  OnKeyPress : TKeyPressEvent read fOldKeyPress write SetOnKeyPress;
      property  OnKeyDown  : TKeyEvent      read fOldKeyDown  write SetOnKeyDown;

  end;


implementation

{ TKeyDownProc }

constructor TKeyDownProc.Create (aKeyDownProc : TKeyEvent);
  begin
    inherited Create;
    fKeyDownProc := aKeyDownProc;
  end;


{ TKeyPressProc }

constructor TKeyPressProc.Create (aKeyPressProc : TKeyPressEvent);
  begin
    inherited Create;
    fKeyPressProc := aKeyPressProc;
  end;


{ TSynEditKbdHandler }

{$IFDEF SYN_CLX}
constructor TSynEditKbdHandler.Create (aControl : TWidgetControl);
{$ELSE}
constructor TSynEditKbdHandler.Create (aControl : TWinControl);
{$ENDIF}
  begin
    { Generic Pointer to the control which needs a handler chain }
    fControl := TKeyboardControl (aControl);

    { Elements to handle KeyDown-Events }
    fInKeyDown := false;
    fKeyDownChain := TList.Create;
    fOldKeyDown := fControl.OnKeyDown;
    fControl.OnKeyDown := EditorKeyDown;

    { Elements to handle KeyPress-Events }
    fInKeyPress := false;
    fKeyPressChain := TList.Create;
    fOldKeyPress := fControl.OnKeyPress;
    fControl.OnKeyPress := EditorKeyPress;
  end;

destructor TSynEditKbdHandler.Destroy;
  begin
    { Reset the control which created the handler chain }
    fControl.OnKeyDown  := fOldKeyDown;
    fControl.OnKeyPress := fOldKeyPress;

    fKeyPressChain.Free;
    fKeyDownChain.Free;

    inherited Destroy;
  end;

procedure TSynEditKbdHandler.SetOnKeyDown (const Value: TKeyEvent);
  begin
    fOldKeyDown := Value;
    fControl.OnKeyDown := EditorKeyDown;
  end;

procedure TSynEditKbdHandler.SetOnKeyPress(const Value: TKeyPressEvent);
  begin
    fOldKeyPress := Value;
    fControl.OnKeyPress := EditorKeyPress;
  end;

procedure TSynEditKbdHandler.EditorKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
  var
    idx : Integer;
  begin
    if fInKeyDown then
      exit;
    fInKeyDown := true;
    try
      with fKeyDownChain do begin
        for idx := Count - 1 downto 0 do begin
          with TKeyDownProc (Items[idx]) do
            if Assigned (OnKeyDown) then begin
              OnKeyDown (Sender,Key,Shift);
              if (Key = 0) then begin
                fInKeyDown := false;
                exit;
              end;
           end;
        end;
      end;
      if Assigned (fOldKeyDown) then
        fOldKeyDown (Sender,Key,Shift);
    finally
      fInKeyDown := false;
    end;
  end;

procedure TSynEditKbdHandler.EditorKeyPress (Sender: TObject; var Key: Char);
  var
    idx : Integer;
  begin
    if fInKeyPress then
      exit;
    fInKeyPress := true;
    try
      with fKeyPressChain do begin
        for idx := Count - 1 downto 0 do begin
          with TKeyPressProc (Items[idx]) do
            if Assigned (OnKeyPress) then begin
              OnKeyPress (Sender,Key);
              if (Key = #0) then begin
                fInKeyPress := false;
                exit;
              end;
            end;
        end;
      end;
      if Assigned (fOldKeyPress) then
        fOldKeyPress (Sender,Key);
    finally
      fInKeyPress := false;
    end;
  end;

procedure TSynEditKbdHandler.ExecuteKeyPress (Sender: TObject; var Key: Char);
  begin
    EditorKeyPress (Sender,Key);
  end;

procedure TSynEditKbdHandler.ExecuteKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    EditorKeyDown (Sender,Key,Shift);
  end;

procedure TSynEditKbdHandler.AddKeyDownHandler(aHandler: TKeyDownProc);
begin
  fKeyDownChain.Add(aHandler);
end;

procedure TSynEditKbdHandler.RemoveKeyDownHandler(aHandler: TKeyDownProc);
begin
  fKeyDownChain.Remove(aHandler);
end;

procedure TSynEditKbdHandler.AddKeyPressHandler(aHandler: TKeyPressProc);
begin
  fKeyPressChain.Add(aHandler);
end;

procedure TSynEditKbdHandler.RemoveKeyPressHandler(aHandler: TKeyPressProc);
begin
  fKeyPressChain.Remove(aHandler);
end;

end.
