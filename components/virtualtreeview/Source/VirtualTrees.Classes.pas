unit VirtualTrees.Classes;

// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// The original code is VirtualTrees.pas, released September 30, 2000.
//
// The initial developer of the original code is digital publishing AG (Munich, Germany, www.digitalpublishing.de),
// written by Mike Lischke (public@soft-gems.net, www.soft-gems.net).
//
// Portions created by digital publishing AG are Copyright
// (C) 1999-2001 digital publishing AG. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------

interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

uses
  Winapi.Windows;

type
  // Helper classes to speed up rendering text formats for clipboard and drag'n drop transfers.
  TBufferedRawByteString = class
  private
    FStart,
    FPosition,
    FEnd: PAnsiChar;
    function GetAsString: RawByteString;
  public
    destructor Destroy; override;

    procedure Add(const S: RawByteString);
    procedure AddNewLine;

    property AsString: RawByteString read GetAsString;
  end;

  TBufferedString = class
  private
    FStart,
    FPosition,
    FEnd: PWideChar;
    function GetAsString: string;
  public
    destructor Destroy; override;

    procedure Add(const S: string);
    procedure AddNewLine;

    property AsString: string read GetAsString;
  end;


implementation

//----------------- TBufferedRawByteString ------------------------------------------------------------------------------------

const
  AllocIncrement = 2 shl 11;  // Must be a power of 2.

destructor TBufferedRawByteString.Destroy;

begin
  FreeMem(FStart);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferedRawByteString.GetAsString: RawByteString;

begin
  SetString(Result, FStart, FPosition - FStart);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBufferedRawByteString.Add(const S: RawByteString);

var
  NewLen,
  LastOffset,
  Len: NativeInt;

begin
  Len := Length(S);
  // Make room for the new string.
  if FEnd - FPosition <= Len then
  begin
    // Round up NewLen so it is always a multiple of AllocIncrement.
    NewLen := FEnd - FStart + (Len + AllocIncrement - 1) and not (AllocIncrement - 1);
    // Keep last offset to restore it correctly in the case that FStart gets a new memory block assigned.
    LastOffset := FPosition - FStart;
    ReallocMem(FStart, NewLen);
    FPosition := FStart + LastOffset;
    FEnd := FStart + NewLen;
  end;
  System.Move(PAnsiChar(S)^, FPosition^, Len);
  System.Inc(FPosition, Len);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBufferedRawByteString.AddNewLine;

var
  NewLen,
  LastOffset: NativeInt;

begin
  // Make room for the CR/LF characters.
  if FEnd - FPosition <= 2 then
  begin
    // Round up NewLen so it is always a multiple of AllocIncrement.
    NewLen := FEnd - FStart + (2 + AllocIncrement - 1) and not (AllocIncrement - 1);
    // Keep last offset to restore it correctly in the case that FStart gets a new memory block assigned.
    LastOffset := FPosition - FStart;
    ReallocMem(FStart, NewLen);
    FPosition := FStart + LastOffset;
    FEnd := FStart + NewLen;
  end;
  FPosition^ := #13;
  System.Inc(FPosition);
  FPosition^ := #10;
  System.Inc(FPosition);
end;

//----------------- TBufferedString --------------------------------------------------------------------------------

destructor TBufferedString.Destroy;

begin
  FreeMem(FStart);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBufferedString.GetAsString: string;

begin
  SetString(Result, FStart, FPosition - FStart);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBufferedString.Add(const S: string);

var
  NewLen,
  LastOffset,
  Len: Integer;

begin
  Len := Length(S);
  if Len = 0 then
    exit;//Nothing to do
  // Make room for the new string.
  if FEnd - FPosition <= Len then
  begin
    // Round up NewLen so it is always a multiple of AllocIncrement.
    NewLen := FEnd - FStart + (Len + AllocIncrement - 1) and not (AllocIncrement - 1);
    // Keep last offset to restore it correctly in the case that FStart gets a new memory block assigned.
    LastOffset := FPosition - FStart;
    ReallocMem(FStart, 2 * NewLen);
    FPosition := FStart + LastOffset;
    FEnd := FStart + NewLen;
  end;
  System.Move(PWideChar(S)^, FPosition^, 2 * Len);
  System.Inc(FPosition, Len);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBufferedString.AddNewLine;

var
  NewLen,
  LastOffset: Integer;

begin
  // Make room for the CR/LF characters.
  if FEnd - FPosition <= 4 then
  begin
    // Round up NewLen so it is always a multiple of AllocIncrement.
    NewLen := FEnd - FStart + (2 + AllocIncrement - 1) and not (AllocIncrement - 1);
    // Keep last offset to restore it correctly in the case that FStart gets a new memory block assigned.
    LastOffset := FPosition - FStart;
    ReallocMem(FStart, 2 * NewLen);
    FPosition := FStart + LastOffset;
    FEnd := FStart + NewLen;
  end;
  FPosition^ := #13;
  System.Inc(FPosition);
  FPosition^ := #10;
  System.Inc(FPosition);
end;


end.
