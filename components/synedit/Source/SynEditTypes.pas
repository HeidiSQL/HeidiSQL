{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTypes.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
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

$Id: SynEditTypes.pas,v 1.13.2.2 2012/09/17 14:17:25 CodehunterWorks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITTYPES}
unit SynEditTypes;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
  SysUtils;

const
// These might need to be localized depending on the characterset because they might be
// interpreted as valid ident characters.
  SynTabGlyph = WideChar($2192);       //'->'
  SynSoftBreakGlyph = WideChar($00AC); //'¬'
  SynLineBreakGlyph = WideChar($00B6); //'¶'
  SynSpaceGlyph = WideChar($2219);     //'·'

type
  ESynError = class(Exception);

  TSynSearchOption = (ssoMatchCase, ssoWholeWord, ssoBackwards,
    ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt);
  TSynSearchOptions = set of TSynSearchOption;

  TCategoryMethod = function(AChar: WideChar): Boolean of object;

  TKeyPressWEvent = procedure(Sender: TObject; var Key: WideChar) of object;

  PSynSelectionMode = ^TSynSelectionMode;
  TSynSelectionMode = (smNormal, smLine, smColumn);

  PBorlandSelectionMode = ^TBorlandSelectionMode;
  TBorlandSelectionMode = (
    bsmInclusive, // selects inclusive blocks. Borland IDE shortcut: Ctrl+O+I
    bsmLine,      // selects line blocks. Borland IDE shortcut: Ctrl+O+L
    bsmColumn,    // selects column blocks. Borland IDE shortcut: Ctrl+O+C
    bsmNormal     // selects normal Block. Borland IDE shortcut: Ctrl+O+K
  );

  //todo: better field names. CharIndex and LineIndex?
  TBufferCoord = record
    Char: Integer;
    Line: Integer;
    {$IFDEF SYN_COMPILER_10_UP}
    class operator Equal(a, b: TBufferCoord): Boolean;
    {$ENDIF}
  end;

  // Codehunter patch: added TBufferBlock
  TBufferBlock = record
    BeginLine, BeginChar: Integer;
    EndLine, EndChar: Integer;
    {$IFDEF SYN_COMPILER_10_UP}
    class operator Equal(a, b: TBufferBlock): Boolean;
    {$ENDIF}
  end;

  TDisplayCoord = record
    Column: Integer;
    Row: Integer;
    {$IFDEF SYN_COMPILER_10_UP}
    class operator Equal(a, b: TDisplayCoord): Boolean;
    {$ENDIF}
  end;

function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
function BufferCoord(AChar, ALine: Integer): TBufferCoord;

implementation

function DisplayCoord(AColumn, ARow: Integer): TDisplayCoord;
begin
  Result.Column := AColumn;
  Result.Row := ARow;
end;

function BufferCoord(AChar, ALine: Integer): TBufferCoord;
begin
  Result.Char := AChar;
  Result.Line := ALine;
end;

{$IFDEF SYN_COMPILER_10_UP}

{ TBufferCoord }

class operator TBufferCoord.Equal(a, b: TBufferCoord): Boolean;
begin
  Result := (a.Char = b.Char) and (a.Line = b.Line);
end;

{ TBufferBlock }

class operator TBufferBlock.Equal(a, b: TBufferBlock): Boolean;
begin
  Result := (a.BeginLine = b.BeginLine) and (a.BeginChar = b.BeginChar) and
    (a.EndLine = b.EndLine) and (a.EndChar = b.EndChar);
end;

{ TDisplayCoord }

class operator TDisplayCoord.Equal(a, b: TDisplayCoord): Boolean;
begin
  Result := (a.Row = b.Row) and (a.Column = b.Column);
end;

{$ENDIF}

end.
