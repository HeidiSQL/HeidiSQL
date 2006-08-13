{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uHighlighterProcs.pas, released 2000-06-23.

The Initial Author of the Original Code is Michael Hieke.
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

$Id: uHighlighterProcs.pas,v 1.3 2002/06/15 06:57:24 etrusco Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit uHighlighterProcs;

interface

uses
  Classes, SynEditHighlighter;

procedure GetHighlighters(AOwner: TComponent; AHighlighters: TStringList;
  AppendToList: boolean);
function GetHighlightersFilter(AHighlighters: TStringList): string;
function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;

implementation

uses
  SysUtils;
  
procedure GetHighlighters(AOwner: TComponent; AHighlighters: TStringList;
  AppendToList: boolean);
var
  i: integer;
  Highlighter: TSynCustomHighlighter;
begin
  if Assigned(AOwner) and Assigned(AHighlighters) then begin
    if not AppendToList then
      AHighlighters.Clear;
    for i := AOwner.ComponentCount - 1 downto 0 do begin
      if not (AOwner.Components[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := AOwner.Components[i] as TSynCustomHighlighter;
      // only one highlighter for each language
      if AHighlighters.IndexOf(Highlighter.GetLanguageName) = -1 then
        AHighlighters.AddObject(Highlighter.GetLanguageName, Highlighter);
    end;
    AHighlighters.Sort;
  end;
end;

function GetHighlightersFilter(AHighlighters: TStringList): string;
var
  i: integer;
  Highlighter: TSynCustomHighlighter;
begin
  Result := '';
  if Assigned(AHighlighters) then
    for i := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[i]);
      if Highlighter.DefaultFilter = '' then
        continue;
      Result := Result + Highlighter.DefaultFilter;
      if Result[Length(Result)] <> '|' then
        Result := Result + '|';
    end;
end;

function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;
var
  ExtLen: integer;
  i, j: integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
begin
  Extension := LowerCase(Extension);
  ExtLen := Length(Extension);
  if Assigned(AHighlighters) and (ExtLen > 0) then begin
    for i := 0 to AHighlighters.Count - 1 do begin
      if not (AHighlighters.Objects[i] is TSynCustomHighlighter) then
        continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[i]);
      Filter := LowerCase(Highlighter.DefaultFilter);
      j := Pos('|', Filter);
      if j > 0 then begin
        Delete(Filter, 1, j);
        j := Pos(Extension, Filter);
        if (j > 0) and
           ((j + ExtLen > Length(Filter)) or (Filter[j + ExtLen] = ';'))
        then begin
          Result := Highlighter;
          exit;
        end;
      end;
    end;
  end;
  Result := nil;
end;

end.
