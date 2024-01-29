unit VirtualTrees.ClipBoard;

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

uses
  Winapi.Windows,
  Winapi.ActiveX,
  System.Classes,
  VirtualTrees.BaseTree;

type
  TClipboardFormatEntry = record
    ID: Word;
    Description: string;
  end;

var
  ClipboardDescriptions: array [1..CF_MAX - 1] of TClipboardFormatEntry = (
    (ID: CF_TEXT; Description: 'Plain text'), // Do not localize
    (ID: CF_BITMAP; Description: 'Windows bitmap'), // Do not localize
    (ID: CF_METAFILEPICT; Description: 'Windows metafile'), // Do not localize
    (ID: CF_SYLK; Description: 'Symbolic link'), // Do not localize
    (ID: CF_DIF; Description: 'Data interchange format'), // Do not localize
    (ID: CF_TIFF; Description: 'Tiff image'), // Do not localize
    (ID: CF_OEMTEXT; Description: 'OEM text'), // Do not localize
    (ID: CF_DIB; Description: 'DIB image'), // Do not localize
    (ID: CF_PALETTE; Description: 'Palette data'), // Do not localize
    (ID: CF_PENDATA; Description: 'Pen data'), // Do not localize
    (ID: CF_RIFF; Description: 'Riff audio data'), // Do not localize
    (ID: CF_WAVE; Description: 'Wav audio data'), // Do not localize
    (ID: CF_UNICODETEXT; Description: 'Unicode text'), // Do not localize
    (ID: CF_ENHMETAFILE; Description: 'Enhanced metafile image'), // Do not localize
    (ID: CF_HDROP; Description: 'File name(s)'), // Do not localize
    (ID: CF_LOCALE; Description: 'Locale descriptor'), // Do not localize
    (ID: CF_DIBV5; Description: 'DIB image V5') // Do not localize
  );


// OLE Clipboard and drag'n drop helper
procedure EnumerateVTClipboardFormats(TreeClass: TVirtualTreeClass; const List: TStrings); overload;
procedure EnumerateVTClipboardFormats(TreeClass: TVirtualTreeClass; var Formats: TFormatEtcArray); overload;
function GetVTClipboardFormatDescription(AFormat: Word): string;
procedure RegisterVTClipboardFormat(AFormat: Word; TreeClass: TVirtualTreeClass; Priority: Cardinal); overload;
function RegisterVTClipboardFormat(const Description: string; TreeClass: TVirtualTreeClass; Priority: Cardinal;
                                   tymed: Integer = TYMED_HGLOBAL; ptd: PDVTargetDevice = nil;
                                   dwAspect: Integer = DVASPECT_CONTENT; lindex: Integer = -1): Word; overload;

//----------------- TClipboardFormats ----------------------------------------------------------------------------------

type
  TClipboardFormatListEntry = class
  public
    Description: string;               // The string used to register the format with Winapi.Windows.
    TreeClass: TVirtualTreeClass;      // The tree class which supports rendering this format.
    Priority: Cardinal;                // Number which determines the order of formats used in IDataObject.
    FormatEtc: TFormatEtc;             // The definition of the format in the IDataObject.
  end;

  TClipboardFormatList = class
  strict private
    class function GetList(): TList; static;
    class property List: TList read GetList;
  protected
   class procedure Sort;
  public
    class procedure Add(const FormatString: string; AClass: TVirtualTreeClass; Priority: Cardinal; AFormatEtc: TFormatEtc);
    class procedure Clear;
    class procedure EnumerateFormats(TreeClass: TVirtualTreeClass; var Formats: TFormatEtcArray;  const AllowedFormats: TClipboardFormats = nil); overload;
    class procedure EnumerateFormats(TreeClass: TVirtualTreeClass; const Formats: TStrings); overload;
    class function FindFormat(const FormatString: string): TClipboardFormatListEntry; overload;
    class function FindFormat(const FormatString: string; var Fmt: Word): TVirtualTreeClass; overload;
    class function FindFormat(Fmt: Word; var Description: string): TVirtualTreeClass; overload;
  end;

var
  // Clipboard format IDs used in OLE drag'n drop and clipboard transfers.
  CF_VIRTUALTREE,
  CF_VTREFERENCE,
  CF_VRTF,
  CF_VRTFNOOBJS,   // Unfortunately CF_RTF* is already defined as being
                   // registration strings so I have to use different identifiers.
  CF_HTML,
  CF_CSV: Word;


implementation

uses
  System.SysUtils;

var
  _List: TList = nil;  //Note - not using class constructors as they are not supported on C++ Builder. See also issue #

procedure EnumerateVTClipboardFormats(TreeClass: TVirtualTreeClass; const List: TStrings);

begin
  TClipboardFormatList.EnumerateFormats(TreeClass, List);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure EnumerateVTClipboardFormats(TreeClass: TVirtualTreeClass; var Formats: TFormatEtcArray);

begin
  TClipboardFormatList.EnumerateFormats(TreeClass, Formats);
end;

//----------------------------------------------------------------------------------------------------------------------

function GetVTClipboardFormatDescription(AFormat: Word): string;

begin
  if TClipboardFormatList.FindFormat(AFormat, Result) = nil then
    Result := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure RegisterVTClipboardFormat(AFormat: Word; TreeClass: TVirtualTreeClass; Priority: Cardinal);

// Registers the given clipboard format for the given TreeClass.

var
  I: Integer;
  Buffer: array[0..2048] of Char;
  FormatEtc: TFormatEtc;

begin

  // Assumes a HGlobal format.
  FormatEtc.cfFormat := AFormat;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := -1;
  FormatEtc.tymed := TYMED_HGLOBAL;

  // Determine description string of the given format. For predefined formats we need the lookup table because they
  // don't have a description string. For registered formats the description string is the string which was used
  // to register them.
  if AFormat < CF_MAX then
  begin
    for I := 1 to High(ClipboardDescriptions) do
      if ClipboardDescriptions[I].ID = AFormat then
      begin
        TClipboardFormatList.Add(ClipboardDescriptions[I].Description, TreeClass, Priority, FormatEtc);
        Break;
      end;
  end
  else
  begin
    GetClipboardFormatName(AFormat, Buffer, Length(Buffer));
    TClipboardFormatList.Add(Buffer, TreeClass, Priority, FormatEtc);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function RegisterVTClipboardFormat(const Description: string; TreeClass: TVirtualTreeClass; Priority: Cardinal;
  tymed: Integer = TYMED_HGLOBAL; ptd: PDVTargetDevice = nil; dwAspect: Integer = DVASPECT_CONTENT;
  lindex: Integer = -1): Word;

// Alternative method to register a certain clipboard format for a given tree class. Registration with the
// clipboard is done here too and the assigned ID returned by the function.
// tymed may contain or'ed TYMED constants which allows to register several storage formats for one clipboard format.

var
  FormatEtc: TFormatEtc;

begin
  Result := RegisterClipboardFormat(PChar(Description));
  FormatEtc.cfFormat := Result;
  FormatEtc.ptd := ptd;
  FormatEtc.dwAspect := dwAspect;
  FormatEtc.lindex := lindex;
  FormatEtc.tymed := tymed;
  TClipboardFormatList.Add(Description, TreeClass, Priority, FormatEtc);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TClipboardFormatList.Sort;

// Sorts all entry for priority (increasing priority value).

  //--------------- local function --------------------------------------------
  procedure QuickSort(L, R: Integer);

  var
    I, J: Integer;
    P, T: TClipboardFormatListEntry;

  begin
    repeat
      I := L;
      J := R;
      P := _List[(L + R) shr 1];
      repeat
        while TClipboardFormatListEntry(_List[I]).Priority < P.Priority do
          Inc(I);
        while TClipboardFormatListEntry(_List[J]).Priority > P.Priority do
          Dec(J);
        if I <= J then
        begin
          T := List[I];
          _List[I] := _List[J];
          _List[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;
  //--------------- end local function ----------------------------------------

begin
  if List.Count > 1 then
    QuickSort(0, List.Count - 1);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TClipboardFormatList.Add(const FormatString: string; AClass: TVirtualTreeClass; Priority: Cardinal; AFormatEtc: TFormatEtc);

// Adds the given data to the internal list. The priority value is used to sort formats for importance. Larger priority
// values mean less priority.

var
  Entry: TClipboardFormatListEntry;

begin
  Entry := TClipboardFormatListEntry.Create;
  Entry.Description := FormatString;
  Entry.TreeClass := AClass;
  Entry.Priority := Priority;
  Entry.FormatEtc := AFormatEtc;
  List.Add(Entry);

  Sort;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TClipboardFormatList.Clear;

var
  I: Integer;

begin
  if Assigned(_List) then begin
    for I := 0 to _List.Count - 1 do
      TClipboardFormatListEntry(List[I]).Free;
    _List.Clear;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TClipboardFormatList.EnumerateFormats(TreeClass: TVirtualTreeClass; var Formats: TFormatEtcArray; const AllowedFormats: TClipboardFormats = nil);

// Returns a list of format records for the given class. If assigned the AllowedFormats is used to limit the
// enumerated formats to those described in the list.

var
  I, Count: Integer;
  Entry: TClipboardFormatListEntry;

begin
  SetLength(Formats, List.Count);
  Count := 0;
  for I := 0 to List.Count - 1 do
  begin
    Entry := List[I];
    // Does the tree class support this clipboard format?
    if TreeClass.InheritsFrom(Entry.TreeClass) then
    begin
      // Is this format allowed to be included?
      if (AllowedFormats = nil) or (AllowedFormats.IndexOf(Entry.Description) > -1) then
      begin
        // The list could change before we use the FormatEtc so it is best not to pass a pointer to the true FormatEtc
        // structure. Instead make a copy and send that.
        Formats[Count] := Entry.FormatEtc;
        Inc(Count);
      end;
    end;
  end;
  SetLength(Formats, Count);
end;

//----------------------------------------------------------------------------------------------------------------------

class procedure TClipboardFormatList.EnumerateFormats(TreeClass: TVirtualTreeClass; const Formats: TStrings);

// Returns a list of format descriptions for the given class.

var
  I: Integer;
  Entry: TClipboardFormatListEntry;

begin
  for I := 0 to List.Count - 1 do
  begin
    Entry := List[I];
    if TreeClass.InheritsFrom(Entry.TreeClass) then
      Formats.Add(Entry.Description);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TClipboardFormatList.FindFormat(const FormatString: string): TClipboardFormatListEntry;

var
  I: Integer;
  Entry: TClipboardFormatListEntry;

begin
  Result := nil;
  for I := List.Count - 1 downto 0 do
  begin
    Entry := List[I];
    if CompareText(Entry.Description, FormatString) = 0 then
    begin
      Result := Entry;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TClipboardFormatList.FindFormat(const FormatString: string; var Fmt: Word): TVirtualTreeClass;

var
  I: Integer;
  Entry: TClipboardFormatListEntry;

begin
  Result := nil;
  for I := List.Count - 1 downto 0 do
  begin
    Entry := List[I];
    if CompareText(Entry.Description, FormatString) = 0 then
    begin
      Result := Entry.TreeClass;
      Fmt := Entry.FormatEtc.cfFormat;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

class function TClipboardFormatList.FindFormat(Fmt: Word; var Description: string): TVirtualTreeClass;

var
  I: Integer;
  Entry: TClipboardFormatListEntry;

begin
  Result := nil;
  for I := List.Count - 1 downto 0 do
  begin
    Entry := List[I];
    if Entry.FormatEtc.cfFormat = Fmt then
    begin
      Result := Entry.TreeClass;
      Description := Entry.Description;
      Break;
    end;
  end;
end;


class function TClipboardFormatList.GetList: TList;
begin
  if not Assigned(_List) then
    _List :=  TList.Create;
  Exit(_List);
end;

initialization

finalization

  TClipboardFormatList.Clear;
  FreeAndNil(_List);

end.

