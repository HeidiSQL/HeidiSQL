{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynExportHTML.pas, released 2000-04-16.

The Original Code is partly based on the mwHTMLExport.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Portions created by James D. Jacobson are Copyright 1999 Martin Waldenburg.
Changes to emit XHTML 1.0 Strict complying code by Maël Hörz.
Unicode translation by Maël Hörz.
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

$Id: SynExportHTML.pas,v 1.19.2.6 2006/05/21 11:59:34 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEXPORTHTML}
unit SynExportHTML;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QGraphics,
  QSynEditExport,
  QSynEditHighlighter,
  QSynUnicode,  
{$ELSE}
  Windows,
  Graphics,
  SynEditExport,
  SynEditHighlighter,
  SynUnicode,    
{$ENDIF}
  Classes;

type
  TSynExporterHTML = class(TSynCustomExporter)
  private
    function AttriToCSS(Attri: TSynHighlighterAttributes;
      UniqueAttriName: string): string;
    function AttriToCSSCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
    function ColorToHTML(AColor: TColor): string;
    function GetStyleName(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes): string;
    function MakeValidName(Name: string): string;
    function StyleNameCallback(Highlighter: TSynCustomHighlighter;
      Attri: TSynHighlighterAttributes; UniqueAttriName: string;
      Params: array of Pointer): Boolean;
  protected
    fCreateHTMLFragment: boolean;
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatBeforeFirstAttribute(BackgroundChanged,
      ForegroundChanged: boolean; FontStylesChanged: TFontStyles); override;
    procedure FormatNewLine; override;
    function GetFooter: WideString; override;
    function GetFormatName: string; override;
    function GetHeader: WideString; override;
    function ReplaceReservedChar(AChar: WideChar): WideString; override;
    function UseBom: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    function SupportedEncodings: TSynEncodings; override;
  published
    property Color;
    property CreateHTMLFragment: boolean read fCreateHTMLFragment
      write fCreateHTMLFragment default False;
    property DefaultFilter;
    property Encoding;
    property Font;
    property Highlighter;
    property Title;
    property UseBackground;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditMiscProcs,
  QSynEditStrConst,
  QSynHighlighterMulti,
{$ELSE}
  SynEditMiscProcs,
  SynEditStrConst,  
  SynHighlighterMulti,
{$ENDIF}
  SysUtils;


{ TSynExporterHTML }

constructor TSynExporterHTML.Create(AOwner: TComponent);
const
  CF_HTML = 'HTML Format';
begin
  inherited Create(AOwner);
  {$IFNDEF SYN_CLX}
  fClipboardFormat := RegisterClipboardFormat(CF_HTML);
  {$ENDIF} // TODO: register for Kylix, too, see what Netscape Composer uses/accepts
  fDefaultFilter := SYNS_FilterHTML;
  FEncoding := seUTF8;
end;

function TSynExporterHTML.AttriToCSS(Attri: TSynHighlighterAttributes;
  UniqueAttriName: string): string;
var
  StyleName: string;
begin
  StyleName := MakeValidName(UniqueAttriName);

  Result := '.' + StyleName + ' { ';
  if UseBackground and (Attri.Background <> clNone) then
    Result := Result + 'background-color: ' + ColorToHTML(Attri.Background) + '; ';
  if Attri.Foreground <> clNone then
    Result := Result + 'color: ' + ColorToHTML(Attri.Foreground) + '; ';

  if fsBold in Attri.Style then
    Result := Result + 'font-weight: bold; ';
  if fsItalic in Attri.Style then
    Result := Result + 'font-style: italic; ';
  if fsUnderline in Attri.Style then
    Result := Result + 'text-decoration: underline; ';
  if fsStrikeOut in Attri.Style then
    Result := Result + 'text-decoration: line-through; ';

  Result := Result + '}';
end;

function TSynExporterHTML.AttriToCSSCallback(Highlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes; UniqueAttriName: string;
  Params: array of Pointer): Boolean;
var
  Styles: ^string;
begin
  Styles := Params[0];
  Styles^ := Styles^ + AttriToCSS(Attri, UniqueAttriName) + #13#10;  
  Result := True; // we want all attributes => tell EnumHighlighterAttris to continue
end;

function TSynExporterHTML.ColorToHTML(AColor: TColor): string;
var
  RGBColor: longint;
  RGBValue: byte;
const
  Digits: array[0..15] of AnsiChar = '0123456789ABCDEF';
begin
  RGBColor := ColorToRGB(AColor);
  Result := '#000000';
  RGBValue := GetRValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[2] := Digits[RGBValue shr  4];
    Result[3] := Digits[RGBValue and 15];
  end;
  RGBValue := GetGValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[4] := Digits[RGBValue shr  4];
    Result[5] := Digits[RGBValue and 15];
  end;
  RGBValue := GetBValue(RGBColor);
  if RGBValue > 0 then
  begin
    Result[6] := Digits[RGBValue shr  4];
    Result[7] := Digits[RGBValue and 15];
  end;
end;

procedure TSynExporterHTML.FormatAfterLastAttribute;
begin
  AddData('</span>');
end;

procedure TSynExporterHTML.FormatAttributeDone(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
begin
  AddData('</span>');
end;

procedure TSynExporterHTML.FormatAttributeInit(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
var
  StyleName: string;
begin
  StyleName := GetStyleName(Highlighter, Highlighter.GetTokenAttribute);
  AddData(Format('<span class="%s">', [StyleName]));
end;

procedure TSynExporterHTML.FormatBeforeFirstAttribute(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
var
  StyleName: string;
begin
  StyleName := GetStyleName(Highlighter, Highlighter.GetTokenAttribute);
  AddData(Format('<span class="%s">', [StyleName]));
end;

procedure TSynExporterHTML.FormatNewLine;
begin
  AddNewLine;
end;

function TSynExporterHTML.GetFooter: WideString;
begin
  Result := '';
  if fExportAsText then
    Result := '</span>'#13#10'</code></pre>'#13#10
  else
    Result := '</code></pre><!--EndFragment-->';
  if not(fCreateHTMLFragment and fExportAsText) then
    Result := Result + '</body>'#13#10'</html>';
end;

function TSynExporterHTML.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatHTML;
end;

function TSynExporterHTML.GetHeader: WideString;
const
  DescriptionSize = 105;
  FooterSize1 = 47;
  FooterSize2 = 31;
  NativeHeader = 'Version:0.9'#13#10 +
                 'StartHTML:%.10d'#13#10 +
                 'EndHTML:%.10d'#13#10 +
                 'StartFragment:%.10d'#13#10 +
                 'EndFragment:%.10d'#13#10;
  HTMLAsTextHeader = '<?xml version="1.0" encoding="%s"?>'#13#10 +
                     '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'#13#10 +
                     '<html xmlns="http://www.w3.org/1999/xhtml">'#13#10 +
                     '<head>'#13#10;
  HTMLAsTextHeader2 ='<meta http-equiv="Content-Type" content="text/html; charset=%s" />'#13#10 +
                     '<meta name="generator" content="SynEdit HTML exporter" />'#13#10 +
                     '<style type="text/css">'#13#10 +
                     '<!--'#13#10 +
                     'body { color: %s; background-color: %s; }'#13#10 +
                     '%s' +
                     '-->'#13#10 +
                     '</style>'#13#10 +
                     '</head>'#13#10 +
                     '<body>'#13#10;
  EncodingStrs: array[TSynEncoding] of string =
    ('UTF-8', 'UTF-16', 'UTF-16', 'ANSI is Unsupported');
var
  EncodingStr, Styles, Header, Header2: string;
begin
  EncodingStr := EncodingStrs[Encoding];
  EnumHighlighterAttris(Highlighter, True, AttriToCSSCallback, [@Styles]);

  Header := Format(HTMLAsTextHeader, [EncodingStr]);
  Header := Header + '<title>' + Title + '</title>'#13#10 +
    Format(HTMLAsTextHeader2, [EncodingStr, ColorToHtml(fFont.Color),
      ColorToHTML(fBackgroundColor), Styles]);

  Result := '';
  if fExportAsText then
  begin
    if not fCreateHTMLFragment then
      Result := Header;

    Result := Result + Format('<pre>'#13#10'<code><span style="font: %dpt %s;">',
      [fFont.Size, fFont.Name]);
  end
  else
  begin
    // Described in http://msdn.microsoft.com/library/sdkdoc/htmlclip/htmlclipboard.htm
    Header2 := '<!--StartFragment--><pre><code>';
    Result := Format(NativeHeader, [DescriptionSize,
      DescriptionSize + Length(Header) + Length(Header2) + GetBufferSize + FooterSize1,
      DescriptionSize + Length(Header),
      DescriptionSize + Length(Header) + Length(Header2) + GetBufferSize + FooterSize2]);
    Result := Result + Header + Header2;
  end;
end;

function TSynExporterHTML.GetStyleName(Highlighter: TSynCustomHighlighter;
  Attri: TSynHighlighterAttributes): string;
begin
  EnumHighlighterAttris(Highlighter, False, StyleNameCallback, [Attri, @Result]);
end;

function TSynExporterHTML.MakeValidName(Name: string): string;
var
  i: Integer;
begin
  Result := LowerCase(Name);
  for i := Length(Result) downto 1 do
    if Result[i] in ['.', '_'] then
      Result[i] := '-'
    else if not(Result[i] in ['a'..'z', '0'..'9', '-']) then
      Delete(Result, i, 1);
end;

function TSynExporterHTML.ReplaceReservedChar(AChar: WideChar): WideString;
begin
  case AChar of
    '&': Result := '&amp;';
    '<': Result := '&lt;';
    '>': Result := '&gt;';
    '"': Result := '&quot;';
    else Result := '';
  end
end;

function TSynExporterHTML.StyleNameCallback(Highlighter: TSynCustomHighlighter;
    Attri: TSynHighlighterAttributes; UniqueAttriName: string;
    Params: array of Pointer): Boolean;
var
  AttriToFind: TSynHighlighterAttributes;
  StyleName: ^string;
begin
  AttriToFind := Params[0];
  StyleName := Params[1];

  if Attri = AttriToFind then
  begin
    StyleName^ := MakeValidName(UniqueAttriName);
    Result := False; // found => inform EnumHighlighterAttris to stop searching
  end
  else
    Result := True;
end;

function TSynExporterHTML.UseBom: Boolean;
begin
  // do not include seUTF8 as some browsers have problems with UTF-8-BOM
  Result := Encoding in [seUTF16LE, seUTF16BE];
end;

function TSynExporterHTML.SupportedEncodings: TSynEncodings;
begin
  Result := [seUTF8, seUTF16LE, seUTF16BE];
end;

end.
