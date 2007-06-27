{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: pas2html.dpr, released 2000-06-23.

The Original Code is the pas2html project of the mwEdit component suite
by Martin Waldenburg and other developers.
The Original Author of the pas2html project is Primoz Gabrijelcic.
Portions written by Primoz Gabrijelcic are copyright 1999 Primoz Gabrijelcic.
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

$Id: pas2html.dpr,v 1.2 2000/11/22 08:37:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$APPTYPE CONSOLE}

program pas2html;

{$I SynEdit.inc}

uses
  Windows, Classes, Clipbrd, SynHighlighterPas, SynExportHTML;

var
  ALines: TStringList;
  Syn: TSynPasSyn;
  Exp: TSynExporterHTML;
begin
  if Clipboard.HasFormat(CF_TEXT) then begin
    ALines := TStringList.Create;
    try
      Syn := TSynPasSyn.Create(nil);
      try
        // get syntax highlighter settings
        Syn.EnumUserSettings(ALines);
        if ALines.Count > 0 then
          Syn.UseUserSettings(ALines.Count - 1);
        // load text from clipboard
        ALines.Text := Clipboard.AsText;  
        // export ALines to HTML, as HTML fragment in text format
        Exp := TSynExporterHTML.Create(nil);
        try
          Exp.Highlighter := Syn;
          Exp.ExportAsText := TRUE;
          Exp.CreateHTMLFragment := TRUE;
          Exp.ExportAll(ALines);
          Exp.CopyToClipboard;
        finally
          Exp.Free;
        end;
      finally
        Syn.Free;
      end;
    finally
      ALines.Free;
    end;
  end;
end.

