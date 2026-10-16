{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Datetime format is dd.MM.yyyy hh:mm:ss.

The Original Code is: SynEditHighlighterOptions.pas, released 12.09.2012.

All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

$Id: SynEditHighlighterOptions.pas,v 1.0.2 25.10.2012 11:16:19 CodehunterWorks Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Last Changes:
  21.09.2012 08:37:10 - Moved from String to WideString
  25.10.2012 11:16:19 - Added DefaultExtension property

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditHighlighterOptions;

interface

uses
  Classes;

type
  TSynEditHighlighterOptions = class(TPersistent)
  private
    FAutoDetectEnabled: Boolean;
    FAutoDetectLineLimit: Cardinal;
    FAutoDetectMatchExpression: WideString;
    FDefaultExtension: WideString;
    FLineCommentarEnd: WideString;
    FLineCommentarStart: WideString;
    FTitle: WideString;
    FVisible: Boolean;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property AutoDetectEnabled: Boolean read FAutoDetectEnabled write FAutoDetectEnabled;
    property AutoDetectLineLimit: Cardinal read FAutoDetectLineLimit write FAutoDetectLineLimit;
    property AutoDetectMatchExpression: WideString read FAutoDetectMatchExpression write FAutoDetectMatchExpression;
    property DefaultExtension: WideString read FDefaultExtension write FDefaultExtension;
    property LineCommentarEnd: WideString read FLineCommentarEnd write FLineCommentarEnd;
    property LineCommentarStart: WideString read FLineCommentarStart write FLineCommentarStart;
    property Title: WideString read FTitle write FTitle;
    property Visible: Boolean read FVisible write FVisible;
  end;

implementation

procedure TSynEditHighlighterOptions.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TSynEditHighlighterOptions) then begin
    with TSynEditHighlighterOptions(Source) do begin
      FAutoDetectEnabled := AutoDetectEnabled;
      FAutoDetectMatchExpression := AutoDetectMatchExpression;
      FAutoDetectLineLimit := AutoDetectLineLimit;
      FDefaultExtension := DefaultExtension;
      FLineCommentarStart := LineCommentarStart;
      FLineCommentarEnd := LineCommentarEnd;
      FTitle := Title;
      FVisible := Visible;
    end;
  end;
end;

procedure TSynEditHighlighterOptions.AssignTo(Dest: TPersistent);
begin
  if Dest.InheritsFrom(TSynEditHighlighterOptions) then begin
    with TSynEditHighlighterOptions(Dest) do begin
      AutoDetectEnabled := FAutoDetectEnabled;
      AutoDetectMatchExpression := FAutoDetectMatchExpression;
      AutoDetectLineLimit := FAutoDetectLineLimit;
      DefaultExtension := FDefaultExtension;
      LineCommentarStart := FLineCommentarStart;
      LineCommentarEnd := FLineCommentarEnd;
      Title := FTitle;
      Visible := FVisible;
    end;
  end;
end;

end.
