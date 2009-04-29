
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntClipBrd;

{$INCLUDE compilers.inc}

interface

uses
  Windows, Clipbrd;

type
{TNT-WARN TClipboard}
  TTntClipboard = class(TClipboard{TNT-ALLOW TClipboard})
  private
    function GetAsWideText: WideString;
    procedure SetAsWideText(const Value: WideString);
  public
    property AsWideText: WideString read GetAsWideText write SetAsWideText;
    property AsText: WideString read GetAsWideText write SetAsWideText;
  end;

{TNT-WARN Clipboard}
function TntClipboard: TTntClipboard;

implementation

{ TTntClipboard }

function TTntClipboard.GetAsWideText: WideString;
var
  Data: THandle;
begin
  Open;
  Data := GetClipboardData(CF_UNICODETEXT);
  try
    if Data <> 0 then
      Result := PWideChar(GlobalLock(Data))
    else
      Result := '';
  finally
    if Data <> 0 then GlobalUnlock(Data);
    Close;
  end;
  if (Data = 0) or (Result = '') then
    Result := inherited AsText
end;

procedure TTntClipboard.SetAsWideText(const Value: WideString);
begin
  Open;
  try
    inherited AsText := Value; {Ensures ANSI compatiblity across platforms.}
    SetBuffer(CF_UNICODETEXT, PWideChar(Value)^, (Length(Value) + 1) * SizeOf(WideChar));
  finally
    Close;
  end;
end;

//------------------------------------------

var
  GTntClipboard: TTntClipboard;

function TntClipboard: TTntClipboard;
begin
  if GTntClipboard = nil then
    GTntClipboard := TTntClipboard.Create;
  Result := GTntClipboard;
end;

initialization

finalization
  GTntClipboard.Free;

end.
