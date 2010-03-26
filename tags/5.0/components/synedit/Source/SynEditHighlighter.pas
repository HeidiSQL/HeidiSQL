{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditHighlighter.pas, released 2000-04-07.

The Original Code is based on mwHighlighter.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

$Id: SynEditHighlighter.pas,v 1.36.2.19 2009/09/28 17:54:20 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{$IFNDEF QSYNEDITHIGHLIGHTER}
unit SynEditHighlighter;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  kTextDrawer,
  Types,
  QGraphics,
  QSynEditTypes,
  QSynEditMiscClasses,
  QSynUnicode,
{$ELSE}
  Graphics,
  Windows,
  Registry,
  IniFiles,
  SynEditTypes,
  SynEditMiscClasses,
  SynUnicode,
{$ENDIF}
  SysUtils,
  Classes;

{$IFNDEF SYN_CLX}
type
  TBetterRegistry = SynEditMiscClasses.TBetterRegistry;
{$ENDIF}

type
  TSynHighlighterAttributes = class(TPersistent)
  private
    fBackground: TColor;
    fBackgroundDefault: TColor;
    fForeground: TColor;
    fForegroundDefault: TColor;
    fFriendlyName: UnicodeString;
    fName: string;
    fStyle: TFontStyles;
    fStyleDefault: TFontStyles;
    fOnChange: TNotifyEvent;
    procedure Changed; virtual;
    function GetBackgroundColorStored: Boolean;
    function GetForegroundColorStored: Boolean;
    function GetFontStyleStored: Boolean;
    procedure SetBackground(Value: TColor);
    procedure SetForeground(Value: TColor);
    procedure SetStyle(Value: TFontStyles);
    function GetStyleFromInt: Integer;
    procedure SetStyleFromInt(const Value: Integer);
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignColorAndStyle(Source: TSynHighlighterAttributes);
    constructor Create(AName: string; AFriendlyName: UnicodeString);
    procedure InternalSaveDefaultValues;
{$IFNDEF SYN_CLX}
    function LoadFromBorlandRegistry(RootKey: HKEY; AttrKey, AttrName: string;
      OldStyle: Boolean): Boolean; virtual;
    function LoadFromRegistry(Reg: TBetterRegistry): Boolean;
    function SaveToRegistry(Reg: TBetterRegistry): Boolean;
    function LoadFromFile(Ini: TIniFile): Boolean;                             
    function SaveToFile(Ini: TIniFile): Boolean;
{$ENDIF}
  public
    property FriendlyName: UnicodeString read fFriendlyName;
    property IntegerStyle: Integer read GetStyleFromInt write SetStyleFromInt;
    property Name: string read fName;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  published
    property Background: TColor read fBackground write SetBackground
      stored GetBackgroundColorStored;
    property Foreground: TColor read fForeground write SetForeground
      stored GetForegroundColorStored;
    property Style: TFontStyles read fStyle write SetStyle
      stored GetFontStyleStored;
  end;

  TSynHighlighterCapability = (
    hcUserSettings, // supports Enum/UseUserSettings
    hcRegistry      // supports LoadFrom/SaveToRegistry
  );

  TSynHighlighterCapabilities = set of TSynHighlighterCapability;

const
  SYN_ATTR_COMMENT           =   0;
  SYN_ATTR_IDENTIFIER        =   1;
  SYN_ATTR_KEYWORD           =   2;
  SYN_ATTR_STRING            =   3;
  SYN_ATTR_WHITESPACE        =   4;
  SYN_ATTR_SYMBOL            =   5;

type
  TSynCustomHighlighter = class(TComponent)
  private
    fAttributes: TStringList;
    fAttrChangeHooks: TSynNotifyEventChain;
    fUpdateCount: Integer;
    fEnabled: Boolean;
    FAdditionalWordBreakChars: TSysCharSet;
    FAdditionalIdentChars: TSysCharSet;
    FExportName: string;
    function GetExportName: string;
    procedure SetEnabled(const Value: Boolean);
    procedure SetAdditionalIdentChars(const Value: TSysCharSet);
    procedure SetAdditionalWordBreakChars(const Value: TSysCharSet);
  protected
    fCasedLine: PWideChar;
    fCasedLineStr: UnicodeString;
    fCaseSensitive: Boolean;
    fDefaultFilter: string;
    fExpandedLine: PWideChar;
    fExpandedLineLen: Integer;
    fExpandedLineStr: UnicodeString;
    fExpandedTokenPos: Integer;
    fLine: PWideChar;
    fLineLen: Integer;
    fLineStr: UnicodeString;
    fLineNumber: Integer;  
    fStringLen: Integer;   
    fToIdent: PWideChar;   
    fTokenPos: Integer;
    fUpdateChange: Boolean;
    Run: Integer;
    ExpandedRun: Integer;
    fOldRun: Integer;
    procedure Loaded; override;
    procedure AddAttribute(Attri: TSynHighlighterAttributes);
    procedure DefHighlightChange(Sender: TObject);
    procedure DefineProperties(Filer: TFiler); override;    
    procedure FreeHighlighterAttributes;                                        
    function GetAttribCount: Integer; virtual;
    function GetAttribute(Index: Integer): TSynHighlighterAttributes; virtual;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      virtual; abstract;
    function GetDefaultFilter: string; virtual;
    function GetSampleSource: UnicodeString; virtual;
    procedure DoSetLine(const Value: UnicodeString; LineNumber: Integer); virtual;
    function IsCurrentToken(const Token: UnicodeString): Boolean; virtual;
    function IsFilterStored: Boolean; virtual;
    function IsLineEnd(Run: Integer): Boolean; virtual;
    procedure SetAttributesOnChange(AEvent: TNotifyEvent);
    procedure SetDefaultFilter(Value: string); virtual;
    procedure SetSampleSource(Value: UnicodeString); virtual;
  protected
    function GetCapabilitiesProp: TSynHighlighterCapabilities;
    function GetFriendlyLanguageNameProp: UnicodeString;
    function GetLanguageNameProp: string;
  public
    class function GetCapabilities: TSynHighlighterCapabilities; virtual;
    class function GetFriendlyLanguageName: UnicodeString; virtual;
    class function GetLanguageName: string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetEol: Boolean; virtual; abstract;
    function GetExpandedToken: UnicodeString; virtual;
    function GetExpandedTokenPos: Integer; virtual;
    function GetKeyWords(TokenKind: Integer): UnicodeString; virtual;
    function GetRange: Pointer; virtual;
    function GetToken: UnicodeString; virtual;
    function GetTokenAttribute: TSynHighlighterAttributes; virtual; abstract;
    function GetTokenKind: Integer; virtual; abstract;
    function GetTokenPos: Integer; virtual;
    function IsKeyword(const AKeyword: UnicodeString): Boolean; virtual;
    procedure Next; virtual;
    procedure NextToEol;
    function PosToExpandedPos(Pos: Integer): Integer;
    procedure SetLineExpandedAtWideGlyphs(const Line, ExpandedLine: UnicodeString;
      LineNumber: Integer); virtual;
    procedure SetLine(const Value: UnicodeString; LineNumber: Integer); virtual;
    procedure SetRange(Value: Pointer); virtual;
    procedure ResetRange; virtual;
    function UseUserSettings(settingIndex: Integer): Boolean; virtual;
    procedure EnumUserSettings(Settings: TStrings); virtual;
{$IFNDEF SYN_CLX}
    function LoadFromRegistry(RootKey: HKEY; Key: string): Boolean; virtual;
    function SaveToRegistry(RootKey: HKEY; Key: string): Boolean; virtual;
    function LoadFromFile(AFileName: string): Boolean;
    function SaveToFile(AFileName: string): Boolean;
{$ENDIF}
    procedure HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    procedure UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
    function IsIdentChar(AChar: WideChar): Boolean; virtual;
    function IsWhiteChar(AChar: WideChar): Boolean; virtual;
    function IsWordBreakChar(AChar: WideChar): Boolean; virtual;
    property FriendlyLanguageName: UnicodeString read GetFriendlyLanguageNameProp;
    property LanguageName: string read GetLanguageNameProp;
  public
    property AdditionalIdentChars: TSysCharSet read FAdditionalIdentChars write SetAdditionalIdentChars;
    property AdditionalWordBreakChars: TSysCharSet read FAdditionalWordBreakChars write SetAdditionalWordBreakChars;
    property AttrCount: Integer read GetAttribCount;
    property Attribute[Index: Integer]: TSynHighlighterAttributes
      read GetAttribute;
    property Capabilities: TSynHighlighterCapabilities read GetCapabilitiesProp;
    property SampleSource: UnicodeString read GetSampleSource write SetSampleSource;
    property CommentAttribute: TSynHighlighterAttributes
      index SYN_ATTR_COMMENT read GetDefaultAttribute;
    property IdentifierAttribute: TSynHighlighterAttributes
      index SYN_ATTR_IDENTIFIER read GetDefaultAttribute;
    property KeywordAttribute: TSynHighlighterAttributes
      index SYN_ATTR_KEYWORD read GetDefaultAttribute;
    property StringAttribute: TSynHighlighterAttributes
      index SYN_ATTR_STRING read GetDefaultAttribute;
    property SymbolAttribute: TSynHighlighterAttributes
      index SYN_ATTR_SYMBOL read GetDefaultAttribute;
    property WhitespaceAttribute: TSynHighlighterAttributes
      index SYN_ATTR_WHITESPACE read GetDefaultAttribute;
    property ExportName: string read GetExportName;
  published
    property DefaultFilter: string read GetDefaultFilter write SetDefaultFilter
      stored IsFilterStored;
    property Enabled: Boolean read fEnabled write SetEnabled default True;
  end;

  TSynCustomHighlighterClass = class of TSynCustomHighlighter;

{$IFNDEF SYN_CPPB_1}
  TSynHighlighterList = class(TList)
  private
    hlList: TList;
    function GetItem(Index: Integer): TSynCustomHighlighterClass;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    function FindByFriendlyName(FriendlyName: string): Integer;
    function FindByName(Name: string): Integer;
    function FindByClass(Comp: TComponent): Integer;
    property Items[Index: Integer]: TSynCustomHighlighterClass
      read GetItem; default;
  end;

  procedure RegisterPlaceableHighlighter(highlighter:
    TSynCustomHighlighterClass);
  function GetPlaceableHighlighters: TSynHighlighterList;
{$ENDIF}

implementation

uses
  SynEditMiscProcs,
{$IFDEF UNICODE}
  WideStrUtils,
{$ENDIF}
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

{$IFNDEF SYN_CPPB_1}
{ THighlighterList }

function TSynHighlighterList.Count: Integer;
begin
  Result := hlList.Count;
end;

constructor TSynHighlighterList.Create;
begin
  inherited Create;
  hlList := TList.Create;
end;

destructor TSynHighlighterList.Destroy;
begin
  hlList.Free;
  inherited;
end;

function TSynHighlighterList.FindByClass(Comp: TComponent): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Comp is Items[i] then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TSynHighlighterList.FindByFriendlyName(FriendlyName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].GetFriendlyLanguageName = FriendlyName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TSynHighlighterList.FindByName(Name: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if Items[i].GetLanguageName = Name then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TSynHighlighterList.GetItem(Index: Integer): TSynCustomHighlighterClass;
begin
  Result := TSynCustomHighlighterClass(hlList[Index]);
end;

var
  G_PlaceableHighlighters: TSynHighlighterList;

  function GetPlaceableHighlighters: TSynHighlighterList;
  begin
    Result := G_PlaceableHighlighters;
  end;

  procedure RegisterPlaceableHighlighter(highlighter: TSynCustomHighlighterClass);
  begin
    if G_PlaceableHighlighters.hlList.IndexOf(highlighter) < 0 then
      G_PlaceableHighlighters.hlList.Add(highlighter);
  end;
{$ENDIF}

{ TSynHighlighterAttributes }

procedure TSynHighlighterAttributes.Assign(Source: TPersistent);
begin
  if Source is TSynHighlighterAttributes then
  begin
    fName := TSynHighlighterAttributes(Source).fName;
    AssignColorAndStyle(TSynHighlighterAttributes(Source));
  end
  else
    inherited Assign(Source);
end;

procedure TSynHighlighterAttributes.AssignColorAndStyle(Source: TSynHighlighterAttributes);
var
  bChanged: Boolean;
begin
  bChanged := False;
  if fBackground <> Source.fBackground then
  begin
    fBackground := Source.fBackground;
    bChanged := True;
  end;
  if fForeground <> Source.fForeground then
  begin
    fForeground := Source.fForeground;
    bChanged := True;
  end;
  if fStyle <> Source.fStyle then
  begin
    fStyle := Source.fStyle;
    bChanged := True;
  end;
  if bChanged then
    Changed;
end;


procedure TSynHighlighterAttributes.Changed;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

constructor TSynHighlighterAttributes.Create(AName: string; AFriendlyName: UnicodeString);
begin
  inherited Create;
  Background := clNone;
  Foreground := clNone;
  fName := AName;
  fFriendlyName := AFriendlyName;
end;

function TSynHighlighterAttributes.GetBackgroundColorStored: Boolean;
begin
  Result := fBackground <> fBackgroundDefault;
end;

function TSynHighlighterAttributes.GetForegroundColorStored: Boolean;
begin
  Result := fForeground <> fForegroundDefault;
end;

function TSynHighlighterAttributes.GetFontStyleStored: Boolean;
begin
  Result := fStyle <> fStyleDefault;
end;

procedure TSynHighlighterAttributes.InternalSaveDefaultValues;
begin
  fForegroundDefault := fForeground;
  fBackgroundDefault := fBackground;
  fStyleDefault := fStyle;
end;

{$IFNDEF SYN_CLX}
function TSynHighlighterAttributes.LoadFromBorlandRegistry(RootKey: HKEY;
  AttrKey, AttrName: string; OldStyle: Boolean): Boolean;
  // How the highlighting information is stored:
  // Delphi 1.0:
  //   I don't know and I don't care.
  // Delphi 2.0 & 3.0:
  //   In the registry branch HKCU\Software\Borland\Delphi\x.0\Highlight
  //   where x=2 or x=3.
  //   Each entry is one string value, encoded as
  //     <foreground RGB>,<background RGB>,<font style>,<default fg>,<default Background>,<fg index>,<Background index>
  //   Example:
  //     0,16777215,BI,0,1,0,15
  //     foreground color (RGB): 0
  //     background color (RGB): 16777215 ($FFFFFF)
  //     font style: BI (bold italic), possible flags: B(old), I(talic), U(nderline)
  //     default foreground: no, specified color will be used (black (0) is used when this flag is 1)
  //     default background: yes, white ($FFFFFF, 15) will be used for background
  //     foreground index: 0 (foreground index (Pal16), corresponds to foreground RGB color)
  //     background index: 15 (background index (Pal16), corresponds to background RGB color)
  // Delphi 4.0 & 5.0:
  //   In the registry branch HKCU\Software\Borland\Delphi\4.0\Editor\Highlight.
  //   Each entry is subkey containing several values:
  //     Foreground Color: foreground index (Pal16), 0..15 (dword)
  //     Background Color: background index (Pal16), 0..15 (dword)
  //     Bold: fsBold yes/no, 0/True (string)
  //     Italic: fsItalic yes/no, 0/True (string)
  //     Underline: fsUnderline yes/no, 0/True (string)
  //     Default Foreground: use default foreground (clBlack) yes/no, False/-1 (string)
  //     Default Background: use default backround (clWhite) yes/no, False/-1 (string)
const
  Pal16: array [0..15] of TColor = (
    clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clLtGray,
    clDkGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite
  );

  function LoadOldStyle(RootKey: HKEY; AttrKey, AttrName: string): Boolean;
  var
    descript: string;
    fgColRGB: string;
    bgColRGB: string;
    fontStyle: string;
    fgDefault: string;
    bgDefault: string;
    fgIndex16: string;
    bgIndex16: string;
    reg: TBetterRegistry;

    function Get(var Name: string): string;
    var
      p: Integer;
    begin
      p := Pos(',', Name);
      if p = 0 then p := Length(Name) + 1;
      Result := Copy(name, 1, p - 1);
      name := Copy(name, p + 1, Length(name) - p);
    end;

  begin { LoadOldStyle }
    Result := False;
    try
      reg := TBetterRegistry.Create;
      reg.RootKey := RootKey;
      try
        with reg do
        begin
          if OpenKeyReadOnly(AttrKey) then
          begin
            try
              if ValueExists(AttrName) then
              begin
                descript := ReadString(AttrName);
                fgColRGB  := Get(descript);
                bgColRGB  := Get(descript);
                fontStyle := Get(descript);
                fgDefault := Get(descript);
                bgDefault := Get(descript);
                fgIndex16 := Get(descript);
                bgIndex16 := Get(descript);
                if bgDefault = '1' then
                  Background := clWindow
                else
                  Background := Pal16[StrToInt(bgIndex16)];
                if fgDefault = '1' then
                  Foreground := clWindowText
                else
                  Foreground := Pal16[StrToInt(fgIndex16)];
                Style := [];
                if Pos('B', fontStyle) > 0 then Style := Style + [fsBold];
                if Pos('I', fontStyle) > 0 then Style := Style + [fsItalic];
                if Pos('U', fontStyle) > 0 then Style := Style + [fsUnderline];
                Result := True;
              end;
            finally
              CloseKey;
            end;
          end; // if
        end; // with
      finally
        reg.Free;
      end;
    except
    end;
  end; { LoadOldStyle }

  function LoadNewStyle(RootKey: HKEY; AttrKey, AttrName: string): Boolean;
  var
    fgColor: Integer;
    bgColor: Integer;
    fontBold: string;
    fontItalic: string;
    fontUnderline: string;
    fgDefault: string;
    bgDefault: string;
    reg: TBetterRegistry;

    function IsTrue(Value: string): Boolean;
    begin
      Result := not ((UpperCase(Value) = 'FALSE') or (Value = '0'));
    end; { IsTrue }

  begin
    Result := False;
    try
      reg := TBetterRegistry.Create;
      reg.RootKey := RootKey;
      try
        with reg do
        begin
          if OpenKeyReadOnly(AttrKey + '\' + AttrName) then
          begin
            try
              if ValueExists('Foreground Color')
                then fgColor := Pal16[ReadInteger('Foreground Color')]
              else if ValueExists('Foreground Color New') then
                fgColor := StringToColor(ReadString('Foreground Color New'))
              else
                Exit;
              if ValueExists('Background Color')
                then bgColor := Pal16[ReadInteger('Background Color')]
              else if ValueExists('Background Color New') then
                bgColor := StringToColor(ReadString('Background Color New'))
              else
                Exit;
              if ValueExists('Bold')
                then fontBold := ReadString('Bold')
                else Exit;
              if ValueExists('Italic')
                then fontItalic := ReadString('Italic')
                else Exit;
              if ValueExists('Underline')
                then fontUnderline := ReadString('Underline')
                else Exit;
              if ValueExists('Default Foreground')
                then fgDefault := ReadString('Default Foreground')
                else Exit;
              if ValueExists('Default Background')
                then bgDefault := ReadString('Default Background')
                else Exit;
              if IsTrue(bgDefault)
                then Background := clWindow
                else Background := bgColor;
              if IsTrue(fgDefault)
                then Foreground := clWindowText
                else Foreground := fgColor;
              Style := [];
              if IsTrue(fontBold) then Style := Style + [fsBold];
              if IsTrue(fontItalic) then Style := Style + [fsItalic];
              if IsTrue(fontUnderline) then Style := Style + [fsUnderline];
              Result := True;
            finally
              CloseKey;
            end;
          end; // if
        end; // with
      finally
        reg.Free;
      end;
    except
    end;
  end; { LoadNewStyle }

begin
  if OldStyle then
    Result := LoadOldStyle(RootKey, AttrKey, AttrName)
  else
    Result := LoadNewStyle(RootKey, AttrKey, AttrName);
end; { TSynHighlighterAttributes.LoadFromBorlandRegistry }
{$ENDIF}

procedure TSynHighlighterAttributes.SetBackground(Value: TColor);
begin
  if fBackGround <> Value then
  begin
    fBackGround := Value;
    Changed;
  end;
end;

procedure TSynHighlighterAttributes.SetForeground(Value: TColor);
begin
  if fForeGround <> Value then
  begin
    fForeGround := Value;
    Changed;
  end;
end;

procedure TSynHighlighterAttributes.SetStyle(Value: TFontStyles);
begin
  if fStyle <> Value then
  begin
    fStyle := Value;
    Changed;
  end;
end;

{$IFNDEF SYN_CLX}
function TSynHighlighterAttributes.LoadFromRegistry(Reg: TBetterRegistry): Boolean;
var
  Key: string;
begin
  Key := Reg.CurrentPath;
  if Reg.OpenKeyReadOnly(Name) then
  begin
    if Reg.ValueExists('Background') then
      Background := Reg.ReadInteger('Background');
    if Reg.ValueExists('Foreground') then
      Foreground := Reg.ReadInteger('Foreground');
    if Reg.ValueExists('Style') then
      IntegerStyle := Reg.ReadInteger('Style');
    reg.OpenKeyReadOnly('\' + Key);
    Result := True;
  end
  else
    Result := False;
end;

function TSynHighlighterAttributes.SaveToRegistry(Reg: TBetterRegistry): Boolean;
var
  Key: string;
begin
  Key := Reg.CurrentPath;
  if Reg.OpenKey(Name, True) then
  begin
    Reg.WriteInteger('Background', Background);
    Reg.WriteInteger('Foreground', Foreground);
    Reg.WriteInteger('Style', IntegerStyle);
    reg.OpenKey('\' + Key, False);
    Result := True;
  end
  else
    Result := False;
end;

function TSynHighlighterAttributes.LoadFromFile(Ini : TIniFile): boolean;
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    Ini.ReadSection(Name, S);
    if S.Count > 0 then
    begin
      if S.IndexOf('Background') <> -1 then
        Background := Ini.ReadInteger(Name, 'Background', Background);
      if S.IndexOf('Foreground') <> -1 then
        Foreground := Ini.ReadInteger(Name, 'Foreground', Foreground);
      if S.IndexOf('Style') <> -1 then
        IntegerStyle := Ini.ReadInteger(Name, 'Style', IntegerStyle);
      Result := true;
    end
    else
      Result := False;
  finally
    S.Free;
  end;
end;

function TSynHighlighterAttributes.SaveToFile(Ini : TIniFile): boolean;
begin
  Ini.WriteInteger(Name, 'Background', Background);
  Ini.WriteInteger(Name, 'Foreground', Foreground);
  Ini.WriteInteger(Name, 'Style', IntegerStyle);
  Result := True;
end;

{$ENDIF}

function TSynHighlighterAttributes.GetStyleFromInt: Integer;
begin
  if fsBold in Style then Result := 1 else Result := 0;
  if fsItalic in Style then Result := Result + 2;
  if fsUnderline in Style then Result:= Result + 4;
  if fsStrikeout in Style then Result:= Result + 8;
end;

procedure TSynHighlighterAttributes.SetStyleFromInt(const Value: Integer);
begin
  if Value and $1 = 0 then  Style:= [] else Style := [fsBold];
  if Value and $2 <> 0 then Style:= Style + [fsItalic];
  if Value and $4 <> 0 then Style:= Style + [fsUnderline];
  if Value and $8 <> 0 then Style:= Style + [fsStrikeout];
end;

{ TSynCustomHighlighter }

constructor TSynCustomHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fAttributes := TStringList.Create;
  fAttributes.Duplicates := dupError;
  fAttributes.Sorted := True;
  fAttrChangeHooks := TSynNotifyEventChain.CreateEx(Self);
  fDefaultFilter := '';
  fEnabled := True;
end;

destructor TSynCustomHighlighter.Destroy;
begin
  inherited Destroy;
  FreeHighlighterAttributes;
  fAttributes.Free;
  fAttrChangeHooks.Free;
end;

procedure TSynCustomHighlighter.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TSynCustomHighlighter.EndUpdate;
begin
  if fUpdateCount > 0 then
  begin
    Dec(fUpdateCount);
    if (fUpdateCount = 0) and fUpdateChange then
    begin
      fUpdateChange := False;
      DefHighlightChange(nil);
    end;
  end;
end;

procedure TSynCustomHighlighter.FreeHighlighterAttributes;
var
  i: Integer;
begin
  if fAttributes <> nil then
  begin
    for i := fAttributes.Count - 1 downto 0 do
      TSynHighlighterAttributes(fAttributes.Objects[i]).Free;
    fAttributes.Clear;
  end;
end;

procedure TSynCustomHighlighter.Assign(Source: TPersistent);
var
  Src: TSynCustomHighlighter;
  i, j: Integer;
  AttriName: string;
  SrcAttri: TSynHighlighterAttributes;
begin
  if (Source <> nil) and (Source is TSynCustomHighlighter) then
  begin
    Src := TSynCustomHighlighter(Source);
    for i := 0 to AttrCount - 1 do
    begin
      // assign first attribute with the same name
      AttriName := Attribute[i].Name;
      for j := 0 to Src.AttrCount - 1 do
      begin
        SrcAttri := Src.Attribute[j];
        if AttriName = SrcAttri.Name then
        begin
          Attribute[i].Assign(SrcAttri);
          break;
        end;
      end;
    end;
    // assign the sample source text only if same or descendant class
    if Src is ClassType then
      SampleSource := Src.SampleSource;
    //fWordBreakChars := Src.WordBreakChars; //TODO: does this make sense anyway?
    DefaultFilter := Src.DefaultFilter;
    Enabled := Src.Enabled;
  end
  else
    inherited Assign(Source);
end;

procedure TSynCustomHighlighter.EnumUserSettings(Settings: TStrings);
begin
  Settings.Clear;
end;

function TSynCustomHighlighter.UseUserSettings(settingIndex: Integer): Boolean;
begin
  Result := False;
end;

{$IFNDEF SYN_CLX}
function TSynCustomHighlighter.LoadFromRegistry(RootKey: HKEY;
  Key: string): Boolean;
var
  r: TBetterRegistry;
  i: Integer;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKeyReadOnly(Key) then
    begin
      Result := True;
      for i := 0 to AttrCount - 1 do
        Result := Attribute[i].LoadFromRegistry(r) and Result;
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynCustomHighlighter.SaveToRegistry(RootKey: HKEY;
  Key: string): Boolean;
var
  r: TBetterRegistry;
  i: Integer;
begin
  r := TBetterRegistry.Create;
  try
    r.RootKey := RootKey;
    if r.OpenKey(Key,True) then
    begin
      Result := True;
      for i := 0 to AttrCount - 1 do
        Result := Attribute[i].SaveToRegistry(r) and Result;
    end
    else
      Result := False;
  finally
    r.Free;
  end;
end;

function TSynCustomHighlighter.LoadFromFile(AFileName : String): boolean;
var 
  AIni: TIniFile;
  i: Integer;
begin
  AIni := TIniFile.Create(AFileName);
  try
    with AIni do
    begin
      Result := True;
      for i := 0 to AttrCount - 1 do
        Result := Attribute[i].LoadFromFile(AIni) and Result;
    end;
  finally
    AIni.Free;
  end;
end;

function TSynCustomHighlighter.SaveToFile(AFileName : String): boolean;
var
  AIni: TIniFile;
  i: integer;
begin
  AIni := TIniFile.Create(AFileName);
  try
    with AIni do
    begin
      Result := True;
      for i := 0 to AttrCount - 1 do
        Result := Attribute[i].SaveToFile(AIni) and Result;
    end;
  finally
    AIni.Free;
  end;
end;

{$ENDIF}

procedure TSynCustomHighlighter.AddAttribute(Attri: TSynHighlighterAttributes);
begin
  fAttributes.AddObject(Attri.Name, Attri);
end;

procedure TSynCustomHighlighter.DefHighlightChange(Sender: TObject);
begin
  if fUpdateCount > 0 then
    fUpdateChange := True
  else if not(csLoading in ComponentState) then
  begin
    fAttrChangeHooks.Sender := Sender;
    fAttrChangeHooks.Fire;
  end;
end;

procedure TSynCustomHighlighter.DefineProperties(Filer: TFiler);
begin
  inherited;
{$IFNDEF UNICODE}
  UnicodeDefineProperties(Filer, Self);
{$ENDIF}
end;

function TSynCustomHighlighter.GetAttribCount: Integer;
begin
  Result := fAttributes.Count;
end;

function TSynCustomHighlighter.GetAttribute(Index: Integer):
  TSynHighlighterAttributes;
begin
  Result := nil;
  if (Index >= 0) and (Index < fAttributes.Count) then
    Result := TSynHighlighterAttributes(fAttributes.Objects[Index]);
end;

class function TSynCustomHighlighter.GetCapabilities: TSynHighlighterCapabilities;
begin
  Result := [hcRegistry]; //registry save/load supported by default
end;

function TSynCustomHighlighter.GetCapabilitiesProp: TSynHighlighterCapabilities;
begin
  Result := GetCapabilities;
end;

function TSynCustomHighlighter.GetDefaultFilter: string;
begin
  Result := fDefaultFilter;
end;

function TSynCustomHighlighter.GetExpandedTokenPos: Integer;
begin
  if fExpandedLine = nil then
    Result := fTokenPos
  else
    Result := fExpandedTokenPos;
end;

function TSynCustomHighlighter.GetExportName: string;
begin
  if FExportName = '' then
    FExportName := SynEditMiscProcs.DeleteTypePrefixAndSynSuffix(ClassName);
  Result := FExportName;
end;

function TSynCustomHighlighter.GetExpandedToken: UnicodeString;
var
  Len: Integer;
begin
  if fExpandedLine = nil then
  begin
    Result := GetToken;
    Exit;
  end;

  Len := ExpandedRun - fExpandedTokenPos;
  SetLength(Result, Len);
  if Len > 0 then
    WStrLCopy(@Result[1], fExpandedLine + fExpandedTokenPos, Len);
end;

class function TSynCustomHighlighter.GetFriendlyLanguageName: UnicodeString;
begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  raise Exception.CreateFmt('%s.GetFriendlyLanguageName not implemented', [ClassName]);
{$ENDIF}
  Result := SYNS_FriendlyLangUnknown;
end;

class function TSynCustomHighlighter.GetLanguageName: string;
begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  raise Exception.CreateFmt('%s.GetLanguageName not implemented', [ClassName]);
{$ENDIF}
  Result := SYNS_LangUnknown;
end;

function TSynCustomHighlighter.GetFriendlyLanguageNameProp: UnicodeString;
begin
  Result := GetFriendlyLanguageName;
end;

function TSynCustomHighlighter.GetLanguageNameProp: string;
begin
  Result := GetLanguageName;
end;

function TSynCustomHighlighter.GetRange: Pointer;
begin
  Result := nil;
end;

function TSynCustomHighlighter.GetToken: UnicodeString;
var
  Len: Integer;
begin
  Len := Run - fTokenPos;
  SetLength(Result, Len);
  if Len > 0 then
    WStrLCopy(@Result[1], fCasedLine + fTokenPos, Len);
end;

function TSynCustomHighlighter.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynCustomHighlighter.GetKeyWords(TokenKind: Integer): UnicodeString;
begin
  Result := '';
end;

function TSynCustomHighlighter.GetSampleSource: UnicodeString;
begin
  Result := '';
end;

procedure TSynCustomHighlighter.HookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Add(ANotifyEvent);
end;

function TSynCustomHighlighter.IsCurrentToken(const Token: UnicodeString): Boolean;
var
  I: Integer;
  Temp: PWideChar;
begin
  Temp := fToIdent;
  if Length(Token) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> Token[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else
    Result := False;
end;

function TSynCustomHighlighter.IsFilterStored: Boolean;
begin
  Result := True;
end;

function TSynCustomHighlighter.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'A'..'Z', 'a'..'z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynCustomHighlighter.IsKeyword(const AKeyword: UnicodeString): Boolean;
begin
  Result := False;
end;

function TSynCustomHighlighter.IsLineEnd(Run: Integer): Boolean;
begin
  Result := (Run >= fLineLen) or (fLine[Run] = #10) or (fLine[Run] = #13);
end;

function TSynCustomHighlighter.IsWhiteChar(AChar: WideChar): Boolean;
begin
  case AChar of
    #0..#32:
      Result := True;
    else
      Result := not (IsIdentChar(AChar) or IsWordBreakChar(AChar))
  end
end;

function TSynCustomHighlighter.IsWordBreakChar(AChar: WideChar): Boolean;
begin
  case AChar of
    #0..#32, '.', ',', ';', ':', '"', '''', '´', '`', '°', '^', '!', '?', '&',
    '$', '@', '§', '%', '#', '~', '[', ']', '(', ')', '{', '}', '<', '>',
    '-', '=', '+', '*', '/', '\', '|':
      Result := True;
    else
      Result := False;
  end;
end;

procedure TSynCustomHighlighter.Next;
var
  Delta: Integer;
begin
  if fOldRun = Run then Exit;

  fExpandedTokenPos := ExpandedRun;
  if fExpandedLine = nil then Exit;

  Delta := Run - fOldRun;
  while Delta > 0 do
  begin
    while fExpandedLine[ExpandedRun] = FillerChar do
      inc(ExpandedRun);
    inc(ExpandedRun);
    dec(Delta);
  end;
  fOldRun := Run;
end;

procedure TSynCustomHighlighter.NextToEol;
begin
  while not GetEol do Next;
end;

procedure TSynCustomHighlighter.ResetRange;
begin
end;

procedure TSynCustomHighlighter.SetAdditionalIdentChars(
  const Value: TSysCharSet);
begin
  FAdditionalIdentChars := Value;
end;

procedure TSynCustomHighlighter.SetAdditionalWordBreakChars(
  const Value: TSysCharSet);
begin
  FAdditionalWordBreakChars := Value;
end;

procedure TSynCustomHighlighter.SetAttributesOnChange(AEvent: TNotifyEvent);
var
  i: Integer;
  Attri: TSynHighlighterAttributes;
begin
  for i := fAttributes.Count - 1 downto 0 do
  begin
    Attri := TSynHighlighterAttributes(fAttributes.Objects[i]);
    if Attri <> nil then
    begin
      Attri.OnChange := AEvent;
      Attri.InternalSaveDefaultValues;
    end;
  end;
end;

procedure TSynCustomHighlighter.SetLineExpandedAtWideGlyphs(const Line,
  ExpandedLine: UnicodeString; LineNumber: Integer);
begin
  fExpandedLineStr := ExpandedLine;
  fExpandedLine := PWideChar(fExpandedLineStr);
  fExpandedLineLen := Length(fExpandedLineStr);
  DoSetLine(Line, LineNumber);
  Next;
end;

procedure TSynCustomHighlighter.SetLine(const Value: UnicodeString; LineNumber: Integer);
begin
  fExpandedLineStr := '';
  fExpandedLine := nil;
  fExpandedLineLen := 0;
  DoSetLine(Value, LineNumber);
  Next;
end;

procedure TSynCustomHighlighter.DoSetLine(const Value: UnicodeString; LineNumber: Integer);
begin
  // UnicodeStrings are not reference counted, hence we need to copy
  if fCaseSensitive then
  begin
    fLineStr := Value;
    fLine := PWideChar(fLineStr);
    fLineLen := Length(fLineStr);
    fCasedLineStr := '';
    fCasedLine := PWideChar(fLineStr);
  end
  else
  begin
    fLineStr := SynWideLowerCase(Value);
    fLine := PWideChar(fLineStr);
    fLineLen := Length(fLineStr);
    fCasedLineStr := Value;
    fCasedLine := PWideChar(fCasedLineStr);
  end;

  Run := 0;
  ExpandedRun := 0;
  fOldRun := Run;
  fLineNumber := LineNumber;
end;

procedure TSynCustomHighlighter.SetRange(Value: Pointer);
begin
end;

procedure TSynCustomHighlighter.SetDefaultFilter(Value: string);
begin
  fDefaultFilter := Value;
end;

procedure TSynCustomHighlighter.SetSampleSource(Value: UnicodeString);
begin
  // TODO: sure this should be empty?
end;

procedure TSynCustomHighlighter.UnhookAttrChangeEvent(ANotifyEvent: TNotifyEvent);
begin
  fAttrChangeHooks.Remove(ANotifyEvent);
end;

procedure TSynCustomHighlighter.SetEnabled(const Value: Boolean);
begin
  if fEnabled <> Value then
  begin
    fEnabled := Value;
    DefHighlightChange(nil);
  end;
end;

procedure TSynCustomHighlighter.Loaded;
begin
  inherited;
  DefHighlightChange(nil);
end;

// Pos and Result are 1-based (i.e. positions in a UnicodeString not a PWideChar)
function TSynCustomHighlighter.PosToExpandedPos(Pos: Integer): Integer;
var
  i: Integer;
begin
  if fExpandedLine = nil then
  begin
    Result := Pos;
    Exit;
  end;

  Result := 0;
  i := 0;
  while i < Pos do
  begin
    while fExpandedLine[Result] = FillerChar do
      inc(Result);
    inc(Result);
    inc(i);
  end;
end;

{$IFNDEF SYN_CPPB_1}
initialization
  G_PlaceableHighlighters := TSynHighlighterList.Create;
finalization
  G_PlaceableHighlighters.Free;
  G_PlaceableHighlighters := nil;
{$ENDIF}
end.
