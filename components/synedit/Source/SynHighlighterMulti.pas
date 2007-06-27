{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterMulti.pas, released 2000-06-23.
The Original Code is based on mwMultiSyn.pas by Willo van der Merwe, part of the
mwEdit component suite.

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

$Id: SynHighlighterMulti.pas,v 1.17 2002/03/18 07:52:18 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Multiple-highlighter syntax highlighter for SynEdit)
@author(Willo van der Merwe <willo@wack.co.za>, converted to SynEdit by David Muir <dhm@dmsoftware.co.uk>)
@created(1999, converted to SynEdit 2000-06-23)
@lastmod(2000-06-23)
The SynHighlighterMulti unit provides SynEdit with a multiple-highlighter syntax highlighter.
This highlighter can be used to highlight text in which several languages are present, such as HTML.
For example, in HTML as well as HTML tags there can also be JavaScript and/or VBScript present.
}
unit SynHighlighterMulti;

{$I SynEdit.inc}

interface

uses
  Classes,
  SynEditTypes,
  SynEditHighlighter;

type
  //GBN 31/01/2002
  TOnCheckMarker=procedure(Sender: TObject; var StartPos, MarkerLen: Integer; var MarkerText: String) of object;

  TgmScheme = class(TCollectionItem)
  private
    fEndExpr: string;
    fStartExpr: string;
    fHighlighter: TSynCustomHighLighter;
    fMarkerAttri: TSynHighlighterAttributes;
    fSchemeName: TComponentName;
    fCaseSensitive: Boolean;
    //GBN 31/01/2002 - Start
    fOnCheckStartMarker: TOnCheckMarker;
    fOnCheckEndMarker: TOnCheckMarker;
    //GBN 31/01/2002 - End
    function ConvertExpression(const Value: string): string;
    procedure MarkerAttriChanged(Sender: TObject);
    procedure SetMarkerAttri(const Value: TSynHighlighterAttributes);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetEndExpr(const Value: string);
    procedure SetStartExpr(const Value: string);
		procedure SetCaseSensitive(const Value: Boolean);

	protected
{$IFDEF SYN_COMPILER_3_UP}
		function GetDisplayName: String; override;
		procedure SetDisplayName(const Value: String); override;
{$ENDIF}
	public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
	published
		property CaseSensitive: Boolean read fCaseSensitive write SetCaseSensitive
			default True;
		property StartExpr: string read fStartExpr write SetStartExpr;
		property EndExpr: string read fEndExpr write SetEndExpr;
		property Highlighter: TSynCustomHighlighter read fHighlighter
			write SetHighlighter;
		property MarkerAttri: TSynHighlighterAttributes read fMarkerAttri
			write SetMarkerAttri;
		property SchemeName: TComponentName read fSchemeName write fSchemeName;
		//GBN 31/01/2002 - Start
		property OnCheckStartMarker: TOnCheckMarker read fOnCheckStartMarker write fOnCheckStartMarker;
		property OnCheckEndMarker: TOnCheckMarker read fOnCheckEndMarker write fOnCheckEndMarker;
		//GBN 31/01/2002 - End
	end;

	TgmSchemeClass = class of TgmScheme;

	TSynMultiSyn = class;

	TgmSchemes = class(TCollection)
	private
		fOwner: TSynMultiSyn;
		function GetItems(Index: integer): TgmScheme;
		procedure SetItems(Index: integer; const Value: TgmScheme);
{$IFDEF SYN_COMPILER_3_UP}
	protected
		function GetOwner: TPersistent; override;
		procedure Update(Item: TCollectionItem); override;
{$ENDIF}
	public
		constructor Create(aOwner: TSynMultiSyn);
		property Items[aIndex: integer]: TgmScheme read GetItems write SetItems;
			default;
	end;

	TgmMarker = class
	protected
		fScheme: integer;
    fStartPos: integer;
    fMarkerLen: integer;
    fMarkerText: string;
    fIsOpenMarker: boolean;
  public
    constructor Create(aScheme, aStartPos, aMarkerLen: integer;
      aIsOpenMarker: boolean; const aMarkerText: string);
  end;

  TSynMultiSyn = class(TSynCustomHighLighter)
  private
    fDefaultLanguageName: String;
    fMarkers: TList;
    fMarker: TgmMarker;
    fNextMarker: integer;
    fCurrScheme: integer;
    fTmpLine: String; {there're lots of highlighters not doing reference count...}
    fTmpRange: pointer; {only works inside a single line for now}
    procedure SetDefaultHighlighter(const Value: TSynCustomHighLighter);
    function GetMarkers(aIndex: integer): TgmMarker;
    property Markers[aIndex: integer]: TgmMarker read GetMarkers;
    //GBN 31/01/2002 - Start
    procedure DoCheckMarker(Scheme:TgmScheme; StartPos, MarkerLen: Integer;
			const MarkerText: String; Start: Boolean);
		//GBN 31/01/2002 - End
	protected
		fSchemes: TgmSchemes;
		fDefaultHighlighter: TSynCustomHighLighter;
		fLine: string;
		fLineNumber: Integer;
		fTokenPos: integer;
		fRun: Integer;
		fSampleSource: string;
		procedure SetSchemes(const Value: TgmSchemes);
		procedure ClearMarkers;
		function GetIdentChars: TSynIdentChars; override;
		function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
		function GetAttribCount: integer; override;
		function GetAttribute(idx: integer): TSynHighlighterAttributes; override;
		procedure HookHighlighter(aHL: TSynCustomHighlighter);
		procedure UnhookHighlighter(aHL: TSynCustomHighlighter);
		procedure Notification(aComp: TComponent; aOp: TOperation); override;
		function GetSampleSource: string; override;
		procedure SetSampleSource(Value: string); override;
	public
		{$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetToken: string; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    property CurrScheme: integer read fCurrScheme write fCurrScheme;
  published
    property Schemes: TgmSchemes read fSchemes write SetSchemes;
    property DefaultHighlighter: TSynCustomHighLighter read fDefaultHighlighter
      write SetDefaultHighlighter;
    property DefaultLanguageName: String read fDefaultLanguageName
      write fDefaultLanguageName;
  end;

const
  MaxNestedMultiSyn = 4;

implementation

uses
  SynRegExpr,
{$IFDEF SYN_CLX}
  QGraphics,
{$ELSE}
  Graphics,
{$ENDIF}
  SysUtils,
  SynEditMiscProcs,
  SynEditStrConst;

const
  { number of bits of the Range that will be used to store the SchemeIndex }
  SchemeIndexSize = 5;
  MaxSchemeCount = (1 shl SchemeIndexSize) -1;
  { number of bits of the Range that will be used to store the SchemeRange }
  SchemeRangeSize = 12;
  MaxSchemeRange = (1 shl SchemeRangeSize) -1;

{ TgmMarker }

constructor TgmMarker.Create(aScheme, aStartPos,
  aMarkerLen: integer; aIsOpenMarker: boolean; const aMarkerText: string);
begin
  fScheme := aScheme;
  fStartPos := aStartPos;
  fMarkerLen := aMarkerLen;
  fIsOpenMarker := aIsOpenMarker;
  fMarkerText := aMarkerText;
end;

{ TSynMultiSyn }

procedure TSynMultiSyn.ClearMarkers;
const
  { if the compiler stops here, something is wrong with the constants above }
  { there is no special reason for this to be here. the constant must be
  declared locally to avoid a compiler hint - or else Delphi shows a hint -, and
  so this function was randomly chosen }
  RangeInfoSize: byte = ( SizeOf(pointer) * 8 ) -
    ( (MaxNestedMultiSyn * SchemeIndexSize) + SchemeRangeSize );
var
  i: integer;
begin
  for i := 0 to fMarkers.Count - 1 do
    TObject(fMarkers[i]).Free;
  fMarkers.Clear;
end;

constructor TSynMultiSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fSchemes := TgmSchemes.Create(Self);
  fCurrScheme := -1;
  fMarkers := TList.Create;
end;

destructor TSynMultiSyn.Destroy;
begin
  fSchemes.Free;
  ClearMarkers;
  fMarkers.Free;
  { unhook notification handlers }
  DefaultHighlighter := nil;
  inherited Destroy;
end;

function TSynMultiSyn.GetAttribCount: integer;
begin
  Result := inherited GetAttribCount + Schemes.Count;
end;

function TSynMultiSyn.GetAttribute(
  idx: integer): TSynHighlighterAttributes;
begin
  if idx < Schemes.Count then
    Result := Schemes[ idx ].MarkerAttri
  else
    Result := inherited GetAttribute( idx + Schemes.Count );
end;

function TSynMultiSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
var
  iHL: TSynCustomHighlighter;
begin
  if (CurrScheme >= 0) and (Schemes[CurrScheme].Highlighter <> nil) then
    iHL := Schemes[CurrScheme].Highlighter
  else
    iHL := DefaultHighlighter;
  { the typecast to TSynMultiSyn is only necessary because the
  GetDefaultAttribute method is protected.
  And don't worry: this really works }
  if iHL <> nil then begin
    Result := TSynMultiSyn(iHL).GetDefaultAttribute(Index)
  end else
    Result := nil;
end;

function TSynMultiSyn.GetEol: Boolean;
begin
  if fMarker <> nil then
    Result := False
  else if fCurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.GetEol
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetEol
  else
    Result := fRun > Length(fLine) + 2;
end;

function TSynMultiSyn.GetIdentChars: TSynIdentChars;
begin
  if CurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.IdentChars
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.IdentChars
  else
    Result := inherited GetIdentChars;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynMultiSyn.GetLanguageName: string;
begin
  Result := SYNS_LangGeneralMulti;
end;

function TSynMultiSyn.GetMarkers(aIndex: integer): TgmMarker;
begin
  Result := TgmMarker( fMarkers[ aIndex ] );
end;

function TSynMultiSyn.GetRange: Pointer;
var
  iHL: TSynCustomHighlighter;
  iSchemeIndex: cardinal;
  iSchemeRange: cardinal;
begin
  if (fCurrScheme < 0) then
    iHL := DefaultHighlighter
  else
    iHL := Schemes[fCurrScheme].Highlighter;
  iSchemeIndex := fCurrScheme +2;
  Assert( iSchemeIndex <= MaxSchemeCount );
  if iHL <> nil then begin
    iSchemeRange := cardinal( iHL.GetRange );
    Assert( (iSchemeRange <= MaxSchemeRange) or (iHL is TSynMultiSyn) );
  end else
    iSchemeRange := 0;
  { checks the limit of nested MultiSyns }
  Assert( iSchemeRange shr ((MaxNestedMultiSyn -1)*SchemeIndexSize + SchemeRangeSize) = 0 );
  iSchemeRange := (iSchemeRange shl SchemeIndexSize) or iSchemeIndex;
  Result := pointer(iSchemeRange);
end;

function TSynMultiSyn.GetToken: string;
begin
  if DefaultHighlighter = nil then
    Result := fLine
  else
    Result := Copy( fLine, fTokenPos +1, fRun - fTokenPos -1)
end;

function TSynMultiSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if fMarker <> nil then
    Result := Schemes[fMarker.fScheme].MarkerAttri
  else if CurrScheme >= 0 then
    Result := Schemes[CurrScheme].Highlighter.GetTokenAttribute
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetTokenAttribute
  else
    Result := nil;
end;

function TSynMultiSyn.GetTokenKind: integer;
begin
  if fMarker <> nil then
    Result := 0
  else if fCurrScheme >= 0 then
    Result := Schemes[fCurrScheme].Highlighter.GetTokenKind
  else if DefaultHighlighter <> nil then
    Result := DefaultHighlighter.GetTokenKind
  else
    Result := 0;
end;

function TSynMultiSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynMultiSyn.HookHighlighter(aHL: TSynCustomHighlighter);
begin
  aHL.FreeNotification( Self );
  aHL.HookAttrChangeEvent( DefHighlightChange );
end;

procedure TSynMultiSyn.Next;

  function FindMarker: TgmMarker;
  var
    c: integer;
  begin
    for c := 0 to fMarkers.Count -1 do
      if Markers[c].fStartPos >= fRun then
      begin
        Result := Markers[c];
        Exit;
      end;
    Result := nil;
  end;

var
  iToken: String;
  iHL: TSynCustomHighlighter;
begin
  if DefaultHighlighter = nil then begin
    if fRun > 1 then
      Inc( fRun )
    else
      fRun := Length(fLine) + 2;
    Exit;
  end;

  if (fNextMarker < fMarkers.Count) and (fRun >= Markers[fNextMarker].fStartPos) then begin
    fMarker := Markers[ fNextMarker ];
    Inc( fNextMarker );
    fTokenPos := fRun -1;
    Inc( fRun, fMarker.fMarkerLen );
    Exit;
  end;
  if (fRun = 1) then begin
    if fMarkers.Count = 0 then
      fTmpLine := fLine
    else
      fTmpLine := Copy( fLine, 1, Markers[fNextMarker].fStartPos -1 );
    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;
    iHL.SetLine( fTmpLine, fLineNumber );
  end else if fMarker <> nil then begin
    if fMarker.fIsOpenMarker then begin
      fTmpRange := DefaultHighlighter.GetRange;
      fCurrScheme := fMarker.fScheme;
      Schemes[CurrScheme].Highlighter.ResetRange;  //GBN 31/01/2002 - >From Flavio
    end else begin
      fCurrScheme := -1;
      DefaultHighlighter.SetRange( fTmpRange );
    end;
    fMarker := nil;
    {}
    if fNextMarker < fMarkers.Count then
      fTmpLine := Copy( fLine, fRun, Markers[fNextMarker].fStartPos - fRun  )
    else
      fTmpLine := Copy( fLine, fRun, MaxInt );
    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;
    iHL.SetLine( fTmpLine, fLineNumber );
  end else begin
    if CurrScheme >= 0 then
      iHL := Schemes[CurrScheme].Highlighter
    else
      iHL := DefaultHighlighter;
    iHL.Next;
  end;

  fTokenPos := iHL.GetTokenPos;
  iToken := iHL.GetToken;
  if fNextMarker > 0 then begin
    with Markers[ fNextMarker -1 ] do
      Inc( fTokenPos, fStartPos + fMarkerLen -1 );
  end;
  Inc( fRun, (fTokenPos - fRun +1) + Length(iToken) );
end;

procedure TSynMultiSyn.Notification(aComp: TComponent; aOp: TOperation);
var
  cScheme: integer;
begin
  inherited;
  if (aOp = opRemove) and (csDestroying in aComp.ComponentState) and
    (aComp is TSynCustomHighlighter)
  then begin
    if DefaultHighlighter = aComp then
      DefaultHighlighter := nil;
    for cScheme := 0 to Schemes.Count -1 do
      if Schemes[ cScheme ].Highlighter = aComp then
        Schemes[ cScheme ].Highlighter := nil;
  end;
end;

procedure TSynMultiSyn.ResetRange;
begin
  fCurrScheme := -1;
  fTmpRange := nil;
  //GBN 31/02/2002 - From Flavio
  DefaultHighlighter.ResetRange;
end;

procedure TSynMultiSyn.SetDefaultHighlighter(
  const Value: TSynCustomHighLighter);
const
  sDefaultHlSetToSelf = 'It is not good to set the DefaultHighlighter '+
    'of a SynMultiSyn to the SynMultiSyn itself. Please do not try this again';
begin
  if DefaultHighlighter <> Value then begin
    if Value = Self then
      raise Exception.Create( sDefaultHlSetToSelf );
    if DefaultHighlighter <> nil then
      UnhookHighlighter( DefaultHighlighter );
    fDefaultHighlighter := Value;
    if DefaultHighlighter <> nil then
      HookHighlighter( DefaultHighlighter );
    { yes, it's necessary }
    if not( csDestroying in ComponentState ) then
      DefHighlightChange( Self );
  end;
end;

//GBN 31/01/2002 - Start
procedure TSynMultiSyn.DoCheckMarker(Scheme:TgmScheme; StartPos, MarkerLen: Integer; const MarkerText: String; Start: Boolean);
var
  aStartPos: Integer;
  aMarkerLen: Integer;
  aMarkerText: String;
begin
  aStartPos:=StartPos;
  aMarkerLen:=MarkerLen;
  aMarkerText:=MarkerText;
  if (Start) and Assigned(Scheme.OnCheckStartMarker) then
    Scheme.OnCheckStartMarker(Self,aStartPos,aMarkerLen,aMarkerText)
  else if (not Start) and Assigned(Scheme.OnCheckEndMarker) then
    Scheme.OnCheckEndMarker(Self,aStartPos,aMarkerLen,aMarkerText);
  if (aMarkerText<>'') and (aMarkerLen>0) then
    begin
    fMarkers.Add(TgmMarker.Create(Scheme.Index, aStartPos, aMarkerLen,Start,aMarkerText));
    end;
end;
//GBN 31/01/2002 - End

procedure TSynMultiSyn.SetLine(NewValue: string; LineNumber: Integer);
var
  iParser: TRegExpr;
  iScheme: TgmScheme;
  iExpr: String;
  iLine: String;
  iEaten: integer;
  cScheme: integer;
begin
  ClearMarkers;

  iParser := TRegExpr.Create;
  try
    iEaten := 0;
    iLine := NewValue;
    if CurrScheme >= 0
    then
      iScheme := fSchemes[ CurrScheme ]
    else
      iScheme := nil;
    while iLine <> '' do
      if iScheme <> nil then begin
        iParser.Expression := iScheme.ConvertExpression( iScheme.EndExpr );
        if iParser.Exec( iScheme.ConvertExpression( iLine ) ) then begin
          iExpr := Copy( NewValue, iParser.MatchPos[0] + iEaten, iParser.MatchLen[0] );
          //GBN 31/01/2002 - Start
          DoCheckMarker(iScheme, iParser.MatchPos[0] + iEaten, iParser.MatchLen[0],iExpr,False);
          //fMarkers.Add( TgmMarker.Create( iScheme.Index, iParser.MatchPos[0] + iEaten,
          //  iParser.MatchLen[0], False, iExpr ) );
          //GBN 31/01/2002 - End
          Delete( iLine, 1, iParser.MatchPos[0] -1 + iParser.MatchLen[0] );
          Inc( iEaten, iParser.MatchPos[0] -1 + iParser.MatchLen[0] );
          iScheme := nil;
        end else
          break;
      end else begin
        for cScheme := 0 to Schemes.Count -1 do begin
          iScheme := Schemes[ cScheme ];
          if (iScheme.StartExpr = '') or (iScheme.EndExpr = '') or
            (iScheme.Highlighter = nil) or (not iScheme.Highlighter.Enabled) then
          begin
            continue;
          end;
          iParser.Expression := iScheme.ConvertExpression( iScheme.StartExpr );
          if iParser.Exec( iScheme.ConvertExpression( iLine ) ) then begin
            iExpr := Copy( NewValue, iParser.MatchPos[0] + iEaten, iParser.MatchLen[0] );
            //GBN 31/01/2002 - Start
            DoCheckMarker(iScheme, iParser.MatchPos[0] + iEaten, iParser.MatchLen[0],iExpr,True);
            //fMarkers.Add( TgmMarker.Create( cScheme, iParser.MatchPos[0] + iEaten,
            //  iParser.MatchLen[0], True, iExpr ) );
            //GBN 31/01/2002 - End
            Delete( iLine, 1, iParser.MatchPos[0] -1 + iParser.MatchLen[0] );
            Inc( iEaten, iParser.MatchPos[0] -1 + iParser.MatchLen[0] );
            break;
          end;
        end; {for}
        if cScheme >= Schemes.Count then
          break;
      end; {else}

  finally
    iParser.Free;
  end;

  if LineNumber <> fLineNumber +1 then
    fTmpRange := nil;
  fLineNumber := LineNumber;
  fLine := NewValue;
  fMarker := nil;
  fRun := 1;
  fTokenPos := 0;
  fNextMarker := 0;
  Next;
end;

procedure TSynMultiSyn.SetRange(Value: Pointer);
var
	iSchemeRange: integer;
begin
	if Value = nil then
		Exit;
	iSchemeRange := integer(Value);
	fCurrScheme := (iSchemeRange and MaxSchemeCount) -2;
	iSchemeRange := iSchemeRange shr SchemeIndexSize;
	if (CurrScheme < 0) then begin
		if DefaultHighlighter <> nil then
			DefaultHighlighter.SetRange( pointer(iSchemeRange) );
	end else begin
		Schemes[CurrScheme].Highlighter.SetRange( pointer(iSchemeRange) );
	end;
end;

procedure TSynMultiSyn.SetSchemes(const Value: TgmSchemes);
begin
	fSchemes.Assign(Value);
end;

procedure TSynMultiSyn.UnhookHighlighter(aHL: TSynCustomHighlighter);
begin
	if csDestroying in aHL.ComponentState then
		Exit;
	aHL.UnhookAttrChangeEvent( DefHighlightChange );
{$IFDEF SYN_COMPILER_5_UP}
	aHL.RemoveFreeNotification( Self );
{$ENDIF}
end;

function TSynMultiSyn.GetSampleSource: string;
begin
	Result := fSampleSource;
end;

procedure TSynMultiSyn.SetSampleSource(Value: string);
begin
	fSampleSource := Value;
end;

{ TgmSchemes }

constructor TgmSchemes.Create(aOwner: TSynMultiSyn);
begin
	inherited Create(TgmScheme);
	fOwner := aOwner;
end;

function TgmSchemes.GetItems(Index: integer): TgmScheme;
begin
	Result := inherited Items[Index] as TgmScheme;
end;

{$IFDEF SYN_COMPILER_3_UP}
function TgmSchemes.GetOwner: TPersistent;
begin
  Result := fOwner;
end;
{$ENDIF}

procedure TgmSchemes.SetItems(Index: integer; const Value: TgmScheme);
begin
  inherited Items[Index] := Value;
end;

{$IFDEF SYN_COMPILER_3_UP}
procedure TgmSchemes.Update(Item: TCollectionItem);
begin
  fOwner.DefHighlightChange( Item );
end;
{$ENDIF}

{ TgmScheme }

function TgmScheme.ConvertExpression(const Value: String): String;
begin
  if not CaseSensitive then
    Result := AnsiUpperCase(Value)
  else
    Result := Value;
end;

constructor TgmScheme.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fCaseSensitive := True;
  fMarkerAttri := TSynHighlighterAttributes.Create(SYNS_AttrMarker);
  fMarkerAttri.OnChange := MarkerAttriChanged;
  MarkerAttri.Background := clYellow;
  MarkerAttri.Style := [fsBold];
  MarkerAttri.InternalSaveDefaultValues;
end;

destructor TgmScheme.Destroy;
begin
  fMarkerAttri.Free;
  { unhook notification handlers }
  Highlighter := nil;
  inherited Destroy;
end;

{$IFDEF SYN_COMPILER_3_UP}
function TgmScheme.GetDisplayName: String;
begin
  if SchemeName <> '' then
    Result := SchemeName
  else
    Result := inherited GetDisplayName;
end;
{$ENDIF SYN_COMPILER_3_UP}

procedure TgmScheme.MarkerAttriChanged(Sender: TObject);
begin
  Changed( False );
end;

procedure TgmScheme.SetCaseSensitive(const Value: Boolean);
begin
  if fCaseSensitive <> Value then
  begin
    fCaseSensitive := Value;
    Changed( False );
  end;
end;

{$IFDEF SYN_COMPILER_3_UP}
procedure TgmScheme.SetDisplayName(const Value: String);
begin
  SchemeName := Value;
end;
{$ENDIF SYN_COMPILER_3_UP}

procedure TgmScheme.SetEndExpr(const Value: string);
var OldValue: String; //GBN 31/01/2002 - From Flavio
begin
  if fEndExpr <> Value then
	begin
    OldValue := fEndExpr; //GBN 31/01/2002 - From Flavio
    fEndExpr := Value;
    //GBN 31/01/2002 - From Flavio
    if ConvertExpression( OldValue ) <> ConvertExpression( Value ) then
      Changed( False );
  end;
end;

procedure TgmScheme.SetHighlighter(const Value: TSynCustomHighLighter);
var
  iOwner: TSynMultiSyn;
begin
  if Highlighter <> Value then
  begin
    iOwner := TgmSchemes(Collection).fOwner;
    if Highlighter <> nil then
      iOwner.UnhookHighlighter( Highlighter );
    fHighlighter := Value;
    if (Highlighter <> nil) and (Highlighter <> TgmSchemes(Collection).fOwner) then
      iOwner.HookHighlighter( Highlighter );
    Changed(False);
  end;
end;

procedure TgmScheme.SetMarkerAttri(const Value: TSynHighlighterAttributes);
begin
  fMarkerAttri.Assign(Value);
end;

procedure TgmScheme.SetStartExpr(const Value: string);
var OldValue: String; //GBN 31/01/2002 - From Flavio
begin
  if fStartExpr <> Value then
  begin
    OldValue   := fStartExpr; //GBN 31/01/2002 - From Flavio
    fStartExpr := Value;
    //GBN 31/01/2002 - From Flavio
    if ConvertExpression( Value ) <> ConvertExpression( OldValue ) then
      Changed( False );
  end;
end;

end.

