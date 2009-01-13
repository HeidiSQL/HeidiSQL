
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntDB;

{$INCLUDE compilers.inc}

interface

uses
  Classes, DB;

type
{TNT-WARN TDateTimeField}
  TTntDateTimeField = class(TDateTimeField{TNT-ALLOW TDateTimeField})
  protected
    procedure SetAsString(const Value: AnsiString); override;
  end;

{TNT-WARN TDateField}
  TTntDateField = class(TDateField{TNT-ALLOW TDateField})
  protected
    procedure SetAsString(const Value: AnsiString); override;
  end;

{TNT-WARN TTimeField}
  TTntTimeField = class(TTimeField{TNT-ALLOW TTimeField})
  protected
    procedure SetAsString(const Value: AnsiString); override;
  end;

  TFieldGetWideTextEvent = procedure(Sender: TField; var Text: WideString;
    DoDisplayText: Boolean) of object;
  TFieldSetWideTextEvent = procedure(Sender: TField; const Text: WideString) of object;

  IWideStringField = interface
    ['{679C5F1A-4356-4696-A8F3-9C7C6970A9F6}']
    {$IFNDEF COMPILER_10_UP}
    function GetAsWideString: WideString;
    procedure SetAsWideString(const Value: WideString);
    {$ENDIF}
    function GetWideDisplayText: WideString;
    function GetWideEditText: WideString;
    procedure SetWideEditText(const Value: WideString);
    //--
    {$IFNDEF COMPILER_10_UP}
    property AsWideString: WideString read GetAsWideString write SetAsWideString{inherited};
    {$ENDIF}
    property WideDisplayText: WideString read GetWideDisplayText;
    property WideText: WideString read GetWideEditText write SetWideEditText;
  end;

{TNT-WARN TWideStringField}
  TTntWideStringField = class(TWideStringField{TNT-ALLOW TWideStringField}, IWideStringField)
  private
    FOnGetText: TFieldGetWideTextEvent;
    FOnSetText: TFieldSetWideTextEvent;
    procedure SetOnGetText(const Value: TFieldGetWideTextEvent);
    procedure SetOnSetText(const Value: TFieldSetWideTextEvent);
    procedure LegacyGetText(Sender: TField; var AnsiText: AnsiString; DoDisplayText: Boolean);
    procedure LegacySetText(Sender: TField; const AnsiText: AnsiString);
    function GetWideDisplayText: WideString;
    function GetWideEditText: WideString;
    procedure SetWideEditText(const Value: WideString);
  protected
    {$IFNDEF COMPILER_10_UP}
    function GetAsWideString: WideString;
    {$ENDIF}
  public
    property Value: WideString read GetAsWideString write SetAsWideString;
    property DisplayText{TNT-ALLOW DisplayText}: WideString read GetWideDisplayText;
    property Text: WideString read GetWideEditText write SetWideEditText;
    {$IFNDEF COMPILER_10_UP}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    {$ENDIF}
    property WideDisplayText: WideString read GetWideDisplayText;
    property WideText: WideString read GetWideEditText write SetWideEditText;
  published
    property OnGetText: TFieldGetWideTextEvent read FOnGetText write SetOnGetText;
    property OnSetText: TFieldSetWideTextEvent read FOnSetText write SetOnSetText;
  end;

  TTntStringFieldEncodingMode = (emNone, emUTF8, emUTF7, emFixedCodePage, emFixedCodePage_Safe);

  //-------------------------------------------------------------------------------------------
  // Comments on TTntStringFieldEncodingMode:
  //
  //  emNone               - Works like TStringField.
  //  emUTF8               - Should work well most databases.
  //  emUTF7               - Almost guaranteed to work with any database. Wasteful in database space.
  //  emFixedCodePage      - Only tested it with Access 97, which doesn't support Unicode.
  //  emFixedCodePage_Safe - Like emFixedCodePage but uses char<=#128.  Wasteful in database space.
  //
  //  Only emUTF8 and emUTF7 fully support Unicode.
  //-------------------------------------------------------------------------------------------

  TTntStringFieldCodePageEnum = (fcpOther,
    fcpThai, fcpJapanese, fcpSimplifiedChinese, fcpTraditionalChinese, fcpKorean,
    fcpCentralEuropean, fcpCyrillic, fcpLatinWestern, fcpGreek, fcpTurkish,
    fcpHebrew, fcpArabic, fcpBaltic, fcpVietnamese);

const
  TntStringFieldCodePageEnumMap: array[TTntStringFieldCodePageEnum] of Word = (0,
    874, 932, 936, 950, 949,
    1250, 1251, 1252, 1253, 1254,
    1255, 1256, 1257, 1258);

type
{TNT-WARN TStringField}
  TTntStringField = class(TStringField{TNT-ALLOW TStringField},IWideStringField)
  private
    FOnGetText: TFieldGetWideTextEvent;
    FOnSetText: TFieldSetWideTextEvent;
    FEncodingMode: TTntStringFieldEncodingMode;
    FFixedCodePage: Word;
    FRawVariantAccess: Boolean;
    procedure SetOnGetText(const Value: TFieldGetWideTextEvent);
    procedure SetOnSetText(const Value: TFieldSetWideTextEvent);
    procedure LegacyGetText(Sender: TField; var AnsiText: AnsiString; DoDisplayText: Boolean);
    procedure LegacySetText(Sender: TField; const AnsiText: AnsiString);
    function GetWideDisplayText: WideString;
    function GetWideEditText: WideString;
    procedure SetWideEditText(const Value: WideString);
    function GetFixedCodePageEnum: TTntStringFieldCodePageEnum;
    procedure SetFixedCodePageEnum(const Value: TTntStringFieldCodePageEnum);
    function IsFixedCodePageStored: Boolean;
  protected
    {$IFDEF COMPILER_10_UP}
    function GetAsWideString: WideString; override;
    procedure SetAsWideString(const Value: WideString); override;
    {$ELSE}
    function GetAsWideString: WideString; virtual;
    procedure SetAsWideString(const Value: WideString); virtual;
    {$ENDIF}
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const Value: Variant); override;
    function GetAsString: string{TNT-ALLOW string}; override;
    procedure SetAsString(const Value: string{TNT-ALLOW string}); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: WideString read GetAsWideString write SetAsWideString;
    property DisplayText{TNT-ALLOW DisplayText}: WideString read GetWideDisplayText;
    property Text: WideString read GetWideEditText write SetWideEditText;
    {$IFNDEF COMPILER_10_UP}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    {$ENDIF}
    property WideDisplayText: WideString read GetWideDisplayText;
    property WideText: WideString read GetWideEditText write SetWideEditText;
  published
    property EncodingMode: TTntStringFieldEncodingMode read FEncodingMode write FEncodingMode default emUTF8;
    property FixedCodePageEnum: TTntStringFieldCodePageEnum read GetFixedCodePageEnum write SetFixedCodePageEnum stored False;
    property FixedCodePage: Word read FFixedCodePage write FFixedCodePage stored IsFixedCodePageStored;
    property RawVariantAccess: Boolean read FRawVariantAccess write FRawVariantAccess default False;
    property OnGetText: TFieldGetWideTextEvent read FOnGetText write SetOnGetText;
    property OnSetText: TFieldSetWideTextEvent read FOnSetText write SetOnSetText;
  end;

//======================
type
{TNT-WARN TMemoField}
  TTntMemoField = class(TMemoField{TNT-ALLOW TMemoField}, IWideStringField)
  private
    FOnGetText: TFieldGetWideTextEvent;
    FOnSetText: TFieldSetWideTextEvent;
    FEncodingMode: TTntStringFieldEncodingMode;
    FFixedCodePage: Word;
    FRawVariantAccess: Boolean;
    procedure SetOnGetText(const Value: TFieldGetWideTextEvent);
    procedure SetOnSetText(const Value: TFieldSetWideTextEvent);
    procedure LegacyGetText(Sender: TField; var AnsiText: AnsiString; DoDisplayText: Boolean);
    procedure LegacySetText(Sender: TField; const AnsiText: AnsiString);
    function GetWideDisplayText: WideString;
    function GetWideEditText: WideString;
    procedure SetWideEditText(const Value: WideString);
    function GetFixedCodePageEnum: TTntStringFieldCodePageEnum;
    procedure SetFixedCodePageEnum(const Value: TTntStringFieldCodePageEnum);
    function IsFixedCodePageStored: Boolean;
  protected
    {$IFDEF COMPILER_10_UP}
    function GetAsWideString: WideString; override;
    procedure SetAsWideString(const Value: WideString); override;
    {$ELSE}
    function GetAsWideString: WideString; virtual;
    procedure SetAsWideString(const Value: WideString); virtual;
    {$ENDIF}
    function GetAsVariant: Variant; override;
    procedure SetVarValue(const Value: Variant); override;
    function GetAsString: string{TNT-ALLOW string}; override;
    procedure SetAsString(const Value: string{TNT-ALLOW string}); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: WideString read GetAsWideString write SetAsWideString;
    property DisplayText{TNT-ALLOW DisplayText}: WideString read GetWideDisplayText;
    property Text: WideString read GetWideEditText write SetWideEditText;
    {$IFNDEF COMPILER_10_UP}
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    {$ENDIF}
    property WideDisplayText: WideString read GetWideDisplayText;
    property WideText: WideString read GetWideEditText write SetWideEditText;
  published
    property EncodingMode: TTntStringFieldEncodingMode read FEncodingMode write FEncodingMode default emUTF8;
    property FixedCodePageEnum: TTntStringFieldCodePageEnum read GetFixedCodePageEnum write SetFixedCodePageEnum stored False;
    property FixedCodePage: Word read FFixedCodePage write FFixedCodePage stored IsFixedCodePageStored;
    property RawVariantAccess: Boolean read FRawVariantAccess write FRawVariantAccess default False;
    property OnGetText: TFieldGetWideTextEvent read FOnGetText write SetOnGetText;
    property OnSetText: TFieldSetWideTextEvent read FOnSetText write SetOnSetText;
  end;

//======================
function GetTntFieldClass(FieldClass: TFieldClass): TFieldClass;

function GetWideDisplayName(Field: TField): WideString; deprecated; // for Unicode-enabled functionality, use Delphi 2006 or newer
function GetWideDisplayLabel(Field: TField): WideString; deprecated; // for Unicode-enabled functionality, use Delphi 2006 or newer
procedure SetWideDisplayLabel(Field: TField; const Value: WideString); deprecated; // for Unicode-enabled functionality, use Delphi 2006 or newer

{TNT-WARN AsString}
{TNT-WARN DisplayText}

function GetAsWideString(Field: TField): WideString;
procedure SetAsWideString(Field: TField; const Value: WideString);

function GetWideDisplayText(Field: TField): WideString;

function GetWideText(Field: TField): WideString;
procedure SetWideText(Field: TField; const Value: WideString);

procedure RegisterTntFields;

{ TTntWideStringField / TTntStringField common handlers }
procedure TntWideStringField_GetWideText(Field: TField;
  var Text: WideString; DoDisplayText: Boolean);
function TntWideStringField_GetWideDisplayText(Field: TField;
  OnGetText: TFieldGetWideTextEvent): WideString;
function TntWideStringField_GetWideEditText(Field: TField;
  OnGetText: TFieldGetWideTextEvent): WideString;
procedure TntWideStringField_SetWideText(Field: TField;
  const Value: WideString);
procedure TntWideStringField_SetWideEditText(Field: TField;
  OnSetText: TFieldSetWideTextEvent; const Value: WideString);


implementation

uses
  SysUtils, MaskUtils, Variants, Contnrs, TntSystem, TntSysUtils;

function GetTntFieldClass(FieldClass: TFieldClass): TFieldClass;
begin
  if FieldClass = TDateTimeField{TNT-ALLOW TDateTimeField} then
    Result := TTntDateTimeField
  else if FieldClass = TDateField{TNT-ALLOW TDateField} then
    Result := TTntDateField
  else if FieldClass = TTimeField{TNT-ALLOW TTimeField} then
    Result := TTntTimeField
  else if FieldClass = TWideStringField{TNT-ALLOW TWideStringField} then
    Result := TTntWideStringField
  else if FieldClass = TStringField{TNT-ALLOW TStringField} then
    Result := TTntStringField
  else
    Result := FieldClass;
end;

function GetWideDisplayName(Field: TField): WideString;
begin
  Result := Field.DisplayName;
end;

function GetWideDisplayLabel(Field: TField): WideString;
begin
  Result := Field.DisplayLabel;
end;

procedure SetWideDisplayLabel(Field: TField; const Value: WideString);
begin
  Field.DisplayLabel := Value;
end;

function GetAsWideString(Field: TField): WideString;
{$IFDEF COMPILER_10_UP}
begin
  if (Field.ClassType = TMemoField{TNT-ALLOW TMemoField}) then
    Result := VarToWideStr(Field.AsVariant) { works for NexusDB BLOB Wide }
  else
    Result := Field.AsWideString
end;
{$ELSE}
var
  WideField: IWideStringField;
begin
  if Field.GetInterface(IWideStringField, WideField) then
    Result := WideField.AsWideString
  else if (Field is TWideStringField{TNT-ALLOW TWideStringField}) then
  begin
    if Field.IsNull then
      // This fixes a bug in TWideStringField.GetAsWideString which does not handle Null at all.
      Result := ''
    else
      Result := TWideStringField{TNT-ALLOW TWideStringField}(Field).Value
  end else if (Field is TMemoField{TNT-ALLOW TMemoField}) then
    Result := VarToWideStr(Field.AsVariant) { works for NexusDB BLOB Wide }
  else
    Result := Field.AsString{TNT-ALLOW AsString};
end;
{$ENDIF}

procedure SetAsWideString(Field: TField; const Value: WideString);
{$IFDEF COMPILER_10_UP}
begin
  if (Field.ClassType = TMemoField{TNT-ALLOW TMemoField}) then
    Field.AsVariant := Value { works for NexusDB BLOB Wide }
  else
    Field.AsWideString := Value;
end;
{$ELSE}
var
  WideField: IWideStringField;
begin
  if Field.GetInterface(IWideStringField, WideField) then
    WideField.AsWideString := Value
  else if (Field is TWideStringField{TNT-ALLOW TWideStringField}) then
    TWideStringField{TNT-ALLOW TWideStringField}(Field).Value := Value
  else if (Field is TMemoField{TNT-ALLOW TMemoField}) then
    Field.AsVariant := Value { works for NexusDB BLOB Wide }
  else
    Field.AsString{TNT-ALLOW AsString} := Value;
end;
{$ENDIF}

function GetWideDisplayText(Field: TField): WideString;
var
  WideField: IWideStringField;
begin
  if Field.GetInterface(IWideStringField, WideField) then
    Result := WideField.WideDisplayText
  else if (Field is TWideStringField{TNT-ALLOW TWideStringField})
  and (not Assigned(Field.OnGetText)) then
    Result := GetAsWideString(Field)
  else
    Result := Field.DisplayText{TNT-ALLOW DisplayText};
end;

function GetWideText(Field: TField): WideString;
var
  WideField: IWideStringField;
begin
  if Field.GetInterface(IWideStringField, WideField) then
    Result := WideField.WideText
  else if (Field is TWideStringField{TNT-ALLOW TWideStringField})
  and (not Assigned(Field.OnGetText)) then
    Result := GetAsWideString(Field)
  else
    Result := Field.Text;
end;

procedure SetWideText(Field: TField; const Value: WideString);
var
  WideField: IWideStringField;
begin
  if Field.GetInterface(IWideStringField, WideField) then
    WideField.WideText := Value
  else if (Field is TWideStringField{TNT-ALLOW TWideStringField})
  and (not Assigned(Field.OnSetText)) then
    SetAsWideString(Field, Value)
  else
    Field.Text := Value
end;

{ TTntDateTimeField }

procedure TTntDateTimeField.SetAsString(const Value: AnsiString);
begin
  if Value = '' then
    inherited
  else
    SetAsDateTime(TntStrToDateTime(Value));
end;

{ TTntDateField }

procedure TTntDateField.SetAsString(const Value: AnsiString);
begin
  if Value = '' then
    inherited
  else
    SetAsDateTime(TntStrToDate(Value));
end;

{ TTntTimeField }

procedure TTntTimeField.SetAsString(const Value: AnsiString);
begin
  if Value = '' then
    inherited
  else
    SetAsDateTime(TntStrToTime(Value));
end;

{ TTntWideStringField / TTntStringField common handlers }

procedure TntWideStringField_LegacyGetText(Sender: TField; OnGetText: TFieldGetWideTextEvent;
  var AnsiText: AnsiString; DoDisplayText: Boolean);
var
  WideText: WideString;
begin
  if Assigned(OnGetText) then begin
    WideText := AnsiText;
    OnGetText(Sender, WideText, DoDisplayText);
    AnsiText := WideText;
  end;
end;

procedure TntWideStringField_LegacySetText(Sender: TField; OnSetText: TFieldSetWideTextEvent;
  const AnsiText: AnsiString);
begin
  if Assigned(OnSetText) then
    OnSetText(Sender, AnsiText);
end;

procedure TntWideStringField_GetWideText(Field: TField;
  var Text: WideString; DoDisplayText: Boolean);
var
  WideStringField: IWideStringField;
begin
  Field.GetInterface(IWideStringField, WideStringField);
  Assert(WideStringField <> nil);
  if DoDisplayText and (Field.EditMaskPtr <> '') then
    { to gain the mask, we lose Unicode! }
    Text := FormatMaskText(Field.EditMaskPtr, GetAsWideString(Field))
  else
    Text := GetAsWideString(Field);
end;

function TntWideStringField_GetWideDisplayText(Field: TField;
  OnGetText: TFieldGetWideTextEvent): WideString;
begin
  Result := '';
  if Assigned(OnGetText) then
    OnGetText(Field, Result, True)
  else if Assigned(Field.OnGetText) then
    Result := Field.DisplayText{TNT-ALLOW DisplayText} {we lose Unicode to handle this event}
  else
    TntWideStringField_GetWideText(Field, Result, True);
end;

function TntWideStringField_GetWideEditText(Field: TField;
  OnGetText: TFieldGetWideTextEvent): WideString;
begin
  Result := '';
  if Assigned(OnGetText) then
    OnGetText(Field, Result, False)
  else if Assigned(Field.OnGetText) then
    Result := Field.Text {we lose Unicode to handle this event}
  else
    TntWideStringField_GetWideText(Field, Result, False);
end;

procedure TntWideStringField_SetWideText(Field: TField;
  const Value: WideString);
{$IFDEF COMPILER_10_UP}
begin
  Field.AsWideString := Value;
end;
{$ELSE}
var
  WideStringField: IWideStringField;
begin
  Field.GetInterface(IWideStringField, WideStringField);
  Assert(WideStringField <> nil);
  WideStringField.SetAsWideString(Value);
end;
{$ENDIF}

procedure TntWideStringField_SetWideEditText(Field: TField;
  OnSetText: TFieldSetWideTextEvent; const Value: WideString);
begin
  if Assigned(OnSetText) then
    OnSetText(Field, Value)
  else if Assigned(Field.OnSetText) then
    Field.Text := Value {we lose Unicode to handle this event}
  else
    TntWideStringField_SetWideText(Field, Value);
end;

{ TTntWideStringField }

{$IFNDEF COMPILER_10_UP}
function TTntWideStringField.GetAsWideString: WideString;
begin
  if not GetData(@Result, False) then
    Result := ''; {fixes a bug in inherited which has unpredictable results for NULL}
end;
{$ENDIF}

procedure TTntWideStringField.LegacyGetText(Sender: TField; var AnsiText: AnsiString;
  DoDisplayText: Boolean);
begin
  TntWideStringField_LegacyGetText(Sender, OnGetText, AnsiText, DoDisplayText);
end;

procedure TTntWideStringField.LegacySetText(Sender: TField; const AnsiText: AnsiString);
begin
  TntWideStringField_LegacySetText(Sender, OnSetText, AnsiText);
end;

procedure TTntWideStringField.SetOnGetText(const Value: TFieldGetWideTextEvent);
begin
  FOnGetText := Value;
  if Assigned(OnGetText) then
    inherited OnGetText := LegacyGetText
  else
    inherited OnGetText := nil;
end;

procedure TTntWideStringField.SetOnSetText(const Value: TFieldSetWideTextEvent);
begin
  FOnSetText := Value;
  if Assigned(OnSetText) then
    inherited OnSetText := LegacySetText
  else
    inherited OnSetText := nil;
end;

function TTntWideStringField.GetWideDisplayText: WideString;
begin
  Result := TntWideStringField_GetWideDisplayText(Self, OnGetText);
end;

function TTntWideStringField.GetWideEditText: WideString;
begin
  Result := TntWideStringField_GetWideEditText(Self, OnGetText);
end;

procedure TTntWideStringField.SetWideEditText(const Value: WideString);
begin
  TntWideStringField_SetWideEditText(Self, OnSetText, Value);
end;

(* This stuffing method works with CJK codepages - intended to store accented characters in Access 97 *)

function SafeStringToWideStringEx(const S: AnsiString; Codepage: Word): WideString;
var
  R: AnsiString;
  i: Integer;
begin
  R := '';
  i := 1;
  while i <= Length(S) do
  begin
    if (S[i] = #128) then
    begin
      Inc(i);
      if S[i] = #128 then
        R := R + #128
      else
        R := R + Chr(Ord(S[i]) + 128);
      Inc(i);
    end
    else
    begin
      R := R + S[I];
      Inc(i);
    end;
  end;
  Result := StringToWideStringEx(R, CodePage);
end;

function SafeWideStringToStringEx(const W: WideString; CodePage: Word): AnsiString;
var
  TempS: AnsiString;
  i: integer;
begin
  TempS := WideStringToStringEx(W, CodePage);
  Result := '';
  for i := 1 to Length(TempS) do
  begin
    if TempS[i] > #128 then
      Result := Result + #128 + Chr(Ord(TempS[i]) - 128)
    else if TempS[i] = #128 then
      Result := Result + #128 + #128
    else
      Result := Result + TempS[i];
  end;
end;

{ TTntStringField }

constructor TTntStringField.Create(AOwner: TComponent);
begin
  inherited;
  FEncodingMode := emUTF8;
  FFixedCodePage := TntStringFieldCodePageEnumMap[fcpLatinWestern]
end;

function TTntStringField.GetFixedCodePageEnum: TTntStringFieldCodePageEnum;
var
  i: TTntStringFieldCodePageEnum;
begin
  Result := fcpOther;
  for i := Low(TntStringFieldCodePageEnumMap) to High(TntStringFieldCodePageEnumMap) do begin
    if TntStringFieldCodePageEnumMap[i] = FixedCodePage then begin
      Result := i;
      Break; {found it}
    end;
  end;
end;

procedure TTntStringField.SetFixedCodePageEnum(const Value: TTntStringFieldCodePageEnum);
begin
  if (Value <> fcpOther) then
    FixedCodePage := TntStringFieldCodePageEnumMap[Value];
end;

function TTntStringField.GetAsVariant: Variant;
begin
  if RawVariantAccess then
    Result := inherited GetAsVariant
  else if IsNull then
    Result := Null
  else
    Result := GetAsWideString;
end;

procedure TTntStringField.SetVarValue(const Value: Variant);
begin
  if RawVariantAccess then
    inherited
  else
    SetAsWideString(Value);
end;

function TTntStringField.GetAsWideString: WideString;
begin
  case EncodingMode of
    emNone:               Result := (inherited GetAsString);
    emUTF8:               Result := UTF8ToWideString(inherited GetAsString);
    emUTF7:             try
                          Result := UTF7ToWideString(inherited GetAsString);
                        except
                          Result := inherited GetAsString;
                        end;
    emFixedCodePage:      Result := StringToWideStringEx(inherited GetAsString, FixedCodePage);
    emFixedCodePage_Safe: Result := SafeStringToWideStringEx(inherited GetAsString, FixedCodePage);
    else
      raise ETntInternalError.Create('Internal Error: Unexpected EncodingMode');
  end;
end;

procedure TTntStringField.SetAsWideString(const Value: WideString);
begin
  case EncodingMode of
    emNone:               inherited SetAsString(Value);
    emUTF8:               inherited SetAsString(WideStringToUTF8(Value));
    emUTF7:               inherited SetAsString(WideStringToUTF7(Value));
    emFixedCodePage:      inherited SetAsString(WideStringToStringEx(Value, FixedCodePage));
    emFixedCodePage_Safe: inherited SetAsString(SafeWideStringToStringEx(Value, FixedCodePage));
    else
      raise ETntInternalError.Create('Internal Error: Unexpected EncodingMode');
  end;
end;

function TTntStringField.GetAsString: string{TNT-ALLOW string};
begin
  if EncodingMode = emNone then
    Result := inherited GetAsString
  else
    Result := GetAsWideString;
end;

procedure TTntStringField.SetAsString(const Value: string{TNT-ALLOW string});
begin
  if EncodingMode = emNone then
    inherited SetAsString(Value)
  else
    SetAsWideString(Value);
end;

procedure TTntStringField.LegacyGetText(Sender: TField; var AnsiText: AnsiString;
  DoDisplayText: Boolean);
begin
  TntWideStringField_LegacyGetText(Sender, OnGetText, AnsiText, DoDisplayText);
end;

procedure TTntStringField.LegacySetText(Sender: TField; const AnsiText: AnsiString);
begin
  TntWideStringField_LegacySetText(Sender, OnSetText, AnsiText);
end;

procedure TTntStringField.SetOnGetText(const Value: TFieldGetWideTextEvent);
begin
  FOnGetText := Value;
  if Assigned(OnGetText) then
    inherited OnGetText := LegacyGetText
  else
    inherited OnGetText := nil;
end;

procedure TTntStringField.SetOnSetText(const Value: TFieldSetWideTextEvent);
begin
  FOnSetText := Value;
  if Assigned(OnSetText) then
    inherited OnSetText := LegacySetText
  else
    inherited OnSetText := nil;
end;

function TTntStringField.GetWideDisplayText: WideString;
begin
  Result := TntWideStringField_GetWideDisplayText(Self, OnGetText)
end;

function TTntStringField.GetWideEditText: WideString;
begin
  Result := TntWideStringField_GetWideEditText(Self, OnGetText);
end;

procedure TTntStringField.SetWideEditText(const Value: WideString);
begin
  TntWideStringField_SetWideEditText(Self, OnSetText, Value);
end;

function TTntStringField.IsFixedCodePageStored: Boolean;
begin
  Result := EncodingMode = emFixedCodePage;
end;

//---------------------------------------------------------------------------------------------
{ TTntMemoField }

constructor TTntMemoField.Create(AOwner: TComponent);
begin
  inherited;
  FEncodingMode := emUTF8;
  FFixedCodePage := TntStringFieldCodePageEnumMap[fcpLatinWestern]
end;

function TTntMemoField.GetFixedCodePageEnum: TTntStringFieldCodePageEnum;
var
  i: TTntStringFieldCodePageEnum;
begin
  Result := fcpOther;
  for i := Low(TntStringFieldCodePageEnumMap) to High(TntStringFieldCodePageEnumMap) do begin
    if TntStringFieldCodePageEnumMap[i] = FixedCodePage then begin
      Result := i;
      Break; {found it}
    end;
  end;
end;

procedure TTntMemoField.SetFixedCodePageEnum(const Value: TTntStringFieldCodePageEnum);
begin
  if (Value <> fcpOther) then
    FixedCodePage := TntStringFieldCodePageEnumMap[Value];
end;

function TTntMemoField.GetAsVariant: Variant;
begin
  if RawVariantAccess then
    Result := inherited GetAsVariant
  else if IsNull then
    Result := Null
  else
    Result := GetAsWideString;
end;

procedure TTntMemoField.SetVarValue(const Value: Variant);
begin
  if RawVariantAccess then
    inherited
  else
    SetAsWideString(Value);
end;

function TTntMemoField.GetAsWideString: WideString;
begin
  case EncodingMode of
    emNone:               Result := (inherited GetAsString);
    emUTF8:               Result := UTF8ToWideString(inherited GetAsString);
    emUTF7:             try
                          Result := UTF7ToWideString(inherited GetAsString);
                        except
                          Result := inherited GetAsString;
                        end;
    emFixedCodePage:      Result := StringToWideStringEx(inherited GetAsString, FixedCodePage);
    emFixedCodePage_Safe: Result := SafeStringToWideStringEx(inherited GetAsString, FixedCodePage);
    else
      raise ETntInternalError.Create('Internal Error: Unexpected EncodingMode');
  end;
end;

procedure TTntMemoField.SetAsWideString(const Value: WideString);
begin
  case EncodingMode of
    emNone:               inherited SetAsString(Value);
    emUTF8:               inherited SetAsString(WideStringToUTF8(Value));
    emUTF7:               inherited SetAsString(WideStringToUTF7(Value));
    emFixedCodePage:      inherited SetAsString(WideStringToStringEx(Value, FixedCodePage));
    emFixedCodePage_Safe: inherited SetAsString(SafeWideStringToStringEx(Value, FixedCodePage));
    else
      raise ETntInternalError.Create('Internal Error: Unexpected EncodingMode');
  end;
end;

function TTntMemoField.GetAsString: string{TNT-ALLOW string};
begin
  if EncodingMode = emNone then
    Result := inherited GetAsString
  else
    Result := GetAsWideString;
end;

procedure TTntMemoField.SetAsString(const Value: string{TNT-ALLOW string});
begin
  if EncodingMode = emNone then
    inherited SetAsString(Value)
  else
    SetAsWideString(Value);
end;

procedure TTntMemoField.LegacyGetText(Sender: TField; var AnsiText: AnsiString;
  DoDisplayText: Boolean);
begin
  TntWideStringField_LegacyGetText(Sender, OnGetText, AnsiText, DoDisplayText);
end;

procedure TTntMemoField.LegacySetText(Sender: TField; const AnsiText: AnsiString);
begin
  TntWideStringField_LegacySetText(Sender, OnSetText, AnsiText);
end;

procedure TTntMemoField.SetOnGetText(const Value: TFieldGetWideTextEvent);
begin
  FOnGetText := Value;
  if Assigned(OnGetText) then
    inherited OnGetText := LegacyGetText
  else
    inherited OnGetText := nil;
end;

procedure TTntMemoField.SetOnSetText(const Value: TFieldSetWideTextEvent);
begin
  FOnSetText := Value;
  if Assigned(OnSetText) then
    inherited OnSetText := LegacySetText
  else
    inherited OnSetText := nil;
end;

function TTntMemoField.GetWideDisplayText: WideString;
begin
  Result := TntWideStringField_GetWideDisplayText(Self, OnGetText)
end;

function TTntMemoField.GetWideEditText: WideString;
begin
  Result := TntWideStringField_GetWideEditText(Self, OnGetText);
end;

procedure TTntMemoField.SetWideEditText(const Value: WideString);
begin
  TntWideStringField_SetWideEditText(Self, OnSetText, Value);
end;

function TTntMemoField.IsFixedCodePageStored: Boolean;
begin
  Result := EncodingMode = emFixedCodePage;
end;
//==================================================================
procedure RegisterTntFields;
begin
  RegisterFields([TTntDateTimeField]);
  RegisterFields([TTntDateField]);
  RegisterFields([TTntTimeField]);
  RegisterFields([TTntWideStringField]);
  RegisterFields([TTntStringField]);
  RegisterFields([TTntMemoField]);
end;

type PFieldClass = ^TFieldClass;

initialization
{$IFDEF TNT_FIELDS}
  PFieldClass(@DefaultFieldClasses[ftDate])^ := TTntDateField;
  PFieldClass(@DefaultFieldClasses[ftTime])^ := TTntTimeField;
  PFieldClass(@DefaultFieldClasses[ftDateTime])^ := TTntDateTimeField;
  PFieldClass(@DefaultFieldClasses[ftWideString])^ := TTntWideStringField;
  PFieldClass(@DefaultFieldClasses[ftString])^ := TTntStringField;
  PFieldClass(@DefaultFieldClasses[ftFixedChar])^ := TTntStringField;
{$ENDIF}

finalization

end.
