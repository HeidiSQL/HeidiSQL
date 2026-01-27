unit generic_types;

{$mode delphi}{$H+}

interface

uses fpjson, jsonparser, SysUtils, RegExpr, SynEditHighlighter, SynHighlighterSQL,
  Classes, Generics.Collections, Graphics, dbstructures;

type
  TThreeStateBoolean = (nbUnset, nbFalse, nbTrue);

  TJSONFloatNumberUntouched = class(TJSONFloatNumber)
  public
    function GetAsString: TJSONStringType; override;
  end;

  // Regular expression with support for European umlauts/accents as \w (word character)
  TRegExprUmlauts = class(TRegExpr)
    public
      constructor Create;
  end;

  // Light and dark Color schemes for SynEdit SQL highlighter and grid colors
  TGridTextColors = Array[TDBDatatypeCategoryIndex] of TColor;
  TAppColorScheme = class(TObject)
    public
      Name: String;
      SynSqlSyn: TSynSqlsyn;
      GridTextColors: TGridTextColors;
      GridNullColors: TGridTextColors;
      ActiveLineBackground: TColor;
      MatchingBraceForeground: TColor;
      MatchingBraceBackground: TColor;
      HightlightSameTextBackground: TColor; // This is for grids only, and there's no foreground setting.
      const GridNullBrightness = 20;
      constructor Create;
      destructor Destroy; override;
      // Load colors from settings
      procedure LoadFromSettings;
      // Write colors to settings
      procedure Apply;
  end;
  //TAppColorSchemeList = specialize TObjectList<TAppColorScheme>;
  TAppColorSchemes = class(TObjectList<TAppColorScheme>)
    public
      const SDarkScheme = 'Dark';
      const SLightScheme = 'Light';
      const SBlackScheme = 'Black';
      const SWhiteScheme = 'White';
      constructor Create(AOwnsObjects: Boolean = True); reintroduce; overload;
      procedure ApplyLight;
      procedure ApplyDark;
  end;

  TFileExtImageIndex = record
    Ext: String;
    ImageIndex: Integer;
  end;

  function GetFileExtImageIndex(Ext: String): Integer;

const FileExtImageIndex: array[0..16] of TFileExtImageIndex = (
  (Ext: 'csv';           ImageIndex: 50),
  (Ext: 'html';          ImageIndex: 32),
  (Ext: 'xml';           ImageIndex: 48),
  (Ext: 'sql';           ImageIndex: 201),
  (Ext: 'LaTeX';         ImageIndex: 153),
  (Ext: 'textile';       ImageIndex: 154),
  (Ext: 'jira-textile';  ImageIndex: 154),
  (Ext: 'php';           ImageIndex: 202),
  (Ext: 'md';            ImageIndex: 199),
  (Ext: 'json';          ImageIndex: 200),
  (Ext: 'jsonl';         ImageIndex: 200),
  (Ext: 'txt';           ImageIndex: 67),
  (Ext: 'zip';           ImageIndex: 53),
  (Ext: 'png';           ImageIndex: 47),
  (Ext: 'jpg';           ImageIndex: 47),
  (Ext: 'pdf';           ImageIndex: 44),
  (Ext: 'sqlite3';       ImageIndex: 196)
  );

var
  AppColorSchemes: TAppColorSchemes;

implementation

uses apphelpers;

function GetFileExtImageIndex(Ext: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i:=Low(FileExtImageIndex) to High(FileExtImageIndex) do begin
    if (FileExtImageIndex[i].Ext = Ext) or ('.'+FileExtImageIndex[i].Ext = Ext) then begin
      Result := FileExtImageIndex[i].ImageIndex;
      break;
    end;
  end;
end;

{ TJSONFloatNumberUntouched }

function TJSONFloatNumberUntouched.GetAsString: TJSONStringType;
var
  Val: TJSONFloat;
  fs: TFormatSettings;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';
  Val := GetAsFloat;
  Result := FloatToStrF(Val, ffFixed, 18, 18, fs);

  // Trim trailing zeros after the decimal point
  while (Length(Result) > 0) and (Result[High(Result)] = '0') do
    Delete(Result, High(Result), 1);
  // Remove trailing decimal separator if no fractional digits left
  if (Length(Result) > 0) and (Result[High(Result)] = fs.DecimalSeparator) then
    Delete(Result, High(Result), 1);
end;

{ TRegExprUmlauts }

constructor TRegExprUmlauts.Create;
begin
  inherited Create;
  WordChars := WordChars + 'äÄöÖüÜÿŸáÁéÉíÍóÓúÚýÝćĆńŃŕŔśŚźŹĺĹàÀèÈìÌòÒùÙâÂêÊîÎôÔûÛãÃõÕñÑçÇşŞţŢåÅæÆœŒøØß';
end;

{ TAppColorScheme }

constructor TAppColorScheme.Create;
begin
  inherited Create;
  SynSqlSyn := TSynSQLSyn.Create(nil);
  SynSqlSyn.SQLDialect := sqlMySQL;
end;

procedure TAppColorScheme.LoadFromSettings;
var
  i: Integer;
  Attri: TSynHighlighterAttributes;
  dtc: TDBDatatypeCategoryIndex;
begin
  Name := _('Current custom settings');
  for i:=0 to SynSqlSyn.AttrCount - 1 do begin
    Attri := SynSqlSyn.Attribute[i];
    Attri.Foreground := AppSettings.ReadInt(asHighlighterForeground, Attri.Name, Attri.Foreground);
    Attri.Background := AppSettings.ReadInt(asHighlighterBackground, Attri.Name, Attri.Background);
    // IntegerStyle gathers all font styles (bold, italic, ...) in one number
    Attri.IntegerStyle := AppSettings.ReadInt(asHighlighterStyle, Attri.Name, Attri.IntegerStyle);
  end;
  ActiveLineBackground := StringToColor(AppSettings.ReadString(asSQLColActiveLine));
  MatchingBraceBackground := StringToColor(AppSettings.ReadString(asSQLColMatchingBraceBackground));
  MatchingBraceForeground := StringToColor(AppSettings.ReadString(asSQLColMatchingBraceForeground));
  HightlightSameTextBackground := AppSettings.ReadInt(asHightlightSameTextBackground);

  GridTextColors[dtcInteger] := AppSettings.ReadInt(asFieldColorNumeric);
  GridTextColors[dtcReal] := AppSettings.ReadInt(asFieldColorReal);
  GridTextColors[dtcText] := AppSettings.ReadInt(asFieldColorText);
  GridTextColors[dtcBinary] := AppSettings.ReadInt(asFieldColorBinary);
  GridTextColors[dtcTemporal] := AppSettings.ReadInt(asFieldColorDatetime);
  GridTextColors[dtcSpatial] := AppSettings.ReadInt(asFieldColorSpatial);
  GridTextColors[dtcOther] := AppSettings.ReadInt(asFieldColorOther);

  // Calculate brighter NULL colors - not part of color presets
  for dtc:=Low(DatatypeCategories) to High(DatatypeCategories) do begin
    GridNullColors[dtc] := ColorAdjustBrightness(GridTextColors[dtc], GridNullBrightness);
  end;
end;

destructor TAppColorScheme.Destroy;
begin
  SynSqlSyn.Free;
  inherited;
end;

procedure TAppColorScheme.Apply;
var
  i: Integer;
  Attri: TSynHighlighterAttributes;
begin
  // Highlighter colors
  for i:=0 to SynSqlSyn.AttrCount - 1 do begin
    Attri := SynSqlSyn.Attribute[i];
    AppSettings.WriteInt(asHighlighterForeground, Attri.Foreground, Attri.Name);
    AppSettings.WriteInt(asHighlighterBackground, Attri.Background, Attri.Name);
    AppSettings.WriteInt(asHighlighterStyle, Attri.IntegerStyle, Attri.Name);
  end;
  // Colors sitting on a TSynEdit, not part of a highlighter
  AppSettings.WriteString(asSQLColActiveLine, ColorToString(ActiveLineBackground));
  AppSettings.WriteString(asSQLColMatchingBraceForeground, ColorToString(MatchingBraceForeground));
  AppSettings.WriteString(asSQLColMatchingBraceBackground, ColorToString(MatchingBraceBackground));
  // Grid data type colors
  AppSettings.WriteInt(asHightlightSameTextBackground, HightlightSameTextBackground);
  AppSettings.WriteInt(asFieldColorNumeric, GridTextColors[dtcInteger]);
  AppSettings.WriteInt(asFieldColorReal, GridTextColors[dtcReal]);
  AppSettings.WriteInt(asFieldColorText, GridTextColors[dtcText]);
  AppSettings.WriteInt(asFieldColorBinary, GridTextColors[dtcBinary]);
  AppSettings.WriteInt(asFieldColorDatetime, GridTextColors[dtcTemporal]);
  AppSettings.WriteInt(asFieldColorSpatial, GridTextColors[dtcSpatial]);
  AppSettings.WriteInt(asFieldColorOther, GridTextColors[dtcOther]);

  AppColorSchemes.First.LoadFromSettings;
end;


{ TAppColorSchemes }

constructor TAppColorSchemes.Create(AOwnsObjects: Boolean = True);
var
  Scheme: TAppColorScheme;
begin
  inherited; // Create(AOwnsObjects);

  // First one always contains the values currently used in the GUI
  Scheme := TAppColorScheme.Create;
  Scheme.LoadFromSettings;
  Add(Scheme);

  Scheme := TAppColorScheme.Create;
  Scheme.Name := SDarkScheme;
  Scheme.SynSqlSyn.CommentAttri.Foreground := 8710076;
  Scheme.SynSqlSyn.DataTypeAttri.Foreground := 11184895;
  Scheme.SynSqlSyn.FunctionAttri.Foreground := 15792639;
  Scheme.SynSqlSyn.IdentifierAttri.Foreground := 6460927;
  Scheme.SynSqlSyn.KeyAttri.Foreground := 15792639;
  Scheme.SynSqlSyn.NumberAttri.Foreground := 4610525;
  Scheme.SynSqlSyn.StringAttri.Foreground := 5293907;
  Scheme.SynSqlSyn.SymbolAttri.Foreground := 15792639;
  Scheme.SynSqlSyn.TableNameAttri.Foreground := 16755327;
  Scheme.SynSqlSyn.VariableAttri.Foreground := clPurple;
  Scheme.ActiveLineBackground := clNone;
  Scheme.MatchingBraceForeground := $0028EFFF;
  Scheme.MatchingBraceBackground := $004D513B;
  Scheme.HightlightSameTextBackground := clBlack;
  Scheme.GridTextColors[dtcInteger] := $00FF9785;
  Scheme.GridTextColors[dtcReal] := $00D07D7D;
  Scheme.GridTextColors[dtcText] := $0073D573;
  Scheme.GridTextColors[dtcBinary] := $00C9767F;
  Scheme.GridTextColors[dtcTemporal] := $007373C9;
  Scheme.GridTextColors[dtcSpatial] := $00CECE73;
  Scheme.GridTextColors[dtcOther] := $0073C1C1;
  Add(Scheme);

  Scheme := TAppColorScheme.Create;
  Scheme.Name := SLightScheme;
  Scheme.SynSqlSyn.CommentAttri.Foreground := clGray;
  Scheme.SynSqlSyn.DataTypeAttri.Foreground := clMaroon;
  Scheme.SynSqlSyn.FunctionAttri.Foreground := clNavy;
  Scheme.SynSqlSyn.IdentifierAttri.Foreground := clOlive;
  Scheme.SynSqlSyn.KeyAttri.Foreground := clBlue;
  Scheme.SynSqlSyn.NumberAttri.Foreground := clPurple;
  Scheme.SynSqlSyn.StringAttri.Foreground := clGreen;
  Scheme.SynSqlSyn.SymbolAttri.Foreground := clBlue;
  Scheme.SynSqlSyn.TableNameAttri.Foreground := clFuchsia;
  Scheme.SynSqlSyn.VariableAttri.Foreground := clPurple;
  Scheme.ActiveLineBackground := clNone;
  Scheme.MatchingBraceForeground := clBlack;
  Scheme.MatchingBraceBackground := clAqua;
  Scheme.HightlightSameTextBackground := $00D8F8FF;
  Scheme.GridTextColors[dtcInteger] := $00FF0000;
  Scheme.GridTextColors[dtcReal] := $00FF0048;
  Scheme.GridTextColors[dtcText] := $00008000;
  Scheme.GridTextColors[dtcBinary] := $00800080;
  Scheme.GridTextColors[dtcTemporal] := $00000080;
  Scheme.GridTextColors[dtcSpatial] := $00808000;
  Scheme.GridTextColors[dtcOther] := $00008080;
  Add(Scheme);

  Scheme := TAppColorScheme.Create;
  Scheme.Name := SBlackScheme;
  Scheme.SynSqlSyn.CommentAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.DataTypeAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.FunctionAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.IdentifierAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.KeyAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.NumberAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.StringAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.SymbolAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.TableNameAttri.Foreground := clBlack;
  Scheme.SynSqlSyn.VariableAttri.Foreground := clBlack;
  Scheme.ActiveLineBackground := clNone;
  Scheme.MatchingBraceForeground := clBlack;
  Scheme.MatchingBraceBackground := clAqua;
  Scheme.HightlightSameTextBackground := $00F4F4F4;
  Scheme.GridTextColors[dtcInteger] := $00000000;
  Scheme.GridTextColors[dtcReal] := $00000000;
  Scheme.GridTextColors[dtcText] := $00000000;
  Scheme.GridTextColors[dtcBinary] := $00000000;
  Scheme.GridTextColors[dtcTemporal] := $00000000;
  Scheme.GridTextColors[dtcSpatial] := $00000000;
  Scheme.GridTextColors[dtcOther] := $00000000;
  Add(Scheme);

  Scheme := TAppColorScheme.Create;
  Scheme.Name := SWhiteScheme;
  Scheme.SynSqlSyn.CommentAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.DataTypeAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.FunctionAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.IdentifierAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.KeyAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.NumberAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.StringAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.SymbolAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.TableNameAttri.Foreground := clWhite;
  Scheme.SynSqlSyn.VariableAttri.Foreground := clWhite;
  Scheme.ActiveLineBackground := clNone;
  Scheme.MatchingBraceForeground := $0028EFFF;
  Scheme.MatchingBraceBackground := $004D513B;
  Scheme.HightlightSameTextBackground := clBlack;
  Scheme.GridTextColors[dtcInteger] := $00FFFFFF;
  Scheme.GridTextColors[dtcReal] := $00FFFFFF;
  Scheme.GridTextColors[dtcText] := $00FFFFFF;
  Scheme.GridTextColors[dtcBinary] := $00FFFFFF;
  Scheme.GridTextColors[dtcTemporal] := $00FFFFFF;
  Scheme.GridTextColors[dtcSpatial] := $00FFFFFF;
  Scheme.GridTextColors[dtcOther] := $00FFFFFF;
  Add(Scheme);
end;

procedure TAppColorSchemes.ApplyLight;
var
  Scheme: TAppColorScheme;
begin
  for Scheme in Self do begin
    if Scheme.Name = SLightScheme then begin
      Scheme.Apply;
      Break;
    end;
  end;
end;

procedure TAppColorSchemes.ApplyDark;
var
  Scheme: TAppColorScheme;
begin
  for Scheme in Self do begin
    if Scheme.Name = SDarkScheme then begin
      Scheme.Apply;
      Break;
    end;
  end;
end;

initialization

SetJSONInstanceType(jitNumberFloat, TJSONFloatNumberUntouched);


end.
