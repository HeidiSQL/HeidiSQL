unit generic_types;

{$mode ObjFPC}

interface

uses fpjson, jsonparser, SysUtils, RegExpr, SynEditHighlighter, SynHighlighterSQL,
  Generics.Collections, Graphics;

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

  // Light and dark Color schemes for SynEdit SQL highlighter
  TSynSQLSynList = specialize TObjectList<TSynSQLSyn>;
  TSynSQLSynSchemes = class(TSynSQLSynList)
    public
      const SDarkScheme = 'Dark';
      const SLightScheme = 'Light';
      const SBlackScheme = 'Black';
      const SWhiteScheme = 'White';
      constructor Create(AOwnsObjects: Boolean = True); reintroduce; overload;
      procedure ApplyScheme(Scheme: TSynSQLSyn); overload;
      procedure ApplyScheme(SchemeName: String); overload;
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
  SQLSynSchemes: TSynSQLSynSchemes;

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

{ TSynSQLSynSchemes }

constructor TSynSQLSynSchemes.Create(AOwnsObjects: Boolean = True);
var
  Scheme: TSynSQLSyn;
begin
  inherited Create(AOwnsObjects);

  Scheme := TSynSQLSyn.Create(nil);
  Scheme.Name := SDarkScheme;
  Scheme.SQLDialect := sqlMySQL;
  Scheme.CommentAttri.Foreground := 8710076;
  Scheme.DataTypeAttri.Foreground := 11184895;
  Scheme.FunctionAttri.Foreground := 15792639;
  Scheme.IdentifierAttri.Foreground := 6460927;
  Scheme.KeyAttri.Foreground := 15792639;
  Scheme.NumberAttri.Foreground := 4610525;
  Scheme.StringAttri.Foreground := 5293907;
  Scheme.SymbolAttri.Foreground := 15792639;
  Scheme.TableNameAttri.Foreground := 16755327;
  Scheme.VariableAttri.Foreground := clPurple;
  Add(Scheme);

  Scheme := TSynSQLSyn.Create(nil);
  Scheme.Name := SLightScheme;
  Scheme.SQLDialect := sqlMySQL;
  Scheme.CommentAttri.Foreground := clGray;
  Scheme.DataTypeAttri.Foreground := clMaroon;
  Scheme.FunctionAttri.Foreground := clNavy;
  Scheme.IdentifierAttri.Foreground := clOlive;
  Scheme.KeyAttri.Foreground := clBlue;
  Scheme.NumberAttri.Foreground := clPurple;
  Scheme.StringAttri.Foreground := clGreen;
  Scheme.SymbolAttri.Foreground := clBlue;
  Scheme.TableNameAttri.Foreground := clFuchsia;
  Scheme.VariableAttri.Foreground := clPurple;
  Add(Scheme);

  Scheme := TSynSQLSyn.Create(nil);
  Scheme.Name := SBlackScheme;
  Scheme.SQLDialect := sqlMySQL;
  Scheme.CommentAttri.Foreground := clBlack;
  Scheme.DataTypeAttri.Foreground := clBlack;
  Scheme.FunctionAttri.Foreground := clBlack;
  Scheme.IdentifierAttri.Foreground := clBlack;
  Scheme.KeyAttri.Foreground := clBlack;
  Scheme.NumberAttri.Foreground := clBlack;
  Scheme.StringAttri.Foreground := clBlack;
  Scheme.SymbolAttri.Foreground := clBlack;
  Scheme.TableNameAttri.Foreground := clBlack;
  Scheme.VariableAttri.Foreground := clBlack;
  Add(Scheme);

  Scheme := TSynSQLSyn.Create(nil);
  Scheme.Name := SWhiteScheme;
  Scheme.SQLDialect := sqlMySQL;
  Scheme.CommentAttri.Foreground := clWhite;
  Scheme.DataTypeAttri.Foreground := clWhite;
  Scheme.FunctionAttri.Foreground := clWhite;
  Scheme.IdentifierAttri.Foreground := clWhite;
  Scheme.KeyAttri.Foreground := clWhite;
  Scheme.NumberAttri.Foreground := clWhite;
  Scheme.StringAttri.Foreground := clWhite;
  Scheme.SymbolAttri.Foreground := clWhite;
  Scheme.TableNameAttri.Foreground := clWhite;
  Scheme.VariableAttri.Foreground := clWhite;
  Add(Scheme);
end;

procedure TSynSQLSynSchemes.ApplyScheme(Scheme: TSynSQLSyn);
var
  i: Integer;
  Attri: TSynHighlighterAttributes;
begin
  for i:=0 to Scheme.AttrCount - 1 do begin
    Attri := Scheme.Attribute[i];
    AppSettings.WriteInt(asHighlighterForeground, Attri.Foreground, Attri.Name);
    AppSettings.WriteInt(asHighlighterBackground, Attri.Background, Attri.Name);
    AppSettings.WriteInt(asHighlighterStyle, Attri.IntegerStyle, Attri.Name);
  end;
  if Scheme.Name = SDarkScheme then begin
    AppSettings.WriteString(asSQLColActiveLine, ColorToString(clNone));
    AppSettings.WriteString(asSQLColMatchingBraceForeground, ColorToString($0028EFFF));
    AppSettings.WriteString(asSQLColMatchingBraceBackground, ColorToString($004D513B));
  end
  else if Scheme.Name = SLightScheme then begin
    AppSettings.WriteString(asSQLColActiveLine, ColorToString(clNone));
    AppSettings.WriteString(asSQLColMatchingBraceForeground, AppSettings.GetDefaultString(asSQLColMatchingBraceForeground));
    AppSettings.WriteString(asSQLColMatchingBraceBackground, AppSettings.GetDefaultString(asSQLColMatchingBraceBackground));
  end
end;

procedure TSynSQLSynSchemes.ApplyScheme(SchemeName: String);
var
  Scheme: TSynSQLSyn;
  Found: Boolean;
begin
  Found := False;
  for Scheme in Self do begin
    if Scheme.Name = SchemeName then begin
      ApplyScheme(Scheme);
      Found := True;
      Break;
    end;
  end;
  if not Found then
    raise Exception.Create('SQL scheme not found: '+SchemeName);
end;

initialization

SetJSONInstanceType(jitNumberFloat, TJSONFloatNumberUntouched);

SQLSynSchemes := TSynSQLSynSchemes.Create(True);

end.
