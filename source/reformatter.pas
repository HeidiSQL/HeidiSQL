unit reformatter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, IdHTTP, IdSSLOpenSSL, System.JSON,
  apphelpers, extra_controls, gnugettext, dbconnection, dbstructures, dbstructures.mysql;

type
  TfrmReformatter = class(TExtForm)
    grpReformatter: TRadioGroup;
    btnCancel: TButton;
    btnOk: TButton;
    lblFormatProviderLink: TLinkLabel;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure grpReformatterClick(Sender: TObject);
    procedure lblFormatProviderLinkLinkClick(Sender: TObject;
      const Link: string; LinkType: TSysLinkType);
  private
    { Private declarations }
    FInputCode, FOutputCode: String;
  public
    { Public declarations }
    function FormatSqlInternal(SQL: String): String;
    function FormatSqlOnlineHeidisql(SQL: String): String;
    function FormatSqlOnlineSqlformatOrg(SQL: String): String;
    property InputCode: String read FInputCode write FInputCode;
    property OutputCode: String read FOutputCode;
  end;

var
  frmReformatter: TfrmReformatter;

implementation

uses main;

{$R *.dfm}


procedure TfrmReformatter.btnOkClick(Sender: TObject);
var
  StartTime: UInt64;
  TimeElapsed: Double;
begin
  Screen.Cursor := crHourGlass;
  try
    StartTime := GetTickCount64;
    case grpReformatter.ItemIndex of
      0: begin
        // Internal
        FOutputCode := FormatSqlInternal(FInputCode);
      end;
      1: begin
        // Online
        FOutputCode := FormatSqlOnlineHeidisql(FInputCode);
      end;
      2: begin
        // sqlformat.org
        FOutputCode := FormatSqlOnlineSqlformatOrg(FInputCode);
      end;
    end;
    // Unify line breaks, so selection end will be correct:
    FOutputCode := fixNewlines(FOutputCode);
    TimeElapsed := GetTickCount64 - StartTime;
    MainForm.LogSQL(f_('Code reformatted in %s, using formatter %s', [FormatTimeNumber(TimeElapsed/1000, True, 3), '#'+grpReformatter.ItemIndex.ToString]));
  except
    on E:EIdHTTPProtocolException do begin
      ErrorDialog(E.Message + sLineBreak + sLineBreak + E.ErrorMessage);
      ModalResult := mrNone;
    end;
    on E:Exception do begin
      ErrorDialog(E.ClassName + ': ' + E.Message);
      ModalResult := mrNone;
    end;

  end;
  Screen.Cursor := crDefault;
end;

procedure TfrmReformatter.FormCreate(Sender: TObject);
begin
  grpReformatter.Items.Clear;
  grpReformatter.Items.Add(_('Internal'));
  grpReformatter.Items.Add(f_('Online on %s', [APPDOMAIN]));
  grpReformatter.Items.Add(f_('Online on %s', ['sqlformat.org']));
  grpReformatter.ItemIndex := AppSettings.ReadInt(asReformatter);
end;

procedure TfrmReformatter.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asReformatter, grpReformatter.ItemIndex);
end;


procedure TfrmReformatter.grpReformatterClick(Sender: TObject);
begin
  case grpReformatter.ItemIndex of
    0: lblFormatProviderLink.Visible := False;
    1: begin
      lblFormatProviderLink.Caption := '<a href="https://github.com/doctrine/sql-formatter">github.com/doctrine/sql-formatter</a> (Jeremy Dorn)';
      lblFormatProviderLink.Visible := True;
    end;
    2: begin
      lblFormatProviderLink.Caption := '<a href="https://sqlformat.org/">SQLFormat.org</a> (Andi Albrecht)';
      lblFormatProviderLink.Visible := True;
    end;
  end;
end;

procedure TfrmReformatter.lblFormatProviderLinkLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  apphelpers.ShellExec(Link);
end;

function TfrmReformatter.FormatSqlInternal(SQL: String): String;
var
  Conn: TDBConnection;
  SQLFunc: TSQLFunction;
  AllKeywords, ImportantKeywords, PairKeywords: TStringList;
  i, Run, KeywordMaxLen: Integer;
  IsEsc, IsQuote, InComment, InBigComment, InString, InKeyword, InIdent, LastWasComment: Boolean;
  c, p: Char;
  Keyword, PreviousKeyword, TestPair: String;
  Datatypes: TDBDataTypeArray;
const
  WordChars = ['a'..'z', 'A'..'Z', '0'..'9', '_', '.'];
  WhiteSpaces = [#9, #10, #13, #32];
begin
  Conn := MainForm.ActiveConnection;
  // Known SQL keywords, get converted to UPPERCASE
  AllKeywords := TStringList.Create;
  AllKeywords.Text := MySQLKeywords.Text;

  for SQLFunc in Conn.SQLFunctions do begin
    // Leave out operator functions like ">>", and the "X()" function so hex values don't get touched
    if (SQLFunc.Declaration <> '') and (SQLFunc.Name <> 'X') then
      AllKeywords.Add(SQLFunc.Name);
  end;
  Datatypes := Conn.Datatypes;
  for i:=Low(Datatypes) to High(Datatypes) do
    AllKeywords.Add(Datatypes[i].Name);
  KeywordMaxLen := 0;
  for i:=0 to AllKeywords.Count-1 do
    KeywordMaxLen := Max(KeywordMaxLen, Length(AllKeywords[i]));

  // A subset of the above list, each of them will get a linebreak left to it
  ImportantKeywords := Explode(',', 'SELECT,FROM,LEFT,RIGHT,STRAIGHT,NATURAL,INNER,JOIN,WHERE,GROUP,ORDER,HAVING,LIMIT,CREATE,DROP,UPDATE,INSERT,REPLACE,TRUNCATE,DELETE');
  // Keywords which followers should not get separated into a new line
  PairKeywords := Explode(',', 'LEFT,RIGHT,STRAIGHT,NATURAL,INNER,ORDER,GROUP');

  IsEsc := False;
  InComment := False;
  InBigComment := False;
  LastWasComment := False;
  InString := False;
  InIdent := False;
  Run := 1;
  Result := '';
  SQL := SQL + ' ';
  SetLength(Result, Length(SQL)*2);
  Keyword := '';
  PreviousKeyword := '';
  for i:=1 to Length(SQL) do begin
    c := SQL[i]; // Current char
    if i > 1 then p := SQL[i-1] else p := #0; // Previous char

    // Detection logic - where are we?
    if c = '\' then IsEsc := not IsEsc
    else IsEsc := False;
    IsQuote := (c = '''') or (c = '"');
    if c = '`' then InIdent := not InIdent;
    if (not IsEsc) and IsQuote then InString := not InString;
    if (c = '#') or ((c = '-') and (p = '-')) then InComment := True;
    if ((c = #10) or (c = #13)) and InComment then begin
      LastWasComment := True;
      InComment := False;
    end;
    if (c = '*') and (p = '/') and (not InComment) and (not InString) then InBigComment := True;
    if (c = '/') and (p = '*') and (not InComment) and (not InString) then InBigComment := False;
    InKeyword := (not InComment) and (not InBigComment) and (not InString) and (not InIdent) and CharInSet(c, WordChars);

    // Creation of returning text
    if InKeyword then begin
      Keyword := Keyword + c;
    end else begin
      if Keyword <> '' then begin
        if AllKeywords.IndexOf(KeyWord) > -1 then begin
          while (Run > 1) and CharInSet(Result[Run-1], WhiteSpaces) do
            Dec(Run);
          Keyword := UpperCase(Keyword);
          if Run > 1 then begin
            // SELECT, WHERE, JOIN etc. get a new line, but don't separate LEFT JOIN with linebreaks
            if LastWasComment or ((ImportantKeywords.IndexOf(Keyword) > -1) and (PairKeywords.IndexOf(PreviousKeyword) = -1)) then
              Keyword := CRLF + Keyword
            else if (Result[Run-1] <> '(') then
              Keyword := ' ' + Keyword;
          end;
          LastWasComment := False;
        end;
        PreviousKeyword := Trim(Keyword);
        Insert(Keyword, Result, Run);
        Inc(Run, Length(Keyword));
        Keyword := '';
      end;
      if (not InComment) and (not InBigComment) and (not InString) and (not InIdent) then begin
        TestPair := Result[Run-1] + c;
        if (TestPair = '  ') or (TestPair = '( ') then begin
          c := Result[Run-1];
          Dec(Run);
        end;
        if (TestPair = ' )') or (TestPair = ' ,') then
          Dec(Run);
      end;
      Result[Run] := c;
      Inc(Run);
    end;

  end;

  // Cut overlength
  SetLength(Result, Run-2);
end;


function TfrmReformatter.FormatSqlOnlineHeidisql(SQL: String): String;
var
  HttpReq: TIdHTTP;
  SSLio: TIdSSLIOHandlerSocketOpenSSL;
  Parameters: TStringList;
begin
  HttpReq := TIdHTTP.Create;
  SSLio := TIdSSLIOHandlerSocketOpenSSL.Create;
  HttpReq.IOHandler := SSLio;
  SSLio.SSLOptions.SSLVersions := [sslvTLSv1_1, sslvTLSv1_2];
  //HttpReq.Request.ContentType := 'application/json';
  HttpReq.Request.CharSet := 'utf-8';
  HttpReq.Request.UserAgent := apphelpers.UserAgent(Self);
  Parameters := TStringList.Create;
  if AppSettings.ReadBool(asTabsToSpaces) then
    Parameters.AddPair('indent', StringOfChar(' ', AppSettings.ReadInt(asTabWidth)))
  else
    Parameters.AddPair('indent', #9);
  Parameters.AddPair('input', FInputCode);
  Result := HttpReq.Post(APPDOMAIN + 'sql-formatter.php', Parameters);
  if Result.IsEmpty then
    raise Exception.Create(_('Empty result from online reformatter'));
  HttpReq.Free;
end;


function TfrmReformatter.FormatSqlOnlineSqlformatOrg(SQL: String): String;
var
  HttpReq: TIdHTTP;
  SSLio: TIdSSLIOHandlerSocketOpenSSL;
  Parameters: TStringList;
  JsonResponseStr: String;
  JsonTmp: TJSONValue;
begin
  HttpReq := TIdHTTP.Create;
  SSLio := TIdSSLIOHandlerSocketOpenSSL.Create;
  HttpReq.IOHandler := SSLio;
  SSLio.SSLOptions.SSLVersions := [sslvTLSv1_1, sslvTLSv1_2];
  HttpReq.Request.CharSet := 'utf-8';
  HttpReq.Request.UserAgent := apphelpers.UserAgent(Self);
  // Parameter documentation: https://sqlformat.org/api/
  Parameters := TStringList.Create;
  Parameters.AddPair('sql', FInputCode);
  Parameters.AddPair('reindent', '1');
  if AppSettings.ReadBool(asTabsToSpaces) then
    Parameters.AddPair('indent_width', AppSettings.ReadInt(asTabWidth).ToString)
  else
    Parameters.AddPair('indent_width', '2');
  Parameters.AddPair('keyword_case', 'upper');
  JsonResponseStr := HttpReq.Post('https://sqlformat.org/api/v1/format', Parameters);
  if JsonResponseStr.IsEmpty then
    raise Exception.Create(_('Empty result from online reformatter'));
  JsonTmp := TJSONObject.ParseJSONValue(JsonResponseStr);
  Result := JsonTmp.FindValue('result').Value;
  JsonTmp.Free;
  HttpReq.Free;
end;

end.
