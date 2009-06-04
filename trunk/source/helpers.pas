unit helpers;


// -------------------------------------
// Functions-library
// -------------------------------------


interface

uses Classes, SysUtils, Graphics, db, clipbrd, dialogs,
  forms, controls, ShellApi, checklst, windows, ZDataset, ZAbstractDataset,
  shlobj, ActiveX, WideStrUtils, VirtualTrees, SynRegExpr, Messages, WideStrings,
  TntCheckLst, Registry, SynEditHighlighter;

type

  TListNodeType = (lntNone, lntDb, lntTable, lntCrashedTable, lntView, lntFunction, lntProcedure, lntColumn);
  TListNode = record
    Text: WideString;
    NodeType: TListNodeType;
  end;

  // Define a record which can hold everything we need for one row / node in a VirtualStringTree
  TVTreeData = record
    Captions: TWideStringList;
    ImageIndex: Integer;
    NodeType: TListNodeType;
  end;
  PVTreedata = ^TVTreeData;

  // Standardize the list with node-data-records to be able to
  // use this type as variables in functions/procedures (fx VT.OnFreeNode)
  TVTreeDataArray = Array of TVTreeData;
  PVTreeDataArray = ^TVTreeDataArray;

  TFileCharset = (fcsAnsi, fcsUnicode, fcsUnicodeSwapped, fcsUtf8);

  TUniClipboard = class(TClipboard)
  private
    procedure SetAsWideString(Value: WideString);
    function GetAsWideString:WideString;
  public
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
  end;

  // Structures for result grids, mapped from a TDataset to some handy VirtualTree structure
  TGridCell = record
    Text: WideString;
    NewText: WideString; // Used to create UPDATE clauses with needed columns
    IsNull: Boolean;
    NewIsNull: Boolean;
    Modified: Boolean;
  end;
  PGridCell = ^TGridCell;
  TGridColumn = record
    Name: WideString;
    DataType: Byte; // @see constants in mysql_structures.pas
    MaxLength: Cardinal;
    IsPriPart: Boolean;
    IsBinary: Boolean;
    IsText: Boolean;
    IsEnum: Boolean;
    IsSet: Boolean;
    IsInt: Boolean;
    IsFloat: Boolean;
    IsDate: Boolean;
    ValueList: TWideStringList;
  end;
  PGridColumn = ^TGridColumn;
  TGridColumns = Array of TGridColumn;
  // Delphi reminder, from Rudy Velthuis (usenet):
  // Records and arrays are passed as pointers, but in the preamble of the
  // called function the pointed-to data is copied onto the stack.  So either
  // use "const x: TBlah" to pass by reference, or create a pointer type.
  // Objects are passed as pointers.
  // (Orthogonal to this, when specifying "var", a pointer (possibly to a pointer)
  // is passed, allowing the called function to alter the caller's reference.)
  PGridColumns = ^TGridColumns;
  TGridRowState = (grsDefault, grsDeleted, grsModified, grsInserted);
  TGridRow = packed record
    Cells: Array of TGridCell;
    State: TGridRowState;
    Loaded: Boolean;
  end;
  PGridRow = ^TGridRow;
  TGridRows = Array of TGridRow;
  TGridResult = record
    Rows: TGridRows;
    Columns: TGridColumns;
  end;
  PGridResult = ^TGridResult;

  TMemoEditor = class(TForm)
  public
    function GetText: WideString; virtual; abstract;
    procedure SetText(text: WideString); virtual; abstract;
    procedure SetMaxLength(len: integer); virtual; abstract;
    procedure SetFont(font: TFont); virtual; abstract;
  end;

  TOrderCol = class(TObject)
    ColumnName: WideString;
    SortDirection: Byte;
  end;
  TOrderColArray = Array of TOrderCol;

  TLineBreaks = (lbsNone, lbsWindows, lbsUnix, lbsMac, lbsWide, lbsMixed);

{$I const.inc}

  function implodestr(seperator: WideString; a: TWideStringList) :WideString;
  function explode(separator, a: WideString) :TWideStringList;
  procedure ensureValidIdentifier(name: String);
  function getEnumValues(str: WideString): WideString;
  function parsesql(sql: WideString) : TWideStringList;
  function sstr(str: WideString; len: Integer) : WideString;
  function encrypt(str: String): String;
  function decrypt(str: String): String;
  function htmlentities(str: WideString): WideString;
  procedure GridToHtml(Grid: TVirtualStringTree; GridData: PGridResult; Title: WideString; S: TStream);
  procedure GridToCsv(Grid: TVirtualStringTree; GridData: PGridResult; Separator, Encloser, Terminator: String; S: TStream);
  procedure GridToXml(Grid: TVirtualStringTree; GridData: PGridResult; root: WideString; S: TStream);
  procedure GridToSql(Grid: TVirtualStringTree; GridData: PGridResult; Tablename: WideString; S: TStream);
  function esc2ascii(str: String): String;
  function StrCmpBegin(Str1, Str2: string): Boolean;
  function Max(A, B: Integer): Integer; assembler;
  function Min(A, B: Integer): Integer; assembler;
  function urlencode(url: String): String;
  function openfs(filename: String): TFileStream;
  procedure wfs( var s: TFileStream; str: WideString = '');
  procedure StreamWrite(S: TStream; Text: WideString = '');
  function fixSQL( sql: WideString; sql_version: Integer = SQL_VERSION_ANSI; cli_workarounds: Boolean = false ): WideString;
  procedure ToggleCheckListBox(list: TTNTCheckListBox; state: Boolean); Overload;
  procedure ToggleCheckListBox(list: TTNTCheckListBox; state: Boolean; list_toggle: TWideStringList); Overload;
  function _GetFileSize(filename: String): Int64;
  function Mince(PathToMince: String; InSpace: Integer): String;
  function MakeInt( Str: String ) : Int64;
  function MakeFloat( Str: String ): Extended;
  function FloatStr(Val: String): String;
  function esc(Text: WideString; ProcessJokerChars: Boolean = false; sql_version: integer = 50000): WideString;
  function ScanNulChar(Text: WideString): Boolean;
  function ScanLineBreaks(Text: WideString): TLineBreaks;
  function RemoveNulChars(Text: WideString): WideString;
  procedure debug(txt: String);
  function fixNewlines(txt: string): string;
  function BoolToStr(val: Boolean): String;
  function StrToBool(val: String): Boolean;
  function GetShellFolder(CSIDL: integer): string;
  function getFilesFromDir( dir: String; pattern: String = '*.*'; hideExt: Boolean = false ): TStringList;
  function goodfilename( str: String ): String;
  function FormatNumber( str: String ): String; Overload;
  function FormatNumber( int: Int64 ): String; Overload;
  function FormatNumber( flt: Double; decimals: Integer = 0 ): String; Overload;
  procedure setLocales;
  function maskSql(sql_version: integer; str: WideString) : WideString;
  procedure ActivateWindow(Window : HWnd);
  function GetApplication(MainForm: HWnd): HWnd;
  procedure ActivateMainForm(MainForm: HWnd);
  procedure ShellExec( cmd: String; path: String = '' );
  function getFirstWord( text: String ): String;
  function ConvertWindowsCodepageToMysqlCharacterSet(codepage: Cardinal): string;
  function LastPos(needle: WideChar; haystack: WideString): Integer;
  function ConvertServerVersion( Version: Integer ): String;
  function FormatByteNumber( Bytes: Int64; Decimals: Byte = 1 ): String; Overload;
  function FormatByteNumber( Bytes: String; Decimals: Byte = 1 ): String; Overload;
  function FormatTimeNumber( Seconds: Cardinal ): String;
  function TColorToHex( Color : TColor ): string;
  function GetVTCaptions( VT: TVirtualStringTree; OnlySelected: Boolean = False; Column: Integer = 0; OnlyNodeType: TListNodeType = lntNone ): TWideStringList;
  procedure SetVTSelection( VT: TVirtualStringTree; Selected: TWideStringList );
  function Pos2(const Needle, HayStack: string; const StartPos: Integer) : Integer;
  function GetTempDir: String;
  function GetDBObjectType( TableStatus: TFields ): TListNodeType;
  procedure SetWindowSizeGrip(hWnd: HWND; Enable: boolean);
  procedure SaveUnicodeFile(Filename: String; Text: WideString);
  function CreateUnicodeFileStream(Filename: String): TFileStream;
  procedure OpenTextFile(const Filename: String; out Stream: TFileStream; out FileCharset: TFileCharset);
  function GetFileCharset(Stream: TFileStream): TFileCharset;
  function ReadTextfileChunk(Stream: TFileStream; FileCharset: TFileCharset; ChunkSize: Int64 = 0): WideString;
  function ReadTextfile(Filename: String): WideString;
  function ReadBinaryFile(Filename: String; MaxBytes: Int64): string;
  procedure CopyToClipboard(Value: WideString);
  procedure StreamToClipboard(S: TMemoryStream);
  function WideHexToBin(text: WideString): string;
  function BinToWideHex(bin: string): WideString;
  procedure CheckHex(text: WideString; errorMessage: string);
  procedure FixVT(VT: TVirtualStringTree);
  function ColorAdjustBrightness(Col: TColor; ShiftPercent: ShortInt): TColor;
  function ComposeOrderClause(Cols: TOrderColArray): WideString;
  procedure OpenRegistry(Session: String = '');
  function GetRegValue( valueName: String; defaultValue: Integer; Session: String = '' ) : Integer; Overload;
  function GetRegValue( valueName: String; defaultValue: Boolean; Session: String = '' ) : Boolean; Overload;
  function GetRegValue( valueName: String; defaultValue: String; Session: String = '' ) : String; Overload;
  procedure ResetVTNodes(Sender: TBaseVirtualTree);
  procedure EnableProgressBar(MaxValue: Integer);
  function CompareNumbers(List: TStringList; Index1, Index2: Integer): Integer;
  function ListIndexByRegExpr(List: TWideStrings; Expression: WideString): Integer;
  procedure RestoreSyneditStyles(Highlighter: TSynCustomHighlighter);
var
  MYSQL_KEYWORDS             : TStringList;
  MainReg                    : TRegistry;


implementation

uses main;

type
  CharacterSet = record
    codepage: Cardinal;
    charset: string;
  end;

  TWndProc = function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

  PGripInfo = ^TGripInfo;
  TGripInfo = record
    OldWndProc: TWndProc;
    Enabled: boolean;
    GripRect: TRect;
  end;

const
  charset_conv_table: array[0..7] of CharacterSet = (
    (codepage: 1250; charset: 'cp1250'), // ANSI Central European; Central European (Windows)
    (codepage: 1251; charset: 'cp1251'), // ANSI Cyrillic; Cyrillic (Windows)
    (codepage: 1252; charset: 'latin1'), // ANSI Latin 1; Western European (Windows)
    (codepage: 1255; charset: 'hebrew'), // ANSI Hebrew; Hebrew (Windows)
    (codepage: 1256; charset: 'cp1256'), // ANSI Arabic; Arabic (Windows)
    (codepage: 1257; charset: 'cp1257'), // ANSI Baltic; Baltic (Windows)
    (codepage: 936; charset: 'gbk'),     // Windows Simplified Chinese GBK
    (codepage: 950; charset: 'big5')     // Windows Chinese Big5
  );
  SizeGripProp = 'SizeGrip';

var
  dbgCounter: Integer = 0;
  DecimalSeparatorSystemdefault: Char;



function WideHexToBin(text: WideString): string;
var
  buf: string;
begin
  buf := text;
  Result := StringOfChar(' ', Length(text) div 2);
  HexToBin(@buf[1], @Result[1], Length(Result));
end;

function BinToWideHex(bin: string): WideString;
var
  buf: string;
begin
  buf := StringOfChar(' ', Length(bin) * 2);
  BinToHex(@bin[1], @buf[1], Length(bin));
  Result := buf;
end;

procedure CheckHex(text: WideString; errorMessage: string);
const
  allowed: string = '0123456789abcdefABCDEF';
var
  i: Cardinal;
begin
  for i := 1 to Length(text) do begin
    if Pos(text[i], allowed) < 1 then begin
      raise Exception.Create(errorMessage);
    end;
  end;
end;


{***
  Convert a TStringList to a string using a separator-string

  @todo Look at each caller to see if escaping is necessary.
  @param string Separator
  @param a TStringList Containing strings
  @return string
}
function implodestr(seperator: WideString; a: TWideStringList) :WideString;
var
  i : Integer;
begin
  Result := '';
  for i:=0 to a.Count-1 do
  begin
    Result := Result + a[i];
    if i < a.Count-1 then
      Result := Result + seperator;
  end;
end;



{***
  Explode a string by separator into a TStringList

  @param string Separator
  @return TStringList
}
function explode(separator, a: WideString) :TWideStringList;
var
  i : Integer;
  item : WideString;
begin
  result := TWideStringList.Create;

  i := pos(separator, a);
  while i > 0 do begin
    item := copy(a, 0, i-1);
    item := trim(item);
    result.Add(item);
    a := copy(a, i+length(separator), length(a));
    i := pos(separator, a);
  end;
  if a <> '' then
    result.Add(trim(a));
end;



{***
  Check for valid identifier (table-/db-/column-name) ?

  @param string Identifier
  @return boolean Name is valid?
  @note rosenfield, 2007-02-01:
    Those certain characters are standard filesystem wildcards * ?,
    pipe redirection characters | < >, standard path separators / \,
    Windows mount point identifiers :, DBMS security / container separator
    characters . and so on.  In other words, characters that may or may
    not be allowed by MySQL and the underlying filesystem, but which are
    really, really, really stupid to use in a table name, since you'll
    get into trouble once trying to use the table/db in a query or move it
    to a different filesystem, or what not.
  @note ansgarbecker, 2007-02-01:
    Since mysql 5.1.6 those problematic characters are encoded in
    a hexadecimal manner if they apply to a file (table) or folder (database)
    But after testing that by renaming a table to a name with a dot
    I still get an error, so we currently should be careful also on a 5.1.6+
  @see http://dev.mysql.com/doc/refman/5.1/en/identifier-mapping.html
}
procedure ensureValidIdentifier( name: String );
var
  i                                     : Integer;
  invalidChars, invalidCharsShown       : String;
  isToolong, hasInvalidChars   : Boolean;
  msgStr                                : String;
begin
  isToolong := false;
  hasInvalidChars := false;

  // Check length
  if (length(name) < 1) or (length(name) > 64) then
    isToolong := true;

  // Check for invalid chars
  invalidChars := '\/:*?"<>|.';
  for i:=1 to length(name) do
  begin
    if (pos( name[i], invalidChars ) > 0 ) then
    begin
      hasInvalidChars := true;
      break;
    end;
  end;

  // Raise exception which explains what's wrong
  if isTooLong or hasInvalidChars then
  begin
    if hasInvalidChars then
    begin
      // Add space between chars for better readability
      invalidCharsShown := '';
      for i:=1 to length(invalidChars) do
      begin
        invalidCharsShown := invalidCharsShown + invalidChars[i] + ' ';
      end;
      msgStr := 'The name "%s" contains some invalid characters.'+
        CRLF+CRLF + 'An identifier must not contain the following characters:'+CRLF+invalidCharsShown;
    end
    else if isToolong then
    begin
      msgStr := 'The name "%s" has '+IntToStr(Length(name))
        +' characters and exceeds the maximum length of 64 characters.';
    end;

    Raise Exception.CreateFmt(msgStr, [name]);
  end;


end;



{***
  Get values from an enum- or set-typed column definition

  @param string Type definition, fx: enum('Y','N')
  @return string Content of brackets
}
function getEnumValues(str: WideString): WideString;
var
  p1,p2        : Integer;
begin
  p1 := pos('(', str);
  Result := '';
  // Only return something if opening bracket was found, otherwise empty string
  if p1 > 0 then
  begin
    for p2:=Length(str) downto 0 do
      if str[p2] = ')' then break;
    result := copy (str, p1+1, p2-p1-1);
  end;
end;



{***
  Add a non-empty value to a Stringlist

  @param TStringList
  @param string to add
  @param string to enclose added string in (use %s)
  @return void
}
procedure addResult(list: TWideStringList; s: WideString; enclose: WideString = '');
begin
  s := trim(s);
  if length(s) > 0 then begin
    if enclose <> '' then s := WideFormat(enclose, [s]);
    list.Add(s);
  end;
  // Avoid memory leak
  s := '';
end;



{***
  Return true if given character represents whitespace.
  Limitations: only recognizes ANSI whitespace.
  Eligible for inlining, hope the compiler does this automatically.
}
function isWhitespace(const c: WideChar): boolean;
begin
  result :=
    (c = #9) or
    (c = #10) or
    (c = #13) or
    (c = #32)
  ;
end;



{***
  Return true if given character represents a number.
  Limitations: only recognizes ANSI numerals.
  Eligible for inlining, hope the compiler does this automatically.
}
function isNumber(const c: WideChar): boolean;
var
  b: word;
begin
  b := ord(c);
  result :=
    (b >= 48) and
    (b <= 57)
  ;
end;



{***
  Scan backwards, returning true if SQL matches the given string case sensitively.
  Limitations: in case insensitive mode, input must be ANSI and lower case (for speed).
  Eligible for inlining, hope the compiler does this automatically.
}
function scanReverse(const haystack: WideString; hayIndex: integer; const needle: WideString; needleEnd: integer; insensitive: boolean): boolean;
var
  b: word;
  c: widechar;
begin
  while (hayIndex > 0) and (needleEnd > 0) do begin
    // Lowercase ANSI A-Z if requested.
    if insensitive then begin
      b := Ord(haystack[hayIndex]);
      if (b > 64) and (b < 91) then b := b - 65 + 97;
      c := WideChar(b);
    end else c := haystack[hayIndex];
    if c <> needle[needleEnd] then begin
      result := false;
      exit;
    end;
    needleEnd := needleEnd - 1;
    hayIndex := hayIndex - 1;
  end;
  result := needleEnd = 0;
end;



{***
  Tokenize sql-script and return a TStringList with sql-statements

  @param String (possibly large) bunch of SQL-statements, separated by semicolon
  @param String SQL start delimiter
  @return TStringList Separated statements
}
function parsesql(sql: WideString) : TWideStringList;
var
  i, j, start, len                  : Integer;
  tmp                               : WideString;
  instring, backslash, incomment    : Boolean;
  inconditional, condterminated     : Boolean;
  inbigcomment, indelimiter         : Boolean;
  delimiter_length                  : Integer;
  encloser, secchar, thdchar        : WideChar;
  conditional                       : WideString;
begin
  result := TWideStringList.Create;
  sql := trim(sql);
  instring := false;
  start := 1;
  len := length(sql);
  backslash := false;
  incomment := false;
  inbigcomment := false;
  inconditional := false;
  condterminated := false;
  indelimiter := false;
  encloser := ' ';
  conditional := '';

  i := 0;
  while i < len do begin
    i := i + 1;

    // Helpers for multi-character tests, avoids testing for string length.
    secchar := '+';
    thdchar := '+';
    if i < length(sql) then secchar := sql[i + 1];
    if i + 1 < length(sql) then thdchar := sql[i + 2];

    // Turn comments into whitespace.
    if (sql[i] = '#') and (not instring) and (not inbigcomment) then begin
      incomment := true;
    end;
    if (sql[i] + secchar = '--') and (not instring) and (not inbigcomment) then begin
      incomment := true;
      sql[i] := ' ';
      if start = i then start := start + 1;
      i := i + 1;
    end;
    if (sql[i] + secchar = '/*') and (not (thdchar = '!')) and (not instring) and (not incomment) then begin
      inbigcomment := true;
      incomment := true;
      sql[i] := ' ';
      if start = i then start := start + 1;
      i := i + 1;
    end;
    if incomment and (not inbigcomment) and (sql[i] in [WideChar(#13), WideChar(#10)]) then begin
      incomment := false;
    end;
    if inbigcomment and (sql[i] + secchar = '*/') then begin
      inbigcomment := false;
      incomment := false;
      sql[i] := ' ';
      if start = i then start := start + 1;
      i := i + 1;
      sql[i] := ' ';
    end;
    if incomment or inbigcomment then begin
      sql[i] := ' ';
    end;

    // Skip whitespace immediately if at start of sentence.
    if (start = i) and isWhitespace(sql[i]) then begin
      start := start + 1;
      if i < len then continue;
    end;

    // Avoid parsing stuff inside string literals.
    if (sql[i] in [WideChar(''''), WideChar('"'), WideChar('`')]) and (not (backslash and instring)) and (not incomment) and (not indelimiter) then begin
      if instring and (sql[i] = encloser) then begin
        if secchar = encloser then
          i := i + 1                            // encoded encloser-char
        else
          instring := not instring              // string closed
      end
      else if not instring then begin           // string is following
        instring := true;
        encloser := sql[i];                     // remember enclosing-character
      end;
      if i < len then continue;
    end;

    if (instring and (sql[i] = '\')) or backslash then
      backslash := not backslash;

    // Allow a DELIMITER command in middle of SQL, like the MySQL CLI does.
    if (not instring) and (not incomment) and (not inconditional) and (not indelimiter) and (start + 8 = i) and scanReverse(sql, i, 'delimiter', 9, true) then begin
      // The allowed DELIMITER format is:
      //   <delimiter> <whitespace(s)> <character(s)> <whitespace(s)> <newline>
      if isWhitespace(secchar) then begin
        indelimiter := true;
        i := i + 1;
        if i < len then continue;
      end;
    end;

    if indelimiter then begin
      if (sql[i] in [WideChar(#13), WideChar(#10)]) or (i = len) then begin
        if (i = len) then j := 1 else j := 0;
        try
          Mainform.Delimiter := copy(sql, start + 10, i + j - (start + 10));
        except on E:Exception do if Mainform.actQueryStopOnErrors.Checked then
          raise Exception.Create(E.Message);
        end;
        indelimiter := false;
        start := i + 1;
      end;
      if i < len then continue;
    end;

    // Handle conditional comments.
    if (not instring) and (not incomment) and (sql[i] + secchar + thdchar = '/*!') then begin
      inconditional := true;
      condterminated := false;
      tmp := '';
      conditional := '';
      i := i + 2;
      if i < len then continue;
    end;

    if inconditional and (conditional = '') then begin
      if not isNumber(sql[i]) then begin
        conditional := tmp;
        // note:
        // we do not trim the start of the SQL inside conditional
        // comments like we do on non-commented sql.
        if i < len then continue;
      end else tmp := tmp + sql[i];
    end;

    if inconditional and (not instring) and (not incomment) and (sql[i] + secchar = '*/') then begin
      inconditional := false;
      if condterminated then begin
        // at least one statement was terminated inside the conditional.
        // if the conditional had no more contents after that statement,
        // clear the end marker.  otherwise, add a new start marker.
        if trim(copy(sql, start, i - start)) = '' then begin
          sql[i] := ' ';
          i := i + 1;
          sql[i] := ' ';
          start := i + 1;
        end else begin
          tmp := '/*!' + conditional + ' ';
          move(tmp[1], sql[start - length(tmp)], length(tmp));
          start := start - length(tmp);
        end;
        condterminated := false;
      end else begin
        if start = i then start := start + 1;
        i := i + 1;
      end;
      if i < len then continue;
    end;

    // Add sql sentence.
    delimiter_length := Length(Mainform.Delimiter);
    if ((not instring) and (scanReverse(sql, i, Mainform.Delimiter, delimiter_length, false)) or (i = len)) then begin
      if (i < len) then j := delimiter_length else begin
        // end of string, add sql sentence but only remove delimiter if it's there
        if scanReverse(sql, i, Mainform.Delimiter, delimiter_length, false) then j := delimiter_length else j := 0;
      end;
      if inconditional then begin
        addResult(result, copy(sql, start, i - start - j + 1), '%s */');
        condterminated := true;
      end else begin
        addResult(result, copy(sql, start, i - start - j + 1));
      end;
      start := i + 1;
    end;
  end;

  // Avoid memory leak
  sql := '';
end;



{***
  Shorten string to length len and append 3 dots

  @param string String to shorten
  @param integer Wished Length of string
  @return string
}
function sstr(str: WideString; len: Integer) : WideString;
begin
  if length(str) > len then
  begin
    str := copy(str, 0, len-1);
    str := str + '�';
  end;
  result := str;
end;



{***
  Password-encryption, used to store session-passwords in registry

  @param string Text to encrypt
  @return string Encrypted Text
}
function encrypt(str: String) : String;
var
  i, salt, nr : integer;
  h : String;
begin
  randomize();
  result := '';
  salt := random(9) + 1;
  for i:=1 to length(str) do begin
    nr := ord(str[i])+salt;
    if nr > 255 then
      nr := nr - 255;
    h := inttohex(nr,0);
    if length(h) = 1 then
      h := '0' + h;
    result := result + h;
  end;
  result := result + inttostr(salt);
end;



{***
  Password-decryption, used to restore session-passwords from registry

  @param string Text to decrypt
  @return string Decrypted Text
}
function decrypt(str: String) : String;
var
  j, salt, nr : integer;
begin
  result := '';
  if str = '' then exit;
  j := 1;
  salt := StrToIntDef(str[length(str)],0);
  result := '';
  while j < length(str)-1 do begin
    nr := StrToInt('$' + str[j] + str[j+1]) - salt;
    if nr < 0 then
      nr := nr + 255;
    result := result + chr(nr);
    inc(j, 2);
  end;
end;



{***
  Convert HTML-characters to their corresponding entities

  @param string Text used for search+replace
  @return string Text with entities
}
function htmlentities(str: WideString) : WideString;
begin
  result := WideStringReplace(str, '&', '&amp;', [rfReplaceAll]);
  result := WideStringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := WideStringReplace(result, '>', '&gt;', [rfReplaceAll]);
end;


procedure ExportStatusMsg(Node: PVirtualNode; RootNodeCount: Cardinal; StreamSize: Int64);
begin
  Mainform.Showstatus('Exporting row '+FormatNumber(Node.Index+1)+' of '+FormatNumber(RootNodeCount)+
    ' ('+IntToStr(Trunc((Node.Index+1) / RootNodeCount *100))+'%, '+FormatByteNumber(StreamSize)+')'
    );
  Mainform.ProgressBarStatus.Position := Node.Index+1;
end;


{***
  Converts a Grid to a HTML-Table.
  @param Grid Object which holds data to export
  @param string Text used in <title>
}
procedure GridToHtml(Grid: TVirtualStringTree; GridData: PGridResult; Title: WideString; S: TStream);
var
  i, MaxSize: Integer;
  tmp, Data, Generator: WideString;
  Node: PVirtualNode;
begin
  if Grid = Mainform.DataGrid then begin
    // Discard all loaded data so EnsureChunkLoaded refetches it with full content lengths.
    // This makes it superflous to call EnsureFullWidth and to have a unique key
    for i := 0 to Length(GridData.Rows) - 1 do
      GridData.Rows[i].Loaded := False;
  end;

  MaxSize := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE) * SIZE_MB;
  EnableProgressBar(Grid.RootNodeCount);
  Generator := APPNAME+' '+FullAppVersion;
  tmp :=
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" ' + CRLF +
    '  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + CRLF + CRLF +
    '<html>' + CRLF +
    '  <head>' + CRLF +
    '    <title>' + Title + '</title>' + CRLF +
    '    <meta name="GENERATOR" content="'+ Generator + '">' + CRLF +
    '    <style type="text/css">' + CRLF +
    '      thead tr {background-color: ActiveCaption; color: CaptionText;}' + CRLF +
    '      th, td {vertical-align: top; font-family: "'+Grid.Font.Name+'"; font-size: '+IntToStr(Grid.Font.Size)+'pt; padding: '+IntToStr(Grid.TextMargin-1)+'px; }' + CRLF +
    '      table, td {border: 1px solid silver;}' + CRLF +
    '      table {border-collapse: collapse;}' + CRLF;
  for i:=0 to Length(GridData.Columns) - 1 do begin
    // Skip hidden key columns.
    if not (coVisible in Grid.Header.Columns[i].Options) then
      Continue;
    // Adjust preferred width of columns.
    tmp := tmp +
     '      thead .col' + IntToStr(i) + ' {width: ' + IntToStr(Grid.Header.Columns[i].Width) + 'px;}' + CRLF;
    // Right-justify all cells to match the grid on screen.
    if Grid.Header.Columns[i].Alignment = taRightJustify then
      tmp := tmp +
        '      .col' + IntToStr(i) + ' {text-align: right;}' + CRLF;
  end;
  // Note for above:
  // Indentation seems to be a mess.  I think we should stick to putting long lines on one line,
  // and just let the editor figure out how to break it if it doesn't fit on screen.  That way
  // the editor can break, indent, and put a visual marker in the gutter to denote the wrap.

  tmp := tmp +
    '    </style>' + CRLF +
    '  </head>' + CRLF + CRLF +
    '  <body>' + CRLF + CRLF +
    '    <table caption="' + Title + ' (' + inttostr(Grid.RootNodeCount) + ' rows)">' + CRLF +
    '      <thead>' + CRLF +
    '        <tr>' + CRLF;
  for i:=0 to Length(GridData.Columns) - 1 do begin
    // Skip hidden key columns.
    if not (coVisible in Grid.Header.Columns[i].Options) then
      Continue;
    // Add header item.
    Data := GridData.Columns[i].Name;
    tmp := tmp + '          <th class="col' + IntToStr(i) + '">' + Data + '</th>' + CRLF;
  end;
  tmp := tmp +
    '        </tr>' + CRLF +
    '      </thead>' + CRLF +
    '      <tbody>' + CRLF;
  StreamWrite(S, tmp);

  Grid.Visible := false;
  Node := Grid.GetFirst;
  while Assigned(Node) do begin
    // Update status once in a while.
    if (Node.Index+1) mod 100 = 0 then
      ExportStatusMsg(Node, Grid.RootNodeCount, S.Size);
    tmp := '        <tr>' + CRLF;
    // Ensure basic data is loaded
    Mainform.EnsureChunkLoaded(Grid, Node, True);
    for i:=0 to Length(GridData.Columns) - 1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      Data := Grid.Text[Node, i];
      // Handle nulls.
      if GridData.Rows[Node.Index].Cells[i].IsNull then Data := TEXT_NULL;
      // Escape HTML control characters in data.
      Data := htmlentities(Data);
      tmp := tmp + '          <td class="col' + IntToStr(i) + '">' + Data + '</td>' + CRLF;
    end;
    tmp := tmp + '        </tr>' + CRLF;
    StreamWrite(S, tmp);
    // Release some memory.
    Mainform.DiscardNodeData(Grid, Node);
    Node := Grid.GetNext(Node);
    if (MaxSize > 0) and Assigned(Node) and (S is TMemoryStream) and (S.Size >= MaxSize) then begin
      MessageDlg(
        Format(MSG_COPYMAXSIZE, [FormatByteNumber(MaxSize), FormatNumber(Node.Index), FormatNumber(Grid.RootNodeCount)]),
        mtWarning, [mbOK], 0);
      break;
    end;
  end;
  // footer:
  tmp :=
    '      </tbody>' + CRLF +
    '    </table>' + CRLF + CRLF +
    '    <p>' + CRLF +
    '      <em>generated ' + DateToStr(now) + ' ' + TimeToStr(now) +
    '      by <a href="'+APPDOMAIN+'">' + Generator + '</a></em>' + CRLF +
    '    </p>' + CRLF + CRLF +
    '  </body>' + CRLF +
    '</html>' + CRLF;
  StreamWrite(S, tmp);
  Grid.Visible := true;
  Mainform.ProgressBarStatus.Visible := False;
  Mainform.Showstatus(STATUS_MSG_READY);
end;



{***
  Converts a TDataSet to CSV-values.
  @param Grid Object which holds data to export
  @param string Field-separator
  @param string Field-encloser
  @param string Line-terminator
}
procedure GridToCsv(Grid: TVirtualStringTree; GridData: PGridResult; Separator, Encloser, Terminator: String; S: TStream);
var
  i, MaxSize: Integer;
  tmp, Data: WideString;
  Node: PVirtualNode;
begin
  if Grid = Mainform.DataGrid then begin
    // Discard all loaded data so EnsureChunkLoaded refetches it with full content lengths.
    // This makes it superflous to call EnsureFullWidth and to have a unique key
    for i := 0 to Length(GridData.Rows) - 1 do
      GridData.Rows[i].Loaded := False;
  end;

  separator := esc2ascii(separator);
  encloser := esc2ascii(encloser);
  terminator := esc2ascii(terminator);
  MaxSize := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE) * SIZE_MB;
  EnableProgressBar(Grid.RootNodeCount);

  tmp := '';
  // Columns
  for i:=0 to Grid.Header.Columns.Count-1 do begin
    // Skip hidden key columns
    if not (coVisible in Grid.Header.Columns[i].Options) then
      Continue;
    Data := GridData.Columns[i].Name;
    // Alter column name in header if data is not raw.
    if GridData.Columns[i].IsBinary then Data := 'HEX(' + Data + ')';
    // Add header item.
    if tmp <> '' then tmp := tmp + Separator;
    tmp := tmp + Encloser + Data + Encloser;
  end;
  tmp := tmp + Terminator;
  StreamWrite(S, tmp);

  Grid.Visible := false;

  // Data:
  Node := Grid.GetFirst;
  while Assigned(Node) do begin
    if (Node.Index+1) mod 100 = 0 then
      ExportStatusMsg(Node, Grid.RootNodeCount, S.Size);
    tmp := '';
    // Ensure basic data is loaded
    Mainform.EnsureChunkLoaded(Grid, Node, True);
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      Data := Grid.Text[Node, i];
      // Remove 0x.
      if GridData.Columns[i].IsBinary then Delete(Data, 1, 2);
      // Unformat float values
      if GridData.Columns[i].IsFloat then Data := FloatStr(Data);
      // Escape encloser characters inside data per de-facto CSV.
      Data := WideStringReplace(Data, Encloser, Encloser + Encloser, [rfReplaceAll]);
      // Special handling for NULL (MySQL-ism, not de-facto CSV: unquote value)
      if GridData.Rows[Node.Index].Cells[i].IsNull then Data := 'NULL'
      else Data := Encloser + Data + Encloser;
      // Add cell.
      if tmp <> '' then tmp := tmp + Separator;
      tmp := tmp + Data;
    end;
    tmp := tmp + Terminator;
    StreamWrite(S, tmp);
    // Release some memory.
    Mainform.DiscardNodeData(Grid, Node);
    Node := Grid.GetNext(Node);
    if (MaxSize > 0) and Assigned(Node) and (S is TMemoryStream) and (S.Size >= MaxSize) then begin
      MessageDlg(
        Format(MSG_COPYMAXSIZE, [FormatByteNumber(MaxSize), FormatNumber(Node.Index), FormatNumber(Grid.RootNodeCount)]),
        mtWarning, [mbOK], 0);
      break;
    end;
  end;
  Grid.Visible := true;
  Mainform.ProgressBarStatus.Visible := False;
  Mainform.showstatus(STATUS_MSG_READY);
end;



{***
  Converts a TDataSet to XML.
  @param Grid Object which holds data to export
  @param string Text used as root-element
}
procedure GridToXml(Grid: TVirtualStringTree; GridData: PGridResult; root: WideString; S: TStream);
var
  i, MaxSize: Integer;
  tmp, Data: WideString;
  Node: PVirtualNode;
begin
  if Grid = Mainform.DataGrid then begin
    // Discard all loaded data so EnsureChunkLoaded refetches it with full content lengths.
    // This makes it superflous to call EnsureFullWidth and to have a unique key
    for i := 0 to Length(GridData.Rows) - 1 do
      GridData.Rows[i].Loaded := False;
  end;

  MaxSize := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE) * SIZE_MB;
  EnableProgressBar(Grid.RootNodeCount);
  tmp := '<?xml version="1.0"?>' + CRLF + CRLF +
      '<table name="'+root+'">' + CRLF;
  StreamWrite(S, tmp);

  // Avoid reloading discarded data before the end.
  Grid.Visible := false;
  Node := Grid.GetFirst;
  while Assigned(Node) do begin
    if (Node.Index+1) mod 100 = 0 then
     ExportStatusMsg(Node, Grid.RootNodeCount, S.Size);
    tmp := #9'<row>' + CRLF;
    // Ensure basic data is loaded.
    Mainform.EnsureChunkLoaded(Grid, Node, True);
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      // Print cell start tag.
      tmp := tmp + #9#9'<' + Grid.Header.Columns[i].Text;
      if GridData.Rows[Node.Index].Cells[i].IsNull then tmp := tmp + ' isnull="true" />' + CRLF
      else begin
        if GridData.Columns[i].IsBinary then tmp := tmp + ' format="hex"';
        tmp := tmp + '>';
        Data := Grid.Text[Node, i];
        // Remove 0x.
        if GridData.Columns[i].IsBinary then Delete(Data, 1, 2);
        // Unformat float values
        if GridData.Columns[i].IsFloat then Data := FloatStr(Data);
        // Escape XML control characters in data.
        Data := htmlentities(Data);
        // Add data and cell end tag.
        tmp := tmp + Data + '</' + Grid.Header.Columns[i].Text + '>' + CRLF;
      end;
    end;
    tmp := tmp + #9'</row>' + CRLF;
    StreamWrite(S, tmp);
    // Release some memory.
    Mainform.DiscardNodeData(Grid, Node);
    Node := Grid.GetNext(Node);
    if (MaxSize > 0) and Assigned(Node) and (S is TMemoryStream) and (S.Size >= MaxSize) then begin
      MessageDlg(
        Format(MSG_COPYMAXSIZE, [FormatByteNumber(MaxSize), FormatNumber(Node.Index), FormatNumber(Grid.RootNodeCount)]),
        mtWarning, [mbOK], 0);
      break;
    end;
  end;
  // footer:
  tmp := '</table>' + CRLF;
  StreamWrite(S, tmp);
  Grid.Visible := true;
  Mainform.ProgressBarStatus.Visible := False;
  Mainform.showstatus(STATUS_MSG_READY);
end;


{***
  Converts a TDataSet to XML.
  @param Grid Object which holds data to export
  @param string Text used as tablename in INSERTs
}
procedure GridToSql(Grid: TVirtualStringTree; GridData: PGridResult; Tablename: WideString; S: TStream);
var
  i, MaxSize: Integer;
  tmp, Data: WideString;
  Node: PVirtualNode;
begin
  if Grid = Mainform.DataGrid then begin
    // Discard all loaded data so EnsureChunkLoaded refetches it with full content lengths.
    // This makes it superflous to call EnsureFullWidth and to have a unique key
    for i := 0 to Length(GridData.Rows) - 1 do
      GridData.Rows[i].Loaded := False;
  end;

  MaxSize := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE) * SIZE_MB;
  EnableProgressBar(Grid.RootNodeCount);
  // Avoid reloading discarded data before the end.
  Grid.Visible := false;
  Node := Grid.GetFirst;
  while Assigned(Node) do begin
    if (Node.Index+1) mod 100 = 0 then
     ExportStatusMsg(Node, Grid.RootNodeCount, S.Size);
    tmp := 'INSERT INTO '+Mainform.Mask(Tablename)+' (';
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      tmp := tmp + Mainform.mask(Grid.Header.Columns[i].Text)+', ';
    end;
    Delete(tmp, Length(tmp)-1, 2);
    tmp := tmp + ') VALUES (';
    // Ensure basic data is loaded.
    Mainform.EnsureChunkLoaded(Grid, Node, True);
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      if GridData.Rows[Node.Index].Cells[i].IsNull then
        tmp := tmp + 'NULL'
      else begin             
        Data := Grid.Text[Node, i];
        // Remove 0x.
        if GridData.Columns[i].IsBinary then Delete(Data, 1, 2);
        // Unformat float values
        if GridData.Columns[i].IsFloat then Data := FloatStr(Data);
        // Add data and cell end tag.
        tmp := tmp + esc(Data);
      end;
      tmp := tmp + ', ';
    end;
    Delete(tmp, Length(tmp)-1, 2);
    tmp := tmp + ');' + CRLF;
    StreamWrite(S, tmp);
    // Release some memory.
    Mainform.DiscardNodeData(Grid, Node);
    Node := Grid.GetNext(Node);
    if (MaxSize > 0) and Assigned(Node) and (S is TMemoryStream) and (S.Size >= MaxSize) then begin
      MessageDlg(
        Format(MSG_COPYMAXSIZE, [FormatByteNumber(MaxSize), FormatNumber(Node.Index), FormatNumber(Grid.RootNodeCount)]),
        mtWarning, [mbOK], 0);
      break;
    end;
  end;
  // footer:
  tmp := CRLF;
  StreamWrite(S, tmp);
  Grid.Visible := true;
  Mainform.ProgressBarStatus.Visible := False;
  Mainform.showstatus(STATUS_MSG_READY);
end;



{***
  Return ASCII-Values from MySQL-Escape-Sequences

  @param string Text to analyze
  @return string Converted text
}
function esc2ascii(str: String): String;
begin
  str := stringreplace(str, '\r', #13, [rfReplaceAll]);
  str := stringreplace(str, '\n', #10, [rfReplaceAll]);
  str := stringreplace(str, '\t', #9, [rfReplaceAll]);
  result := str;
end;



{***
  Get higher value of two integers

  @param integer Number 1 to compare
  @param integer Number 2 to compare
  @return integer Higher value
}
function Max(A, B: Integer): Integer; assembler;
asm
  CMP EAX,EDX
  JG  @Exit
  MOV EAX,EDX
@Exit:
end;



{***
  Get lower value of two integers

  @param integer Number 1 to compare
  @param integer Number 2 to compare
  @return integer Lower value of both parameters
}
function Min(A, B: Integer): Integer; assembler;
asm
  CMP EAX,EDX
  JL  @Exit
  MOV EAX,EDX
@Exit:
end;



{***
  Left hand string-comparison

  @param string Text 1 to compare
  @param string Text 2 to compare
  @return boolean Does the longer string of both contain the shorter string at the beginning?
}
function StrCmpBegin(Str1, Str2: string): Boolean;
begin
  if ((Str1 = '') or (Str2 = '')) and (Str1 <> Str2) then
    Result := False
  else
    Result := (StrLComp(PChar(Str1), PChar(Str2),
      Min(Length(Str1), Length(Str2))) = 0);
end;



{***
  Encode spaces (and more to come) in URLs

  @param string URL to encode
  @return string
}
function urlencode(url: String): String;
begin
  result := stringreplace(url, ' ', '+', [rfReplaceAll]);
end;


{***
  Create UTF-8 text file
}
function openfs(filename: String): TFileStream;
begin
    Result := TFileStream.Create(filename, fmCreate);
    Result.WriteBuffer(sUTF8BOMString, 3);
end;

{***
  Write a line of text plus CRLF to existing FileStream in UTF-8
  @param TFileStream
  @param string Text to write
  @return void
}
procedure wfs( var s: TFileStream; str: WideString = '');
begin
  StreamWrite(s, str + CRLF);
end;


{**
  Write some UTF8 text to a file- or memorystream
}
procedure StreamWrite(S: TStream; Text: WideString = '');
var
  utf8: string;
begin
  utf8 := Utf8Encode(Text);
  S.Write(utf8[1], Length(utf8));
end;



{***
  Make SQL statement compatible with either ANSI SQL or MySQL
  @note Works around MySQL placement of semicolon in conditional
        statements, which is not compatible with standard SQL (making
        the whole point of conditional statements slightly moot?).
        The broken format is unfortunately the only format that the
        mysql cli will eat, according to one user...

        btnExportClick() could be rewritten to better handle this sort
        of thing, but for now / with the current code, this is the easiest
        way of accomplishing the desired effect.
  @param string SQL statement
  @param integer MySQL-version or SQL_VERSION_ANSI
  @return string SQL
}
function fixSQL( sql: WideString; sql_version: Integer = SQL_VERSION_ANSI; cli_workarounds: Boolean = false ): WideString;
var
  rx : TRegExpr;
begin
  result := sql;

  // For mysqldump and mysql.exe CLI compatibility
  if cli_workarounds then
  begin
    result := WideStringReplace(result, ';*/', '*/;', [rfReplaceAll]);
  end;

  // Detect if SQL is a CREATE TABLE statement
  if copy( result, 1, 12 ) = 'CREATE TABLE' then
  begin
    rx := TRegExpr.Create;
    // Greedy: take as many chars as I can get
    rx.ModifierG := True;
    // Case insensitive
    rx.ModifierI := True;

    if sql_version < 40100 then begin
      // Strip charset definition
      // see issue #1685835
      rx.Expression := '\s+(DEFAULT\s+)?(CHARSET|CHARACTER SET)=\w+';
      result := rx.Replace(result, '' );
      // Strip collation part
      rx.Expression := '\s+COLLATE=\w+';
      result := rx.Replace(result, '' );
    end;

    if sql_version = SQL_VERSION_ANSI then begin
      // Switch quoting char
      result := StringReplace(result, '`', '"', [rfReplaceAll]);
      // Strip ENGINE|TYPE
      rx.Expression := '\s+(ENGINE|TYPE)=\w+';
      result := rx.Replace(result, '' );
    end;

    // Turn ENGINE to TYPE
    if sql_version < 40102 then
      result := WideStringReplace(result, 'ENGINE=', 'TYPE=', [rfReplaceAll])
    else
      result := WideStringReplace(result, 'TYPE=', 'ENGINE=', [rfReplaceAll]);

    // Mask USING {BTREE,HASH,RTREE} from older servers.
    rx.Expression := '\s(USING\s+\w+)';
    result := rx.Replace(result, ' /*!50100 $1 */', True);

    rx.Free;
  end;

end;


{***
  Check/Uncheck all items in a CheckListBox

  @param TCheckListBox List with checkable items
  @param boolean Check them?
  @return void
}
procedure ToggleCheckListBox(list: TTNTCheckListBox; state: Boolean);
var
  i : Integer;
begin
  // select all/none
  for i:=0 to list.Items.Count-1 do
    list.checked[i] := state;
end;


{***
  Check/Uncheck items in a CheckListBox which come in a second list

  @param TCheckListBox List with checkable items
  @param boolean Check them?
  @param TStringList Second list with items to change
  @return void
}
procedure ToggleCheckListBox(list: TTNTCheckListBox; state: Boolean; list_toggle: TWideStringList);
var
  i : Integer;
begin
  for i:=0 to list.Items.Count-1 do begin
    if list_toggle.IndexOf(list.Items[i]) > -1 then
      list.Checked[i] := state
    else
      list.Checked[i] := not state;
  end;
end;



{***
  Get filesize of a given file

  @param string Filename
  @return int64 Size in bytes
}
function _GetFileSize(filename: String): Int64;
var
  i64: record
    LoDWord: LongWord;
    HiDWord: LongWord;
  end;
  stream : TFileStream;
begin
  Stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    i64.LoDWord := GetFileSize(Stream.Handle, @i64.HiDWord);
  finally
    Stream.Free;
  end;
  if (i64.LoDWord = MAXDWORD) and (GetLastError <> 0) then
    Result := 0
  else
    Result := PInt64(@i64)^;
end;



{***
  Change "C:\Program Files\Delphi\DDrop\TargetDemo\main.pas"
  to:    "C:\Program Files\..\main.pas"

  @param string File/Directory
  @param integer Shorten name to this maximum amount of chars
  @return string
}
function Mince(PathToMince: String; InSpace: Integer): String;
Var
  sl: TStringList;
  sHelp, sFile: String;
  iPos: Integer;

Begin
  sHelp := PathToMince;
  iPos := Pos('\', sHelp);
  If iPos = 0 Then
  Begin
    Result := PathToMince;
  End
  Else
  Begin
    sl := TStringList.Create;
    // Decode string
    While iPos <> 0 Do
    Begin
      sl.Add(Copy(sHelp, 1, (iPos - 1)));
      sHelp := Copy(sHelp, (iPos + 1), Length(sHelp));
      iPos := Pos('\', sHelp);
    End;
    If sHelp <> '' Then
    Begin
      sl.Add(sHelp);
    End;
    // Encode string
    sFile := sl[sl.Count - 1];
    sl.Delete(sl.Count - 1);
    Result := '';
    While (Length(Result + sFile) < InSpace) And (sl.Count <> 0) Do
    Begin
      Result := Result + sl[0] + '\';
      sl.Delete(0);
    End;
    If sl.Count = 0 Then
    Begin
      Result := Result + sFile;
    End
    Else
    Begin
      Result := Result + '..\' + sFile;
    End;
    sl.Free;
  End;
End;



{***
  Convert a string-number to an integer-number

  @param string String-number
  @return int64
}
function MakeInt( Str: String ) : Int64;
begin
  // Result has to be of integer type
  Result := Trunc( MakeFloat( Str ) );
end;


{***
  Convert a string-number to an floatingpoint-number

  @param String text representation of a number
  @return Extended
}
function MakeFloat( Str: String ): Extended;
var
  i : Integer;
  StrNumber : String;
  p_kb, p_mb, p_gb, p_tb, p_pb : Integer;
begin
  StrNumber := '';
  for i:=1 to Length(Str) do
  begin
    if (Str[i] in ['0'..'9', DecimalSeparator]) or ((Str[i] = '-') and (StrNumber='')) then
    begin
      // Avoid confusion and AV in StrToFloat()
      if (ThousandSeparator = DecimalSeparator) and (Str[i] = DecimalSeparator) then
        continue;
      StrNumber := StrNumber + Str[i];
    end;
  end;

  // Convert result to a floating point value to ensure
  // we don't discard decimal digits for the next step
  if (StrNumber = '') or (StrNumber = '-') then Result := 0
  else Result := StrToFloat( StrNumber );

  // Detect if the string was previously formatted by FormatByteNumber
  // and convert it back by multiplying it with its byte unit
  p_kb := Pos(NAME_KB, Str);
  p_mb := Pos(NAME_MB, Str);
  p_gb := Pos(NAME_GB, Str);
  p_tb := Pos(NAME_TB, Str);
  p_pb := Pos(NAME_PB, Str);

  if (p_kb > 0) and (p_kb = Length(Str)-Length(NAME_KB)+1) then
    Result := Result * SIZE_KB
  else if (p_mb > 0) and (p_mb = Length(Str)-Length(NAME_MB)+1) then
    Result := Result * SIZE_MB
  else if (p_gb > 0) and (p_gb = Length(Str)-Length(NAME_GB)+1) then
    Result := Result * SIZE_GB
  else if (p_tb > 0) and (p_tb = Length(Str)-Length(NAME_TB)+1) then
    Result := Result * SIZE_TB
  else if (p_pb > 0) and (p_pb = Length(Str)-Length(NAME_PB)+1) then
    Result := Result * SIZE_PB;
end;


{**
  Unformat a formatted float value. Used for CSV export and composing WHERE clauses for grid editing.
}
function FloatStr(Val: String): String;
begin
  Result := Val;
  if ThousandSeparator <> DecimalSeparator then
    Result := StringReplace(Val, ThousandSeparator, '', [rfReplaceAll]);
  Result := StringReplace(Val, DecimalSeparator, '.', [rfReplaceAll]);
end;


{***
 Attempt to do string replacement faster than StringReplace and WideStringReplace.
}
function escChars(const Text: WideString; EscChar, Char1, Char2, Char3, Char4: WideChar): WideString;
const
  // Attempt to match whatever the CPU cache will hold.
  block: Cardinal = 65536;
var
  bstart, bend, matches, i: Cardinal;
  // These could be bumped to uint64 if necessary.
  len, respos: Cardinal;
  next: WideChar;
begin
  len := Length(Text);
  Result := '';
  bend := 0;
  respos := 0;
  repeat
    bstart := bend + 1;
    bend := bstart + block - 1;
    if bend > len then bend := len;
    matches := 0;
    for i := bstart to bend do if
      (Text[i] = Char1) or
      (Text[i] = Char2) or
      (Text[i] = Char3) or
      (Text[i] = Char4)
    then Inc(matches);
    SetLength(Result, bend + 1 - bstart + matches + respos);
    for i := bstart to bend do begin
      next := Text[i];
      if
        (next = Char1) or
        (next = Char2) or
        (next = Char3) or
        (next = Char4)
      then begin
        Inc(respos);
        Result[respos] := EscChar;
        // Special values for MySQL escape.
        if next = #13 then next := 'r';
        if next = #10 then next := 'n';
        if next = #0 then next := '0';
      end;
      Inc(respos);
      Result[respos] := next;
    end;
  until bend = len;
end;


{***
  Escape all kinds of characters:
  - single-backslashes which represent normal parts of the text and not escape-sequences
  - characters which MySQL doesn't strictly care about, but which might confuse editors etc.
  - single and double quotes in a text string
  - joker-chars for LIKE-comparisons
  Finally, surround the text by single quotes.

  @param string Text to escape
  @param boolean Escape text so it can be used in a LIKE-comparison
  @return string
}
function esc(Text: WideString; ProcessJokerChars: Boolean = false; sql_version: integer = 50000): WideString;
var
  c1, c2, c3, c4, EscChar: WideChar;
begin
  c1 := '''';
  c2 := '\';
  c3 := '%';
  c4 := '_';
  EscChar := '\';
  if (not ProcessJokerChars) or (sql_version = SQL_VERSION_ANSI) then begin
    // Do not escape joker-chars which are used in a LIKE-clause
    c4 := '''';
    c3 := '''';
  end;
  if sql_version = SQL_VERSION_ANSI then begin
    c2 := '''';
    EscChar := '''';
  end;
  Result := escChars(Text, EscChar, c1, c2, c3, c4);
  if sql_version <> SQL_VERSION_ANSI then begin
    // Remove characters that SynEdit chokes on, so that
    // the SQL file can be non-corruptedly loaded again.
    c1 := #13;
    c2 := #10;
    c3 := #0;
    c4 := #0;
    // TODO: SynEdit also chokes on WideChar($2028) and possibly WideChar($2029).
    Result := escChars(Result, EscChar, c1, c2, c3, c4);
  end;
  if not ProcessJokerChars then begin
    // Add surrounding single quotes only for non-LIKE-values
    // because in all cases we're using ProcessLIKEChars we
    // need to add leading and/or trailing joker-chars by hand
    // without being escaped
    Result := WideChar(#39) + Result + WideChar(#39);
  end;
end;



{***
  Detect NUL character in a text.
  Useful because fx SynEdit cuts of all text after it encounters a NUL.
}
function ScanNulChar(Text: WideString): boolean;
var
  i: integer;
begin
  result := false;
  for i:=1 to length(Text) do
  begin
    if Text[i] = #0 then
    begin
      result := true;
      exit;
    end;
  end;
end;



{***
  SynEdit removes all newlines and semi-randomly decides a
  new newline format to use for any text edited.
  See also: Delphi's incomplete implementation of TTextLineBreakStyle in System.pas

  @param string Text to test
  @return TLineBreaks
}
function ScanLineBreaks(Text: WideString): TLineBreaks;
var
  i: integer;
  c: WideChar;
  procedure SetResult(Style: TLineBreaks);
  begin
    // Note: Prefer "(foo <> a) and (foo <> b)" over "not (foo in [a, b])" in excessive loops
    // for performance reasons - there is or was a Delphi bug leaving those inline SETs in memory
    // after usage. Unfortunately can't remember which bug id it was and if it still exists.
    if (Result <> lbsNone) and (Result <> Style) then
      Result := lbsMixed
    else
      Result := Style;
  end;
begin
  Result := lbsNone;
  if length(Text) = 0 then exit;
  i := 1;
  repeat
    c := Text[i];
    if c = #13 then begin
      if (i < length(Text)) and (Text[i+1] = #10) then begin
        Inc(i);
        SetResult(lbsWindows);
      end else
        SetResult(lbsMac);
    end else if c = LB_UNIX then
      SetResult(lbsUnix)
    else if c = LB_WIDE then
      SetResult(lbsWide);
    i := i + 1;
    // No need to do more checks after detecting mixed style
    if Result = lbsMixed then
      break;
  until i > length(Text);
end;



{***
  Mangle input text so that SynEdit can load it.

  @param string Text to test
  @return Boolean
}
function RemoveNulChars(Text: WideString): WideString;
var
  i: integer;
  c: WideChar;
begin
  SetLength(Result, Length(Text));
  if Length(Text) = 0 then Exit;
  i := 1;
  repeat
    c := Text[i];
    if c = #0 then Result[i] := #32
    else Result[i] := c;
    i := i + 1;
  until i > length(Text);
end;



{***
  Use DebugView from SysInternals or Delphi's Event Log to view.

  @param string Text to ouput
  @return void
}
procedure debug(txt: String);
begin
  if length(txt) = 0 then txt := '(debug: blank output?)';
  // Todo: not thread safe.
  dbgCounter := dbgCounter + 1;
  txt := Format(APPNAME+': %d %s', [dbgCounter, txt]);
  OutputDebugString(PChar(txt));
end;



{***
  Unify CR's and LF's to CRLF

  @param string Text to fix
  @return string
}
function fixNewlines(txt: string): string;
begin
  txt := StringReplace(txt, CRLF, #10, [rfReplaceAll]);
  txt := StringReplace(txt, #13, #10, [rfReplaceAll]);
  txt := StringReplace(txt, #10, CRLF, [rfReplaceAll]);
  result := txt;
end;



{***
  Convert a boolean True/False to a string Y/N

  @see TUserManagerForm::getColumnNamesOrValues
  @param boolean Value to convert
  @return string
}
function BoolToStr(val: Boolean): String;
begin
  if val then
    result := 'Y'
  else
    result := 'N';
end;


function StrToBool(val: String): Boolean;
begin
  Result := val = 'Y';
end;


{***
  Get the path of a Windows(r)-shellfolder, specified by an integer or a constant

  @param integer Number or constant
  @return string Path
}
function GetShellFolder(CSIDL: integer): string;
var
  pidl                   : PItemIdList;
  FolderPath             : string;
  SystemFolder           : Integer;
  Malloc                 : IMalloc;
begin
  Malloc := nil;
  FolderPath := '';
  SHGetMalloc(Malloc);
  if Malloc = nil then
  begin
    Result := FolderPath;
    Exit;
  end;
  try
    SystemFolder := CSIDL;
    if SUCCEEDED(SHGetSpecialFolderLocation(0, SystemFolder, pidl)) then
    begin
      SetLength(FolderPath, max_path);
      if SHGetPathFromIDList(pidl, PChar(FolderPath)) then
      begin
        SetLength(FolderPath, length(PChar(FolderPath)));
      end;
    end;
    Result := FolderPath;
  finally
    Malloc.Free(pidl);
  end;
end;



{***
  Return all files in a given directory into a TStringList

  @param string Folderpath
  @param string Filepattern to filter files, defaults to all files (*.*)
  @return TStringList Filenames
}
function getFilesFromDir( dir: String; pattern: String = '*.*'; hideExt: Boolean = false ): TStringList;
var
  sr : TSearchRec;
  s : String;
begin
  result := TStringList.Create;
  if dir[length(dir)] <> '\' then
    dir := dir + '\';
  if FindFirst( dir + pattern, $3F, sr ) = 0 then
  begin
    repeat
      if (sr.Attr and faAnyFile) > 0 then begin
        s := sr.Name;
        if hideExt and (Pos('.', s) > 0) then begin
          SetLength(s, Pos('.', s) - 1);
        end;
        result.Add( s );
      end;
    until FindNext( sr ) <> 0;
    // FindClose( sr );
  end;
end;



{***
  Remove special characters from a filename

  @param string Filename
  @return string
}
function goodfilename( str: String ): String;
var
  c : Char;
begin
  result := str;
  for c in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] do
    result := StringReplace( result, c, '_', [rfReplaceAll] );
end;



{***
  Return a formatted number from a string
  by first converting it to a float and then to the desired format

  @param string Text containing a number
  @return string
}
function FormatNumber( str: String ): String; Overload;
begin
  if str <> '' then
    result := FormatNumber( StrToFloat( str ) )
  else
    result := FormatNumber( 0 );
end;



{***
  Return a formatted number from an integer

  @param int64 Number to format
  @return string
}
function FormatNumber( int: Int64 ): String; Overload;
begin
  result := FormatNumber( int, 0 );
end;



{***
  Return a formatted number from a float
  This function is called by two overloaded functions

  @param double Number to format
  @param integer Number of decimals
  @return string
}
function FormatNumber( flt: Double; decimals: Integer = 0 ): String; Overload;
begin
  result := trim( format( '%10.'+IntToStr(decimals)+'n', [flt] ) );
end;



{***
  Set global variables containing the standard local format for date and time
  values. Standard means the MySQL-standard format, which is YYYY-MM-DD HH:MM:SS

  @note Be aware that Delphi internally converts the slashes in ShortDateFormat
        to the DateSeparator
}
procedure setLocales;
begin
  DateSeparator := '-';
  TimeSeparator := ':';
  ShortDateFormat := 'yyyy/mm/dd';
  LongTimeFormat := 'hh:nn:ss';
  if DecimalSeparatorSystemdefault = '' then
    DecimalSeparatorSystemdefault := DecimalSeparator;
  DecimalSeparator := DecimalSeparatorSystemdefault;
end;



{***
  Quote identifiers either with double quotes to be ANSI-compatibile,
  mysql-compatible backticks, or not at all for mysql below 3.23

  @param integer MySQL version as compact number
  @param string Identifier
  @return string (not) quoted identifier
  @see http://www.heidisql.com/forum/viewtopic.php?t=161
}
function maskSql(sql_version: integer; str: WideString) : WideString;
begin
  // Quote ANSI-compatible (but not MySQL-compatible)?
  if sql_version = SQL_VERSION_ANSI then
  begin
    result := WideStringReplace(str, '"', '""', [rfReplaceAll]);
    result := '"' + result + '"';
  end
  // Quote MySQL-compatible
  else if sql_version >= 32300 then
  begin
    result := WideStringReplace(str, '`', '``', [rfReplaceAll]);
    result := '`' + result + '`';
  end
  else
  begin
    result := str;
  end;
end;



{***
  Given a Delphi MainForm, acquire the handle of that form's Application.

  @param HWnd The mainform's window handle
  @return HWnd
}
function GetApplication(MainForm: HWnd): HWnd;
begin
  result := GetWindowLong(MainForm, GWL_HWNDPARENT);
end;



{***
  Given a Delphi MainForm, activate the application it belongs to.

  @param HWnd The mainform's window handle
}
procedure ActivateMainForm(MainForm: HWnd);
var
  delphiApp: HWnd;
begin
  delphiApp := GetApplication(MainForm);
  ActivateWindow(delphiApp);
end;



{***
  Activate a specific form

  @note Copyright: This function was nicked from usenet:
        Delphi & focus control by Tony Tanzillo in autodesk.autocad.customization.vba.
  @param HWnd Form to activate
}
procedure ActivateWindow(Window : HWnd);
var
  state : TWindowPlacement;
begin
  if not IsWindow(Window) then exit;
  if IsIconic(Window) then begin
    state.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Window, PWindowPlacement(@state));
    if (state.flags and WPF_RESTORETOMAXIMIZED) = WPF_RESTORETOMAXIMIZED then begin
      ShowWindow(Window, SW_SHOWMAXIMIZED)
    end else begin
      ShowWindow(Window, SW_RESTORE);
    end;
  end;
  BringWindowToTop(Window);
  SetForegroundWindow(Window);
end;



{***
  Open URL or execute system command

  @param string Command or URL to execute
  @param string Working directory, only usefull is first param is a system command
}
procedure ShellExec( cmd: String; path: String = '' );
var
  ppath : PChar;
begin
  if path <> '' then
    ppath := pchar(path)
  else
    ppath := nil;
  ShellExecute(0, 'open', pchar(cmd), Nil, ppath, SW_SHOWNORMAL);
end;



{***
  Returns first word of a given text
  @param string Given text
  @return string First word-boundary
}
function getFirstWord( text: String ): String;
var
  i : Integer;
  wordChars, wordCharsFirst : Set of Char;
begin
  result := '';
  text := trim( text );
  // First char in word must not be numerical. Fixes queries like
  // /*!40000 SHOW ENGINES */ to be recognized as "result"-queries
  // while not breaking getFirstWord in situations where the second
  // or later char can be a number (fx the collation in createdatabase).
  wordChars := ['a'..'z', 'A'..'Z', '0'..'9', '_', '-'];
  wordCharsFirst := wordChars - ['0'..'9'];
  i := 1;

  // Find beginning of the first word, ignoring non-alphanumeric chars at the very start
  // @see bug #1692828
  while i < Length(text) do
  begin
    if (text[i] in wordCharsFirst) then
    begin
      // Found beginning of word!
      break;
    end;
    if i = Length(text)-1 then
    begin
      // Give up in the very last loop, reset counter
      // and break. We can't find the start of a word
      i := 1;
      break;
    end;
    inc(i);
  end;

  // Add chars as long as they're alpha-numeric
  while i <= Length(text) do
  begin
    if ((result = '') and (text[i] in wordCharsFirst)) or (text[i] in wordChars) then
    begin
      result := result + text[i];
    end
    else
    begin
      // Stop here because we found a non-alphanumeric char.
      // This applies to all different whitespaces, brackets, commas etc.
      break;
    end;
    inc(i);
  end;
end;


{***
  This function maps MySQL character sets to Windows ANSI codepages.
}
function ConvertWindowsCodepageToMysqlCharacterSet(codepage: Cardinal): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Length(charset_conv_table) - 1 do begin
    if charset_conv_table[i].codepage = codepage then begin
      result := charset_conv_table[i].charset;
      exit;
    end;
  end;
end;


{**
  Get last position of substr in str
  @param string Substring
  @param string Text
  @return Integer Last position
}
function LastPos(needle: WideChar; haystack: WideString): Integer;
var
  reverse: WideString;
  i, len, w: Integer;
begin
  // Reverse string.
  len := Length(haystack);
  SetLength(reverse, len);
  for i := 1 to len do reverse[i] := haystack[len - i + 1];
  // Look for needle.
  Result := 0;
  w := Pos(needle, reverse);
  if w > 0 then Result := len - w + 1;
end;


{**
  Convert integer version to real version string
}
function ConvertServerVersion( Version: Integer ): String;
var
  v : String;
  v1, v2 : Byte;
begin
  v := IntToStr( Version );
  v1 := StrToIntDef( v[2]+v[3], 0 );
  v2 := StrToIntDef( v[4]+v[5], 0 );
  Result := v[1] + '.' + IntToStr(v1) + '.' + IntToStr(v2);
end;


{**
  Format a filesize to automatically use the best fitting expression
  16 100 000 Bytes -> 16,1 MB
  4 500 Bytes -> 4,5 KB
  @param Int64 Number of Bytes
  @param Byte Decimals to display when bytes is bigger than 1M
}
function FormatByteNumber( Bytes: Int64; Decimals: Byte = 1 ): String; Overload;
begin
  if Bytes >= SIZE_PB then
    Result := FormatNumber( Bytes / SIZE_PB, Decimals ) + NAME_PB
  else if Bytes >= SIZE_TB then
    Result := FormatNumber( Bytes / SIZE_TB, Decimals ) + NAME_TB
  else if Bytes >= SIZE_GB then
    Result := FormatNumber( Bytes / SIZE_GB, Decimals ) + NAME_GB
  else if Bytes >= SIZE_MB then
    Result := FormatNumber( Bytes / SIZE_MB, Decimals ) + NAME_MB
  else if Bytes >= SIZE_KB then
    Result := FormatNumber( Bytes / SIZE_KB, Decimals ) + NAME_KB
  else
    Result := FormatNumber( Bytes ) + NAME_BYTES
end;


{**
  An overloaded function of the previous one which can
  take a string as input
}
function FormatByteNumber( Bytes: String; Decimals: Byte = 1 ): String; Overload;
begin
  Result := FormatByteNumber( MakeInt(Bytes), Decimals );
end;


{**
  Format a number of seconds to a human readable time format
  @param Cardinal Number of seconds
  @result String 12:34:56
}
function FormatTimeNumber( Seconds: Cardinal ): String;
var
  h, m, s : Integer;
begin
  s := Seconds mod (60*60*24);
  h := s div (60*60);
  s := s mod (60*60);
  m := s div 60;
  s := s mod 60;
  Result := Format('%.2d:%.2d:%.2d', [h, m, s]);
end;


{**
  Return TColor value in XXXXXX format
  (X being a hex digit)
}
function TColorToHex( Color : TColor ): string;
begin
  Result := '#' +
    { red   } IntToHex( GetRValue( Color ), 2 ) +
    { green } IntToHex( GetGValue( Color ), 2 ) +
    { blue }  IntToHex( GetBValue( Color ), 2 );
end;


{**
  Return a TStringList with captions from all selected nodes in a VirtualTree
  Especially helpful when toMultiSelect is True
}
function GetVTCaptions( VT: TVirtualStringTree; OnlySelected: Boolean = False; Column: Integer = 0; OnlyNodeType: TListNodeType = lntNone ): TWideStringList;
var
  SelectedNodes : TNodeArray;
  NodeData: PVTreeData;
  i: Integer;
  a : TVTreeDataArray;
begin
  Result := TWideStringList.Create;
  if OnlySelected then
  begin
    // Fetch only selected nodes
    SelectedNodes := VT.GetSortedSelection(False);
    for i := 0 to Length(SelectedNodes) - 1 do
    begin
      NodeData := VT.GetNodeData( SelectedNodes[i] );
      if (OnlyNodeType = lntNone) // Add all nodes, regardless of their types
        or (NodeData.NodeType = OnlyNodeType) then // Node in loop is of specified type
        Result.Add( NodeData.Captions[Column] );
    end;
  end
  else begin
    // Fetch all nodes
    a := Mainform.GetVTreeDataArray( VT )^;
    for i := 0 to High(a) do begin
      if (OnlyNodeType = lntNone)
        or (a[i].NodeType = OnlyNodeType) then
        Result.Add( a[i].Captions[ Column ] );
    end;
  end;
end;


{**
  The opposite of GetVTCaptions in "OnlySelected"-Mode:
  Set selected nodes in a VirtualTree
}
procedure SetVTSelection( VT: TVirtualStringTree; Selected: TWideStringList );
var
  Node: PVirtualNode;
  NodeData: PVTreeData;
  IsSel, FoundFocus: Boolean;
begin
  Node := VT.GetFirst;
  FoundFocus := False;
  while Assigned(Node) do begin
    NodeData := VT.GetNodeData(Node);
    IsSel := Selected.IndexOf(NodeData.Captions[0]) > -1;
    VT.Selected[Node] := IsSel;
    if IsSel and not FoundFocus then begin
      VT.FocusedNode := Node;
      FoundFocus := True;
    end;
    if IsSel and not (toMultiSelect in VT.TreeOptions.SelectionOptions) then
      break;
    Node := VT.GetNext(Node);
  end;
end;


function Pos2(const Needle, HayStack: string; const StartPos: Integer) : Integer;
var
  NewHayStack: string;
begin
  NewHayStack := Copy(HayStack, StartPos, Length(HayStack));
  Result := Pos(Needle, NewHayStack);
  if Result > 0 then Result := Result + StartPos - 1;
end;


function GetTempDir: String;
var
  TempPath: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, PAnsiChar(@TempPath));
  Result := StrPas(TempPath);
end;


// Tell type of db object (table|view) by a given row from a SHOW TABLE STATUS result
function GetDBObjectType( TableStatus: TFields ): TListNodeType;
var
  t: String;
begin
  {**
    @see http://dev.mysql.com/doc/refman/5.1/en/show-table-status.html
      For views, all the fields displayed by SHOW TABLE STATUS are NULL except
      that Name indicates the view name and Comment says view.
    @note The "Comment" column can contain different content, normally "VIEW"
      but for views which is missing its tables, it says
      "Views bla references invalid..."
  }
  Result := lntTable;
  if TableStatus.FindField('Type') <> nil then begin
    t := TableStatus.FindField('Type').AsString;
    if t = 'BASE TABLE' then
      Result := lntTable
    else if t = 'VIEW' then
      Result := lntView
    else if t = 'FUNCTION' then
      Result := lntFunction
    else if t = 'PROCEDURE' then
      Result := lntProcedure;
  end else begin
    if
      TableStatus[1].IsNull and  // Engine column is NULL for views
      TableStatus[2].IsNull and
      (Pos('VIEW', UpperCase(TableStatus.FieldByName(DBO_COMMENT).AsWideString)) > 0)
      then Result := lntView;
    if
      TableStatus[1].IsNull and
      TableStatus[2].IsNull and
      (Pos('MARKED AS CRASHED', UpperCase(TableStatus.FieldByName(DBO_COMMENT).AsWideString)) > 0)
      then Result := lntCrashedTable;
  end;
end;


{
  Code taken from SizeGripHWND.pas:
  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  Alle Rechte vorbehalten.

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
}
function SizeGripWndProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Info: PGripInfo;
  dc: HDC;
  pt: TPoint;

  // Invalidate the current grip rectangle
  procedure InvalidateGrip;
  begin
    with Info^ do
      if (GripRect.Right > GripRect.Left) and
         (GripRect.Bottom > GripRect.Top) then
        InvalidateRect(hWnd, @GripRect, true);
  end;

  // Update (and invalidate) the current grip rectangle
  procedure UpdateGrip;
  begin
    with Info^ do
    begin
      GetClientRect(hWnd, GripRect);
      GripRect.Left := GripRect.Right - GetSystemMetrics(SM_CXHSCROLL);
      GripRect.Top := GripRect.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    end;

    InvalidateGrip;
  end;

  function CallOld: LRESULT;
  begin
    Result := CallWindowProc(@Info^.OldWndProc, hWnd, Msg, wParam, lParam);
  end;

begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if Info = nil then
    Result := DefWindowProc(hWnd, Msg, wParam, lParam)
  else if not Info^.Enabled then
    Result := CallOld
  else
  begin
    case Msg of
      WM_NCDESTROY: begin
        Result := CallOld;

        SetWindowLong(hWnd, GWL_WNDPROC, LongInt(@Info^.OldWndProc));
        RemoveProp(hWnd, SizeGripProp);
        Dispose(Info);
      end;

      WM_PAINT: begin
        Result := CallOld;
        if wParam = 0 then
        begin
          dc := GetDC(hWnd);
          DrawFrameControl(dc, Info^.GripRect, DFC_SCROLL, DFCS_SCROLLSIZEGRIP);
          ReleaseDC(hWnd, dc);
        end;
      end;

      WM_NCHITTEST: begin
        pt.x := TSmallPoint(lParam).x;
        pt.y := TSmallPoint(lParam).y;
        ScreenToClient(hWnd, pt);
        if PtInRect(Info^.GripRect, pt) then
          Result := HTBOTTOMRIGHT
        else
          Result := CallOld;
      end;

      WM_SIZE: begin
        InvalidateGrip;
        Result := CallOld;
        UpdateGrip;
      end;

      else
        Result := CallOld;
    end;
  end;
end;

{ Note that SetWindowSizeGrip(..., false) does not really remove the hook -
  it just sets "Enabled" to false. The hook plus all data is removed when
  the window is destroyed.
}
procedure SetWindowSizeGrip(hWnd: HWND; Enable: boolean);
var
  Info: PGripInfo;
begin
  Info := PGripInfo(GetProp(hWnd, SizeGripProp));
  if (Info = nil) and Enable then
  begin
    New(Info);
    FillChar(Info^, SizeOf(TGripInfo), 0);

    with Info^ do
    begin
      Info^.OldWndProc := TWndProc(Pointer(GetWindowLong(hWnd, GWL_WNDPROC)));

      GetClientRect(hWnd, GripRect);
      GripRect.Left := GripRect.Right - GetSystemMetrics(SM_CXHSCROLL);
      GripRect.Top := GripRect.Bottom - GetSystemMetrics(SM_CYVSCROLL);
    end;

    SetProp(hWnd, SizeGripProp, Cardinal(Info));
    SetWindowLong(hWnd, GWL_WNDPROC, LongInt(@SizeGripWndProc));
  end;

  if (Info <> nil) then
    if Enable <> Info^.Enabled then
      with Info^ do
      begin
        Enabled := Enable;
        if (GripRect.Right > GripRect.Left) and
           (GripRect.Bottom > GripRect.Top) then
          InvalidateRect(hWnd, @GripRect, true);
      end;
end;


{**
  Save a textfile with unicode
}
procedure SaveUnicodeFile(Filename: String; Text: WideString);
var
  f: TFileStream;
begin
  f := CreateUnicodeFileStream(Filename);
  f.WriteBuffer(Pointer(Text)^, Length(Text) * 2);
  f.Free;
end;


function CreateUnicodeFileStream(Filename: String): TFileStream;
var
  header: array[0..1] of Byte;
begin
  header[0] := $FF;
  header[1] := $FE;
  Result := TFileStream.Create(Filename, fmCreate or fmOpenWrite);
  Result.WriteBuffer(header, 2);
end;


{**
  Open a textfile unicode safe and return a stream + its charset
}
procedure OpenTextFile(const Filename: String; out Stream: TFileStream; out FileCharset: TFileCharset);
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  Stream.Position := 0;
  FileCharset := GetFileCharset(Stream);
end;


{**
  Detect a file's character set which can be
  UTF-16 BE with BOM
  UTF-16 LE with BOM
  UTF-8 with or without BOM
  ANSI
  @see http://en.wikipedia.org/wiki/Byte_Order_Mark
}
function GetFileCharset(Stream: TFileStream): TFileCharset;
var
  ByteOrderMark: WideChar;
  BytesRead: Integer;
  Utf8Test: array[0..2] of AnsiChar;
  Buffer: array of Byte;
  BufferSize, i, FoundUTF8Strings: Integer;
const
  UNICODE_BOM = WideChar($FEFF);
  UNICODE_BOM_SWAPPED = WideChar($FFFE);
  UTF8_BOM = AnsiString(#$EF#$BB#$BF);
  MinimumCountOfUTF8Strings = 1;
  MaxBufferSize = $4000;

  // 3 trailing bytes are the maximum in valid UTF-8 streams,
  // so a count of 4 trailing bytes is enough to detect invalid UTF-8 streams
  function CountOfTrailingBytes: Integer;
  begin
    Result := 0;
    inc(i);
    while (i < BufferSize) and (Result < 4) do begin
      if Buffer[i] in [$80..$BF] then
        inc(Result)
      else
        Break;
      inc(i);
    end;
  end;
begin
  // Byte Order Mark
  ByteOrderMark := #0;
  if (Stream.Size - Stream.Position) >= SizeOf(ByteOrderMark) then begin
    BytesRead := Stream.Read(ByteOrderMark, SizeOf(ByteOrderMark));
    if (ByteOrderMark <> UNICODE_BOM) and (ByteOrderMark <> UNICODE_BOM_SWAPPED) then begin
      ByteOrderMark := #0;
      Stream.Seek(-BytesRead, soFromCurrent);
      if (Stream.Size - Stream.Position) >= Length(Utf8Test) * SizeOf(AnsiChar) then begin
        BytesRead := Stream.Read(Utf8Test[0], Length(Utf8Test) * SizeOf(AnsiChar));
        if Utf8Test <> UTF8_BOM then
          Stream.Seek(-BytesRead, soFromCurrent);
      end;
    end;
  end;
  // Test Byte Order Mark
  if ByteOrderMark = UNICODE_BOM then
    Result := fcsUnicode
  else if ByteOrderMark = UNICODE_BOM_SWAPPED then
    Result := fcsUnicodeSwapped
  else if Utf8Test = UTF8_BOM then
    Result := fcsUtf8
  else begin
    { @note Taken from SynUnicode.pas }
    { If no BOM was found, check for leading/trailing byte sequences,
      which are uncommon in usual non UTF-8 encoded text.

      NOTE: There is no 100% save way to detect UTF-8 streams. The bigger
            MinimumCountOfUTF8Strings, the lower is the probability of
            a false positive. On the other hand, a big MinimumCountOfUTF8Strings
            makes it unlikely to detect files with only little usage of non
            US-ASCII chars, like usual in European languages. }

    // if no special characteristics are found it is not UTF-8
    Result := fcsAnsi;

    // if Stream is nil, let Delphi raise the exception, by accessing Stream,
    // to signal an invalid result

    // start analysis at actual Stream.Position
    BufferSize := Min(MaxBufferSize, Stream.Size - Stream.Position);

    if BufferSize > 0 then begin
      SetLength(Buffer, BufferSize);
      Stream.ReadBuffer(Buffer[0], BufferSize);
      Stream.Seek(-BufferSize, soFromCurrent);

      FoundUTF8Strings := 0;
      i := 0;
      while i < BufferSize do begin
        if FoundUTF8Strings = MinimumCountOfUTF8Strings then begin
          Result := fcsUtf8;
          Break;
        end;
        case Buffer[i] of
          $00..$7F: // skip US-ASCII characters as they could belong to various charsets
            ;
          $C2..$DF:
            if CountOfTrailingBytes = 1 then
              inc(FoundUTF8Strings)
            else
              Break;
          $E0:
            begin
              inc(i);
              if (i < BufferSize) and (Buffer[i] in [$A0..$BF]) and (CountOfTrailingBytes = 1) then
                inc(FoundUTF8Strings)
              else
                Break;
            end;
          $E1..$EC, $EE..$EF:
            if CountOfTrailingBytes = 2 then
              inc(FoundUTF8Strings)
            else
              Break;
          $ED:
            begin
              inc(i);
              if (i < BufferSize) and (Buffer[i] in [$80..$9F]) and (CountOfTrailingBytes = 1) then
                inc(FoundUTF8Strings)
              else
                Break;
            end;
          $F0:
            begin
              inc(i);
              if (i < BufferSize) and (Buffer[i] in [$90..$BF]) and (CountOfTrailingBytes = 2) then
                inc(FoundUTF8Strings)
              else
                Break;
            end;
          $F1..$F3:
            if CountOfTrailingBytes = 3 then
              inc(FoundUTF8Strings)
            else
              Break;
          $F4:
            begin
              inc(i);
              if (i < BufferSize) and (Buffer[i] in [$80..$8F]) and (CountOfTrailingBytes = 2) then
                inc(FoundUTF8Strings)
              else
                Break;
            end;
          $C0, $C1, $F5..$FF: // invalid UTF-8 bytes
            Break;
          $80..$BF: // trailing bytes are consumed when handling leading bytes,
                     // any occurence of "orphaned" trailing bytes is invalid UTF-8
            Break;
        end;
        inc(i);
      end;
    end;
  end;
end;

{**
  Read a chunk out of a textfile unicode safe by passing a stream and its charset
}
function ReadTextfileChunk(Stream: TFileStream; FileCharset: TFileCharset; ChunkSize: Int64 = 0): WideString;
var
  SA: AnsiString;
  P: PWord;
  DataLeft: Int64;
begin
  DataLeft := Stream.Size - Stream.Position;
  if (ChunkSize = 0) or (ChunkSize > DataLeft) then
    ChunkSize := DataLeft;
  if (FileCharset in [fcsUnicode, fcsUnicodeSwapped]) then begin
    // BOM indicates Unicode text stream
    if ChunkSize < SizeOf(WideChar) then
      Result := ''
    else begin
      SetLength(Result, ChunkSize div SizeOf(WideChar));
      Stream.Read(PWideChar(Result)^, ChunkSize);
      if FileCharset = fcsUnicodeSwapped then begin
        P := PWord(PWideChar(Result));
        While (P^ <> 0) do begin
          P^ := MakeWord(HiByte(P^), LoByte(P^));
          Inc(P);
        end;
      end;
    end;
  end else if FileCharset = fcsUtf8 then begin
    // BOM indicates UTF-8 text stream
    SetLength(SA, ChunkSize div SizeOf(AnsiChar));
    Stream.Read(PAnsiChar(SA)^, ChunkSize);
    Result := UTF8Decode(SA);
  end else begin
    // without byte order mark it is assumed that we are loading ANSI text
    SetLength(SA, ChunkSize div SizeOf(AnsiChar));
    Stream.Read(PAnsiChar(SA)^, ChunkSize);
    Result := SA;
  end;
end;

{**
  Read a unicode or ansi file into memory
}
function ReadTextfile(Filename: String): WideString;
var
  Stream: TFileStream;
  FileCharset: TFileCharset;
begin
  OpenTextfile(Filename, Stream, FileCharset);
  Result := ReadTextfileChunk(Stream, FileCharset);
  Stream.Free;
end;

function ReadBinaryFile(Filename: String; MaxBytes: Int64): string;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  Stream.Position := 0;
  if (MaxBytes < 1) or (MaxBytes > Stream.Size) then MaxBytes := Stream.Size;
  SetLength(Result, MaxBytes);
  Stream.Read(PChar(Result)^, Length(Result));
  Stream.Free;
end;

{ TUniClipboard }

function TUniClipboard.GetAsWideString: WideString;
var Data: THandle;
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
end;

procedure TUniClipboard.SetAsWideString(Value: WideString);
begin
  SetBuffer(CF_UNICODETEXT, PWideChar(Value)^, 2 * (Length(Value) + 1));
end;

procedure CopyToClipboard(Value: WideString);
var
  CB: TUniClipboard;
begin
  CB := TUniClipboard.Create;
  CB.AsWideString := Value;
end;


procedure StreamToClipboard(S: TMemoryStream);
var
  Content: String;
begin
  SetLength(Content, S.Size);
  S.Position := 0;
  S.Read(Pointer(Content)^, S.Size);
  CopyToClipboard(Utf8Decode(Content));
  // Free memory
  SetString(Content, nil, 0);
end;


procedure FixVT(VT: TVirtualStringTree);
var
  ReadOnlyNodeHeight: Integer;
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  // Resize hardcoded node height to work with different DPI settings
  ReadOnlyNodeHeight := VT.Canvas.TextHeight('A') + 6;
  if toEditable in VT.TreeOptions.MiscOptions then begin
    // Editable nodes must have enough height for a TEdit, including its cursor
    // Code taken from StdCtrls.TCustomEdit.AdjustHeight
    DC := GetDC(0);
    GetTextMetrics(DC, SysMetrics);
    SaveFont := SelectObject(DC, VT.Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    I := GetSystemMetrics(SM_CYBORDER) * 6;
    VT.DefaultNodeHeight := Metrics.tmHeight + I;
  end else
    VT.DefaultNodeHeight := ReadOnlyNodeHeight;
  // The header needs slightly more height than the normal nodes
  VT.Header.Height := Trunc(ReadOnlyNodeHeight * 1.2);
end;


function ColorAdjustBrightness(Col: TColor; ShiftPercent: ShortInt): TColor;
var
  r, g, b, ShiftBytes: SmallInt;
begin
  // Split colors
  r := GetRValue(Col);
  g := GetGValue(Col);
  b := GetBValue(Col);
  // Convert percent value to bytes
  ShiftBytes := Round(ShiftPercent * 255 / 100);
  // Adjust single colors
  r := r + ShiftBytes;
  g := g + ShiftBytes;
  b := b + ShiftBytes;
  // Fix exceeded bounds
  if r > 255 then r := 255 else if r < 0 then r := 0;
  if g > 255 then g := 255 else if g < 0 then g := 0;
  if b > 255 then b := 255 else if b < 0 then b := 0;
  // Merge result
  Result := RGB(r, g, b);
end;


{**
  Concat all sort options to a ORDER clause
}
function ComposeOrderClause(Cols: TOrderColArray): WideString;
var
  i : Integer;
  sort : String;
begin
  result := '';
  for i := 0 to Length(Cols) - 1 do
  begin
    if result <> '' then
      result := result + ', ';
    if Cols[i].SortDirection = ORDER_ASC then
      sort := TXT_ASC
    else
      sort := TXT_DESC;
    result := result + Mainform.Mask( Cols[i].ColumnName ) + ' ' + sort;
  end;
end;


{**
  Init main registry object and open desired key
  Outsoureced from GetRegValue() to avoid redundant code
  in these 3 overloaded methods.
}
procedure OpenRegistry(Session: String = '');
var
  folder : String;
begin
  if MainReg = nil then
    MainReg := TRegistry.Create;
  folder := REGPATH;
  if Session <> '' then
    folder := folder + REGKEY_SESSIONS + Session;
  if MainReg.CurrentPath <> folder then
    MainReg.OpenKey(folder, true);
end;


{**
  Read a numeric preference value from registry
}
function GetRegValue( valueName: String; defaultValue: Integer; Session: String = '' ) : Integer;
begin
  result := defaultValue;
  OpenRegistry(Session);
  if MainReg.ValueExists( valueName ) then
    result := MainReg.ReadInteger( valueName );
end;


{***
  Read a boolean preference value from registry
  @param string Name of the value
  @param boolean Default-value to return if valueName was not found
  @param string Subkey of REGPATH where to search for the value
}
function GetRegValue( valueName: String; defaultValue: Boolean; Session: String = '' ) : Boolean;
begin
  result := defaultValue;
  OpenRegistry(Session);
  if MainReg.ValueExists( valueName ) then
    result := MainReg.ReadBool( valueName );
end;



{***
  Read a text preference value from registry
}
function GetRegValue( valueName: String; defaultValue: String; Session: String = '' ) : String;
begin
  result := defaultValue;
  OpenRegistry(Session);
  if MainReg.ValueExists( valueName ) then
    result := MainReg.ReadString( valueName );
end;


procedure ResetVTNodes(Sender: TBaseVirtualTree);
var
  Node: PVirtualNode;
begin
  // Forces a VirtualTree to (re-)initialize its nodes.
  // I wonder why this is not implemented in VirtualTree.
  Node := Sender.GetFirst;
  while Assigned(Node) do begin
    Node.States := Node.States - [vsInitialized];
    Node := Sender.GetNext(Node);
  end;
end;


procedure EnableProgressBar(MaxValue: Integer);
begin
  Mainform.ProgressBarStatus.Visible := True;
  Mainform.ProgressBarStatus.Max := MaxValue;
  Mainform.ProgressBarStatus.Position := 0;
end;


function CompareNumbers(List: TStringList; Index1, Index2: Integer): Integer;
var
  Number1, Number2 : Extended;
begin
  // Custom sort method for TStringLists
  Number1 := MakeFloat( List[Index1] );
  Number2 := MakeFloat( List[Index2] );
  if Number1 > Number2 then
    Result := 1
  else if Number1 = Number2 then
    Result := 0
  else
    Result := -1;
end;


function ListIndexByRegExpr(List: TWideStrings; Expression: WideString): Integer;
var
  rx: TRegExpr;
  i: Integer;
begin
  // Find item in stringlist by passing a regular expression
  rx := TRegExpr.Create;
  rx.Expression := Expression;
  rx.ModifierI := True;
  Result := -1;
  for i := 0 to List.Count - 1 do begin
    if rx.Exec(List[i]) then begin
      Result := i;
      break;
    end;
  end;
  FreeAndNil(rx);
end;


procedure RestoreSyneditStyles(Highlighter: TSynCustomHighlighter);
var
  i: Integer;
  Attri: TSynHighlighterAttributes;
begin
  // Read font color, bold + italic style of SynEdit attributes from registry
  // Default colors defined and used from highlighter on main form
  for i := 0 to Highlighter.AttrCount - 1 do begin
    Attri := Highlighter.Attribute[i];
    Attri.Foreground := GetRegValue(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_FG, Mainform.SynSQLSyn1.Attribute[i].Foreground);
    Attri.Background := GetRegValue(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_BG, Mainform.SynSQLSyn1.Attribute[i].Background);
    Attri.IntegerStyle := GetRegValue(REGPREFIX_SQLATTRI+Attri.FriendlyName+REGPOSTFIX_SQL_STYLE, Mainform.SynSQLSyn1.Attribute[i].IntegerStyle)
  end;
end;


initialization



// Keywords copied from SynHighligherSQL
MYSQL_KEYWORDS := TStringList.Create;
MYSQL_KEYWORDS.CommaText := 'ACTION,AFTER,AGAINST,AGGREGATE,ALGORITHM,ALL,ALTER,ANALYZE,AND,ANY,AS,' +
  'ASC,AT,AUTO_INCREMENT,AVG_ROW_LENGTH,BACKUP,BEFORE,BEGIN,BENCHMARK,BETWEEN,BINLOG,BIT,' +
  'BOOL,BOTH,BY,CACHE,CALL,CASCADE,CASCADED,CHANGE,CHARACTER,CHARSET,CHECK,' +
  'CHECKSUM,CLIENT,COLLATE,COLLATION,COLUMN,COLUMNS,COMMENT,COMMIT,' +
  'COMMITTED,COMPLETION,CONCURRENT,CONNECTION,CONSISTENT,CONSTRAINT,' +
  'CONVERT,CONTAINS,CONTENTS,CREATE,CROSS,DATA,DATABASE,DATABASES,' +
  'DEALLOCATE,DEC,DEFAULT,DEFINER,DELAYED,DELAY_KEY_WRITE,DELETE,DESC,' +
  'DETERMINISTIC,DIRECTORY,DISABLE,DISCARD,DESCRIBE,DISTINCT,DISTINCTROW,' +
  'DIV,DROP,DUAL,DUMPFILE,DUPLICATE,EACH,ELSE,ENABLE,ENCLOSED,END,ENDS,' +
  'ENGINE,ENGINES,ESCAPE,ESCAPED,ERRORS,EVENT,EVENTS,EVERY,EXECUTE,EXISTS,' +
  'EXPANSION,EXPLAIN,FALSE,FIELDS,FILE,FIRST,FLUSH,FOR,FORCE,FOREIGN,FROM,' +
  'FULL,FULLTEXT,FUNCTION,FUNCTIONS,GLOBAL,GRANT,GRANTS,GROUP,HAVING,HELP,' +
  'HIGH_PRIORITY,HOSTS,IDENTIFIED,IGNORE,INDEX,INFILE,INNER,INSERT,' +
  'INSERT_METHOD,INSTALL,INT1,INT2,INT3,INT4,INT8,INTO,IO_THREAD,IS,' +
  'ISOLATION,INVOKER,JOIN,KEY,KEYS,KILL,LAST,LEADING,LEAVES,LEVEL,LESS,' +
  'LIKE,LIMIT,LINEAR,LINES,LIST,LOAD,LOCAL,LOCK,LOGS,LONG,LOW_PRIORITY,' +
  'MASTER,MASTER_HOST,MASTER_LOG_FILE,MASTER_LOG_POS,MASTER_CONNECT_RETRY,' +
  'MASTER_PASSWORD,MASTER_PORT,MASTER_SSL,MASTER_SSL_CA,MASTER_SSL_CAPATH,' +
  'MASTER_SSL_CERT,MASTER_SSL_CIPHER,MASTER_SSL_KEY,MASTER_USER,MATCH,' +
  'MAX_ROWS,MAXVALUE,MIDDLEINT,MIN_ROWS,MOD,MODE,MODIFY,MODIFIES,NAMES,' +
  'NATURAL,NEW,NO,NODEGROUP,NOT,NULL,OJ,OFFSET,OLD,ON,OPTIMIZE,OPTION,' +
  'OPTIONALLY,OPEN,OR,ORDER,OUTER,OUTFILE,PACK_KEYS,PARTIAL,PARTITION,' +
  'PARTITIONS,PLUGIN,PLUGINS,PREPARE,PRESERVE,PRIMARY,PRIVILEGES,PROCEDURE,' +
  'PROCESS,PROCESSLIST,QUERY,RAID_CHUNKS,RAID_CHUNKSIZE,RAID_TYPE,RANGE,' +
  'READ,REBUILD,REFERENCES,REGEXP,RELAY_LOG_FILE,RELAY_LOG_POS,RELOAD,' +
  'RENAME,REORGANIZE,REPAIR,REPEATABLE,REPLACE,REPLICATION,RESTRICT,RESET,' +
  'RESTORE,RETURN,RETURNS,REVOKE,RLIKE,ROLLBACK,ROLLUP,ROUTINE,ROW,' +
  'ROW_FORMAT,ROWS,SAVEPOINT,SCHEDULE,SCHEMA,SCHEMAS,SECURITY,SELECT,' +
  'SERIALIZABLE,SESSION,SET,SHARE,SHOW,SHUTDOWN,SIMPLE,SLAVE,SNAPSHOT,' +
  'SONAME,SQL,SQL_BIG_RESULT,SQL_BUFFER_RESULT,SQL_CACHE,' +
  'SQL_CALC_FOUND_ROWS,SQL_NO_CACHE,SQL_SMALL_RESULT,SQL_THREAD,START,' +
  'STARTING,STARTS,STATUS,STOP,STORAGE,STRAIGHT_JOIN,SUBPARTITION,' +
  'SUBPARTITIONS,SUPER,TABLE,TABLES,TABLESPACE,TEMPORARY,TERMINATED,THAN,' +
  'THEN,TO,TRAILING,TRANSACTION,TRIGGER,TRIGGERS,TRUE,TYPE,UNCOMMITTED,' +
  'UNINSTALL,UNIQUE,UNLOCK,UPDATE,UPGRADE,UNION,USAGE,USE,USING,VALUES,' +
  'VARIABLES,VARYING,VIEW,WARNINGS,WHERE,WITH,WORK,WRITE';


end.


