unit helpers;


// -------------------------------------
// Functions-library
// -------------------------------------


interface

uses
  Classes, SysUtils, Graphics, GraphUtil, ClipBrd, Dialogs, Forms, Controls, ComCtrls, ShellApi, CheckLst,
  Windows, Contnrs, ShlObj, ActiveX, VirtualTrees, SynRegExpr, Messages, WideStrUtils, Math,
  Registry, SynEditHighlighter, DateUtils, Generics.Collections, StrUtils, AnsiStrings, TlHelp32, Types,
  mysql_connection, mysql_structures;

type

  // Define a record which can hold everything we need for one row / node in a VirtualStringTree
  TVTreeData = record
    Captions: TStringList;
    ImageIndex: Integer;
    NodeType: TListNodeType;
  end;
  PVTreedata = ^TVTreeData;

  // Standardize the list with node-data-records to be able to
  // use this type as variables in functions/procedures (fx VT.OnFreeNode)
  TVTreeDataArray = Array of TVTreeData;
  PVTreeDataArray = ^TVTreeDataArray;

  TFileCharset = (fcsAnsi, fcsUnicode, fcsUnicodeSwapped, fcsUtf8);

  TOrderCol = class(TObject)
    ColumnName: String;
    SortDirection: Byte;
  end;
  TOrderColArray = Array of TOrderCol;

  TLineBreaks = (lbsNone, lbsWindows, lbsUnix, lbsMac, lbsWide, lbsMixed);

  TDBObjectEditor = class(TFrame)
    private
      FModified: Boolean;
      procedure SetModified(Value: Boolean);
    protected
    public
      DBObject: TDBObject;
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Init(Obj: TDBObject); virtual;
      function DeInit: TModalResult;
      property Modified: Boolean read FModified write SetModified;
      function ApplyModifications: TModalResult; virtual; abstract;
  end;
  TDBObjectEditorClass = class of TDBObjectEditor;

  TWndProc = function (hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  PGripInfo = ^TGripInfo;
  TGripInfo = record
    OldWndProc: TWndProc;
    Enabled: boolean;
    GripRect: TRect;
  end;

  TSQLSentence = class(TObject)
    LeftOffset, RightOffset: Integer;
    SQL: String;
  end;
  TSQLBatch = TObjectList<TSQLSentence>;


{$I const.inc}

  function implodestr(seperator: String; a: TStrings) :String;
  function Explode(Separator, Text: String) :TStringList;
  procedure ExplodeQuotedList(Text: String; var List: TStringList);
  procedure ensureValidIdentifier(name: String);
  function getEnumValues(str: String): String;
  function GetSQLSplitMarkers(const SQL: String): TSQLBatch;
  function SplitSQL(const SQL: String): TSQLBatch;
  function sstr(str: String; len: Integer) : String;
  function encrypt(str: String): String;
  function decrypt(str: String): String;
  function htmlentities(str: String): String;
  procedure GridToHtml(Grid: TVirtualStringTree; S: TStream);
  procedure GridToCsv(Grid: TVirtualStringTree; Separator, Encloser, Terminator: String; S: TStream);
  procedure GridToXml(Grid: TVirtualStringTree; S: TStream);
  procedure GridToSql(Grid: TVirtualStringTree; S: TStream);
  function BestTableName(Data: TMySQLQuery): String;
  function esc2ascii(str: String): String;
  function urlencode(url: String): String;
  procedure StreamWrite(S: TStream; Text: String = '');
  function fixSQL( sql: String; sql_version: Integer = SQL_VERSION_ANSI; cli_workarounds: Boolean = false ): String;
  procedure ToggleCheckListBox(list: TCheckListBox; state: Boolean); Overload;
  procedure ToggleCheckListBox(list: TCheckListBox; state: Boolean; list_toggle: TStringList); Overload;
  function _GetFileSize(filename: String): Int64;
  function Mince(PathToMince: String; InSpace: Integer): String;
  function MakeInt( Str: String ) : Int64;
  function MakeFloat( Str: String ): Extended;
  function CleanupNumber(Str: String): String;
  function esc(Text: String; ProcessJokerChars: Boolean=false): String;
  function ScanNulChar(Text: String): Boolean;
  function ScanLineBreaks(Text: String): TLineBreaks;
  function RemoveNulChars(Text: String): String;
  procedure debug(txt: String);
  function fixNewlines(txt: String): String;
  function GetShellFolder(CSIDL: integer): string;
  function getFilesFromDir( dir: String; pattern: String = '*.*'; hideExt: Boolean = false ): TStringList;
  function goodfilename( str: String ): String;
  function FormatNumber( str: String; Thousands: Boolean=True): String; Overload;
  function UnformatNumber(Val: String): String;
  function FormatNumber( int: Int64; Thousands: Boolean=True): String; Overload;
  function FormatNumber( flt: Double; decimals: Integer = 0; Thousands: Boolean=True): String; Overload;
  procedure setLocales;
  procedure ShellExec(cmd: String; path: String=''; params: String='');
  function getFirstWord( text: String ): String;
  function LastPos(needle: Char; haystack: String): Integer;
  function FormatByteNumber( Bytes: Int64; Decimals: Byte = 1 ): String; Overload;
  function FormatByteNumber( Bytes: String; Decimals: Byte = 1 ): String; Overload;
  function FormatTimeNumber( Seconds: Cardinal ): String;
  function GetTempDir: String;
  procedure SetWindowSizeGrip(hWnd: HWND; Enable: boolean);
  procedure SaveUnicodeFile(Filename: String; Text: String);
  procedure OpenTextFile(const Filename: String; out Stream: TFileStream; out FileCharset: TFileCharset);
  function GetFileCharset(Stream: TFileStream): TFileCharset;
  function ReadTextfileChunk(Stream: TFileStream; FileCharset: TFileCharset; ChunkSize: Int64 = 0): String;
  function ReadTextfile(Filename: String): String;
  function ReadBinaryFile(Filename: String; MaxBytes: Int64): AnsiString;
  procedure StreamToClipboard(Text, HTML: TStream; CreateHTMLHeader: Boolean);
  function WideHexToBin(text: String): AnsiString;
  function BinToWideHex(bin: AnsiString): String;
  procedure CheckHex(text: String; errorMessage: string);
  procedure FixVT(VT: TVirtualStringTree; MultiLineCount: Word=1);
  function GetTextHeight(Font: TFont): Integer;
  function ColorAdjustBrightness(Col: TColor; Shift: SmallInt): TColor;
  function ComposeOrderClause(Cols: TOrderColArray): String;
  procedure OpenRegistry(Session: String = '');
  function GetRegValue( valueName: String; defaultValue: Integer; Session: String = '' ) : Integer; Overload;
  function GetRegValue( valueName: String; defaultValue: Boolean; Session: String = '' ) : Boolean; Overload;
  function GetRegValue( valueName: String; defaultValue: String; Session: String = '' ) : String; Overload;
  procedure DeInitializeVTNodes(Sender: TBaseVirtualTree);
  procedure EnableProgressBar(MaxValue: Integer);
  function CompareNumbers(List: TStringList; Index1, Index2: Integer): Integer;
  function ListIndexByRegExpr(List: TStrings; Expression: String): Integer;
  procedure SelectNode(VT: TVirtualStringTree; idx: Cardinal; ParentNode: PVirtualNode=nil); overload;
  procedure SelectNode(VT: TVirtualStringTree; Node: PVirtualNode); overload;
  function DateBackFriendlyCaption(d: TDateTime): String;
  procedure InheritFont(AFont: TFont);
  function GetLightness(AColor: TColor): Byte;
  procedure ParseTableStructure(CreateTable: String; Columns: TTableColumnList; Keys: TTableKeyList; ForeignKeys: TForeignKeyList);
  procedure ParseViewStructure(ViewName: String; Columns: TTableColumnList);
  procedure ParseRoutineStructure(CreateCode: String; Parameters: TRoutineParamList;
    var Deterministic: Boolean; var Returns, DataAccess, Security, Comment, Body: String);
  function ReformatSQL(SQL: String): String;
  function ParamBlobToStr(lpData: Pointer): TStringlist;
  function ParamStrToBlob(out cbData: DWORD): Pointer;
  function CheckForSecondInstance: Boolean;
  function GetParentFormOrFrame(Comp: TWinControl): TWinControl;
  function GetIndexIcon(IndexType: String): Integer;
  function KeyPressed(Code: Integer): Boolean;
  function GeneratePassword(Len: Integer): String;
  procedure InvalidateVT(VT: TVirtualStringTree; RefreshTag: Integer; ImmediateRepaint: Boolean);
  procedure HandlePortableSettings(StartupMode: Boolean);
  function LoadConnectionParams(Session: String): TConnectionParameters;
  function CompareAnyNode(Text1, Text2: String): Integer;
  function GetColumnDefaultType(var Text: String): TColumnDefaultType;
  function GetColumnDefaultClause(DefaultType: TColumnDefaultType; Text: String): String;

var
  MainReg: TRegistry;
  RegPath: String = '\Software\' + APPNAME + '\';
  PortableMode: Boolean = False;
  MutexHandle: THandle = 0;
  dbgCounter: Integer = 0;
  DecimalSeparatorSystemdefault: Char;


implementation

uses main, uVistaFuncs, table_editor, view, routine_editor, trigger_editor, event_editor;



function WideHexToBin(text: String): AnsiString;
var
  buf: AnsiString;
begin
  buf := AnsiString(text);
  SetLength(Result, Length(text) div 2);
  HexToBin(PAnsiChar(buf), @Result[1], Length(Result));
end;

function BinToWideHex(bin: AnsiString): String;
var
  buf: AnsiString;
begin
  SetLength(buf, Length(bin) * 2);
  BinToHex(@bin[1], PAnsiChar(buf), Length(bin));
  Result := String(buf);
end;

procedure CheckHex(text: String; errorMessage: string);
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
function implodestr(seperator: String; a: TStrings) :String;
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



function Explode(Separator, Text: String): TStringList;
var
  i: Integer;
  Item: String;
begin
  // Explode a string by separator into a TStringList
  Result := TStringList.Create;
  while true do begin
    i := Pos(Separator, Text);
    if i = 0 then begin
      // Last or only segment: Add to list if it's the last. Add also if it's not empty and list is empty.
      // Do not add if list is empty and text is also empty.
      if (Result.Count > 0) or (Text <> '') then
        Result.Add(Text);
      break;
    end;
    Item := Trim(Copy(Text, 1, i-1));
    Result.Add(Item);
    Delete(Text, 1, i-1+Length(Separator));
  end;
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
function getEnumValues(str: String): String;
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


function GetSQLSplitMarkers(const SQL: String): TSQLBatch;
var
  i, AllLen, DelimLen, DelimStart, LastLeftOffset, RightOffset, LastNewLineOffset: Integer;
  c, n, LastStringEncloser: Char;
  Delim, DelimTest, QueryTest: String;
  InString, InComment, InBigComment, InEscape: Boolean;
  Marker: TSQLSentence;
  rx: TRegExpr;
const
  StringEnclosers = ['"', '''', '`'];
  NewLines = [#13, #10];
begin
  // Scan SQL batch for delimiters and return a list with start + end offsets
  AllLen := Length(SQL);
  i := 0;
  LastLeftOffset := 1;
  Delim := Mainform.Delimiter;
  InString := False; // Loop in "enclosed string" or `identifier`
  InComment := False; // Loop in one-line comment (# or --)
  InBigComment := False; // Loop in /* multi-line */ or /*! condictional comment */
  InEscape := False; // Previous char was backslash
  LastStringEncloser := #0;
  DelimLen := Length(Delim);
  Result := TSQLBatch.Create;
  rx := TRegExpr.Create;
  rx.Expression := '^\s*DELIMITER\s+(\S+)\s*$';
  rx.ModifierI := True;
  rx.ModifierM := True;
  while i < AllLen do begin
    Inc(i);
    // Current and next char
    c := SQL[i];
    if i < AllLen then n := SQL[i+1]
    else n := #0;

    // Check for comment syntax and for enclosed literals, so a query delimiter can be ignored
    if (not InComment) and (not InBigComment) and (not InString) and ((c + n = '--') or (c = '#')) then
      InComment := True;
    if (not InComment) and (not InBigComment) and (not InString) and (c + n = '/*') then
      InBigComment := True;
    if InBigComment and (not InComment) and (not InString) and (c + n = '*/') then
      InBigComment := False;
    if (not InEscape) and (not InComment) and (not InBigComment) and CharInSet(c, StringEnclosers) then begin
      if (not InString) or (InString and (c = LastStringEncloser)) then begin
        InString := not InString;
        LastStringEncloser := c;
      end;
    end;
    if (CharInSet(c, NewLines) and (not CharInSet(n, NewLines))) or (i = 1) then begin
      if i > 1 then
        InComment := False;
      if (not InString) and (not InBigComment) and rx.Exec(copy(SQL, LastLeftOffset, 100)) then begin
        Delim := rx.Match[1];
        DelimLen := Length(Delim);
        Inc(i, rx.MatchLen[0]+1);
        LastLeftOffset := i;
        continue;
      end;
    end;
    if not InEscape then
      InEscape := c = '\'
    else
      InEscape := False;

    // Prepare delimiter test string
    if (not InComment) and (not InString) and (not InBigComment) then begin
      DelimStart := Max(1, i+1-DelimLen);
      DelimTest := Copy(SQL, DelimStart, i-Max(i-DelimLen, 0));
    end else
      DelimTest := '';

    // End of query or batch reached. Add query markers to result list if sentence is not empty.
    if (DelimTest = Delim) or (i = AllLen) then begin
      RightOffset := i+1;
      if DelimTest = Delim then
        Dec(RightOffset, DelimLen);
      QueryTest := Trim(Copy(SQL, LastLeftOffset, RightOffset-LastLeftOffset));
      if (QueryTest <> '') and (QueryTest <> Delim) then begin
        Marker := TSQLSentence.Create;
        Marker.LeftOffset := LastLeftOffset;
        Marker.RightOffset := RightOffset;
        Result.Add(Marker);
        LastLeftOffset := i+1;
      end;
    end;
  end;
end;


function SplitSQL(const SQL: String): TSQLBatch;
var
  Query: TSQLSentence;
begin
  // Return a list of queries tokenized from a big string. Replaces earlier parseSQL() method.
  Result := GetSQLSplitMarkers(SQL);
  for Query in Result do
    Query.SQL := Copy(SQL, Query.LeftOffset, Query.RightOffset-Query.LeftOffset);
end;



{***
  Shorten string to length len and append 3 dots

  @param string String to shorten
  @param integer Wished Length of string
  @return string
}
function sstr(str: String; len: Integer) : String;
begin
  if length(str) > len then
  begin
    str := copy(str, 0, len-1);
    str := str + '…';
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
function htmlentities(str: String) : String;
begin
  result := StringReplace(str, '&', '&amp;', [rfReplaceAll]);
  result := StringReplace(result, '<', '&lt;', [rfReplaceAll]);
  result := StringReplace(result, '>', '&gt;', [rfReplaceAll]);
end;


procedure ExportStatusMsg(Node: PVirtualNode; RootNodeCount: Cardinal; StreamSize: Int64);
begin
  Mainform.ShowStatusMsg('Exporting row '+FormatNumber(Node.Index+1)+' of '+FormatNumber(RootNodeCount)+
    ' ('+IntToStr(Trunc((Node.Index+1) / RootNodeCount *100))+'%, '+FormatByteNumber(StreamSize)+')'
    );
  Mainform.ProgressBarStatus.Position := Node.Index+1;
end;


{***
  Converts a Grid to a HTML-Table.
  @param Grid Object which holds data to export
  @param string Text used in <title>
}
procedure GridToHtml(Grid: TVirtualStringTree; S: TStream);
var
  i, MaxSize: Integer;
  tmp, Data, Generator, Title: String;
  Node: PVirtualNode;
  GridData: TMySQLQuery;
  SelectionOnly: Boolean;
  NodeCount: Cardinal;
  RowNum: PCardinal;
begin
  // Only process selected nodes for "Copy as ..." actions
  SelectionOnly := S is TMemoryStream;

  Mainform.DataGridEnsureFullRows(Grid, SelectionOnly);
  GridData := Mainform.GridResult(Grid);
  Title := BestTableName(GridData);

  MaxSize := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE) * SIZE_MB;

  if SelectionOnly then
    NodeCount := Grid.SelectedCount
  else
    NodeCount := Grid.RootNodeCount;
  EnableProgressBar(NodeCount);
  Generator := APPNAME+' '+Mainform.AppVersion;
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
  for i:=0 to Grid.Header.Columns.Count-1 do begin
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
    '    <table caption="' + Title + ' (' + inttostr(NodeCount) + ' rows)">' + CRLF +
    '      <thead>' + CRLF +
    '        <tr>' + CRLF;
  for i:=0 to Grid.Header.Columns.Count-1 do begin
    // Skip hidden key columns.
    if not (coVisible in Grid.Header.Columns[i].Options) then
      Continue;
    // Add header item.
    tmp := tmp + '          <th class="col' + IntToStr(i) + '">' + Grid.Header.Columns[i].Text + '</th>' + CRLF;
  end;
  tmp := tmp +
    '        </tr>' + CRLF +
    '      </thead>' + CRLF +
    '      <tbody>' + CRLF;
  StreamWrite(S, tmp);

  if SelectionOnly then Node := Grid.GetFirstSelected else Node := Grid.GetFirst;
  while Assigned(Node) do begin
    // Update status once in a while.
    if (Node.Index+1) mod 100 = 0 then
      ExportStatusMsg(Node, NodeCount, S.Size);
    RowNum := Grid.GetNodeData(Node);
    GridData.RecNo := RowNum^;
    tmp := '        <tr>' + CRLF;
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      Data := GridData.Col(i);
      // Handle nulls.
      if GridData.IsNull(i) then Data := TEXT_NULL;
      // Keep formatted numeric values
      if Mainform.prefExportLocaleNumbers and (GridData.DataType(i).Category in [dtcInteger, dtcReal]) then
        Data := FormatNumber(Data, False);
      // Escape HTML control characters in data.
      Data := htmlentities(Data);
      tmp := tmp + '          <td class="col' + IntToStr(i) + '">' + Data + '</td>' + CRLF;
    end;
    tmp := tmp + '        </tr>' + CRLF;
    StreamWrite(S, tmp);
    if SelectionOnly then Node := Grid.GetNextSelected(Node) else Node := Grid.GetNext(Node);
    if (MaxSize > 0) and Assigned(Node) and (S is TMemoryStream) and (S.Size >= MaxSize) then begin
      MessageDlg(
        Format(MSG_COPYMAXSIZE, [FormatByteNumber(MaxSize), FormatNumber(Node.Index), FormatNumber(NodeCount)]),
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
  Mainform.ProgressBarStatus.Visible := False;
  Mainform.ShowStatusMsg;
end;



{***
  Converts grid contents to CSV-values.
  @param Grid Object which holds data to export
  @param string Field-separator
  @param string Field-encloser
  @param string Line-terminator
}
procedure GridToCsv(Grid: TVirtualStringTree; Separator, Encloser, Terminator: String; S: TStream);
var
  i, MaxSize: Integer;
  tmp, Data: String;
  Node: PVirtualNode;
  GridData: TMySQLQuery;
  SelectionOnly: Boolean;
  NodeCount: Cardinal;
  RowNum: PCardinal;
begin
  // Only process selected nodes for "Copy as ..." actions
  SelectionOnly := S is TMemoryStream;

  Mainform.DataGridEnsureFullRows(Grid, SelectionOnly);
  GridData := Mainform.GridResult(Grid);

  separator := esc2ascii(separator);
  encloser := esc2ascii(encloser);
  terminator := esc2ascii(terminator);
  MaxSize := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE) * SIZE_MB;

  if SelectionOnly then
    NodeCount := Grid.SelectedCount
  else
    NodeCount := Grid.RootNodeCount;
  EnableProgressBar(NodeCount);

  tmp := '';
  // Columns
  for i:=0 to Grid.Header.Columns.Count-1 do begin
    // Skip hidden key columns
    if not (coVisible in Grid.Header.Columns[i].Options) then
      Continue;
    Data := Grid.Header.Columns[i].Text;
    // Alter column name in header if data is not raw.
    if (GridData.DataType(i).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
      Data := 'HEX(' + Data + ')';
    // Add header item.
    if tmp <> '' then tmp := tmp + Separator;
    tmp := tmp + Encloser + Data + Encloser;
  end;
  tmp := tmp + Terminator;
  StreamWrite(S, tmp);

  // Data:
  if SelectionOnly then Node := Grid.GetFirstSelected else Node := Grid.GetFirst;
  while Assigned(Node) do begin
    if (Node.Index+1) mod 100 = 0 then
      ExportStatusMsg(Node, NodeCount, S.Size);
    RowNum := Grid.GetNodeData(Node);
    GridData.RecNo := RowNum^;
    tmp := '';
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      if (GridData.DataType(i).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
        Data := GridData.BinColAsHex(i)
      else
        Data := GridData.Col(i);
      // Unformat numeric values
      if Mainform.prefExportLocaleNumbers and (GridData.DataType(i).Category in [dtcInteger, dtcReal]) then
        Data := FormatNumber(Data, False);
      // Escape encloser characters inside data per de-facto CSV.
      Data := StringReplace(Data, Encloser, Encloser + Encloser, [rfReplaceAll]);
      // Special handling for NULL (MySQL-ism, not de-facto CSV: unquote value)
      if GridData.IsNull(i) then Data := 'NULL'
      else Data := Encloser + Data + Encloser;
      // Add cell.
      if tmp <> '' then tmp := tmp + Separator;
      tmp := tmp + Data;
    end;
    tmp := tmp + Terminator;
    StreamWrite(S, tmp);
    if SelectionOnly then Node := Grid.GetNextSelected(Node) else Node := Grid.GetNext(Node);
    if (MaxSize > 0) and Assigned(Node) and (S is TMemoryStream) and (S.Size >= MaxSize) then begin
      MessageDlg(
        Format(MSG_COPYMAXSIZE, [FormatByteNumber(MaxSize), FormatNumber(Node.Index), FormatNumber(NodeCount)]),
        mtWarning, [mbOK], 0);
      break;
    end;
  end;
  Mainform.ProgressBarStatus.Visible := False;
  Mainform.ShowStatusMsg;
end;



{***
  Converts grid contents to XML.
  @param Grid Object which holds data to export
  @param string Text used as root-element
}
procedure GridToXml(Grid: TVirtualStringTree; S: TStream);
var
  i, MaxSize: Integer;
  tmp, Data, root: String;
  Node: PVirtualNode;
  GridData: TMySQLQuery;
  SelectionOnly: Boolean;
  NodeCount: Cardinal;
  RowNum: PCardinal;
begin
  // Only process selected nodes for "Copy as ..." actions
  SelectionOnly := S is TMemoryStream;

  Mainform.DataGridEnsureFullRows(Grid, SelectionOnly);
  GridData := Mainform.GridResult(Grid);
  root := BestTableName(GridData);

  MaxSize := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE) * SIZE_MB;

  if SelectionOnly then
    NodeCount := Grid.SelectedCount
  else
    NodeCount := Grid.RootNodeCount;
  EnableProgressBar(NodeCount);
  tmp := '<?xml version="1.0"?>' + CRLF + CRLF +
      '<table name="'+root+'">' + CRLF;
  StreamWrite(S, tmp);

  if SelectionOnly then Node := Grid.GetFirstSelected else Node := Grid.GetFirst;
  while Assigned(Node) do begin
    if (Node.Index+1) mod 100 = 0 then
     ExportStatusMsg(Node, NodeCount, S.Size);
    RowNum := Grid.GetNodeData(Node);
    GridData.RecNo := RowNum^;
    tmp := #9'<row>' + CRLF;
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      // Print cell start tag.
      tmp := tmp + #9#9'<' + Grid.Header.Columns[i].Text;
      if GridData.IsNull(i) then tmp := tmp + ' isnull="true" />' + CRLF
      else begin
        if (GridData.DataType(i).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
          tmp := tmp + ' format="hex"';
        tmp := tmp + '>';
        if (GridData.DataType(i).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
          Data := GridData.BinColAsHex(i)
        else
          Data := GridData.Col(i);
        // Escape XML control characters in data.
        Data := htmlentities(Data);
        // Add data and cell end tag.
        tmp := tmp + Data + '</' + Grid.Header.Columns[i].Text + '>' + CRLF;
      end;
    end;
    tmp := tmp + #9'</row>' + CRLF;
    StreamWrite(S, tmp);
    if SelectionOnly then Node := Grid.GetNextSelected(Node) else Node := Grid.GetNext(Node);
    if (MaxSize > 0) and Assigned(Node) and (S is TMemoryStream) and (S.Size >= MaxSize) then begin
      MessageDlg(
        Format(MSG_COPYMAXSIZE, [FormatByteNumber(MaxSize), FormatNumber(Node.Index), FormatNumber(NodeCount)]),
        mtWarning, [mbOK], 0);
      break;
    end;
  end;
  // footer:
  tmp := '</table>' + CRLF;
  StreamWrite(S, tmp);
  Mainform.ProgressBarStatus.Visible := False;
  Mainform.ShowStatusMsg;
end;


{***
  Converts grid contents to XML.
  @param Grid Object which holds data to export
  @param string Text used as tablename in INSERTs
}
procedure GridToSql(Grid: TVirtualStringTree; S: TStream);
var
  i, MaxSize: Integer;
  tmp, Data, TableName: String;
  Node: PVirtualNode;
  GridData: TMySQLQuery;
  SelectionOnly: Boolean;
  NodeCount: Cardinal;
  RowNum: PCardinal;
begin
  // Only process selected nodes for "Copy as ..." actions
  SelectionOnly := S is TMemoryStream;

  Mainform.DataGridEnsureFullRows(Grid, SelectionOnly);
  GridData := Mainform.GridResult(Grid);
  TableName := BestTableName(GridData);

  MaxSize := GetRegValue(REGNAME_COPYMAXSIZE, DEFAULT_COPYMAXSIZE) * SIZE_MB;

  if SelectionOnly then
    NodeCount := Grid.SelectedCount
  else
    NodeCount := Grid.RootNodeCount;
  EnableProgressBar(NodeCount);

  if SelectionOnly then Node := Grid.GetFirstSelected else Node := Grid.GetFirst;
  while Assigned(Node) do begin
    if (Node.Index+1) mod 100 = 0 then
      ExportStatusMsg(Node, NodeCount, S.Size);
    RowNum := Grid.GetNodeData(Node);
    GridData.RecNo := RowNum^;
    tmp := 'INSERT INTO '+Mainform.Mask(Tablename)+' (';
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      tmp := tmp + Mainform.mask(Grid.Header.Columns[i].Text)+', ';
    end;
    Delete(tmp, Length(tmp)-1, 2);
    tmp := tmp + ') VALUES (';
    for i:=0 to Grid.Header.Columns.Count-1 do begin
      // Skip hidden key columns
      if not (coVisible in Grid.Header.Columns[i].Options) then
        Continue;
      if GridData.IsNull(i) then
        tmp := tmp + 'NULL'
      else begin
        if (GridData.DataType(i).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
          Data := GridData.BinColAsHex(i)
        else
          Data := GridData.Col(i);
        if not (GridData.DataType(i).Category in [dtcInteger, dtcReal]) then
          Data := esc(Data);
        tmp := tmp + Data;
      end;
      tmp := tmp + ', ';
    end;
    Delete(tmp, Length(tmp)-1, 2);
    tmp := tmp + ');' + CRLF;
    StreamWrite(S, tmp);
    if SelectionOnly then Node := Grid.GetNextSelected(Node) else Node := Grid.GetNext(Node);
    if (MaxSize > 0) and Assigned(Node) and (S is TMemoryStream) and (S.Size >= MaxSize) then begin
      MessageDlg(
        Format(MSG_COPYMAXSIZE, [FormatByteNumber(MaxSize), FormatNumber(Node.Index), FormatNumber(NodeCount)]),
        mtWarning, [mbOK], 0);
      break;
    end;
  end;
  // footer:
  tmp := CRLF;
  StreamWrite(S, tmp);
  Mainform.ProgressBarStatus.Visible := False;
  Mainform.ShowStatusMsg;
end;


function BestTableName(Data: TMySQLQuery): String;
begin
  // Get table name from result if possible. Used by GridToXYZ() functions.
  try
    Result := Data.TableName;
  except
    Result := 'UnknownTable';
  end;
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
  Encode spaces (and more to come) in URLs

  @param string URL to encode
  @return string
}
function urlencode(url: String): String;
begin
  result := stringreplace(url, ' ', '+', [rfReplaceAll]);
end;


{**
  Write some UTF8 text to a file- or memorystream
}
procedure StreamWrite(S: TStream; Text: String = '');
var
  utf8: AnsiString;
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
function fixSQL( sql: String; sql_version: Integer = SQL_VERSION_ANSI; cli_workarounds: Boolean = false ): String;
var
  rx : TRegExpr;
begin
  result := sql;

  // For mysqldump and mysql.exe CLI compatibility
  if cli_workarounds then
  begin
    result := StringReplace(result, ';*/', '*/;', [rfReplaceAll]);
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
      result := rx.Replace(result, '', false);
      // Strip collation part
      rx.Expression := '\s+COLLATE=\w+';
      result := rx.Replace(result, '', false);
    end;

    if sql_version = SQL_VERSION_ANSI then begin
      // Switch quoting char
      result := StringReplace(result, '`', '"', [rfReplaceAll]);
      // Strip ENGINE|TYPE
      rx.Expression := '\s+(ENGINE|TYPE)=\w+';
      result := rx.Replace(result, '', false);
    end;

    // Turn ENGINE to TYPE
    if sql_version < 40102 then
      result := StringReplace(result, 'ENGINE=', 'TYPE=', [rfReplaceAll])
    else
      result := StringReplace(result, 'TYPE=', 'ENGINE=', [rfReplaceAll]);

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
procedure ToggleCheckListBox(list: TCheckListBox; state: Boolean);
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
procedure ToggleCheckListBox(list: TCheckListBox; state: Boolean; list_toggle: TStringList);
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


function CleanupNumber(Str: String): String;
var
  i: Integer;
  HasDecimalSep: Boolean;
begin
  // Ensure the passed string contains a valid number, which is convertable by StrToFloat afterwards
  // Return it as string again, as there are callers which need to handle unsigned bigint's somehow -
  // there is no unsigned 64 bit integer type in Delphi.
  Result := '';
  HasDecimalSep := False;
  for i:=1 to Length(Str) do begin
    if CharInSet(Str[i], ['0'..'9', DecimalSeparator]) or ((Str[i] = '-') and (Result='')) then
    begin
      // Avoid confusion and AV in StrToFloat()
      if (ThousandSeparator = DecimalSeparator) and (Str[i] = DecimalSeparator) then
        continue;
      // Ensure only 1 decimalseparator is left
      if (Str[i] = DecimalSeparator) and HasDecimalSep then
        continue;
      if Str[i] = DecimalSeparator then
        HasDecimalSep := True;
      Result := Result + Str[i];
    end;
  end;
  if (Result = '') or (Result = '-') then
    Result := '0';
end;


{***
  Convert a string-number to an floatingpoint-number

  @param String text representation of a number
  @return Extended
}
function MakeFloat( Str: String ): Extended;
var
  p_kb, p_mb, p_gb, p_tb, p_pb : Integer;
begin
  // Convert result to a floating point value to ensure
  // we don't discard decimal digits for the next step
  Result := StrToFloat(CleanupNumber(Str));

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


function esc(Text: String; ProcessJokerChars: Boolean=false): String;
begin
  Result := Mainform.Connection.EscapeString(Text, ProcessJokerChars);
end;


{***
  Detect NUL character in a text.
  Useful because fx SynEdit cuts of all text after it encounters a NUL.
}
function ScanNulChar(Text: String): boolean;
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
function ScanLineBreaks(Text: String): TLineBreaks;
var
  i: integer;
  c: Char;
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
function RemoveNulChars(Text: String): String;
var
  i: integer;
  c: Char;
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
function fixNewlines(txt: String): String;
begin
  txt := StringReplace(txt, CRLF, #10, [rfReplaceAll]);
  txt := StringReplace(txt, #13, #10, [rfReplaceAll]);
  txt := StringReplace(txt, #10, CRLF, [rfReplaceAll]);
  result := txt;
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



{**
  Unformat a formatted integer or float. Used for CSV export and composing WHERE clauses for grid editing.
}
function UnformatNumber(Val: String): String;
var
  i: Integer;
  HasDecim: Boolean;
  c: Char;
const
  Numbers = ['0'..'9'];
begin
  Result := '';
  HasDecim := False;
  for i:=1 to Length(Val) do begin
    c := Val[i];
    if CharInSet(c, Numbers) or ((c = '-') and (i = 1)) then
      Result := Result + c
    else if (c = DecimalSeparator) and (not HasDecim) then begin
      Result := Result + '.';
      HasDecim := True;
    end else if c <> ThousandSeparator then
      break;
  end;
  if Result = '' then
    Result := '0';
end;


{***
  Return a formatted integer or float from a string
  @param string Text containing a number
  @return string
}
function FormatNumber(str: String; Thousands: Boolean=True): String; Overload;
var
  i, p, Left: Integer;
begin
  Result := StringReplace(str, '.', DecimalSeparator, [rfReplaceAll]);
  if Thousands then begin
    // Do not add thousand separators to zerofilled numbers
    if ((Length(Result) >= 1) and (Result[1] = '0'))
      or ((Length(Result) >= 2) and (Result[1] = '-') and (Result[2] = '0'))
    then
      Exit;
    p := Pos(DecimalSeparator, Result);
    if p = 0 then p := Length(Result)+1;
    Left := 2;
    if (Length(Result) >= 1) and (Result[1] = '-') then
      Left := 3;
    if p > 0 then for i:=p-1 downto Left do begin
      if (p-i) mod 3 = 0 then
        Insert(ThousandSeparator, Result, i);
    end;
  end;
end;



{***
  Return a formatted number from an integer

  @param int64 Number to format
  @return string
}
function FormatNumber(int: Int64; Thousands: Boolean=True): String; Overload;
begin
  result := FormatNumber(IntToStr(int), Thousands);
end;



{***
  Return a formatted number from a float
  This function is called by two overloaded functions

  @param double Number to format
  @param integer Number of decimals
  @return string
}
function FormatNumber(flt: Double; decimals: Integer = 0; Thousands: Boolean=True): String; Overload;
begin
  Result := Format('%10.'+IntToStr(decimals)+'f', [flt]);
  Result := Trim(Result);
  Result := FormatNumber(Result, Thousands);
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
  Open URL or execute system command

  @param string Command or URL to execute
  @param string Working directory, only usefull is first param is a system command
}
procedure ShellExec(cmd: String; path: String=''; params: String='');
begin
  ShellExecute(0, 'open', PChar(cmd), PChar(params), PChar(path), SW_SHOWNORMAL);
end;



{***
  Returns first word of a given text
  @param string Given text
  @return string First word-boundary
}
function getFirstWord( text: String ): String;
var
  i : Integer;
  wordChars, wordCharsFirst : TSysCharSet;
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
    if CharInSet(text[i], wordCharsFirst) then
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
    if ((result = '') and CharInSet(text[i], wordCharsFirst)) or CharInSet(text[i], wordChars) then
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


{**
  Get last position of substr in str
  @param string Substring
  @param string Text
  @return Integer Last position
}
function LastPos(needle: Char; haystack: String): Integer;
var
  reverse: String;
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
  d, h, m, s : Integer;
begin
  s := Seconds;
  d := s div (60*60*24);
  s := s mod (60*60*24);
  h := s div (60*60);
  s := s mod (60*60);
  m := s div 60;
  s := s mod 60;
  if d > 0 then
    Result := Format('%d days, %.2d:%.2d:%.2d', [d, h, m, s])
  else
    Result := Format('%.2d:%.2d:%.2d', [h, m, s]);
end;


function GetTempDir: String;
var
  TempPath: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, PChar(@TempPath));
  Result := StrPas(TempPath);
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
procedure SaveUnicodeFile(Filename: String; Text: String);
var
  f: TFileStream;
begin
  f := TFileStream.Create(Filename, fmCreate or fmOpenWrite);
  StreamWrite(f, Text);
  f.Free;
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
  ByteOrderMark: Char;
  BytesRead: Integer;
  Utf8Test: array[0..2] of AnsiChar;
  Buffer: array of Byte;
  BufferSize, i, FoundUTF8Strings: Integer;
const
  UNICODE_BOM = Char($FEFF);
  UNICODE_BOM_SWAPPED = Char($FFFE);
  UTF8_BOM = AnsiString(#$EF#$BB#$BF);
  MinimumCountOfUTF8Strings = 1;
  MaxBufferSize = 100000;

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
function ReadTextfileChunk(Stream: TFileStream; FileCharset: TFileCharset; ChunkSize: Int64 = 0): String;
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
    if ChunkSize < SizeOf(Char) then
      Result := ''
    else begin
      SetLength(Result, ChunkSize div SizeOf(Char));
      Stream.Read(PChar(Result)^, ChunkSize);
      if FileCharset = fcsUnicodeSwapped then begin
        P := PWord(PChar(Result));
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
    Result := UTF8ToString(SA);
  end else begin
    // without byte order mark it is assumed that we are loading ANSI text
    SetLength(SA, ChunkSize div SizeOf(AnsiChar));
    Stream.Read(PAnsiChar(SA)^, ChunkSize);
    Result := String(SA);
  end;
end;

{**
  Read a unicode or ansi file into memory
}
function ReadTextfile(Filename: String): String;
var
  Stream: TFileStream;
  FileCharset: TFileCharset;
begin
  OpenTextfile(Filename, Stream, FileCharset);
  Result := ReadTextfileChunk(Stream, FileCharset);
  Stream.Free;
end;

function ReadBinaryFile(Filename: String; MaxBytes: Int64): AnsiString;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  Stream.Position := 0;
  if (MaxBytes < 1) or (MaxBytes > Stream.Size) then MaxBytes := Stream.Size;
  SetLength(Result, MaxBytes);
  Stream.Read(PAnsiChar(Result)^, Length(Result));
  Stream.Free;
end;


procedure StreamToClipboard(Text, HTML: TStream; CreateHTMLHeader: Boolean);
var
  TextContent, HTMLContent: AnsiString;
  GlobalMem: HGLOBAL;
  lp: PChar;
  ClpLen: Integer;
  CF_HTML: Word;
begin
  // Copy unicode text to clipboard
  if Assigned(Text) then begin
    SetLength(TextContent, Text.Size);
    Text.Position := 0;
    Text.Read(PAnsiChar(TextContent)^, Text.Size);
    Clipboard.AsText := Utf8ToString(TextContent);
    SetString(TextContent, nil, 0);
  end;

  if Assigned(HTML) then begin
    // If wanted, add a HTML portion, so formatted text can be pasted in WYSIWYG
    // editors (mostly MS applications).
    // Note that the content is UTF8 encoded ANSI. Using unicode variables results in raw
    // text pasted in editors. TODO: Find out why and optimize redundant code away by a loop.
    OpenClipBoard(0);
    CF_HTML := RegisterClipboardFormat('HTML Format');
    SetLength(HTMLContent, HTML.Size);
    HTML.Position := 0;
    HTML.Read(PAnsiChar(HTMLContent)^, HTML.Size);
    if CreateHTMLHeader then begin
      HTMLContent := 'Version:0.9' + CRLF +
        'StartHTML:-1' + CRLF +
        'EndHTML:-1' + CRLF +
        'StartFragment:000081' + CRLF +
        'EndFragment:°°°°°°' + CRLF +
        HTMLContent + CRLF;
      HTMLContent := AnsiStrings.StringReplace(
        HTMLContent, '°°°°°°',
        AnsiStrings.Format('%.6d', [Length(HTMLContent)]),
        []);
    end;
    ClpLen := Length(HTMLContent) + 1;
    GlobalMem := GlobalAlloc(GMEM_DDESHARE + GMEM_MOVEABLE, ClpLen);
    lp := GlobalLock(GlobalMem);
    Move(PAnsiChar(HTMLContent)^, lp[0], ClpLen);
    SetString(HTMLContent, nil, 0);
    GlobalUnlock(GlobalMem);
    SetClipboardData(CF_HTML, GlobalMem);
    CloseClipboard;
  end;
end;


procedure FixVT(VT: TVirtualStringTree; MultiLineCount: Word=1);
var
  SingleLineHeight: Integer;
  Node: PVirtualNode;
begin
  // Resize hardcoded node height to work with different DPI settings
  VT.BeginUpdate;
  SingleLineHeight := GetTextHeight(VT.Font);
  VT.DefaultNodeHeight := SingleLineHeight * MultiLineCount + 6;
  // The header needs slightly more height than the normal nodes
  VT.Header.Height := Trunc(SingleLineHeight * 1.5);
  // Apply new height to multi line grid nodes
  Node := VT.GetFirstInitialized;
  while Assigned(Node) do begin
    VT.NodeHeight[Node] := VT.DefaultNodeHeight;
    VT.MultiLine[Node] := MultiLineCount > 1;
    Node := VT.GetNextInitialized(Node);
  end;
  VT.EndUpdate;
  // Disable hottracking in non-Vista mode, looks ugly in XP, but nice in Vista
  if (toUseExplorerTheme in VT.TreeOptions.PaintOptions) and IsWindowsVista then
    VT.TreeOptions.PaintOptions := VT.TreeOptions.PaintOptions + [toHotTrack]
  else
    VT.TreeOptions.PaintOptions := VT.TreeOptions.PaintOptions - [toHotTrack];
  VT.OnGetHint := MainForm.vstGetHint;
  VT.OnScroll := MainForm.vstScroll;
  VT.ShowHint := True;
  VT.HintMode := hmToolTip;
  // Apply case insensitive incremental search event
  if VT.IncrementalSearch <> isNone then
    VT.OnIncrementalSearch := Mainform.vstIncrementalSearch;
end;


function GetTextHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  // Code taken from StdCtrls.TCustomEdit.AdjustHeight
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;


function ColorAdjustBrightness(Col: TColor; Shift: SmallInt): TColor;
var
  Lightness: Byte;
begin
  // If base color is bright, make bg color darker (grey), and vice versa, so that
  // colors work with high contrast mode for accessibility
  Lightness := GetLightness(Col);
  if (Lightness < 128) and (Shift < 0) then
    Shift := Abs(Shift)
  else if (Lightness > 128) and (Shift > 0) then
    Shift := 0 - Abs(Shift);
  Result := ColorAdjustLuma(Col, Shift, true);
end;


{**
  Concat all sort options to a ORDER clause
}
function ComposeOrderClause(Cols: TOrderColArray): String;
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
  if MainReg = nil then begin
    MainReg := TRegistry.Create;
    HandlePortableSettings(True);
  end;
  folder := RegPath;
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
  @param string Subkey of RegPath where to search for the value
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


procedure DeInitializeVTNodes(Sender: TBaseVirtualTree);
var
  Node: PVirtualNode;
begin
  // Forces a VirtualTree to (re-)initialize its nodes.
  // I wonder why this is not implemented in VirtualTree.
  Node := Sender.GetFirstInitialized;
  while Assigned(Node) do begin
    Node.States := Node.States - [vsInitialized];
    Node := Sender.GetNextInitialized(Node);
  end;
end;


procedure EnableProgressBar(MaxValue: Integer);
begin
  Mainform.ProgressBarStatus.State := pbsNormal;
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


function ListIndexByRegExpr(List: TStrings; Expression: String): Integer;
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


procedure SelectNode(VT: TVirtualStringTree; idx: Cardinal; ParentNode: PVirtualNode=nil); overload;
var
  Node: PVirtualNode;
begin
  // Helper to focus and highlight a node by its index
  if Assigned(ParentNode) then
    Node := VT.GetFirstChild(ParentNode)
  else
    Node := VT.GetFirst;
  while Assigned(Node) do begin
    if Node.Index = idx then begin
      SelectNode(VT, Node);
      break;
    end;
    Node := VT.GetNextSibling(Node);
  end;
end;


procedure SelectNode(VT: TVirtualStringTree; Node: PVirtualNode); overload;
begin
  VT.ClearSelection;
  VT.FocusedNode := Node;
  VT.Selected[Node] := True;
  VT.ScrollIntoView(Node, False);
end;


function DateBackFriendlyCaption(d: TDateTime): String;
var
  MonthsAgo, DaysAgo, HoursAgo, MinutesAgo: Int64;
begin
  MonthsAgo := MonthsBetween(Now, d);
  DaysAgo := DaysBetween(Now, d);
  HoursAgo := HoursBetween(Now, d);
  MinutesAgo := MinutesBetween(Now, d);
  if MonthsAgo = 1 then Result := FormatNumber(MonthsAgo)+' month ago'
  else if MonthsAgo > 1 then Result := FormatNumber(MonthsAgo)+' months ago'
  else if DaysAgo = 1 then Result := FormatNumber(DaysAgo)+' day ago'
  else if DaysAgo > 1 then Result := FormatNumber(DaysAgo)+' days ago'
  else if HoursAgo = 1 then Result := FormatNumber(HoursAgo)+' hour ago'
  else if HoursAgo > 1 then Result := FormatNumber(HoursAgo)+' hours ago'
  else if MinutesAgo = 1 then Result := FormatNumber(MinutesAgo)+' minute ago'
  else if MinutesAgo > 0 then Result := FormatNumber(MinutesAgo)+' minutes ago'
  else Result := 'less than a minute ago';
end;


procedure ExplodeQuotedList(Text: String; var List: TStringList);
var
  i: Integer;
  Quote: Char;
  Opened, Closed: Boolean;
  Item: String;
begin
  Text := Trim(Text);
  if Length(Text) > 0 then
    Quote := Text[1]
  else
    Quote := '`';
  Opened := False;
  Closed := True;
  Item := '';
  for i:=1 to Length(Text) do begin
    if Text[i] = Quote then begin
      Opened := not Opened;
      Closed := not Closed;
      if Closed then begin
        List.Add(Item);
        Item := '';
      end;
      Continue;
    end;
    if Opened and (not Closed) then
      Item := Item + Text[i];
  end;
end;


procedure InheritFont(AFont: TFont);
begin
  AFont.Name := Mainform.Font.Name;
  AFont.Size := Mainform.Font.Size;
end;


function GetLightness(AColor: TColor): Byte;
var
  R, G, B: Byte;
  MaxValue, MinValue: Double;
  Lightness: Double;
begin
  R := GetRValue(ColorToRGB(AColor));
  G := GetGValue(ColorToRGB(AColor));
  B := GetBValue(ColorToRGB(AColor));
  MaxValue := Max(Max(R,G),B);
  MinValue := Min(Min(R,G),B);
  Lightness := (((MaxValue + MinValue) * 240) + 255 ) / 510;
  Result := Round(Lightness);
end;


procedure ParseTableStructure(CreateTable: String; Columns: TTableColumnList; Keys: TTableKeyList; ForeignKeys: TForeignKeyList);
var
  ColSpec: String;
  rx, rxCol: TRegExpr;
  i: Integer;
  InLiteral: Boolean;
  Col: TTableColumn;
  Key: TTableKey;
  ForeignKey: TForeignKey;
begin
  if Assigned(Columns) then Columns.Clear;
  if Assigned(Keys) then Keys.Clear;
  if Assigned(ForeignKeys) then ForeignKeys.Clear;
  if CreateTable = '' then
    Exit;
  rx := TRegExpr.Create;
  rx.ModifierS := False;
  rx.ModifierM := True;
  rx.Expression := '^\s+[`"]([^`"]+)[`"]\s(\w+)';
  rxCol := TRegExpr.Create;
  rxCol.ModifierI := True;
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(Columns) then
      break;
    ColSpec := '';
    for i:=rx.MatchPos[2]+rx.MatchLen[2] to Length(CreateTable) do begin
      if CharInSet(CreateTable[i], [#13, #10]) then
        break;
      ColSpec := ColSpec + CreateTable[i];
    end;

    // Strip trailing comma
    if (ColSpec <> '') and (ColSpec[Length(ColSpec)] = ',') then
      Delete(ColSpec, Length(ColSpec), 1);

    Col := TTableColumn.Create;
    Columns.Add(Col);
    Col.Name := rx.Match[1];
    Col.OldName := Col.Name;
    Col.Status := esUntouched;

    // Datatype
    Col.DataType := GetDatatypeByName(UpperCase(rx.Match[2]));

    // Length / Set
    // Various datatypes, e.g. BLOBs, don't have any length property
    InLiteral := False;
    if (ColSpec <> '') and (ColSpec[1] = '(') then begin
      for i:=2 to Length(ColSpec) do begin
        if (ColSpec[i] = ')') and (not InLiteral) then
          break;
        if ColSpec[i] = '''' then
          InLiteral := not InLiteral;
      end;
      Col.LengthSet := Copy(ColSpec, 2, i-2);
      Delete(ColSpec, 1, i);
    end;
    ColSpec := Trim(ColSpec);

    // Unsigned
    if UpperCase(Copy(ColSpec, 1, 8)) = 'UNSIGNED' then begin
      Col.Unsigned := True;
      Delete(ColSpec, 1, 9);
    end else
      Col.Unsigned := False;

    // Collation
    rxCol.Expression := '^(CHARACTER SET \w+\s+)?COLLATE (\w+)\b';
    if rxCol.Exec(ColSpec) then begin
      Col.Collation := rxCol.Match[2];
      Delete(ColSpec, 1, rxCol.MatchLen[0]+1);
    end;

    // Allow NULL
    if UpperCase(Copy(ColSpec, 1, 8)) = 'NOT NULL' then begin
      Col.AllowNull := False;
      Delete(ColSpec, 1, 9);
    end else begin
      Col.AllowNull := True;
      // Sporadically there is a "NULL" found at this position.
      if UpperCase(Copy(ColSpec, 1, 4)) = 'NULL' then
        Delete(ColSpec, 1, 5);
    end;

    // Default value
    Col.DefaultType := cdtNothing;
    Col.DefaultText := '';
    if UpperCase(Copy(ColSpec, 1, 14)) = 'AUTO_INCREMENT' then begin
      Col.DefaultType := cdtAutoInc;
      Col.DefaultText := 'AUTO_INCREMENT';
      Delete(ColSpec, 1, 15);
    end else if UpperCase(Copy(ColSpec, 1, 8)) = 'DEFAULT ' then begin
      Delete(ColSpec, 1, 8);
      if UpperCase(Copy(ColSpec, 1, 4)) = 'NULL' then begin
        Col.DefaultType := cdtNull;
        Col.DefaultText := 'NULL';
        Delete(ColSpec, 1, 5);
      end else if UpperCase(Copy(ColSpec, 1, 17)) = 'CURRENT_TIMESTAMP' then begin
        Col.DefaultType := cdtCurTS;
        Col.DefaultText := 'CURRENT_TIMESTAMP';
        Delete(ColSpec, 1, 18);
      end else if ColSpec[1] = '''' then begin
        InLiteral := True;
        for i:=2 to Length(ColSpec) do begin
          if ColSpec[i] = '''' then
            InLiteral := not InLiteral
          else if not InLiteral then
            break;
        end;
        Col.DefaultType := cdtText;
        Col.DefaultText := Copy(ColSpec, 2, i-3);
        // A single quote gets escaped by single quote - remove the escape char - escaping is done in Save action afterwards
        Col.DefaultText := StringReplace(Col.DefaultText, '''''', '''', [rfReplaceAll]);
        Delete(ColSpec, 1, i);
      end;
    end;
    if UpperCase(Copy(ColSpec, 1, 27)) = 'ON UPDATE CURRENT_TIMESTAMP' then begin
      // Adjust default type
      case Col.DefaultType of
        cdtText: Col.DefaultType := cdtTextUpdateTS;
        cdtNull: Col.DefaultType := cdtNullUpdateTS;
        cdtCurTS: Col.DefaultType := cdtCurTSUpdateTS;
      end;
      Delete(ColSpec, 1, 28);
    end;

    // Comment
    if UpperCase(Copy(ColSpec, 1, 9)) = 'COMMENT ''' then begin
      InLiteral := True;
      for i:=10 to Length(ColSpec) do begin
        if ColSpec[i] = '''' then
          InLiteral := not InLiteral
        else if not InLiteral then
          break;
      end;
      Col.Comment := Copy(ColSpec, 10, i-11);
      Col.Comment := StringReplace(Col.Comment, '''''', '''', [rfReplaceAll]);
      Delete(ColSpec, 1, i);
    end;

    if not rx.ExecNext then
      break;
  end;

  // Detect keys
  // PRIMARY KEY (`id`), UNIQUE KEY `id` (`id`), KEY `id_2` (`id`) USING BTREE,
  // KEY `Text` (`Text`(100)), FULLTEXT KEY `Email` (`Email`,`Text`)
  rx.Expression := '^\s+((\w+)\s+)?KEY\s+([`"]?([^`"]+)[`"]?\s+)?\((.+)\)(\s+USING\s+(\w+))?,?$';
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(Keys) then
      break;
    Key := TTableKey.Create;
    Keys.Add(Key);
    Key.Name := rx.Match[4];
    if Key.Name = '' then Key.Name := rx.Match[2]; // PRIMARY
    Key.OldName := Key.Name;
    Key.IndexType := rx.Match[2];
    Key.OldIndexType := Key.IndexType;
    Key.Algorithm := rx.Match[7];
    if Key.IndexType = '' then Key.IndexType := 'KEY'; // KEY
    Key.Columns := Explode(',', rx.Match[5]);
    for i:=0 to Key.Columns.Count-1 do begin
      rxCol.Expression := '^[`"]?([^`"]+)[`"]?(\((\d+)\))?$';
      if rxCol.Exec(Key.Columns[i]) then begin
        Key.Columns[i] := rxCol.Match[1];
        Key.SubParts.Add(rxCol.Match[3]);
      end;
    end;
    if not rx.ExecNext then
      break;
  end;

  // Detect foreign keys
  // CONSTRAINT `FK1` FOREIGN KEY (`which`) REFERENCES `fk1` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
  rx.Expression := '\s+CONSTRAINT\s+[`"]([^`"]+)[`"]\sFOREIGN KEY\s+\(([^\)]+)\)\s+REFERENCES\s+[`"]([^\(]+)[`"]\s\(([^\)]+)\)(\s+ON DELETE (RESTRICT|CASCADE|SET NULL|NO ACTION))?(\s+ON UPDATE (RESTRICT|CASCADE|SET NULL|NO ACTION))?';
  if rx.Exec(CreateTable) then while true do begin
    if not Assigned(ForeignKeys) then
      break;
    ForeignKey := TForeignKey.Create;
    ForeignKeys.Add(ForeignKey);
    ForeignKey.KeyName := rx.Match[1];
    ForeignKey.OldKeyName := ForeignKey.KeyName;
    ForeignKey.KeyNameWasCustomized := True;
    ForeignKey.ReferenceTable := StringReplace(rx.Match[3], '`', '', [rfReplaceAll]);
    ForeignKey.ReferenceTable := StringReplace(ForeignKey.ReferenceTable, '"', '', [rfReplaceAll]);
    ExplodeQuotedList(rx.Match[2], ForeignKey.Columns);
    ExplodeQuotedList(rx.Match[4], ForeignKey.ForeignColumns);
    if rx.Match[6] <> '' then
      ForeignKey.OnDelete := rx.Match[6];
    if rx.Match[8] <> '' then
      ForeignKey.OnUpdate := rx.Match[8];
    if not rx.ExecNext then
      break;
  end;

  FreeAndNil(rxCol);
  FreeAndNil(rx);
end;


procedure ParseViewStructure(ViewName: String; Columns: TTableColumnList);
var
  rx: TRegExpr;
  Col: TTableColumn;
  Results: TMySQLQuery;
begin
  // Views reveal their columns only with a SHOW COLUMNS query.
  // No keys available in views - SHOW KEYS always returns an empty result
  Columns.Clear;
  rx := TRegExpr.Create;
  rx.Expression := '^(\w+)(\((.+)\))?';
  Results := Mainform.Connection.GetResults('SHOW COLUMNS FROM '+Mainform.mask(ViewName));
  while not Results.Eof do begin
    Col := TTableColumn.Create;
    Columns.Add(Col);
    Col.Name := Results.Col('Field');
    Col.AllowNull := Results.Col('Null') = 'YES';
    if rx.Exec(Results.Col('Type')) then begin
      Col.DataType := GetDatatypeByName(rx.Match[1]);
      Col.LengthSet := rx.Match[3];
    end;
    Results.Next;
  end;
  rx.Free;
end;


procedure ParseRoutineStructure(CreateCode: String; Parameters: TRoutineParamList;
  var Deterministic: Boolean; var Returns, DataAccess, Security, Comment, Body: String);
var
  Params: String;
  ParenthesesCount: Integer;
  rx: TRegExpr;
  i: Integer;
  Param: TRoutineParam;
begin
  // Parse CREATE code of stored function or procedure to detect parameters
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.ModifierG := True;
  // CREATE DEFINER=`root`@`localhost` PROCEDURE `bla2`(IN p1 INT, p2 VARCHAR(20))
  // CREATE DEFINER=`root`@`localhost` FUNCTION `test3`(`?b` varchar(20)) RETURNS tinyint(4)
  // CREATE DEFINER=`root`@`localhost` PROCEDURE `test3`(IN `Param1` int(1) unsigned)
  ParenthesesCount := 0;
  Params := '';
  for i:=1 to Length(CreateCode) do begin
    if CreateCode[i] = ')' then begin
      Dec(ParenthesesCount);
      if ParenthesesCount = 0 then
        break;
    end;
    if ParenthesesCount >= 1 then
      Params := Params + CreateCode[i];
    if CreateCode[i] = '(' then
      Inc(ParenthesesCount);
  end;
  rx.Expression := '(^|,)\s*((IN|OUT|INOUT)\s+)?(\S+)\s+([^\s,\(]+(\([^\)]*\))?[^,]*)';
  if rx.Exec(Params) then while true do begin
    Param := TRoutineParam.Create;
    Param.Context := UpperCase(rx.Match[3]);
    if Param.Context = '' then
      Param.Context := 'IN';
    Param.Name := WideDequotedStr(rx.Match[4], '`');
    Param.Datatype := rx.Match[5];
    Parameters.Add(Param);
    if not rx.ExecNext then
      break;
  end;

  // Cut left part including parameters, so it's easier to parse the rest
  CreateCode := Copy(CreateCode, i+1, MaxInt);
  // CREATE PROCEDURE sp_name ([proc_parameter[,...]]) [characteristic ...] routine_body
  // CREATE FUNCTION sp_name ([func_parameter[,...]]) RETURNS type [characteristic ...] routine_body
  // LANGUAGE SQL
  //  | [NOT] DETERMINISTIC                                              // IS_DETERMINISTIC
  //  | { CONTAINS SQL | NO SQL | READS SQL DATA | MODIFIES SQL DATA }   // DATA_ACCESS
  //  | SQL SECURITY { DEFINER | INVOKER }                               // SECURITY_TYPE
  //  | COMMENT 'string'                                                 // COMMENT

  rx.Expression := '\bLANGUAGE SQL\b';
  if rx.Exec(CreateCode) then
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  rx.Expression := '\bRETURNS\s+(\w+(\([^\)]*\))?)';
  if rx.Exec(CreateCode) then begin
    Returns := rx.Match[1];
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  end;
  rx.Expression := '\b(NOT\s+)?DETERMINISTIC\b';
  if rx.Exec(CreateCode) then begin
    Deterministic := rx.MatchLen[1] = -1;
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  end;
  rx.Expression := '\b(CONTAINS SQL|NO SQL|READS SQL DATA|MODIFIES SQL DATA)\b';
  if rx.Exec(CreateCode) then begin
    DataAccess := rx.Match[1];
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  end;
  rx.Expression := '\bSQL\s+SECURITY\s+(DEFINER|INVOKER)\b';
  if rx.Exec(CreateCode) then begin
    Security := rx.Match[1];
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
  end;
  rx.ModifierG := False;
  rx.Expression := '\bCOMMENT\s+''((.+)[^''])''[^'']';
  if rx.Exec(CreateCode) then begin
    Comment := StringReplace(rx.Match[1], '''''', '''', [rfReplaceAll]);
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]-1);
  end;
  rx.Expression := '^\s*CHARSET\s+[\w\d]+\s';
  if rx.Exec(CreateCode) then
    Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]-1);
  // Tata, remaining code is the routine body
  Body := TrimLeft(CreateCode);

  rx.Free;
end;


function ReformatSQL(SQL: String): String;
var
  AllKeywords, ImportantKeywords, PairKeywords: TStringList;
  i, Run, KeywordMaxLen: Integer;
  IsEsc, IsQuote, InComment, InBigComment, InString, InKeyword, InIdent, LastWasComment: Boolean;
  c, p: Char;
  Keyword, PreviousKeyword, TestPair: String;
const
  WordChars = ['a'..'z', 'A'..'Z', '0'..'9', '_', '.'];
  WhiteSpaces = [#9, #10, #13, #32];
begin
  // Known SQL keywords, get converted to UPPERCASE
  AllKeywords := TStringList.Create;
  AllKeywords.Text := MySQLKeywords.Text;
  for i:=Low(MySQLFunctions) to High(MySQLFunctions) do begin
    // Leave out operator functions like ">>", and the "X()" function so hex values don't get touched
    if (MySQLFunctions[i].Declaration <> '') and (MySQLFunctions[i].Name <> 'X') then
      AllKeywords.Add(MySQLFunctions[i].Name);
  end;
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



{ *** TDBObjectEditor }

constructor TDBObjectEditor.Create(AOwner: TComponent);
begin
  inherited;
  // Do not set alClient via DFM! In conjunction with ExplicitXXX properties that
  // repeatedly breaks the GUI layout when you reload the project
  Align := alClient;
  InheritFont(Font);
  ScaleControls(Screen.PixelsPerInch, FORMS_DPI);
end;

destructor TDBObjectEditor.Destroy;
begin
  inherited;
end;

procedure TDBObjectEditor.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

procedure TDBObjectEditor.Init(Obj: TDBObject);
begin
  Mainform.ShowStatusMsg('Initializing editor ...');
  DBObject := Obj;
  Mainform.UpdateEditorTab;
  Screen.Cursor := crHourglass;
  MainForm.SetupSynEditors;
end;

function TDBObjectEditor.DeInit: TModalResult;
var
  Msg, ObjType: String;
begin
  // Ask for saving modifications
  Result := mrOk;
  if Modified then begin
    ObjType := LowerCase(DBObject.ObjType);
    if DBObject.Name <> '' then
      Msg := 'Save modified '+ObjType+' "'+DBObject.Name+'"?'
    else
      Msg := 'Save new '+ObjType+'?';
    Result := MessageDlg(Msg, mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case Result of
      mrYes: Result := ApplyModifications;
      mrNo: Modified := False;
    end;
  end;
end;



// Following code taken from OneInst.pas, http://assarbad.net/de/stuff/!import/nico.old/
// Slightly modified to better integrate that into our code, comments translated from german.

// Fetch and separate command line parameters into strings
function ParamBlobToStr(lpData: Pointer): TStringlist;
var
  pStr: PChar;
begin
  Result := TStringlist.Create;
  pStr := lpData;
  while pStr[0] <> #0 do
  begin
    Result.Add(string(pStr));
    pStr := @pStr[lstrlen(pStr) + 1];
  end;
end;

// Pack current command line parameters
function ParamStrToBlob(out cbData: DWORD): Pointer;
var
  Loop: Integer;
  pStr: PChar;
begin
  for Loop := 1 to ParamCount do
    cbData := cbData + DWORD(Length(ParamStr(Loop))*2 + 1);
  cbData := cbData + 2; // include appending #0#0
  Result := GetMemory(cbData);
  ZeroMemory(Result, cbData);
  pStr := Result;
  for Loop := 1 to ParamCount do
  begin
    debug(ParamStr(Loop));
    lstrcpy(pStr, PChar(ParamStr(Loop)));
    pStr := @pStr[lstrlen(pStr) + 1];
  end;
end;

procedure HandleSecondInstance;
var
  Run: DWORD;
  Now: DWORD;
  Msg: TMsg;
  Wnd: HWND;
  Dat: TCopyDataStruct;
begin
  // MessageBox(0, 'already running', nil, MB_ICONINFORMATION);
  // Send a message to all main windows (HWND_BROADCAST) with the identical,
  // previously registered message id. We should only get reply from 0 or 1
  // instances.
  // (Broadcast should only be called with registered message ids!)

  SendMessage(HWND_BROADCAST, SecondInstMsgId, GetCurrentThreadId, 0);

  // Waiting for reply by first instance. For those of you which didn't knew:
  // Threads have message queues too ;o)
  Wnd := 0;
  Run := GetTickCount;
  while True do
  begin
    if PeekMessage(Msg, 0, SecondInstMsgId, SecondInstMsgId, PM_NOREMOVE) then
    begin
      GetMessage(Msg, 0, SecondInstMsgId, SecondInstMsgId);
      if Msg.message = SecondInstMsgId then
      begin
        Wnd := Msg.wParam;
        Break;
      end;
    end;
    Now := GetTickCount;
    if Now < Run then
      Run := Now;  // Avoid overflow, each 48 days.
    if Now - Run > 5000 then
      Break;
  end;

  if (Wnd <> 0) and IsWindow(Wnd) then
  begin
    // As a reply we got a handle to which we now send current parameters
    Dat.dwData := SecondInstMsgId;
    Dat.lpData := ParamStrToBlob(Dat.cbData);
    SendMessage(Wnd, WM_COPYDATA, 0, LPARAM(@Dat));
    FreeMemory(Dat.lpData);

    // Bring first instance to front
    if not IsWindowVisible(Wnd) then
      ShowWindow(Wnd, SW_RESTORE);
    BringWindowToTop(Wnd);
    SetForegroundWindow(Wnd);
  end;
end;

function CheckForSecondInstance: Boolean;
var
  Loop: Integer;
  MutexName: PChar;
begin
  // Try to create a system wide named kernel object (mutex). And check if that
  // already exists.
  // The name of such a mutex must not be longer than MAX_PATH (260) chars and
  // can contain all chars but not '\'

  Result := False;
  MutexName := PChar(APPNAME);
  for Loop := lstrlen(MutexName) to MAX_PATH - 1 do
  begin
    MutexHandle := CreateMutex(nil, False, MutexName);
    if (MutexHandle = 0) and (GetLastError = INVALID_HANDLE_VALUE) then
      // Looks like there is already a mutex using this name
      // Try to solve that by appending an underscore
      lstrcat(MutexName, '_')
    else
      // At least no naming conflict
      Break;
  end;

  case GetLastError of
    0: begin
        // We created the mutex, so this is the first instance
      end;
    ERROR_ALREADY_EXISTS:
      begin
        // There is already one instance
        try
          HandleSecondInstance;
        finally
          // Terminating is done in .dpr file, before Application.Initialize
          Result := True;
        end;
      end;
  else
    // No clue why we should get here. Oh, maybe Microsoft has changed rules, again.
    // However, we return false and let the application start
  end;
end;


function GetParentFormOrFrame(Comp: TWinControl): TWinControl;
begin
  Result := Comp;
  while True do begin
    Result := Result.Parent;
    // On a windows shutdown, GetParentForm() seems sporadically unable to find the owner form
    // In that case we would cause an exception when accessing it. Emergency break in that case.
    // See issue #1462
    if (not Assigned(Result)) or (Result is TCustomForm) or (Result is TFrame) then
      break;
  end;
end;


function GetIndexIcon(IndexType: String): Integer;
begin
  // Detect key icon index for specified index
  if IndexType = PKEY then Result := ICONINDEX_PRIMARYKEY
  else if IndexType = KEY then Result := ICONINDEX_INDEXKEY
  else if IndexType = UKEY then Result := ICONINDEX_UNIQUEKEY
  else if IndexType = FKEY then Result := ICONINDEX_FULLTEXTKEY
  else if IndexType = SKEY then Result := ICONINDEX_SPATIALKEY
  else Result := -1;
end;


function KeyPressed(Code: Integer): Boolean;
var
  State: TKeyboardState;
begin
  // Checks whether a key is pressed, defined by virtual key code
  GetKeyboardState(State);
  Result := (State[Code] and 128) <> 0;
end;


function GeneratePassword(Len: Integer): String;
var
  i: Integer;
  CharTable: String;
const
  Consos = 'bcdfghjklmnpqrstvwxyzBCDFGHJKLMNPQRSTVWXYZ';
  Vocals = 'aeiouAEIOU';
begin
  // Create a random, mnemonic password
  SetLength(Result, Len);
  for i:=1 to Len do begin
    if i mod 2 = 0 then
      CharTable := Vocals
    else
      CharTable := Consos;
    Result[i] := CharTable[Random(Length(CharTable)-1)+1];
  end;
end;


procedure InvalidateVT(VT: TVirtualStringTree; RefreshTag: Integer; ImmediateRepaint: Boolean);
begin
  // Avoid AVs in OnDestroy events
  if not Assigned(VT) then
    Exit;
  VT.Tag := RefreshTag;
  if VT = Mainform.DBtree then begin
    VT.ResetNode(VT.GetFirst);
    if ImmediateRepaint then
      VT.ReinitNode(VT.GetFirst, False);
  end else begin
    if ImmediateRepaint then
      VT.Repaint
    else
      VT.Invalidate;
  end;
end;


procedure HandlePortableSettings(StartupMode: Boolean);
var
  Content, FileName, Name, Value, KeyPath: String;
  Lines, Segments, AllKeys: TStringList;
  i: Integer;
  DataType: TRegDataType;
  Proc: TProcessEntry32;
  ProcRuns: Boolean;
  SnapShot: THandle;
  rx: TRegExpr;
const
  Chr10Replacement = '<}}}>';
  Chr13Replacement = '<{{{>';
  Delimiter = '<|||>';

  procedure ReadKeyToContent(Path: String);
  var
    Names: TStringList;
    i: Integer;
    SubPath: String;
  begin
    MainReg.OpenKeyReadOnly(Path);
    SubPath := Copy(Path, Length(RegPath)+1, MaxInt);
    Names := TStringList.Create;
    MainReg.GetValueNames(Names);
    for i:=0 to Names.Count-1 do begin
      DataType := MainReg.GetDataType(Names[i]);
      Content := Content +
        SubPath + Names[i] + Delimiter +
        IntToStr(Integer(DataType)) + Delimiter;
      case DataType of
        rdString: begin
          Value := MainReg.ReadString(Names[i]);
          Value := StringReplace(Value, #13, Chr13Replacement, [rfReplaceAll]);
          Value := StringReplace(Value, #10, Chr10Replacement, [rfReplaceAll]);
        end;
        rdInteger:
          Value := IntToStr(MainReg.ReadInteger(Names[i]));
        rdBinary, rdUnknown, rdExpandString:
          MessageDlg(Names[i]+' has an unsupported data type.', mtError, [mbOK], 0);
      end;
      Content := Content + Value + CRLF;
    end;
    Names.Clear;
    MainReg.GetKeyNames(Names);
    for i:=0 to Names.Count-1 do
      ReadKeyToContent(Path + Names[i] + '\');
    Names.Free;
  end;
begin
  // Export registry keys and values into textfile, for portable reasons
  FileName := ExtractFilePath(ParamStr(0)) + 'portable_settings.txt';
  if not FileExists(FileName) then
    Exit;

  // Open the right key
  if StartupMode then begin
    RegPath := '\Software\' + APPNAME + ' Portable '+IntToStr(GetCurrentProcessId)+'\';
    PortableMode := True;
  end else begin
    // Do not work like a portable on exit, if at application start we didn't either
    if not PortableMode then
      Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    if StartupMode then begin
      Content := ReadTextfile(FileName);
      Lines := Explode(CRLF, Content);
      for i:=0 to Lines.Count-1 do begin
        // Each line has 3 segments: reg path | data type | value. Continue if explode finds less or more than 3.
        Segments := Explode(Delimiter, Lines[i]);
        if Segments.Count <> 3 then
          continue;
        KeyPath := RegPath + ExtractFilePath(Segments[0]);
        Name := ExtractFileName(Segments[0]);
        DataType := TRegDataType(StrToInt(Segments[1]));
        MainReg.OpenKey(KeyPath, True);
        if MainReg.ValueExists(Name) then
          Continue; // Don't touch value if already there
        Value := '';
        if Segments.Count >= 3 then
          Value := Segments[2];
        case DataType of
          rdString: begin
            Value := StringReplace(Value, Chr13Replacement, #13, [rfReplaceAll]);
            Value := StringReplace(Value, Chr10Replacement, #10, [rfReplaceAll]);
            MainReg.WriteString(Name, Value);
          end;
          rdInteger:
            MainReg.WriteInteger(Name, MakeInt(Value));
          rdBinary, rdUnknown, rdExpandString:
            MessageDlg(Name+' has an unsupported data type.', mtError, [mbOK], 0);
        end;
        Segments.Free;
      end;
      Lines.Free;
    end else begin
      // Application closes: Recursively read values in keys and their subkeys into textfile
      ReadKeyToContent(RegPath);
      SaveUnicodeFile(FileName, Content);
      MainReg.CloseKey;
      MainReg.DeleteKey(RegPath);

      // Remove dead keys from instances which didn't close clean, e.g. because of an AV
      SnapShot := CreateToolhelp32Snapshot(TH32CS_SnapProcess, 0);
      Proc.dwSize := Sizeof(Proc);
      MainReg.OpenKeyReadOnly('\Software\');
      AllKeys := TStringList.Create;
      MainReg.GetKeyNames(AllKeys);
      rx := TRegExpr.Create;
      rx.Expression := '^' + QuoteRegExprMetaChars(APPNAME) + ' Portable (\d+)$';
      for i:=0 to AllKeys.Count-1 do begin
        if not rx.Exec(AllKeys[i]) then
          Continue;
        ProcRuns := False;
        if Process32First(SnapShot, Proc) then while True do begin
          ProcRuns := rx.Match[1] = IntToStr(Proc.th32ProcessID);
          if ProcRuns or (not Process32Next(SnapShot, Proc)) then
            break;
        end;
        if not ProcRuns then
          MainReg.DeleteKey(AllKeys[i]);
      end;
      MainReg.CloseKey;
      CloseHandle(SnapShot);
      AllKeys.Free;
      rx.Free;
    end;
  except
    On E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
  Screen.Cursor := crDefault;

end;


function LoadConnectionParams(Session: String): TConnectionParameters;
begin
  if not Mainreg.KeyExists(REGPATH + REGKEY_SESSIONS + Session) then
    raise Exception.Create('Error: Session "'+Session+'" not found in registry.')
  else begin
    Result := TConnectionParameters.Create;
    Result.NetType := TNetType(GetRegValue(REGNAME_NETTYPE, Integer(ntTCPIP), Session));
    Result.Hostname := GetRegValue(REGNAME_HOST, '', Session);
    Result.Username := GetRegValue(REGNAME_USER, '', Session);
    Result.Password := decrypt(GetRegValue(REGNAME_PASSWORD, '', Session));
    Result.Port := StrToIntDef(GetRegValue(REGNAME_PORT, '', Session), DEFAULT_PORT);
    Result.AllDatabases := GetRegValue(REGNAME_DATABASES, '', Session);
    Result.SSHHost := GetRegValue(REGNAME_SSHHOST, '', Session);
    Result.SSHPort := GetRegValue(REGNAME_SSHPORT, DEFAULT_SSHPORT, Session);
    Result.SSHUser := GetRegValue(REGNAME_SSHUSER, '', Session);
    Result.SSHPassword := decrypt(GetRegValue(REGNAME_SSHPASSWORD, '', Session));
    Result.SSHPrivateKey := GetRegValue(REGNAME_SSHKEY, '', Session);
    Result.SSHLocalPort := GetRegValue(REGNAME_SSHLOCALPORT, 0, Session);
    Result.SSHPlinkExe := GetRegValue(REGNAME_PLINKEXE, '');
    Result.SSLPrivateKey := GetRegValue(REGNAME_SSL_KEY, '', Session);
    Result.SSLCertificate := GetRegValue(REGNAME_SSL_CERT, '', Session);
    Result.SSLCACertificate := GetRegValue(REGNAME_SSL_CA, '', Session);
    Result.StartupScriptFilename := GetRegValue(REGNAME_STARTUPSCRIPT, '', Session);
    if GetRegValue(REGNAME_COMPRESSED, DEFAULT_COMPRESSED, Session) then
      Result.Options := Result.Options + [opCompress]
    else
      Result.Options := Result.Options - [opCompress];
  end;
end;


function CompareAnyNode(Text1, Text2: String): Integer;
var
  Number1, Number2 : Extended;
begin
  Result := 0;
  // Apply different comparisons for numbers and text
  if (StrToIntDef(Copy(Text1, 0, 1), -1) <> -1) and (StrToIntDef(Copy(Text2, 0, 1), -1) <> -1) then begin
    // Assuming numeric values
    Number1 := MakeFloat(Text1);
    Number2 := MakeFloat(Text2);
    if Number1 > Number2 then
      Result := 1
    else if Number1 = Number2 then
      Result := 0
    else if Number1 < Number2 then
      Result := -1;
  end else begin
    // Compare Strings
    Result := CompareText(Text1, Text2);
  end;
end;


function GetColumnDefaultType(var Text: String): TColumnDefaultType;
begin
  Result := TColumnDefaultType(MakeInt(Copy(Text, 1, 1)));
  Text := Copy(Text, 2, Length(Text)-1);
end;


function GetColumnDefaultClause(DefaultType: TColumnDefaultType; Text: String): String;
begin
  case DefaultType of
    cdtNothing:        Result := '';
    cdtText:           Result := 'DEFAULT '+esc(Text);
    cdtTextUpdateTS:   Result := 'DEFAULT '+esc(Text)+' ON UPDATE CURRENT_TIMESTAMP';
    cdtNull:           Result := 'DEFAULT NULL';
    cdtNullUpdateTS:   Result := 'DEFAULT NULL ON UPDATE CURRENT_TIMESTAMP';
    cdtCurTS:          Result := 'DEFAULT CURRENT_TIMESTAMP';
    cdtCurTSUpdateTS:  Result := 'DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP';
    cdtAutoInc:        Result := 'AUTO_INCREMENT';
  end;
end;



end.


