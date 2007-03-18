unit helpers;


// -------------------------------------
// Functions-library
// -------------------------------------


interface

uses Classes, SysUtils, Graphics, db, clipbrd, dialogs,
  forms, controls, ShellApi, checklst, windows, ZDataset, ZAbstractDataset,
  shlobj, ActiveX;

{$I const.inc}

  function trimc(s: String; c: Char) : String;
  function implode(seperator: String; a: array of string) :String;
  function implodestr(seperator: String; a: TStringList) :String;
  function explode(separator, a: String) :TStringList;
  function strpos(haystack, needle: String; offset: Integer=0) : Integer;
  procedure ensureValidIdentifier(name: String);
  function getEnumValues(str: String):String;
  function parsesql(sql: String) : TStringList;
  function sstr(str: String; len: Integer) : String;
  function notinlist(str: String; strlist: TStrings): Boolean;
  function inarray(str: String; a: Array of String): Boolean;
  function encrypt(str: String): String;
  function decrypt(str: String): String;
  function htmlentities(str: String): String;
  function dataset2html(ds: TDataset; htmltitle: String; filename: String = ''; ConvertHTMLEntities: Boolean = true; Generator: String = ''): Boolean;
  function dataset2csv(ds: TDataset; Separator, Encloser, Terminator: String; filename: String = ''): Boolean;
  function dataset2xml(ds: TDataset; title: String; filename: String = ''): Boolean;
  function esc2ascii(str: String): String;
  function StrCmpBegin(Str1, Str2: string): Boolean;
  function Max(A, B: Integer): Integer; assembler;
  function Min(A, B: Integer): Integer; assembler;
  function urlencode(url: String): String;
  procedure wfs( var s: TFileStream; str: String = '');
  function fixSQL( sql: String; sql_version: Integer = SQL_VERSION_ANSI ): String;
  procedure ToggleCheckListBox(list: TCheckListBox; state: Boolean);
  function _GetFileSize(filename: String): Int64;
  function Mince(PathToMince: String; InSpace: Integer): String;
  function MakeInt( Str: String ) : Int64;
  function esc(Text: string; ProcessJokerChars: Boolean = false): string;
  function hasNullChar(Text: string): boolean;
  function hasIrregularChars(Text: string): boolean;
  function hasIrregularNewlines(Text: string): boolean;
  function escapeAuto(Text: string): string;
  procedure debug(txt: String);
  function fixNewlines(txt: string): string;
  function bool2str( boolval : Boolean ) : String;
  function GetShellFolder(CSIDL: integer): string;
  function getFilesFromDir( dir: String; pattern: String = '*.*' ): TStringList;
  function goodfilename( str: String ): String;
  function FormatNumber( str: String ): String; Overload;
  function FormatNumber( int: Int64 ): String; Overload;
  function FormatNumber( flt: Double; decimals: Integer = 0 ): String; Overload;
  procedure setLocales;
  function maskSql(sql_version: integer; str: String) : String;
  procedure ActivateWindow(Window : HWnd);
  procedure ShellExec( cmd: String; path: String = '' );
  function ExpectResultSet(ASql: String): Boolean;
  function getFirstWord( text: String ): String;
  function ConvertWindowsCodepageToMysqlCharacterSet(codepage: Cardinal): string;

var
  MYSQL_KEYWORDS             : TStringList;


implementation

type
  CharacterSet = record
    codepage: Cardinal;
    charset: string;
  end;

const
  charset_conv_table: array[0..4] of CharacterSet = (
    (codepage: 1250; charset: 'cp1250'), // ANSI Central European; Central European (Windows)
    (codepage: 1251; charset: 'cp1251'), // ANSI Cyrillic; Cyrillic (Windows)
    (codepage: 1252; charset: 'latin1'), // ANSI Latin 1; Western European (Windows)
    (codepage: 1256; charset: 'cp1256'), // ANSI Arabic; Arabic (Windows)
    (codepage: 1257; charset: 'cp1257')  // ANSI Baltic; Baltic (Windows)
  );

var
  dbgCounter: Integer = 0;
  DecimalSeparatorSystemdefault: Char;



{***
  Trim off chars from string

  @param string Input-string
  @param char Which character to to use for trimming
  @return string
}
function trimc(s: String; c: char) : String;
var a,z: Integer;
begin
  if c = '' then c := '"';
  if s <> '' then
  begin

    a := 1;
    while s[a] = c do
    begin
      delete(s, a, 1);
      if s = '' then
        exit;
    end;

    z := length(s);
    while s[z] = c do
    begin
      delete(s, z, 1);
      dec(z);
    end;

  end;

  result := s;
end;



{***
  Convert an array to string using a separator-string

  @todo Look at each caller to see if escaping is necessary.
  @param string Separator
  @param a array Containing strings
  @return string
}
function implode(seperator: String; a: array of string) :String;
var
  i : Integer;
  text : String;
begin
  result := '';
  for i:=1 to high(a) do
  begin
    text := text + a[i];
    if i < high(a) then
      text := text + seperator;
  end;
  result := text;
end;



{***
  Convert a TStringList to a string using a separator-string

  @todo Look at each caller to see if escaping is necessary.
  @param string Separator
  @param a TStringList Containing strings
  @return string
}
function implodestr(seperator: String; a: TStringList) :String;
var
  i : Integer;
  text : String;
begin
  result := '';
  for i:=0 to a.Count-1 do
  begin
    text := text + a[i];
    if i < a.Count-1 then
      text := text + seperator;
  end;
  result := text;
end;



{***
  Explode a string by separator into a TStringList

  @param string Separator
  @return TStringList
}
function explode(separator, a: String) :TStringList;
var
  i : Integer;
  item : String;
begin
  result := TStringList.Create();

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
  Return first position of needle in haystack (from char[offset])

  @param string Target
  @param string Text to find
  @return Integer
}
function strpos( haystack, needle: String; offset: Integer=0 ) : Integer;
begin
  haystack := copy(haystack, offset, length(haystack));
  result := pos(needle, haystack);
  if result > 0 then
    result := result + offset-1;
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
function getEnumValues(str: String):String;
var
  p1,p2        : Integer;
begin
  p1 := pos('(', str);
  for p2:=strlen(pchar(str)) downto 0 do
    if str[p2] = ')' then break;
  result := copy (str, p1+1, p2-p1-1);
end;



{***
  Add a non-empty value to a Stringlist

  @param TStringList
  @param string to add
  @return void
}
procedure addResult(list: TStringList; s: string);
begin
  s := trim(s);
  if length(s) > 0 then
    list.Add(s);
end;



{***
  Tokenize sql-script and return a TStringList with sql-statements

  @param string (possibly large) bunch of SQL-statements, separated by semicolon
  @return TStringList Separated statements
}
function parsesql(sql: String) : TStringList;
var
  i, start                          : Integer;
  instring, backslash, incomment    : Boolean;
  inconditional, condterminated     : Boolean;
  inbigcomment                      : Boolean;
  encloser, secchar, thdchar        : Char;
begin
  result := TStringList.Create;
  sql := trim(sql);
  instring := false;
  start := 1;
  backslash := false;
  incomment := false;
  inbigcomment := false;
  inconditional := false;
  condterminated := false;
  encloser := ' ';

  i := 0;
  while i < length(sql) do begin
    i := i + 1;

    secchar := ' ';
    thdchar := ' ';
    if i < length(sql) then secchar := sql[i + 1];
    if i + 1 < length(sql) then thdchar := sql[i + 2];

    if (sql[i] = '#') and (not instring) and (not inbigcomment) then begin
      incomment := true;
    end;
    if (sql[i] + secchar = '--') and (not instring) and (not inbigcomment) then begin
      incomment := true;
      sql[i] := ' ';
      i := i + 1;
    end;
    if (sql[i] + secchar = '/*') and (not (thdchar = '!')) and (not instring) and (not incomment) then begin
      inbigcomment := true;
      incomment := true;
      sql[i] := ' ';
      i := i + 1;
    end;
    if (sql[i] in [#13, #10]) and incomment and (not inbigcomment) then incomment := false;
    if (sql[i] + secchar = '*/') and inbigcomment then begin
      inbigcomment := false;
      incomment := false;
      sql[i] := ' ';
      i := i + 1;
      sql[i] := ' ';
      continue;
    end;
    if incomment or inbigcomment then begin
      sql[i] := ' ';
      continue;
    end;

    if (sql[i] in ['''', '"', '`']) and (not (backslash and instring)) and (not incomment) then begin
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
      continue;
    end;

    if (not instring) and (not incomment) and (sql[i] + secchar + thdchar = '/*!') then begin
      inconditional := true;
      condterminated := false;
      i := i + 2;
      continue;
    end;

    if (not instring) and (not incomment) and (sql[i] + secchar = '*/') and inconditional then begin
    // note:
    // we do not trim the start of the SQL inside conditional
    // comments like we do on non-commented sql.
      inconditional := false;
      if condterminated then begin
        addResult(result, trim(copy(sql, start, i-start)) + '*/');
        start := i+2;
        condterminated := false;
      end;
      // note:
      // the trail of the SQL inside the conditional comment will
      // not get trimmed, as we otherwise do (above) in cases where
      // the semicolon is contained within the conditional comment.
      i := i + 1;
      continue;
    end;

    if (sql[i] = '\') or backslash then
      backslash := not backslash;

    if (sql[i] = ';') and (not instring) then begin
      if inconditional then
      begin
        // note:
        // this logic is wrong, it only supports 1 statement
        // inside each /*!nnnnn blah */ conditional comment.
        condterminated := true;
        sql[i] := ' ';
      end else begin
        addResult(result, copy(sql, start, i-start));
        start := i+1;
      end;
    end;
  end;

  if start < i then
    addResult(result, copy(sql, start, i-start+1));
end;



{***
  Shorten string to length len and append 3 dots

  @param string String to shorten
  @param integer Wished Length of string
  @return string
}
function sstr(str: String; len: Integer) : String;
begin
  if length(str) >= len then
  begin
    str := copy(str, 0, len);
    str := str + '...';
  end;
  result := str;
end;



{***
  Check existance of a string in a TStrings

  @param string Searched text
  @param TStrings List of Strings
  @return boolean
}
function notinlist(str: String; strlist: TStrings): Boolean;
var i: Integer;
begin
  result := true;
  for i:=0 to strlist.Count-1 do
  begin
    if str = strlist[i] then
    begin
      result := false;
      break;
    end;
  end;
end;



{***
  Check existance of a string in an Array

  @param string Searched text
  @param array List of Strings
  @return boolean
}
function inarray(str: String; a: Array of String): Boolean;
var i : Integer;
begin
  result := false;
  i := 0;
  while i < length(a) do
  begin
    if a[i] = str then
    begin
      result := true;
      break;
    end;
    inc(i);
  end;
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
  result := stringreplace(str, '&', '&amp;', [rfReplaceAll]);
  result := stringreplace(result, '<', '&lt;', [rfReplaceAll]);
  result := stringreplace(result, '>', '&gt;', [rfReplaceAll]);
end;



{***
  Converts a TDataSet to a HTML-Table.
  If a filename is given, save HTML to disk, otherwise copy content to clipboard

  @param TDataSet Object which holds data to export
  @param string Text used in <title>
  @param string Filename to use for saving. If not given, copy to clipboard.
  @param boolean Use htmlentities() on cell-contents?
  @param string Generator, used for meta-tag in HTML-head
  @return boolean True on access, False in case of any error
}
function dataset2html(ds: TDataset; htmltitle: String; filename: String = ''; ConvertHTMLEntities: Boolean = true; Generator: String = ''): Boolean;
var
  I, J                      : Integer;
  Buffer, cbuffer, data     : string;
  FStream                   : TFileStream;
  blobfilename, extension   : String;
  bf                        : Textfile;
  header                    : String;
  cursorpos                 : Integer;
begin
  FStream := nil;
  if filename <> '' then try
    FStream := TFileStream.Create(FileName, fmCreate)
  except
    messagedlg('File could not be opened.' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
    dataset2html := false;
    exit;
  end;
  try
    try
      if FStream = nil then clipboard.astext := '';
      buffer := '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + crlf + crlf +
        '<html>' + crlf +
        '<head>' + crlf +
        '  <title>' + htmltitle + '</title>' + crlf +
        '  <meta name="GENERATOR" content="'+ Generator + '">' + crlf +
        '  <style type="text/css"><!--' + crlf +
        '    .header {background-color: ActiveCaption; color: CaptionText;}' + crlf +
        '    th {vertical-align: top;}' + crlf +
        '    td {vertical-align: top;}' + crlf +
        '  --></style>' + crlf +
        '</head>' + crlf + crlf +
        '<body>' + crlf + crlf +
        '<h3>' + htmltitle + ' (' + inttostr(ds.RecordCount) + ' Records)</h3>' + crlf + crlf +
        '<table border="1">' + crlf +
        '  <tr class="header">' + crlf;
      for j:=0 to ds.FieldCount-1 do
        buffer := buffer + '    <th>' + ds.Fields[j].FieldName + '</th>' + crlf;
      buffer := buffer + '  </tr>' + crlf;

      if FStream <> nil then FStream.Write(pchar(buffer)^, length(buffer))
      else cbuffer := buffer;

      cursorpos := ds.RecNo;
      ds.DisableControls;
      ds.First;
      for I := 0 to ds.RecordCount-1 do
      begin
        Buffer := '  <tr>' + crlf;
        // collect data:
        for j:=0 to ds.FieldCount-1 do
        begin
          data := ds.Fields[j].AsString;
          if (filename <> '') and ds.Fields[j].IsBlob then
          begin
            header := copy(data, 0, 20);
            extension := '';
            if pos('JFIF', header) <> 0 then
              extension := 'jpg'
            else if StrCmpBegin('GIF', header) then
              extension := 'gif'
            else if StrCmpBegin('BM', header) then
              extension := 'bmp';
            if extension <> '' then begin
              blobfilename := 'rec'+inttostr(i)+'fld'+inttostr(j)+'.'+extension;
              AssignFile(bf, blobfilename);
              Rewrite(bf);
              Write(bf, data);
              CloseFile(bf);
              data := '<a href="'+blobfilename+'"><img border="0" src="'+blobfilename+'" alt="'+blobfilename+' ('+floattostr(length(data) div 1024)+' KB)" width="100" /></a>';
            end
            else
            begin
              if ConvertHTMLEntities then data := htmlentities(data);
              data := stringreplace(data, #10, #10+'<br>', [rfReplaceAll]);
              data := data + '&nbsp;';
            end;
          end
          else
          begin
            if ConvertHTMLEntities then
              data := htmlentities(data);
            data := stringreplace(data, #10, #10+'<br>', [rfReplaceAll]);
            data := data + '&nbsp;';
          end;
          Buffer := Buffer + '    <td>' + data + '</td>' + crlf;
        end;
        buffer := buffer + '  </tr>' + crlf;
        // write buffer:
        if FStream <> nil then FStream.Write(pchar(buffer)^, length(buffer))
        else cbuffer := cbuffer + buffer;
        ds.Next;
      end;
      ds.RecNo := cursorpos;
      ds.EnableControls;
      // footer:
      buffer := '</table>' + crlf +  crlf + '<p>' + crlf +
        '<em>generated ' + datetostr(now) + ' ' + timetostr(now) +
        ' by <a href="http://www.heidisql.com/">' + Generator + '</a></em></p>' + crlf + crlf +
        '</body></html>';
      if FStream <> nil then FStream.Write(pchar(buffer)^, length(buffer))
      else begin
        cbuffer := cbuffer + buffer;
        clipboard.astext := cbuffer;
      end;
    except
      on e: Exception do begin
        MessageDlg(e.Message, mtError, [mbOK], 0);
        result := false;
       exit;
      end;
    end;
  finally
    if FStream <> nil then FStream.Free;
    Screen.Cursor := crDefault;
  end;
  // open file:
  if filename <> '' then
    ShellExec( filename );
  result := true;
end;



{***
  Converts a TDataSet to CSV-values.
  If a filename is given, save CSV-data to disk, otherwise copy content to clipboard

  @param TDataSet Object which holds data to export
  @param string Field-separator
  @param string Field-encloser
  @param string Line-terminator
  @param string Filename to use for saving. If not given, copy to clipboard.
  @return boolean True on access, False in case of any error
}
function dataset2csv(ds: TDataSet; Separator, Encloser, Terminator: String; filename: String = ''): Boolean;
var
  I, J                      : Integer;
  Buffer, cbuffer           : string;
  FStream                   : TFileStream;
  cursorpos                 : Integer;
begin
  if ds=nil then
    MessageDlg ('Invalid dataset!',mterror, [mbOK], 0);

  separator := esc2ascii(separator);
  encloser := esc2ascii(encloser);
  terminator := esc2ascii(terminator);

  FStream := nil;
  if filename <> '' then
  try
    FStream := TFileStream.Create(FileName, fmCreate)
  except
    messagedlg('File could not be opened.' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
    result := false;
    exit;
  end;

  try
    try
      Buffer := '';
      if FStream = nil then clipboard.astext := '';

      // collect fields:
      for j:=0 to ds.FieldCount-1 do begin
        if j > 0 then
          Buffer := Buffer + Separator;
        Buffer := Buffer + Encloser + ds.Fields[J].FieldName + Encloser;
      end;
      // write buffer:
      if FStream <> nil then FStream.Write(pchar(buffer)^, length(buffer))
      else cbuffer := cbuffer + buffer;

      // collect data:
      cursorpos := ds.RecNo;
      ds.DisableControls;
      ds.First;
      for i:=0 to ds.RecordCount-1 do
      begin
        Buffer := '';
        Buffer := Buffer + Terminator;
        for j:=0 to ds.FieldCount-1 do
        begin
          if j>0 then
            Buffer := Buffer + Separator;
          Buffer := Buffer + Encloser + ds.Fields[j].AsString + Encloser;
        end;
        // write buffer:
        if FStream <> nil then FStream.Write(pchar(buffer)^, length(buffer))
        else cbuffer := cbuffer + buffer;
        ds.Next;
      end;
      ds.RecNo := cursorpos;
      ds.EnableControls;
      if FStream = nil then clipboard.astext := cbuffer;
    except
      on e: Exception do begin
        MessageDlg(e.Message, mtError, [mbOK], 0);
        result := false;
       exit;
      end;
    end;
  finally
    if FStream <> nil then FStream.Free;
    Screen.Cursor := crDefault;
  end;
  result := true;
end;



{***
  Converts a TDataSet to XML.
  If a filename is given, save XML to disk, otherwise copy content to clipboard

  @param TDataSet Object which holds data to export
  @param string Text used as root-element
  @param string Filename to use for saving. If not given, copy to clipboard.
  @return boolean True on access, False in case of any error
}
function dataset2xml(ds: TDataset; title: String; filename: String = ''): Boolean;
var
  I, J                      : Integer;
  Buffer, cbuffer, data     : string;
  FStream                   : TFileStream;
  cursorpos                 : Integer;
begin
  FStream := nil;
  if filename <> '' then
  try
    FStream := TFileStream.Create(FileName, fmCreate)
  except
    messagedlg('File could not be opened.' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
    result := false;
    exit;
  end;

  try
    try
      if FStream = nil then clipboard.astext := '';
      buffer := '<?xml version="1.0"?>' + crlf + crlf +
        '<'+title+'>' + crlf;
      if FStream <> nil then FStream.Write(pchar(buffer)^, length(buffer))
      else cbuffer := buffer;

      cursorpos := ds.RecNo;
      ds.DisableControls;
      ds.First;
      for i:=0 to ds.RecordCount-1 do
      begin
        Buffer := #9'<row>' + crlf;
        // collect data:
        for j:=0 to ds.FieldCount-1 do
        begin
          data := ds.Fields[j].AsString;
          data := htmlentities(data);
          Buffer := Buffer + #9#9'<'+ds.Fields[j].FieldName+'>' + data + '</'+ds.Fields[j].FieldName+'>' + crlf;
        end;
        buffer := buffer + #9'</row>' + crlf;
        // write buffer:
        if FStream <> nil then FStream.Write(pchar(buffer)^, length(buffer))
        else cbuffer := cbuffer + buffer;
        ds.Next;
      end;
      ds.RecNo := cursorpos;
      ds.EnableControls;
      // footer:
      buffer := '</'+title+'>' + crlf;
      if FStream <> nil then FStream.Write(pchar(buffer)^, length(buffer))
      else begin
        cbuffer := cbuffer + buffer;
        clipboard.astext := cbuffer;
      end;
    except
      on e: Exception do begin
        MessageDlg(e.Message, mtError, [mbOK], 0);
        result := false;
       exit;
      end;
    end;
  finally
    if FStream <> nil then FStream.Free;
    Screen.Cursor := crDefault;
  end;
  result := true;
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
  Write text to existing FileStream

  @param TFileStream
  @param string Text to write
  @return void
}
procedure wfs( var s: TFileStream; str: String = '');
begin
  str := str + crlf;
  s.Write(pchar(str)^, length(str))
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
function fixSQL( sql: String; sql_version: Integer = SQL_VERSION_ANSI ): String;
begin
  result := sql;
  if sql_version > SQL_VERSION_ANSI then // For all MySQL-versions
  begin
    result := StringReplace(result, ';*/', '*/;', [rfReplaceAll]);
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
var
  i : Integer;
  StrWithInts : String;
begin
  StrWithInts := '';
  for i:=1 to Length(str) do
  begin
    if StrToIntDef( str[i], -1 ) <> -1 then
    begin
      StrWithInts := StrWithInts + str[i];
    end;
  end;
  result := StrToInt64Def( StrWithInts, 0 );
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
function esc(Text: string; ProcessJokerChars: Boolean = false): string;
begin
  // Replace single-backslashes with double-backslashes BEFORE
  // special characters get escaped using their escape-sequence
  // Fixes issue #1648978 "exported sql has \\r\\n instead of \r\n for CRLFs"
  Result := StringReplace(Text, '\', '\\', [rfReplaceAll]);

  {NUL} Result := StringReplace(Result, #0, '\0', [rfReplaceAll]);
  {BS}  Result := StringReplace(Result, #8, '\b', [rfReplaceAll]);
  {TAB} Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
  {CR}  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  {LF}  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  {EOF} Result := StringReplace(Result, #26, '\Z', [rfReplaceAll]);

  {DQ}  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  {SQ}  Result := StringReplace(Result, #39, #39#39, [rfReplaceAll]);

  if ProcessJokerChars then
  begin
    // Escape joker-chars which are used in a LIKE-clause
    Result := StringReplace(Result, '%', '\%', [rfReplaceAll]);
    Result := StringReplace(Result, '_', '\_', [rfReplaceAll]);
  end
  else
  begin
    // Add surrounding single quotes only for non-LIKE-values
    // because in all cases we're using ProcessLIKEChars we
    // need to add leading and/or trailing joker-chars by hand
    // without being escaped
    Result := #39 + Result + #39;
  end;

end;



{***
  Detect NUL character in a text.
}
function hasNullChar(Text: string): boolean;
var
  i: integer;
begin
  result := false;
  for i:=1 to length(Text) do
  begin
    if Ord(Text[i]) = 0 then
    begin
      result := true;
      exit;
    end;
  end;
end;



{***
  Detect non-latin1 characters in a text (except 9, 10 and 13)

  @param string Text to test
  @return boolean Text has any latin1-characters?
}
function hasIrregularChars(Text: string): boolean;
var
  i: integer;
  b: byte;
begin
  result := false;
  for i:=1 to length(Text) do
  begin
    b := Ord(Text[i]);
    // Latin1 characters is everything except 0..31 and 127..159.
    // 9..13 is HTAB, LF, VTAB, FF, CR.  We only allow 9, 10 and 13,
    // because those are the ones that we can escape in esc().
    if b in [0..8, 11..12, 14..31, 127..159] then
    begin
      result := true;
      exit;
    end;
  end;
end;



{***
  The MEMO editor changes all single CRs or LFs to Windows CRLF
  behind the user's back, so it's not useful for editing fields
  where these kinds of line endings occur.  At least not without
  asking the user if it's ok to auto convert to windows format first.
  For now, we just disallow editing so no-one gets surprised.

  @param string Text to test
  @return boolean Text has some non-windows linebreaks?
}
function hasIrregularNewlines(Text: string): boolean;
var
  i: integer;
  b, b2: byte;
begin
  result := false;
  if length(Text) = 0 then exit;
  result := true;
  i := 1;
  repeat
    b := Ord(Text[i]);
    if b = 13 then
    begin
      b2 := Ord(Text[i+1]);
      if b2 = 10 then i := i + 1
      else exit;
    end
    else if b = 10 then exit;
    i := i + 1;
  until i >= length(Text);
  if i = length(Text) then
  begin
    b := Ord(Text[length(Text)]);
    if b in [10, 13] then exit;
  end;
  result := false;
end;



{***
  Escape everything within a single CHAR() call.
  Tried a more efficient implementation, but using more than a few CHAR()
  calls quickly blows mysqld's default stack, effectively limiting us to
  this approach.

  @todo Instead of CHAR(127, 127), we could use x'7F7F' or 0x7F7F.
        Find out when these notations were introduced and switch to
        one of them (0x7F7F ?) if they work on all MySQL versions..
  @param string Text to escape
  @return string
}
function escAllCharacters(Text: string): string;
const
  VALUES_PER_ROW: integer = 15;
var
  i: integer;
  s, tmp: string;
begin
  s := 'CHAR(' + CRLF;
  for i:=1 to length(Text) do
  begin
    Str(Ord(Text[i]), tmp);
    if i < length(Text) then
    begin
      s := s + tmp + ', ';
      if i mod VALUES_PER_ROW = 0 then s := s + CRLF;
    end
    else s := s + tmp;
  end;
  s := s + ')';
  Result := s;
end;



{***
  Escapes as necessary.

  @param string Text to escape
  @return string
}
function escapeAuto(Text: string): string;
begin
  if hasIrregularChars(Text) then
  begin
    Result := escAllCharacters(Text);
  end
  else
  begin
    Result := esc(Text);
  end;
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
function bool2str( boolval : Boolean ) : String;
begin
  if boolval then
    result := 'Y'
  else
    result := 'N';
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
function getFilesFromDir( dir: String; pattern: String = '*.*' ): TStringList;
var
  sr : TSearchRec;
begin
  result := TStringList.Create;
  if dir[length(dir)] <> '\' then
    dir := dir + '\';
  if FindFirst( dir + pattern, $3F, sr ) = 0 then
  begin
    repeat
      if (sr.Attr and faAnyFile) > 0 then
        result.Add( sr.Name );
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
  result := FormatNumber( StrToFloat( str ) );
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
function maskSql(sql_version: integer; str: String) : String;
begin
  // Quote ANSI-compatible (but not MySQL-compatible)?
  if sql_version = SQL_VERSION_ANSI then
  begin
    result := StringReplace(str, '"', '""', [rfReplaceAll]);
    result := '"' + result + '"';
  end
  // Quote MySQL-compatible
  else if sql_version >= 32300 then
  begin
    result := StringReplace(str, '`', '``', [rfReplaceAll]);
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
  Checks if the SQL will bring up any resultset.
  Important to know for deciding on whether to do a
  ZQuery.Open or ZQuery.ExecSQL

  @param string (part of) SQL-query
  @return boolean
  @see TMDIChild:ExecSQLClick
  @see TMysqlQueryThread:Execute
}
function ExpectResultSet(ASql: String): Boolean;
const
  RESULTSET_KEYWORDS : array[0..11] of string[10] =
  (
   'ANALYZE',
   'CALL',
   'CHECK',
   'DESC',
   'DESCRIBE',
   'EXECUTE',
   'EXPLAIN',
   'HELP',
   'OPTIMIZE',
   'REPAIR',
   'SELECT',
   'SHOW'
  );
var
  kw : String;
  i : Integer;
begin
  Result := False;

  // Find keyword and check existance in const-array of resultset-keywords
  kw := UpperCase( getFirstWord( ASql ) );
  for i := Low(RESULTSET_KEYWORDS) to High(RESULTSET_KEYWORDS) do
  begin
    if kw = RESULTSET_KEYWORDS[i] then
    begin
      Result := True;
      break;
    end;
  end;

end;



{***
  Returns first word of a given text
  @param string Given text
  @return string First word-boundary
}
function getFirstWord( text: String ): String;
var
  i : Integer;
begin
  result := '';
  text := trim( text );
  // Check chars if they are either alphabetic or numeric
  // Starting with char nr. 2, because we expect the very first
  // char to be meant to be some part of a word, even if THIS one
  // not alphabetical or numerical.
  for i := 2 to Length(text) - 1 do
  begin
    if not (text[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-']) then
    begin
      // Stop here because we found a non-alphabetic or non-numeric chars.
      // This applies to all different whitespaces, brackets, commas etc.
      result := copy( text, 1, i-1 );
      break;
    end;
  end;
end;


{***
  HeidiSQL is currently a non-Unicode application, blindly
  following the ANSI codepage in use by Windows.

  This function can help when determining which
  MySQL character set matches the ANSI codepage.

  Delphi has excellent support for Unicode strings
  (automatic conversion between single- and multi-byte,
  etc.), so if Zeos could be coerced to regard MySQL
  strings as UTF-8 (or even look at @character_set_results,
  ho hum...), we could skip all this and just use
  "SET NAMES utf8".
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


