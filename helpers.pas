unit helpers;


// -------------------------------------
// HeidiSQL
// Functions-library
// -------------------------------------


interface

uses Classes, SysUtils, Graphics, db, clipbrd, dialogs,
  forms, controls, ShellApi, checklst, windows, ZDataset, ZAbstractDataset,
  shlobj, ActiveX;

  function trimc(s: String; c: Char) : String;
  // TODO: Look at each caller to see if escaping is necessary.
  function implode(seperator: String; a: array of string) :String;
  // TODO: Look at each caller to see if escaping is necessary.
  function implodestr(seperator: String; a: TStringList) :String;
  // TODO: Look at each caller to see if escaping is necessary.
  function implodestrs(seperator: String; a: TStrings) :String;
  function explode(separator, a: String) :TStringList;
  function strpos(haystack, needle: String; offset: Integer=0) : Integer;
  function validname(name: String) : boolean;
  function getklammervalues(str: String):String;
  function parsesql(sql: String) : TStringList;
  function sstr(str: String; len: Integer) : String;
  function notinlist(str: String; strlist: TStrings): Boolean;
  function escape_string(Value: String; StrLen: Integer=-1) : String;
  function inarray(str: String; a: Array of String): Boolean;
  function encrypt(str: String): String;
  function decrypt(str: String): String;
  function htmlentities(str: String): String;
  function dataset2html(ds: TZQuery; htmltitle: String; filename: String = ''; ConvertHTMLEntities: Boolean = true; Generator: String = ''): Boolean;
  function dataset2csv(ds: TZQuery; Separator, Encloser, Terminator: String; filename: String = ''): Boolean;
  function dataset2xml(ds: TZQuery; title: String; filename: String = ''): Boolean;
  function esc2ascii(str: String): String;
  function StrCmpBegin(Str1, Str2: string): Boolean;
  function Max(A, B: Integer): Integer; assembler;
  function Min(A, B: Integer): Integer; assembler;
  function urlencode(url: String): String;
  procedure wfs( var s: TFileStream; str: String = '');
  procedure ToggleCheckListBox(list: TCheckListBox; state: Boolean);
  function _GetFileSize(filename: String): Int64;
  function Mince(PathToMince: String; InSpace: Integer): String;
  procedure RenameRegistryItem(AKey: HKEY; Old, New: String);
  procedure CopyRegistryKey(Source, Dest: HKEY);
  procedure DeleteRegistryKey(Key: HKEY);
  function MakeInt( Str: String ) : Integer;
  function esc(Text: string): string;
  function escLike(Text: string): string;
  function hasIrregularChars(Text: string): boolean;
  function hasIrregularNewlines(Text: string): boolean;
  function escOtherChars(Text: string): string;
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
  function maskSql(mysql_version: integer; str: String) : String;
  procedure ActivateWindow(Window : HWnd);

var
  MYSQL_KEYWORDS             : TStringList;


implementation

const
	CRLF = #13#10;

var
  dbgCounter: Integer = 0;
  DecimalSeparatorSystemdefault: Char;



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

function implodestrs(seperator: String; a: TStrings) :String;
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


// explode a string by separator into a TStringList
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



// return first position of needle in haystack (from char[offset])
function strpos(haystack, needle: String; offset: Integer=0) : Integer;
begin
  haystack := copy(haystack, offset, length(haystack));
  result := pos(needle, haystack);
  if result > 0 then
    result := result + offset-1;
end;



// valid table/db-name?
function validname(name: String) : boolean;
var
  i: Integer;
begin
  result := false;
  if (length(name) > 0) and (length(name) < 65) then
    result := true;

  for i:=1 to length(name) do
  begin
    if (name[i] in ['\','/',':','*','?','"','<','>','|','.']) then
    begin
      result := false;
      break;
    end;
  end;

end;


function getklammervalues(str: String):String;
var
  p1,p2        : Integer;
begin
  p1 := pos('(', str);
  for p2:=strlen(pchar(str)) downto 0 do
    if str[p2] = ')' then break;
  result := copy (str, p1+1, p2-p1-1);
end;


procedure addResult(list: TStringList; s: string);
begin
  s := trim(s);
  if length(s) > 0 then list.Add(s);
end;

// tokenize sql-script and return a TStringList with sql-statements
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


// shorten string to length len and append 3 dots
function sstr(str: String; len: Integer) : String;
begin
  if length(str) >= len then
  begin
    str := copy(str, 0, len);
    str := str + '...';
  end;
  result := str;
end;


// str in TStrings strlist?
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



// addslashes...
{function escape_string(str: String) : String;
var
  i : Integer;
  escaped : Array of char;
begin
  result := '';
  i := 1;
  while(i < length(str)+1) do begin
    case ord(str[i]) of
      13: result := result + '\r';
      10: result := result + '\n';
      9:  result := result + '\t';
      92, 34, 39: result := result + '\' + str[i]; //  \ " '
      else result := result + str[i];
    end;
    inc(i);
  end;
end;
}

// addslashes with String...
function escape_string(Value: String; StrLen: Integer=-1) : String;
var
  I, Add, Len: Integer;
  Ptr: PChar;
begin
  Add := 0;
  if StrLen = -1 then Len := Length(Value)
  else Len := StrLen;
  for I := 1 to Len do
    if Value[I] in ['''', '"', '\', #26, #10, #13, #0] then
      Inc(Add);
  SetLength(Result, Len + Add);
  Ptr := PChar(Result);
  for I := 1 to Len do
  begin
    if Value[I] in ['''', '"', '\', #26, #10, #13, #0] then
    begin
      Ptr^ := '\';
      Inc(Ptr);
      case Value[I] of
        #26: Ptr^ := 'Z';
        #10: Ptr^ := 'n';
        #13: Ptr^ := 'r';
        #0: Ptr^ := '0';
        else Ptr^ := Value[I];
      end;
    end else
      Ptr^ := Value[I];
    Inc(Ptr);
  end;
end;



function inarray(str: String; a: Array of String): Boolean;
var i : Integer;
begin
  result := false;
  i := 0;
  while i < length(a) do begin
    if a[i] = str then begin
      result := true;
      break;
    end;
    inc(i);
  end;
end;


// password-encryption
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


// password-decryption
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


// convert html-chars to their entities
function htmlentities(str: String) : String;
begin
  result := stringreplace(str, '<', '&lt;', [rfReplaceAll]);
  result := stringreplace(result, '>', '&gt;', [rfReplaceAll]);
  result := stringreplace(result, '&', '&amp;', [rfReplaceAll]);
end;



// convert a TZDataSet to HTML-Table.
// if a filename is given, save HTML to disk, otherwise to clipboard
function dataset2html(ds: TZQuery; htmltitle: String; filename: String = ''; ConvertHTMLEntities: Boolean = true; Generator: String = ''): Boolean;
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
    shellexecute(0, 'open', pchar(filename), Nil, NIL, 5);
  result := true;
end;


// convert a TDataSet to CSV-Values.
// if a filename is given, save CSV-data to disk, otherwise to clipboard
function dataset2csv(ds: TZQuery; Separator, Encloser, Terminator: String; filename: String = ''): Boolean;
var
  I, J                      : Integer;
  Buffer, cbuffer           : string;
  FStream                   : TFileStream;
  cursorpos                 : Integer;
begin
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



// convert a TZDataSet to XML.
// if a filename is given, save XML to disk, otherwise to clipboard
function dataset2xml(ds: TZQuery; title: String; filename: String = ''): Boolean;
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


// return ASCII-Values from MySQL-Escape-Sequences
function esc2ascii(str: String): String;
begin
  str := stringreplace(str, '\r', #13, [rfReplaceAll]);
  str := stringreplace(str, '\n', #10, [rfReplaceAll]);
  str := stringreplace(str, '\t', #9, [rfReplaceAll]);
  result := str;
end;


// Get maximum value
function Max(A, B: Integer): Integer; assembler;
asm
  CMP EAX,EDX
  JG  @Exit
  MOV EAX,EDX
@Exit:
end;

// Get minimum value
function Min(A, B: Integer): Integer; assembler;
asm
  CMP EAX,EDX
  JL  @Exit
  MOV EAX,EDX
@Exit:
end;


// string compare from the begin
function StrCmpBegin(Str1, Str2: string): Boolean;
begin
  if ((Str1 = '') or (Str2 = '')) and (Str1 <> Str2) then
    Result := False
  else
    Result := (StrLComp(PChar(Str1), PChar(Str2),
      Min(Length(Str1), Length(Str2))) = 0);
end;


function urlencode(url: String): String;
begin
  result := stringreplace(url, ' ', '+', [rfReplaceAll]);
end;


// Write str to FileStream
procedure wfs( var s: TFileStream; str: String = '');
begin
  str := str + crlf;
  s.Write(pchar(str)^, length(str))
end;



procedure ToggleCheckListBox(list: TCheckListBox; state: Boolean);
var
  i : Integer;
begin
  // select all/none
  for i:=0 to list.Items.Count-1 do
    list.checked[i] := state;
end;


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



{=========================================================}
Function Mince(PathToMince: String; InSpace: Integer): String;
{=========================================================}
// "C:\Program Files\Delphi\DDrop\TargetDemo\main.pas"
// "C:\Program Files\..\main.pas"
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


procedure RenameRegistryItem(AKey: HKEY; Old, New: String);

var OldKey,
    NewKey  : HKEY;
    Status  : Integer;

begin
  // Open Source key
  Status:=RegOpenKey(AKey,PChar(Old),OldKey);
  if Status = ERROR_SUCCESS then
  begin
    // Create Destination key
    Status:=RegCreateKey(AKey,PChar(New),NewKey);
    if Status = ERROR_SUCCESS then CopyRegistryKey(OldKey,NewKey);
    RegCloseKey(OldKey);
    RegCloseKey(NewKey);
    // Delete last top-level key
    RegDeleteKey(AKey,PChar(Old));
  end;
end;

//--------------------------------------------------------------------------------

procedure CopyRegistryKey(Source, Dest: HKEY);

const DefValueSize  = 512;
      DefBufferSize = 8192;

var Status      : Integer;
    Key         : Integer;
    ValueSize,
    BufferSize  : Cardinal;
    KeyType     : Integer;
    ValueName   : String;
    Buffer      : Pointer;
    NewTo,
    NewFrom     : HKEY;

begin
  SetLength(ValueName,DefValueSize);
  Buffer:=AllocMem(DefBufferSize);
  try
    Key:=0;
    repeat
      ValueSize:=DefValueSize;
      BufferSize:=DefBufferSize;
      //  enumerate data values at current key
      Status:=RegEnumValue(Source,Key,PChar(ValueName),ValueSize,nil,@KeyType,Buffer,@BufferSize);
      if Status = ERROR_SUCCESS then
      begin
        // move each value to new place
        Status:=RegSetValueEx(Dest,PChar(ValueName),0,KeyType,Buffer,BufferSize);
         // delete old value
        RegDeleteValue(Source,PChar(ValueName));
      end;
    until Status <> ERROR_SUCCESS; // Loop until all values found

    // start over, looking for keys now instead of values
    Key:=0;
    repeat
      ValueSize:=DefValueSize;
      BufferSize:=DefBufferSize;
      Status:=RegEnumKeyEx(Source,Key,PChar(ValueName),ValueSize,nil,Buffer,@BufferSize,nil);
      // was a valid key found?
      if Status = ERROR_SUCCESS then
      begin
        // open the key if found
        Status:=RegCreateKey(Dest,PChar(ValueName),NewTo);
        if Status = ERROR_SUCCESS then
        begin                                       //  Create new key of old name
          Status:=RegCreateKey(Source,PChar(ValueName),NewFrom);
          if Status = ERROR_SUCCESS then
          begin
            // if that worked, recurse back here
            CopyRegistryKey(NewFrom,NewTo);
            RegCloseKey(NewFrom);
            RegDeleteKey(Source,PChar(ValueName));
          end;
          RegCloseKey(NewTo);
        end;
      end;
    until Status <> ERROR_SUCCESS; // loop until key enum fails
  finally
    FreeMem(Buffer);
  end;
end;

//--------------------------------------------------------------------------------

procedure DeleteRegistryKey(Key: HKEY);

const DefValueSize  = 512;
      DefBufferSize = 8192;

var Status     : Integer;
    Index      : Integer;
    ValueSize,
    BufferSize : Cardinal;
    KeyType    : Integer;
    ValueName  : String;
    Buffer     : Pointer;
    SubKey     : HKEY;

begin
  SetLength(ValueName,DefValueSize);
  Buffer:=AllocMem(DefBufferSize);
  try
    Index:=0;
    repeat
      ValueSize:=DefValueSize;
      BufferSize:=DefBufferSize;
      // enumerate data values at current key
      Status:=RegEnumValue(Key,Index,PChar(ValueName),ValueSize,nil,@KeyType,Buffer,@BufferSize);
      // delete old value
      if Status = ERROR_SUCCESS then RegDeleteValue(Key,PChar(ValueName));
    until Status <> ERROR_SUCCESS; // Loop until all values found

    // start over, looking for keys now instead of values
    Index:=0;
    repeat
      ValueSize:=DefValueSize;
      BufferSize:=DefBufferSize;
      Status:=RegEnumKeyEx(Key,Index,PChar(ValueName),ValueSize,nil,Buffer,@BufferSize,nil);
      // was a valid key found?
      if Status = ERROR_SUCCESS then
      begin
        // open the key if found
        Status:=RegOpenKey(Key,PChar(ValueName),SubKey);
        if Status = ERROR_SUCCESS then
        begin
          // if that worked, recurse back here
          DeleteRegistryKey(SubKey);
          RegCloseKey(SubKey);
          RegDeleteKey(Key,PChar(ValueName));
        end;
      end;
    until Status <> ERROR_SUCCESS; // loop until key enum fails
  finally
    FreeMem(Buffer);
  end;
end;

function MakeInt( Str: String ) : Integer;
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
  result := StrToIntDef( StrWithInts, 0 );
end;

// Escape text string.
function esc(Text: string): string;
begin
  Result := StringReplace(Text, #39, #39#39, [rfReplaceAll]);
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  Result := #39 + Result + #39;
end;

// Escape text string for comparison.
function escLike(Text: string): string;
begin
  Result := StringReplace(Text, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '\''', [rfReplaceAll]);
  Result := StringReplace(Result, '%', '\%', [rfReplaceAll]);
  Result := StringReplace(Result, '_', '\_', [rfReplaceAll]);
end;

// Escape characters which MySQL doesn't strictly care about, but which might confuse editors etc.
function escOtherChars(Text: string): string;
begin
  Result := StringReplace(Text, '"', '\"', [rfReplaceAll]);
  {NUL} Result := StringReplace(Text, #0, '\0', [rfReplaceAll]);
  {BS}  Result := StringReplace(Result, #8, '\b', [rfReplaceAll]);
  {TAB} Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
  {CR}  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  {LF}  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  {EOF} Result := StringReplace(Result, #26, '\Z', [rfReplaceAll]);
end;

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
    // because those are the ones that we can escape in escOtherChars().
    if b in [0..8, 11..12, 14..31, 127..159] then
    begin
      result := true;
      exit;
    end;
  end;
end;

// The MEMO editor changes all single CRs or LFs to Windows CRLF
// behind the user's back, so it's not useful for editing fields
// where these kinds of line endings occur.  At least not without
// asking the user if it's ok to auto convert to windows format first.
// For now, we just disallow editing so no-one gets surprised.
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

// Escape everything within a single CHAR() call.
// Tried a more efficient implementation, but using more than a few CHAR() calls quickly blows mysqld's default stack, effectively limiting us to this approach.
function escAllCharacters(Text: string): string;
const
  VALUES_PER_ROW: integer = 15;
var
  i: integer;
  s, tmp: string;
begin
  // Instead of CHAR(127, 127), we could use x'7F7F' or 0x7F7F.
  // TODO: Find out when these notations were introduced and switch to one of them (0x7F7F ?) if they work on all MySQL versions..
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

// Escapes as necessary.
function escapeAuto(Text: string): string;
begin
  if hasIrregularChars(Text) then
  begin
    Result := escAllCharacters(Text);
  end
  else
  begin
    Result := esc(escOtherChars(Text));
  end;
end;

// Use DebugView from SysInternals or Delphi's Event Log to view.
procedure debug(txt: String);
begin
  if length(txt) = 0 then txt := '(debug: blank output?)';
  // Todo: not thread safe.
  dbgCounter := dbgCounter + 1;
  txt := Format('heidisql: %d %s', [dbgCounter, txt]);
  OutputDebugString(PChar(txt));
end;

function fixNewlines(txt: string): string;
begin
  txt := StringReplace(txt, #13#10, #10, [rfReplaceAll]);
  txt := StringReplace(txt, #13, #10, [rfReplaceAll]);
  txt := StringReplace(txt, #10, #13#10, [rfReplaceAll]);
  result := txt;
end;


function bool2str( boolval : Boolean ) : String;
begin
  if boolval then
    result := 'Y'
  else
    result := 'N';
end;


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


function goodfilename( str: String ): String;
var
  c : Char;
begin
  result := str;
  for c in ['\', '/', ':', '*', '?', '"', '<', '>', '|'] do
    result := StringReplace( result, c, '_', [rfReplaceAll] );
end;


// Return a formatted number from a string
function FormatNumber( str: String ): String; Overload;
begin
  result := FormatNumber( StrToFloat( str ) );
end;

// Return a formatted number from an integer
function FormatNumber( int: Int64 ): String; Overload;
begin
  result := FormatNumber( int, 0 );
end;

// Return a formatted number from a float
function FormatNumber( flt: Double; decimals: Integer = 0 ): String; Overload;
begin
  result := trim( format( '%10.'+IntToStr(decimals)+'n', [flt] ) );
end;

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

function maskSql(mysql_version: integer; str: String) : String;
var
  i, o                    : byte;
  hasbadchar, iskeyword   : Boolean;
begin
  if mysql_version >= 32300 then
  begin
    // only mask if needed
    hasbadchar := false;
    for i:=1 to length(str) do
    begin
      o := ord( str[i] );
      // digits, upper chars, lower chars and _ are allowed
      hasbadchar := not (o in [48..57, 65..90, 97..122, 95]);
      // see bug 1500753
      if (i = 1) and not hasbadchar then
        hasbadchar := o in [48..57];
      if hasbadchar then
        break;
    end;

    iskeyword := ( MYSQL_KEYWORDS.IndexOf( str ) > -1 );

    if hasbadchar or iskeyword then
    begin
      result := StringReplace(str, '`', '``', [rfReplaceAll]);
      result := '`' + result + '`';
    end
    else
      result := str;
  end
  else
    result := str;
end;


// Copyright: This function was nicked from usenet:
// Delphi & focus control by Tony Tanzillo in autodesk.autocad.customization.vba.
procedure ActivateWindow(Window : HWnd);
var
  state : TWindowPlacement;
begin
  If not IsWindow(Window) then exit;
  If IsIconic(Window) then
  begin
    State.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Window, PWindowPlacement(@State));
    if (State.flags and 2) = 2 then
      ShowWindow(Window, SW_SHOWMAXIMIZED)
    else
      ShowWindow(Window, SW_RESTORE);
  end;
  BringWindowToTop(Window);
  SetForegroundWindow(Window);
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

