unit helpers;


// -------------------------------------
// HeidiSQL
// Functions-library
// -------------------------------------


interface

uses main, Classes, SysUtils, Graphics, db, clipbrd, dialogs,
  forms, controls, ShellApi, checklst, windows, ZDataset, ZAbstractDataset;

  function trimc(s: String; c: Char) : String;
  function implode(seperator: String; a: array of string) :String;
  function implodestr(seperator: String; a: TStringList) :String;
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
  function dataset2html(ds: TZQuery; htmltitle: String; filename: String = ''): Boolean;
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


implementation

const
	CRLF = #13#10;




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
    if (name[i] in ['\','/',':','*','?','"','<','>','|','.',' ']) then
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


// tokenize sql-script and return a TStringList with sql-statements
function parsesql(sql: String) : TStringList;
var
  i, start                          : Integer;
  instring, backslash, incomment    : Boolean;
  encloser                          : Char;
begin
  result := TStringList.Create;
  sql := trim(sql);
  instring := false;
  start := 1;
  backslash := false;
  incomment := false;
  encloser := ' ';

  for i:=1 to length(sql) do begin
    if (sql[i] in ['#']) and (not backslash) and (not instring) then begin
      incomment := not incomment;
      sql[i] := ' ';
      continue;
    end;
    if (sql[i] = #13) and incomment then
      incomment := false;
    if incomment then begin
      sql[i] := ' ';
      continue;
    end;

    if (sql[i] in ['''','"']) and (not backslash) and (not incomment) then begin
      if instring and (sql[i] = encloser) then  // string closed
        instring := not instring
      else if (not instring) then begin         // string is following
        instring := true;
        encloser := sql[i];                     // remember enclosing-character
      end;
    end;

    if (sql[i] = '\') or backslash then
      backslash := not backslash;

    if (sql[i] = ';') and (not instring) then
    begin
      result.Add(trim(copy(sql, start, i-start)));
      start := i+1;
    end;
  end;

  if start < i then
    result.Add(trim(copy(sql, start, i-start)));

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
function dataset2html(ds: TZQuery; htmltitle: String; filename: String = ''): Boolean;
var
  I, J                      : Integer;
  Buffer, cbuffer, data     : string;
  FStream                   : TFileStream;
  blobfilename, extension   : String;
  bf                        : Textfile;
  header                    : String;
  cursorpos                 : Integer;
begin
  try
    if filename <> '' then
      FStream := TFileStream.Create(FileName, fmCreate)
    else
      clipboard.astext := '';
    buffer := '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + crlf + crlf +
      '<html>' + crlf +
      '<head>' + crlf +
      '  <title>' + htmltitle + '</title>' + crlf +
      '  <meta name="GENERATOR" content="'+ appname + ' ' + appversion + '">' + crlf +
      '  <style type="text/css"><!--' + crlf +
      '    .header {background-color: ActiveCaption; color: CaptionText;}' + crlf +
      '    th {vertical-align: top;}' + crlf +
      '    td {vertical-align: top;}' + crlf +
      '  //--></style>' + crlf +
      '</head>' + crlf + crlf +
      '<body>' + crlf + crlf +
      '<h3>' + htmltitle + ' (' + inttostr(ds.RecordCount) + ' Records)</h3>' + crlf + crlf +
      '<table border="1">' + crlf +
      '  <tr class="header">' + crlf;
    for j:=0 to ds.FieldCount-1 do
      buffer := buffer + '    <th>' + ds.Fields[j].FieldName + '</th>' + crlf;
    buffer := buffer + '  </tr>' + crlf;
    if filename <> '' then
      FStream.Write(pchar(buffer)^, length(buffer))
    else
      cbuffer := buffer;

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
            if mainform.ConvertHTMLEntities then
              data := htmlentities(data);
            data := stringreplace(data, #10, #10+'<br>', [rfReplaceAll]);
            data := data + '&nbsp;';
          end;
        end
        else
        begin
          if mainform.ConvertHTMLEntities then
            data := htmlentities(data);
          data := stringreplace(data, #10, #10+'<br>', [rfReplaceAll]);
          data := data + '&nbsp;';
        end;
        Buffer := Buffer + '    <td>' + data + '</td>' + crlf;
      end;
      buffer := buffer + '  </tr>' + crlf;
      // write buffer:
      if filename <> '' then
        FStream.Write(pchar(buffer)^, length(buffer))
      else
        cbuffer := cbuffer + buffer;
      ds.Next;
    end;
    ds.RecNo := cursorpos;
    ds.EnableControls;
    // footer:
    buffer := '</table>' + crlf +  crlf + '<p>' + crlf +
      '<em>generated ' + datetostr(now) + ' ' + timetostr(now) +
      ' by <a href="http://www.'+appname+'.com/">' + appname + ' ' + appversion + '</a></em></p>' + crlf + crlf +
      '</body></html>';
    if filename <> '' then
      FStream.Write(pchar(buffer)^, length(buffer))
    else
    begin
      cbuffer := cbuffer + buffer;
      clipboard.astext := cbuffer;
    end;

  except
    Screen.Cursor := crDefault;
    if filename <> '' then
    begin
      messagedlg('File could not be opened.' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
      FStream.Free;
    end else
      messagedlg('Error while copying data to Clipboard.', mterror, [mbOK], 0)
  end;
  if filename <> '' then
    FStream.Free;
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

  try
    Buffer := '';
    if filename <> '' then
      FStream := TFileStream.Create(FileName, fmCreate)
    else
      clipboard.astext := '';

    // collect fields:
    for j:=0 to ds.FieldCount-1 do begin
      if j > 0 then
        Buffer := Buffer + Separator;
      Buffer := Buffer + Encloser + ds.Fields[J].FieldName + Encloser;
    end;
    // write buffer:
    if filename <> '' then
      FStream.Write(pchar(buffer)^, length(buffer))
    else
      cbuffer := cbuffer + buffer;

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
      if filename <> '' then
        FStream.Write(pchar(buffer)^, length(buffer))
      else
        cbuffer := cbuffer + buffer;
      ds.Next;
    end;
    ds.RecNo := cursorpos;
    ds.EnableControls;
    if filename = '' then
      clipboard.astext := cbuffer;
  except
    Screen.Cursor := crDefault;
    if filename <> '' then
    begin
      messagedlg('File could not be opened.' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
      FStream.Free;
    end
    else
      messagedlg('Error while copying data to Clipboard.', mterror, [mbOK], 0)
  end;
  if filename <> '' then
    FStream.Free;

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
  try
    if filename <> '' then
      FStream := TFileStream.Create(FileName, fmCreate)
    else
      clipboard.astext := '';
    buffer := '<?xml version="1.0"?>' + crlf + crlf +
           '<'+title+'>' + crlf;
    if filename <> '' then
      FStream.Write(pchar(buffer)^, length(buffer))
    else
      cbuffer := buffer;

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
      if filename <> '' then
        FStream.Write(pchar(buffer)^, length(buffer))
      else
        cbuffer := cbuffer + buffer;
      ds.Next;
    end;
    ds.RecNo := cursorpos;
    ds.EnableControls;
    // footer:
    buffer := '</'+title+'>' + crlf;
    if filename <> '' then
      FStream.Write(pchar(buffer)^, length(buffer))
    else begin
      cbuffer := cbuffer + buffer;
      clipboard.astext := cbuffer;
    end;

  except
    Screen.Cursor := crDefault;
    if filename <> '' then
    begin
      messagedlg('File could not be opened.' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
      FStream.Free;
    end else
      messagedlg('Error while copying data to Clipboard.', mterror, [mbOK], 0)
  end;
  if filename <> '' then
    FStream.Free;
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
  try
    Stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
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
  Result := StringReplace(Text, #39, #39#39, [rfReplaceAll]);
  Result := StringReplace(Result, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '%', '\%', [rfReplaceAll]);
  Result := StringReplace(Result, '_', '\_', [rfReplaceAll]);
  Result := #39 + Result + #39;
end;

end.

