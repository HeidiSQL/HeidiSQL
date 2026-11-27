unit dbstructures.mssql;

{$mode delphi}{$H+}

interface

uses
  dbstructures, StrUtils, SysUtils;

type
  TMsSqlProvider = class(TSqlProvider)
    public
      function GetSql(AId: TQueryId): string; override;
  end;

  TMsSqlRoutineParam = record
    Name, Datatype, Context: String;
  end;
  // Segments of a CREATE PROCEDURE/FUNCTION code
  TMsSqlRoutineStructure = record
    Preamble: String; // Comments in front of the CREATE keyword
    Returns: String;  // Function result, without the RETURNS keyword
    Options: String;  // WITH and FOR REPLICATION clauses
    Body: String;     // Everything after the AS keyword
    Params: array of TMsSqlRoutineParam;
  end;

  function ParseMsSqlRoutine(const Code: String; out Routine: TMsSqlRoutineStructure): Boolean;

var

  MSSQLDatatypes: TDBDataTypeArray =
  [
    (
      Index:           dbdtUnknown;
      NativeTypes:     '99999';
      Name:            'UNKNOWN';
      Description:     'Unknown data type';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index: dbdtTinyint;
      Name:            'TINYINT';
      Description:     'Integer data from 0 through 255.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtSmallint;
      Name:            'SMALLINT';
      Description:     'Integer data from -2^15 (-32,768) through 2^15 - 1 (32,767).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtInt;
      Name:            'INT';
      Description:     'Integer (whole number) data from -2^31 (-2,147,483,648) through 2^31 - 1 (2,147,483,647).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBigint;
      Name:            'BIGINT';
      Description:     'Integer (whole number) data from -2^63 (-9,223,372,036,854,775,808) through 2^63-1 (9,223,372,036,854,775,807).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtBit;
      Name:            'BIT';
      Description:     '0 or 1';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtDecimal;
      Name:            'DECIMAL';
      Description:     'Fixed precision and scale numeric data from -10^38 +1 through 10^38 -1.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '10,0';
      Category:        dtcReal;
    ),
    (
      Index:           dbdtNumeric;
      Name:            'NUMERIC';
      Description:     'Functionally equivalent to decimal.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      DefLengthSet:    '10,0';
      Category:        dtcReal;
    ),
    (
      Index:           dbdtMoney;
      Name:            'MONEY';
      Description:     'Monetary data values from -2^63 (-922,337,203,685,477.5808) through 2^63 - 1 (+922,337,203,685,477.5807), with accuracy to a ten-thousandth of a monetary unit.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtSmallmoney;
      Name:            'SMALLMONEY';
      Description:     'Monetary data values from -214,748.3648 through +214,748.3647, with accuracy to a ten-thousandth of a monetary unit.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtFloat;
      Name:            'FLOAT';
      Description:     'Floating precision number data with the following valid values: -1.79E + 308 through -2.23E - 308, 0 and 2.23E + 308 through 1.79E + 308.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtReal;
      Name:            'REAL';
      Description:     'Floating precision number data with the following valid values: -3.40E + 38 through -1.18E - 38, 0 and 1.18E - 38 through 3.40E + 38.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtTime;
      Name:            'TIME';
      Description:     'The time data type stores time values only, based on a 24-hour clock. '+
        'The time data type has a range of 00:00:00.0000000 through 23:59:59.9999999 with an '+
        'accuracy of 100 nanoseconds. The default value is 00:00:00.0000000 (midnight). The '+
        'time data type supports user-defined fractional second precision, and the storage '+
        'size varies from 3 to 6 bytes, based on the precision specified.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDate;
      Name:            'DATE';
      Description:     'The date data type has a range of January 1, 01 through December 31, '+
        '9999 with an accuracy of 1 day. The default value is January 1, 1900. The storage size '+
        'is 3 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime;
      Name:            'DATETIME';
      Description:     'Date and time data from January 1, 1753, through December 31, 9999, with an accuracy of three-hundredths of a second, or 3.33 milliseconds.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetime2;
      Name:            'DATETIME2';
      Description:     'Date and time data from January 1,1 AD through December 31, 9999 AD, with an accuracy of three-hundredths of a second, or 3.33 milliseconds.';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzzzzzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtDatetimeOffset;
      Name:            'DATETIMEOFFSET';
      Description:     'Defines a date that is combined with a time of a day that has time zone awareness and is based on a 24-hour clock.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss.zzzzzzz';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtSmalldatetime;
      Name:            'SMALLDATETIME';
      Description:     'Date and time data from January 1, 1900, through June 6, 2079, with an accuracy of one minute.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        False;
      Format:          'yyyy-mm-dd hh:nn:ss';
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTimestamp;
      Name:            'TIMESTAMP';
      Description:     'A database-wide unique number that gets updated every time a row gets updated.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtChar;
      Name:            'CHAR';
      Description:     'Fixed-length non-Unicode character data with a maximum length of 8,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtVarchar;
      Name:            'VARCHAR';
      Description:     'Variable-length non-Unicode data with a maximum of 8,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtText;
      Name:            'TEXT';
      Description:     'Variable-length non-Unicode data with a maximum length of 2^31 - 1 (2,147,483,647) characters.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtNchar;
      Name:            'NCHAR';
      Description:     'Fixed-length Unicode data with a maximum length of 4,000 characters.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtNvarchar;
      Name:            'NVARCHAR';
      Description:     'Variable-length Unicode data with a maximum length of 4,000 characters. sysname is a system-supplied user-defined data type that is functionally equivalent to nvarchar(128) and is used to reference database object names.';
      HasLength:       True;
      RequiresLength:  True;
      HasBinary:       False;
      HasDefault:      True;
      LoadPart:        True;
      DefLengthSet:    '50';
      Category:        dtcText;
    ),
    (
      Index:           dbdtNtext;
      Name:            'NTEXT';
      Description:     'Variable-length Unicode data with a maximum length of 2^30 - 1 (1,073,741,823) characters.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtBinary;
      Name:            'BINARY';
      Description:     'Fixed-length binary data with a maximum length of 8,000 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtVarbinary;
      Name:            'VARBINARY';
      Description:     'Variable-length binary data with a maximum length of 8,000 bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtImage;
      Name:            'IMAGE';
      Description:     'Variable-length binary data with a maximum length of 2^31 - 1 (2,147,483,647) bytes.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtCursor;
      Name:            'CURSOR';
      Description:     'A reference to a cursor.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtSqlvariant;
      Name:            'SQL_VARIANT';
      Description:     'A data type that stores values of various SQL Server-supported data types, except text, ntext, timestamp, and sql_variant.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtTable;
      Name:            'TABLE';
      Description:     'A special data type used to store a result set for later processing .';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtUniqueidentifier;
      Name:            'UNIQUEIDENTIFIER';
      Description:     'A globally unique identifier (GUID).';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtHierarchyid;
      Name:            'HIERARCHYID';
      Description:     'Represents a position in a hierarchy.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtXML;
      Name:            'XML';
      Description:     'Lets you store XML documents and fragments.';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    )
  ];


implementation

{$I const.inc}

function ParseMsSqlRoutine(const Code: String; out Routine: TMsSqlRoutineStructure): Boolean;
var
  Cur, Len, TokStart, TokEnd: Integer;
  Tok, LastTok: String;
  HasParens, HasReturns: Boolean;
  Depth, PartStart, PartEnd, PrevEnd, OptionsStart, ParamIdx: Integer;

  function IsWordChar(c: Char): Boolean;
  begin
    Result := (c in ['A'..'Z', 'a'..'z', '0'..'9', '_', '@', '#', '$']) or (c >= #128);
  end;

  // Move to the next token, skipping whitespace and comments. Sets an empty token at the end of the code.
  procedure NextToken;
  var
    CommentDepth: Integer;
    QuoteEnd: Char;
  begin
    while Cur <= Len do begin
      if Code[Cur] <= ' ' then
        Inc(Cur)
      else if (Code[Cur] = '-') and (Cur < Len) and (Code[Cur+1] = '-') then begin
        while (Cur <= Len) and (Code[Cur] <> #10) do
          Inc(Cur);
      end
      else if (Code[Cur] = '/') and (Cur < Len) and (Code[Cur+1] = '*') then begin
        // Block comments can be nested in T-SQL
        CommentDepth := 1;
        Inc(Cur, 2);
        while (Cur <= Len) and (CommentDepth > 0) do begin
          if (Code[Cur] = '/') and (Cur < Len) and (Code[Cur+1] = '*') then begin
            Inc(CommentDepth);
            Inc(Cur, 2);
          end
          else if (Code[Cur] = '*') and (Cur < Len) and (Code[Cur+1] = '/') then begin
            Dec(CommentDepth);
            Inc(Cur, 2);
          end
          else
            Inc(Cur);
        end;
      end
      else
        Break;
    end;
    TokStart := Cur;
    if Cur <= Len then begin
      if Code[Cur] in ['[', '"', ''''] then begin
        // Quoted identifier or string literal. A doubled closing character is an escaped one.
        if Code[Cur] = '[' then
          QuoteEnd := ']'
        else
          QuoteEnd := Code[Cur];
        Inc(Cur);
        while Cur <= Len do begin
          if Code[Cur] <> QuoteEnd then
            Inc(Cur)
          else if (Cur < Len) and (Code[Cur+1] = QuoteEnd) then
            Inc(Cur, 2)
          else begin
            Inc(Cur);
            Break;
          end;
        end;
      end
      else if IsWordChar(Code[Cur]) then begin
        while (Cur <= Len) and IsWordChar(Code[Cur]) do
          Inc(Cur);
      end
      else
        Inc(Cur);
    end;
    TokEnd := Cur;
    Tok := UpperCase(Copy(Code, TokStart, TokEnd-TokStart));
  end;

begin
  // Split the code of a stored procedure or function into its segments. Other than in MySQL, the
  // parameters of a procedure are mostly not enclosed in parentheses, and comments may precede the CREATE keyword:
  //   CREATE [OR ALTER] {PROC|PROCEDURE} [schema.]name[;number] [(] @param [AS] type [= default] [OUT|OUTPUT|READONLY], ... [)]
  //     [WITH option, ...] [FOR REPLICATION] AS body
  //   CREATE [OR ALTER] FUNCTION [schema.]name ([@param [AS] type [= default] [READONLY], ...])
  //     RETURNS type [WITH option, ...] [AS] body
  // See GitHub issue #667
  Result := False;
  Routine.Preamble := '';
  Routine.Returns := '';
  Routine.Options := '';
  Routine.Body := '';
  SetLength(Routine.Params, 0);
  Cur := 1;
  Len := Length(Code);

  NextToken;
  if (Tok <> 'CREATE') and (Tok <> 'ALTER') then
    Exit;
  Routine.Preamble := Copy(Code, 1, TokStart-1);
  NextToken;
  if Tok = 'OR' then begin
    NextToken;
    NextToken;
  end;
  if (Tok <> 'PROC') and (Tok <> 'PROCEDURE') and (Tok <> 'FUNCTION') then
    Exit;

  // Name, probably with schema
  NextToken;
  NextToken;
  while Tok = '.' do begin
    NextToken;
    if Tok <> '.' then
      NextToken;
  end;
  // Number of a numbered procedure
  if Tok = ';' then begin
    NextToken;
    NextToken;
  end;

  // Parameters
  HasParens := Tok = '(';
  if HasParens then
    NextToken;
  while (Tok <> '') and (Tok[1] = '@') do begin
    ParamIdx := Length(Routine.Params);
    SetLength(Routine.Params, ParamIdx+1);
    Routine.Params[ParamIdx].Name := Copy(Code, TokStart, TokEnd-TokStart);
    NextToken;
    if Tok = 'AS' then
      NextToken;
    PartStart := TokStart;
    PartEnd := TokStart;
    PrevEnd := TokStart;
    LastTok := '';
    Depth := 0;
    while Tok <> '' do begin
      if Depth = 0 then begin
        if Tok = ',' then
          Break;
        if HasParens and (Tok = ')') then
          Break;
        if (not HasParens) and ((Tok = 'AS') or (Tok = 'WITH') or (Tok = 'FOR')) then
          Break;
      end;
      if Tok = '(' then
        Inc(Depth)
      else if Tok = ')' then
        Dec(Depth);
      PrevEnd := PartEnd;
      PartEnd := TokEnd;
      LastTok := Tok;
      NextToken;
    end;
    Routine.Params[ParamIdx].Context := 'IN';
    if (LastTok = 'OUT') or (LastTok = 'OUTPUT') then begin
      Routine.Params[ParamIdx].Context := 'OUT';
      PartEnd := PrevEnd;
    end;
    // Keep a default value and READONLY attached to the data type
    Routine.Params[ParamIdx].Datatype := Trim(Copy(Code, PartStart, PartEnd-PartStart));
    if Tok = ',' then
      NextToken;
  end;
  if HasParens and (Tok = ')') then
    NextToken;

  // RETURNS clause and options, up to the AS keyword which introduces the body
  HasReturns := Tok = 'RETURNS';
  if HasReturns then
    NextToken;
  PartStart := TokStart;
  PartEnd := TokStart;
  OptionsStart := 0;
  LastTok := '';
  Depth := 0;
  while Tok <> '' do begin
    // WITH EXECUTE AS ... is an option, not the start of the body
    if (Depth = 0) and (Tok = 'AS') and (LastTok <> 'EXECUTE') and (LastTok <> 'EXEC') then
      Break;
    // AS is optional in functions
    if (Depth = 0) and HasReturns and ((Tok = 'BEGIN') or (Tok = 'RETURN')) then
      Break;
    if Tok = '(' then
      Inc(Depth)
    else if Tok = ')' then
      Dec(Depth);
    if (Depth = 0) and (OptionsStart = 0) and ((Tok = 'WITH') or (Tok = 'FOR')) then
      OptionsStart := TokStart;
    PartEnd := TokEnd;
    LastTok := Tok;
    NextToken;
  end;
  if Tok = '' then
    Exit;
  if Tok = 'AS' then
    TokStart := TokEnd;
  if OptionsStart > 0 then begin
    Routine.Options := Trim(Copy(Code, OptionsStart, PartEnd-OptionsStart));
    PartEnd := OptionsStart;
  end;
  if HasReturns then
    Routine.Returns := Trim(Copy(Code, PartStart, PartEnd-PartStart));
  Routine.Body := Trim(Copy(Code, TokStart, Length(Code)));
  Result := True;
end;


function TMsSqlProvider.GetSql(AId: TQueryId): string;
begin
  case AId of
    qDatabaseTable: Result := IfThen(
      ServerVersion<=899,
      'master..sysdatabases',
      'sys.databases'
      );
    qDatabaseTableId: Result := IfThen(
      ServerVersion<=899,
      'dbid',
      'database_id'
      );
    qDatabaseDrop: Result := 'DROP DATABASE %s';
    qDbObjectsTable: Result := IfThen(
      ServerVersion<=899,
      '..sysobjects',
      '.sys.objects'
      );
    qDbObjectsCreateCol: Result := IfThen(
      ServerVersion<=899,
      'crdate',
      'create_date'
      );
    qDbObjectsUpdateCol: Result := IfThen(
      ServerVersion<=899,
      '',
      'modify_date'
      );
    qDbObjectsTypeCol: Result := IfThen(
      ServerVersion<=899,
      'xtype',
      'type'
      );
    qRenameTable: Result := 'EXEC sp_rename %s, %s';
    qRenameView: Result := 'EXEC sp_rename %s, %s';
    qCurrentUserHost: Result := 'SELECT SYSTEM_USER';
    qLikeCompare: Result := '%s LIKE %s ESCAPE ''\''';
    qAddColumn: Result := 'ADD %s';
    qChangeColumn: Result := 'ALTER COLUMN %s %s';
    qRenameColumn: Result := 'EXEC sp_rename %s, %s, ''COLUMN''';
    qAutoInc: Result := 'IDENTITY(1,1)';
    qSetColumnComment: Result := 'IF NOT EXISTS (' + sLineBreak +
      '    SELECT 1' + sLineBreak +
      '    FROM fn_listextendedproperty (' + sLineBreak +
      '        N''MS_Description'',' + sLineBreak +
      '        N''SCHEMA'', :EscapedSchema,' + sLineBreak +
      '        N''TABLE'',  :EscapedName,' + sLineBreak +
      '        N''COLUMN'', :TargetColumn' + sLineBreak +
      '    )' + sLineBreak +
      ')' + sLineBreak +
      'BEGIN' + sLineBreak +
      '    EXEC sys.sp_addextendedproperty' + sLineBreak +
      '        @name        = N''MS_Description'',' + sLineBreak +
      '        @value       = :NewComment,' + sLineBreak +
      '        @level0type  = N''SCHEMA'', @level0name = :EscapedSchema,' + sLineBreak +
      '        @level1type  = N''TABLE'',  @level1name = :EscapedName,' + sLineBreak +
      '        @level2type  = N''COLUMN'', @level2name = :TargetColumn' + sLineBreak +
      'END' + sLineBreak +
      'ELSE BEGIN' + sLineBreak +
      '    EXEC sys.sp_updateextendedproperty' + sLineBreak +
      '        @name        = N''MS_Description'',' + sLineBreak +
      '        @value       = :NewComment,' + sLineBreak +
      '        @level0type  = N''SCHEMA'', @level0name = :EscapedSchema,' + sLineBreak +
      '        @level1type  = N''TABLE'',  @level1name = :EscapedName,' + sLineBreak +
      '        @level2type  = N''COLUMN'', @level2name = :TargetColumn' + sLineBreak +
      'END';
    qSetTableComment: Result := 'IF NOT EXISTS (' + sLineBreak +
      '    SELECT 1' + sLineBreak +
      '    FROM fn_listextendedproperty (' + sLineBreak +
      '        N''MS_Description'',' + sLineBreak +
      '        N''SCHEMA'', :EscapedSchema,' + sLineBreak +
      '        N''TABLE'',  :EscapedName,' + sLineBreak +
      '        NULL, NULL' + sLineBreak +
      '    )' + sLineBreak +
      ')' + sLineBreak +
      'BEGIN' + sLineBreak +
      '    EXEC sys.sp_addextendedproperty' + sLineBreak +
      '        @name        = N''MS_Description'',' + sLineBreak +
      '        @value       = :NewComment,' + sLineBreak +
      '        @level0type  = N''SCHEMA'', @level0name = :EscapedSchema,' + sLineBreak +
      '        @level1type  = N''TABLE'',  @level1name = :EscapedName' + sLineBreak +
      'END' + sLineBreak +
      'ELSE BEGIN' + sLineBreak +
      '    EXEC sys.sp_updateextendedproperty' + sLineBreak +
      '        @name        = N''MS_Description'',' + sLineBreak +
      '        @value       = :NewComment,' + sLineBreak +
      '        @level0type  = N''SCHEMA'', @level0name = :EscapedSchema,' + sLineBreak +
      '        @level1type  = N''TABLE'',  @level1name = :EscapedName' + sLineBreak +
      'END';
    // Server configuration options, no session scope. Names are required for sp_configure.
    qSessionVariables, qGlobalVariables: Result := IfThen(
      FServerVersion >= 900,
      'SELECT name, CAST(value_in_use AS BIGINT) AS value FROM sys.configurations ORDER BY name',
      'SELECT comment, value FROM master.dbo.syscurconfigs ORDER BY comment'
      );
    // Format args for setting variables: 0 = name, 1 = escaped name, 2 = value
    qSetGlobalVariable: Result := IfThen(
      FServerVersion >= 900,
      'EXEC sp_configure %1:s, %2:s',
      ''
      );
    qApplyGlobalVariables: Result := 'RECONFIGURE';
    // Columns: id, user, host, db, command, time, state, program, memory, progress, info
    qProcessList: Result := 'SELECT p.spid, RTRIM(p.loginame) AS loginname, RTRIM(p.hostname) AS hostname,'+
      ' d.name, p.cmd, p.waittime, RTRIM(p.status) AS status, RTRIM(p.program_name) AS program_name,'+
      ' s.memory_usage*8192 AS memory, ROUND(NULLIF(r.percent_complete, 0), 1) AS progress, NULL AS Info'+
      ' FROM sys.sysprocesses AS p'+
      ' INNER JOIN '+GetSql(qDatabaseTable)+' AS d ON p.dbid=d.'+GetSql(qDatabaseTableId)+
      ' LEFT JOIN sys.dm_exec_sessions AS s ON s.session_id=p.spid'+
      ' LEFT JOIN sys.dm_exec_requests AS r ON r.session_id=p.spid';
    qISSchemaCol: Result := '%s_CATALOG';
    qUSEQuery: Result := 'USE %s';
    qKillQuery: Result := 'KILL %d';
    qKillProcess: Result := 'KILL %d';
    qFuncLength: Result := 'LEN';
    qFuncCeil: Result := 'CEILING';
    qFuncLeft: Result := 'LEFT(%s, %d)';
    qFuncNow: Result := 'GETDATE()';
    // Session wide, as the grid's SELECT runs in its own batch, where SCOPE_IDENTITY() is NULL
    qFuncLastAutoIncNumber: Result := '@@IDENTITY';
    qLockedTables: Result := '';
    qDisableForeignKeyChecks: Result := '';
    qEnableForeignKeyChecks: Result := '';
    qForeignKeyDrop: Result := 'DROP CONSTRAINT %s';
    qGetTableColumns: Result := '';
    qGetCollations: Result := 'SELECT '''' AS "Collation", '+
      ''''' AS "Charset", 0 AS "Id", '+
      ''''' AS "Default", '''' AS "Compiled", '+
      '1 AS "Sortlen"';
    qGetCharsets: Result := 'SELECT name AS Charset, description AS Description FROM master.sys.syscharsets';
    qGetRowCountApprox: Result := IfThen(
      FServerVersion >= 900,
      'SELECT SUM("rows") FROM "sys"."partitions" WHERE "index_id" IN (0, 1) AND "object_id" = object_id(:EscapedDbSchemaName)',
      ''
      );
    qCastAsText: Result := 'CAST(%s AS NVARCHAR('+IntToStr(GRIDMAXDATA)+'))';
    // MS SQL Server login manager
    qMssqlLoginAttributes: Result :=
      'SELECT p.name, p.type_desc, p.is_disabled, p.default_database_name, ' +
      's.is_policy_checked, s.is_expiration_checked ' +
      'FROM sys.server_principals AS p ' +
      'LEFT JOIN sys.sql_logins AS s ON s.principal_id = p.principal_id ' +
      'WHERE p.type IN (''S'', ''U'', ''G'') ' +
      'ORDER BY p.name';
    qMssqlServerRoles: Result :=
      'SELECT name FROM sys.server_principals WHERE type = ''R'' ORDER BY name';
    qMssqlLoginMemberships: Result :=
      'SELECT r.name ' +
      'FROM sys.server_role_members AS rm ' +
      'JOIN sys.server_principals AS r ON r.principal_id = rm.role_principal_id ' +
      'JOIN sys.server_principals AS m ON m.principal_id = rm.member_principal_id ' +
      'WHERE m.name = %s ' +
      'ORDER BY r.name';
    qMssqlDatabases: Result := IfThen(
      ServerVersion<=899,
      'SELECT name FROM master..sysdatabases',
      'SELECT name FROM sys.databases'
      ) + ' ORDER BY name';
    qMssqlLoginCreate: Result :=
      'CREATE LOGIN %s WITH PASSWORD = %s, CHECK_POLICY = %s, CHECK_EXPIRATION = %s%s';
    qMssqlLoginAlter: Result := 'ALTER LOGIN %s %s';
    qMssqlLoginRename: Result := 'ALTER LOGIN %s WITH NAME = %s';
    qMssqlLoginDrop: Result := 'DROP LOGIN %s';
    qMssqlRoleMembershipGrant: Result :=
      IfThen(
        ServerVersion>=1100,
        'ALTER SERVER ROLE %s ADD MEMBER %s',
        'EXEC sp_addsrvrolemember %s, %s'
      );
    qMssqlRoleMembershipRevoke: Result :=
      IfThen(
        ServerVersion>=1100,
        'ALTER SERVER ROLE %s DROP MEMBER %s',
        'EXEC sp_dropsrvrolemember %s, %s'
      );
    else Result := inherited;
  end;
end;


end.
