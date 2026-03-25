unit dbstructures.interbase;


interface

uses
  dbstructures, StrUtils;

type
  TInterbaseProvider = class(TSqlProvider)
    public
      function GetSql(AId: TQueryId): string; override;
  end;

var

  // Interbase field types
  // Taken from https://docwiki.embarcadero.com/InterBase/2020/en/RDB$FIELDS
  InterbaseDatatypes: Array[0..13] of TDBDatatype =
  (
    (
      Index:           dbdtUnknown;
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
      Index:           dbdtBlob;
      NativeTypes:     '261';
      Name:            'BLOB';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcBinary;
    ),
    (
      Index:           dbdtBool;
      NativeTypes:     '17';
      Name:            'BOOLEAN';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcOther;
    ),
    (
      Index:           dbdtChar;
      NativeTypes:     '14';
      Name:            'CHAR';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcText;
    ),
    (
      Index:           dbdtVarchar;
      NativeTypes:     '37|40';
      Name:            'VARCHAR';
      HasLength:       True;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        True;
      Category:        dtcText;
    ),
    (
      Index:           dbdtFloat;
      NativeTypes:     '10|11';
      Name:            'FLOAT';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtDouble;
      NativeTypes:     '27';
      Name:            'DOUBLE PRECISION';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcReal;
    ),
    (
      Index:           dbdtBigint;
      NativeTypes:     '16';
      Name:            'INT64';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtInt;
      NativeTypes:     '8';
      Name:            'INTEGER';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtNumeric;
      NativeTypes:     '9';
      Name:            'QUAD';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtSmallint;
      NativeTypes:     '7';
      Name:            'SMALLINT';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcInteger;
    ),
    (
      Index:           dbdtDate;
      NativeTypes:     '12';
      Name:            'DATE';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTime;
      NativeTypes:     '13';
      Name:            'TIME';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcTemporal;
    ),
    (
      Index:           dbdtTimestamp;
      NativeTypes:     '35';
      Name:            'TIMESTAMP';
      HasLength:       False;
      RequiresLength:  False;
      HasBinary:       False;
      HasDefault:      False;
      LoadPart:        False;
      Category:        dtcTemporal;
    )
  );


implementation


{ TInterbaseProvider }

function TInterbaseProvider.GetSql(AId: TQueryId): string;
begin
  case AId of
    qDatabaseDrop: Result := 'DROP DATABASE %s';
    qEmptyTable: Result := 'TRUNCATE %s';
    qRenameTable: Result := 'RENAME TABLE %s TO %s';
    qRenameView: Result := 'RENAME TABLE %s TO %s';
    qCurrentUserHost: Result := IfThen(
      FNetType in [ntInterbase_TCPIP, ntInterbase_Local],
      'select user from rdb$database',
      'select current_user || ''@'' || mon$attachments.mon$remote_host from mon$attachments where mon$attachments.mon$attachment_id = current_connection'
      );
    qLikeCompare: Result := '%s LIKE %s';
    qAddColumn: Result := 'ADD COLUMN %s';
    qChangeColumn: Result := 'CHANGE COLUMN %s %s';
    qRenameColumn: Result := '';
    qSessionVariables: Result := 'SHOW VARIABLES';
    qGlobalVariables: Result := 'SHOW GLOBAL VARIABLES';
    qISSchemaCol: Result := '%s_SCHEMA';
    qUSEQuery: Result := '';
    qKillQuery: Result := 'KILL %d';
    qKillProcess: Result := 'KILL %d';
    qFuncLength: Result := 'LENGTH';
    qFuncCeil: Result := 'CEIL';
    qFuncLeft: Result := 'SUBSTR(%s, 1, %d)';
    qFuncNow: Result := ' cast(''now'' as timestamp) from rdb$database';
    qFuncLastAutoIncNumber: Result := 'LAST_INSERT_ID()';
    qLockedTables: Result := '';
    qDisableForeignKeyChecks: Result := '';
    qEnableForeignKeyChecks: Result := '';
    qForeignKeyDrop: Result := 'DROP FOREIGN KEY %s';
    qGetTableColumns: Result := 'SELECT r.RDB$FIELD_NAME AS field_name,'+
      '   r.RDB$DESCRIPTION AS field_description,'+
      '   r.RDB$DEFAULT_VALUE AS field_default_value,'+
      '   r.RDB$NULL_FLAG AS null_flag,'+
      '   f.RDB$FIELD_LENGTH AS field_length,'+
      '   f.RDB$FIELD_PRECISION AS field_precision,'+
      '   f.RDB$FIELD_SCALE AS field_scale,'+
      '   f.RDB$FIELD_TYPE AS field_type,'+
      '   f.RDB$FIELD_SUB_TYPE AS field_subtype,'+
      '   coll.RDB$COLLATION_NAME AS field_collation,'+
      '   cset.RDB$CHARACTER_SET_NAME AS field_charset'+
      ' FROM RDB$RELATION_FIELDS r'+
      ' LEFT JOIN RDB$FIELDS f ON r.RDB$FIELD_SOURCE = f.RDB$FIELD_NAME'+
      ' LEFT JOIN RDB$CHARACTER_SETS cset ON f.RDB$CHARACTER_SET_ID = cset.RDB$CHARACTER_SET_ID'+
      ' LEFT JOIN RDB$COLLATIONS coll ON f.RDB$COLLATION_ID = coll.RDB$COLLATION_ID'+
      '                              AND F.RDB$CHARACTER_SET_ID = COLL.RDB$CHARACTER_SET_ID'+
      ' WHERE r.RDB$RELATION_NAME=%s'+
      ' ORDER BY r.RDB$FIELD_POSITION';
    qGetCollations: Result := 'SELECT RDB$COLLATION_NAME AS "Collation",'+
      '   RDB$COLLATION_ID AS "Id",'+
      '   RDB$CHARACTER_SET_ID'+
      ' FROM RDB$COLLATIONS';
    qGetCharsets: Result := 'SELECT RDB$CHARACTER_SET_NAME AS "Charset", RDB$CHARACTER_SET_NAME AS "Description" FROM RDB$CHARACTER_SETS';
  end;
end;


end.