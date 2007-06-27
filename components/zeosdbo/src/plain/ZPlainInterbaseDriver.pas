{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for Interbase            }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZPlainInterbaseDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZCompatibility, ZPlainDriver;

const
  IBLocalBufferLength = 512;
  IBBigLocalBufferLength = IBLocalBufferLength * 2;
  IBHugeLocalBufferLength = IBBigLocalBufferLength * 20;

  ISC_NULL = -1;
  ISC_NOTNULL = 0;

  ISC_TRUE                      = 1;
  ISC_FALSE                     = 0;
  DSQL_CLOSE                    = 1;
  DSQL_DROP                     = 2;

  SQLDA_VERSION1                = 1;
  SQLDA_VERSION2                = 2;
  SQL_DIALECT_V5                = 1;
  SQL_DIALECT_V6                = 2;
  SQL_DIALECT_CURRENT = SQL_DIALECT_V6; (* latest IB DIALECT *)

  { SQL definitions }
  SQL_VARYING                    = 448;
  SQL_TEXT                       = 452;
  SQL_DOUBLE                     = 480;
  SQL_FLOAT                      = 482;
  SQL_LONG                       = 496;
  SQL_SHORT                      = 500;
  SQL_TIMESTAMP                  = 510;
  SQL_BLOB                       = 520;
  SQL_D_FLOAT                    = 530;
  SQL_ARRAY                      = 540;
  SQL_QUAD                       = 550;
  SQL_TYPE_TIME                  = 560;
  SQL_TYPE_DATE                  = 570;
  SQL_INT64                      = 580;
  SQL_BOOLEAN                    = 590;
  SQL_DATE                       = SQL_TIMESTAMP;

  { SQL types definitions from RDB$FIELDS}
  RDB_VARCHAR = 37;
  RDB_VARCHAR2 = 38;
  RDB_CSTRING = 40;
  RDB_CSTRING2 = 41;
  RDB_CHAR = 14;
  RDB_CHAR2 = 15;
  RDB_D_FLOAT = 11;
  RDB_DOUBLE = 27;
  RDB_FLOAT = 10;
  RDB_INT64 = 16;
  RDB_QUAD = 9;
  RDB_BLOB_ID = 45;
  RDB_INTEGER = 8;
  RDB_SMALLINT = 7;
  RDB_DATE = 12;
  RDB_TIME = 13;
  RDB_TIMESTAMP = 35;
  RDB_BOOLEAN = 17;
  RDB_BLOB = 261;
  { SQL subtypes definitions from RDB$FIELDS}
  CS_NONE = 0;
  CS_BINARY = 1;
  CS_ASCII = 2;
  CS_UNICODE_FSS = 3;
  CS_METADATA = CS_UNICODE_FSS;

  RDB_BLOB_NONE = 0;
  RDB_BLOB_TEXT = 1;
  RDB_BLOB_BLR = 2;
  RDB_BLOB_ACL = 3;
  RDB_BLOB_RESERVED = 4;
  RDB_BLOB_ENCODED = 5;
  RDB_BLOB_DESCRIPTION = 6;
  RDB_NUMBERS_NONE = 0;
  RDB_NUMBERS_NUMERIC = 1;
  RDB_NUMBERS_DECIMAL = 2;

  { Blob Subtypes }
  { types less than zero are reserved for customer use }
  isc_blob_untyped               = 0;

  { internal subtypes }
  isc_blob_text                  = 1;
  isc_blob_blr                   = 2;
  isc_blob_acl                   = 3;
  isc_blob_ranges                = 4;
  isc_blob_summary               = 5;
  isc_blob_format                = 6;
  isc_blob_tra                   = 7;
  isc_blob_extfile               = 8;

  { the range 20-30 is reserved for dBASE and Paradox types }
  isc_blob_formatted_memo        = 20;
  isc_blob_paradox_ole           = 21;
  isc_blob_graphic               = 22;
  isc_blob_dbase_ole             = 23;
  isc_blob_typed_binary          = 24;

  {* Blob information items *}
  isc_info_blob_num_segments = 4;
  isc_info_blob_max_segment = 5;
  isc_info_blob_total_length = 6;
  isc_info_blob_type = 7;

  {* error codes *}
  isc_segment                    =  335544366;
  isc_segstr_eof                 =  335544367;
  isc_lock_conflict              =  335544345;

  { Database parameter block stuff }
  isc_dpb_version1               = 1;
  isc_dpb_cdd_pathname           = 1;
  isc_dpb_allocation             = 2;
  isc_dpb_journal                = 3;
  isc_dpb_page_size              = 4;
  isc_dpb_num_buffers            = 5;
  isc_dpb_buffer_length          = 6;
  isc_dpb_debug                  = 7;
  isc_dpb_garbage_collect        = 8;
  isc_dpb_verify                 = 9;
  isc_dpb_sweep                  = 10;
  isc_dpb_enable_journal         = 11;
  isc_dpb_disable_journal        = 12;
  isc_dpb_dbkey_scope            = 13;
  isc_dpb_number_of_users        = 14;
  isc_dpb_trace                  = 15;
  isc_dpb_no_garbage_collect     = 16;
  isc_dpb_damaged                = 17;
  isc_dpb_license                = 18;
  isc_dpb_sys_user_name          = 19;
  isc_dpb_encrypt_key            = 20;
  isc_dpb_activate_shadow        = 21;
  isc_dpb_sweep_interval         = 22;
  isc_dpb_delete_shadow          = 23;
  isc_dpb_force_write            = 24;
  isc_dpb_begin_log              = 25;
  isc_dpb_quit_log               = 26;
  isc_dpb_no_reserve             = 27;
  isc_dpb_user_name              = 28;
  isc_dpb_password               = 29;
  isc_dpb_password_enc           = 30;
  isc_dpb_sys_user_name_enc      = 31;
  isc_dpb_interp                 = 32;
  isc_dpb_online_dump            = 33;
  isc_dpb_old_file_size          = 34;
  isc_dpb_old_num_files          = 35;
  isc_dpb_old_file               = 36;
  isc_dpb_old_start_page         = 37;
  isc_dpb_old_start_seqno        = 38;
  isc_dpb_old_start_file         = 39;
  isc_dpb_drop_walfile           = 40;
  isc_dpb_old_dump_id            = 41;
  isc_dpb_wal_backup_dir         = 42;
  isc_dpb_wal_chkptlen           = 43;
  isc_dpb_wal_numbufs            = 44;
  isc_dpb_wal_bufsize            = 45;
  isc_dpb_wal_grp_cmt_wait       = 46;
  isc_dpb_lc_messages            = 47;
  isc_dpb_lc_ctype               = 48;
  isc_dpb_cache_manager          = 49;
  isc_dpb_shutdown               = 50;
  isc_dpb_online                 = 51;
  isc_dpb_shutdown_delay         = 52;
  isc_dpb_reserved               = 53;
  isc_dpb_overwrite              = 54;
  isc_dpb_sec_attach             = 55;
  isc_dpb_disable_wal            = 56;
  isc_dpb_connect_timeout        = 57;
  isc_dpb_dummy_packet_interval  = 58;
  isc_dpb_gbak_attach            = 59;
  isc_dpb_sql_role_name          = 60;
  isc_dpb_set_page_buffers       = 61;
  isc_dpb_working_directory      = 62;
  isc_dpb_SQL_dialect            = 63;
  isc_dpb_set_db_readonly        = 64;
  isc_dpb_set_db_SQL_dialect     = 65;
  isc_dpb_gfix_attach            = 66;
  isc_dpb_gstat_attach           = 67;
  isc_dpb_last_dpb_constant      = isc_dpb_gstat_attach;

  { isc_dpb_verify specific flags }
  isc_dpb_pages                  = 1;
  isc_dpb_records                = 2;
  isc_dpb_indices                = 4;
  isc_dpb_transactions           = 8;
  isc_dpb_no_update              = 16;
  isc_dpb_repair                 = 32;
  isc_dpb_ignore                 = 64;

  { isc_dpb_shutdown specific flags }
  isc_dpb_shut_cache             = 1;
  isc_dpb_shut_attachment        = 2;
  isc_dpb_shut_transaction       = 4;
  isc_dpb_shut_force             = 8;

  { Transaction parameter block stuff }
  isc_tpb_version1               = 1;
  isc_tpb_version3               = 3;
  isc_tpb_consistency            = 1;
  isc_tpb_concurrency            = 2;
  isc_tpb_shared                 = 3;
  isc_tpb_protected              = 4;
  isc_tpb_exclusive              = 5;
  isc_tpb_wait                   = 6;
  isc_tpb_nowait                 = 7;
  isc_tpb_read                   = 8;
  isc_tpb_write                  = 9;
  isc_tpb_lock_read              = 10;
  isc_tpb_lock_write             = 11;
  isc_tpb_verb_time              = 12;
  isc_tpb_commit_time            = 13;
  isc_tpb_ignore_limbo           = 14;
  isc_tpb_read_committed         = 15;
  isc_tpb_autocommit             = 16;
  isc_tpb_rec_version            = 17;
  isc_tpb_no_rec_version         = 18;
  isc_tpb_restart_requests       = 19;
  isc_tpb_no_auto_undo           = 20;
  isc_tpb_last_tpb_constant      = isc_tpb_no_auto_undo;

  { Blob Parameter Block }
  isc_bpb_version1               = 1;
  isc_bpb_source_type            = 1;
  isc_bpb_target_type            = 2;
  isc_bpb_type                   = 3;
  isc_bpb_source_interp          = 4;
  isc_bpb_target_interp          = 5;
  isc_bpb_filter_parameter       = 6;

  { SQL information items }
  isc_info_sql_select            = 4;
  isc_info_sql_bind              = 5;
  isc_info_sql_num_variables     = 6;
  isc_info_sql_describe_vars     = 7;
  isc_info_sql_describe_end      = 8;
  isc_info_sql_sqlda_seq         = 9;
  isc_info_sql_message_seq       = 10;
  isc_info_sql_type              = 11;
  isc_info_sql_sub_type          = 12;
  isc_info_sql_scale             = 13;
  isc_info_sql_length            = 14;
  isc_info_sql_null_ind          = 15;
  isc_info_sql_field             = 16;
  isc_info_sql_relation          = 17;
  isc_info_sql_owner             = 18;
  isc_info_sql_alias             = 19;
  isc_info_sql_sqlda_start       = 20;
  isc_info_sql_stmt_type         = 21;
  isc_info_sql_get_plan          = 22;
  isc_info_sql_records           = 23;
  isc_info_sql_batch_fetch       = 24;

  { SQL information return values }
  isc_info_sql_stmt_select         = 1;
  isc_info_sql_stmt_insert         = 2;
  isc_info_sql_stmt_update         = 3;
  isc_info_sql_stmt_delete         = 4;
  isc_info_sql_stmt_ddl            = 5;
  isc_info_sql_stmt_get_segment    = 6;
  isc_info_sql_stmt_put_segment    = 7;
  isc_info_sql_stmt_exec_procedure = 8;
  isc_info_sql_stmt_start_trans    = 9;
  isc_info_sql_stmt_commit         = 10;
  isc_info_sql_stmt_rollback       = 11;
  isc_info_sql_stmt_select_for_upd = 12;
  isc_info_sql_stmt_set_generator  = 13;

  isc_bpb_type_segmented           = 0;
  isc_bpb_type_stream              = 1;

  {************** Information call declarations **************}

  { Common, structural codes }
  isc_info_end                     = 1;
  isc_info_truncated               = 2;
  isc_info_error                   = 3;
  isc_info_data_not_ready          = 4;
  isc_info_flag_end                = 127;

  { Request information items }
  isc_info_number_messages         = 4;
  isc_info_max_message             = 5;
  isc_info_max_send                = 6;
  isc_info_max_receive             = 7;
  isc_info_state                   = 8;
  isc_info_message_number          = 9;
  isc_info_message_size            = 10;
  isc_info_request_cost            = 11;
  isc_info_access_path             = 12;
  isc_info_req_select_count        = 13;
  isc_info_req_insert_count        = 14;
  isc_info_req_update_count        = 15;
  isc_info_req_delete_count        = 16;

  { Database information items }
  isc_info_db_id                 =          4;
  isc_info_reads                 =          5;
  isc_info_writes                =          6;
  isc_info_fetches               =          7;
  isc_info_marks                 =          8;
  isc_info_implementation        =         11;
  isc_info_version               =         12;
  isc_info_base_level            =         13;
  isc_info_page_size             =         14;
  isc_info_num_buffers           =         15;
  isc_info_limbo                 =         16;
  isc_info_current_memory        =         17;
  isc_info_max_memory            =         18;
  isc_info_window_turns          =         19;
  isc_info_license               =         20;
  isc_info_allocation            =         21;
  isc_info_attachment_id         =         22;
  isc_info_read_seq_count        =         23;
  isc_info_read_idx_count        =         24;
  isc_info_insert_count          =         25;
  isc_info_update_count          =         26;
  isc_info_delete_count          =         27;
  isc_info_backout_count         =         28;
  isc_info_purge_count           =         29;
  isc_info_expunge_count         =         30;
  isc_info_sweep_interval        =         31;
  isc_info_ods_version           =         32;
  isc_info_ods_minor_version     =         33;
  isc_info_no_reserve            =         34;
  isc_info_logfile               =         35;
  isc_info_cur_logfile_name      =         36;
  isc_info_cur_log_part_offset   =         37;
  isc_info_num_wal_buffers       =         38;
  isc_info_wal_buffer_size       =         39;
  isc_info_wal_ckpt_length       =         40;
  isc_info_wal_cur_ckpt_interval =         41;
  isc_info_wal_prv_ckpt_fname    =         42;
  isc_info_wal_prv_ckpt_poffset  =         43;
  isc_info_wal_recv_ckpt_fname   =         44;
  isc_info_wal_recv_ckpt_poffset =         45;
  isc_info_wal_grpc_wait_usecs   =         47;
  isc_info_wal_num_io            =         48;
  isc_info_wal_avg_io_size       =         49;
  isc_info_wal_num_commits       =         50;
  isc_info_wal_avg_grpc_size     =         51;
  isc_info_forced_writes         =         52;
  isc_info_user_names            =         53;
  isc_info_page_errors           =         54;
  isc_info_record_errors         =         55;
  isc_info_bpage_errors          =         56;
  isc_info_dpage_errors          =         57;
  isc_info_ipage_errors          =         58;
  isc_info_ppage_errors          =         59;
  isc_info_tpage_errors          =         60;
  isc_info_set_page_buffers      =         61;
  isc_info_db_SQL_dialect        =         62;
  isc_info_db_read_only          =         63;
  isc_info_db_size_in_pages      =         64;

type
  ULong                = Cardinal;
  UChar                = Char;
  Short                = SmallInt;

  ISC_LONG             = LongInt;
  UISC_LONG            = ULong;
  ISC_INT64            = Int64;
  ISC_STATUS           = LongInt;
  UISC_STATUS          = ULong;
  PISC_LONG            = ^ISC_LONG;
  PUISC_LONG           = ^UISC_LONG;
  PISC_STATUS          = ^ISC_STATUS;
  PPISC_STATUS         = ^PISC_STATUS;
  PUISC_STATUS         = ^UISC_STATUS;
  PShort               = ^Short;
  PPChar               = ^PChar;
  UShort               = Word;
  PVoid                = Pointer;

  { C Date/Time Structure }
  TCTimeStructure = record
    tm_sec:        Integer;   { Seconds }
    tm_min:        Integer;   { Minutes }
    tm_hour:       Integer;   { Hour (0--23) }
    tm_mday:       Integer;   { Day of month (1--31) }
    tm_mon:        Integer;   { Month (0--11) }
    tm_year:       Integer;   { Year (calendar year minus 1900) }
    tm_wday:       Integer;   { Weekday (0--6) Sunday = 0) }
    tm_yday:       Integer;   { Day of year (0--365) }
    tm_isdst:      Integer;   { 0 if daylight savings time is not in effect) }
  end;
  PCTimeStructure = ^TCTimeStructure;
  TM = TCTimeStructure;
  PTM = ^TM;

  TISC_VARYING = record
    strlen:       Short;
    str:          array[0..0] of Char;
  end;
  PISC_VARYING = ^TISC_VARYING;

  { InterBase Handle Definitions }
  TISC_BLOB_HANDLE              = PVoid;
  PISC_BLOB_HANDLE              = ^TISC_BLOB_HANDLE;
  TISC_DB_HANDLE                = PVoid;
  PISC_DB_HANDLE                = ^TISC_DB_HANDLE;
  TISC_STMT_HANDLE              = PVoid;
  PISC_STMT_HANDLE              = ^TISC_STMT_HANDLE;
  TISC_TR_HANDLE                = PVoid;
  PISC_TR_HANDLE                = ^TISC_TR_HANDLE;
  TISC_CALLBACK                 = procedure;

  { Time & Date Support }
  ISC_DATE = LongInt;
  PISC_DATE = ^ISC_DATE;
  ISC_TIME = ULong;
  PISC_TIME = ^ISC_TIME;

  TISC_TIMESTAMP = record
    timestamp_time:     ISC_TIME;
    timestamp_date:     ISC_DATE;
  end;
  PISC_TIMESTAMP = ^TISC_TIMESTAMP;

  { Blob id structure }
  TGDS_QUAD = record
    gds_quad_high:  ISC_LONG;
    gds_quad_low:   UISC_LONG;
  end;
  PGDS_QUAD            = ^TGDS_QUAD;

  TISC_QUAD            = TGDS_QUAD;
  PISC_QUAD            = ^TISC_QUAD;

  TISC_ARRAY_BOUND = record
    array_bound_lower:  Short;
    array_bound_upper:  Short;
  end;
  PISC_ARRAY_BOUND = ^TISC_ARRAY_BOUND;

  TISC_ARRAY_DESC = record
    array_desc_dtype:   UChar;
    array_desc_scale:   Char;
    array_desc_length:  Short;
    array_desc_field_name: array[0..31] of Char;
    array_desc_relation_name: array[0..31] of Char;
    array_desc_dimensions: Short;
    array_desc_flags: Short;
    array_desc_bounds: array[0..15] of TISC_ARRAY_BOUND;
  end;
  PISC_ARRAY_DESC = ^TISC_ARRAY_DESC;

  TISC_BLOB_DESC = record
    blob_desc_subtype:          Short;
    blob_desc_charset:          Short;
    blob_desc_segment_size:     Short;
    blob_desc_field_name:       array[0..31] of UChar;
    blob_desc_relation_name:    array[0..31] of UChar;
  end;
  PISC_BLOB_DESC = ^TISC_BLOB_DESC;

  { Declare the extended SQLDA }
  TXSQLVAR = record
    sqltype:            Short;     { datatype of field }
    sqlscale:           Short;     { scale factor }
    sqlsubtype:         Short;     { datatype subtype - BLOBs }
			           { & text types only }
    sqllen:             Short;     { length of data area }
    sqldata:            PChar;     { address of data }
    sqlind:             PSmallInt;  { address of indicator }
                                   { variable }
    sqlname_length:     Short;     { length of sqlname field }
    { name of field, name length + space for NULL }
    sqlname:            array[0..31] of Char;
    relname_length:     Short;     { length of relation name }
    { field's relation name + space for NULL }
    relname:            array[0..31] of Char;
    ownname_length:     Short;     { length of owner name }
    { relation's owner name + space for NULL }
    ownname:            array[0..31] of Char;
    aliasname_length:   Short;     { length of alias name }
    { relation's alias name + space for NULL }
    aliasname:          array[0..31] of Char;
  end;
  PXSQLVAR = ^TXSQLVAR;

  TXSQLDA = record
    version:            Short;     { version of this XSQLDA }
    { XSQLDA name field }
    sqldaid:            array[0..7] of Char;
    sqldabc:            ISC_LONG;  { length in bytes of SQLDA }
    sqln:               Short;     { number of fields allocated }
    sqld:               Short;     { actual number of fields }
    { first field address }
    sqlvar:             array[0..0] of TXSQLVAR;
  end;
  PXSQLDA = ^TXSQLDA;

 {****************************************************}
 { This record type is for passing arguments to       }
 { isc_start_transaction (See docs)                   }
 {****************************************************}
  TISC_START_TRANS = record
    db_handle:          PISC_DB_HANDLE;
    tpb_length:         Word;
    tpb_address:        PChar;
  end;

 {****************************************************}
 { This record type is for passing arguments to       }
 { isc_start_multiple (see docs)                      }
 {****************************************************}
  TISC_TEB = record
    db_handle:          PISC_DB_HANDLE;
    tpb_length:         LongInt;
    tpb_address:        PChar;
  end;
  PISC_TEB = ^TISC_TEB;
  TISC_TEB_ARRAY = array[0..0] of TISC_TEB;
  PISC_TEB_ARRAY = ^TISC_TEB_ARRAY;

  { Interbase status array }
  PARRAY_ISC_STATUS = ^TARRAY_ISC_STATUS;
  TARRAY_ISC_STATUS = array[0..20] of ISC_STATUS;

  {** Represents a generic interface to Interbase native API. }
  IZInterbasePlainDriver = interface (IZPlainDriver)
    ['{AE2C4379-4E47-4752-BC01-D405ACC337F5}']
    function isc_attach_database (status_vector: PISC_STATUS;
      db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
    function isc_detach_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short;
      result_buffer: PChar): ISC_STATUS;
    function isc_array_gen_sdl(status_vector: PISC_STATUS;
      isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
      isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS;
      table_name: PChar; column_name: PChar;
      sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    function isc_free(isc_arg1: PChar): ISC_LONG;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(sqlcode: Short; buffer: PChar;
      buffer_length: Short);
    function isc_interprete(buffer: PChar; status_vector: PPISC_STATUS): ISC_STATUS;
    function isc_start_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PChar): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_dsql_allocate_statement(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
      statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
      length: Word; statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: Word): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar;
      buffer_length: Short; buffer: PChar): ISC_STATUS;
    function isc_open_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_buffer: PChar): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_address: PChar): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar): PISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_get_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
      seg_buffer_length: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_event_block(event_buffer: PPChar; result_buffer: PPChar;
      id_count: Word; event_list: array of PChar): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS;
      buffer_length: Short; event_buffer: PChar; result_buffer: PChar);
    function isc_cancel_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
      event_buffer: PChar; event_function: TISC_CALLBACK;
      event_function_arg: PVoid): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    function isc_vax_integer(buffer: PChar; length: Short): ISC_LONG;

    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP;
      tm_date: PCTimeStructure);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure;
      ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure;
      ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure;
      ib_timestamp: PISC_TIMESTAMP);
  end;

  {** Represents an interface to Interbase 6+ native API. }
  IZInterbase6PlainDriver = interface (IZInterbasePlainDriver)
    ['{AFCC45CF-CF6D-499B-8EC2-5C1737A59E30}']
  end;

  {** Represents class to Interbase 6+ native API. }
  TZInterbase6PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZInterbasePlainDriver, IZInterbase6PlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    function isc_attach_database (status_vector: PISC_STATUS;
      db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
    function isc_detach_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short;
      result_buffer: PChar): ISC_STATUS;
    function isc_array_gen_sdl(status_vector: PISC_STATUS;
      isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
      isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS;
      table_name: PChar; column_name: PChar;
      sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    function isc_free(isc_arg1: PChar): ISC_LONG;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(sqlcode: Short; buffer: PChar;
      buffer_length: Short);
    function isc_interprete(buffer: PChar; status_vector: PPISC_STATUS): ISC_STATUS;
    function isc_start_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PChar): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_dsql_allocate_statement(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
      statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
      length: Word; statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: Word): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar;
      buffer_length: Short; buffer: PChar): ISC_STATUS;
    function isc_open_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_buffer: PChar): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_address: PChar): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar): PISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_get_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
      seg_buffer_length: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_event_block(event_buffer: PPChar; result_buffer: PPChar;
      id_count: Word; event_list: array of PChar): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS;
      buffer_length: Short; event_buffer: PChar; result_buffer: PChar);
    function isc_cancel_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
      event_buffer: PChar; event_function: TISC_CALLBACK;
      event_function_arg: PVoid): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    { Interbase Version 6 routines }
    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP;
      tm_date: PCTimeStructure);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure;
      ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure;
      ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure;
      ib_timestamp: PISC_TIMESTAMP);
    function isc_vax_integer(buffer: PChar; length: Short): ISC_LONG;
  end;

  {** Represents an interface to Interbase 5 native API. }
  IZInterbase5PlainDriver = interface (IZInterbasePlainDriver)
    ['{0AF9A168-9494-4327-AD35-6A2FA6E811DD}']
  end;

  {** Represents class to Interbase 5 native API. }
  TZInterbase5PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZInterbasePlainDriver, IZInterbase5PlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    function isc_attach_database (status_vector: PISC_STATUS;
      db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
    function isc_detach_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short;
      result_buffer: PChar): ISC_STATUS;
    function isc_array_gen_sdl(status_vector: PISC_STATUS;
      isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
      isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS;
      table_name: PChar; column_name: PChar;
      sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    function isc_free(isc_arg1: PChar): ISC_LONG;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(sqlcode: Short; buffer: PChar;
      buffer_length: Short);
    function isc_interprete(buffer: PChar; status_vector: PPISC_STATUS): ISC_STATUS;
    function isc_start_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PChar): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_dsql_allocate_statement(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
      statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
      length: Word; statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: Word): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar;
      buffer_length: Short; buffer: PChar): ISC_STATUS;
    function isc_open_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_buffer: PChar): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_address: PChar): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar): PISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_get_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
      seg_buffer_length: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_event_block(event_buffer: PPChar; result_buffer: PPChar;
      id_count: Word; event_list: array of PChar): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS;
      buffer_length: Short; event_buffer: PChar; result_buffer: PChar);
    function isc_cancel_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
      event_buffer: PChar; event_function: TISC_CALLBACK;
      event_function_arg: PVoid): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    function isc_vax_integer(buffer: PChar; length: Short): ISC_LONG;

    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP;
      tm_date: PCTimeStructure);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure;
      ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure;
      ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure;
      ib_timestamp: PISC_TIMESTAMP);
  end;

  {** Represents an interface to Interbase 6+ native API. }
  IZFirebird10PlainDriver = interface (IZInterbasePlainDriver)
    ['{AFCC45CF-CF6D-499B-8EC2-5C1737A59E30}']
  end;

  {** Represents class to Interbase 6+ native API. }
  TZFirebird10PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZInterbasePlainDriver, IZFirebird10PlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    function isc_attach_database (status_vector: PISC_STATUS;
      db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
    function isc_detach_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short;
      result_buffer: PChar): ISC_STATUS;
    function isc_array_gen_sdl(status_vector: PISC_STATUS;
      isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
      isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS;
      table_name: PChar; column_name: PChar;
      sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    function isc_free(isc_arg1: PChar): ISC_LONG;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(sqlcode: Short; buffer: PChar;
      buffer_length: Short);
    function isc_interprete(buffer: PChar; status_vector: PPISC_STATUS): ISC_STATUS;
    function isc_start_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PChar): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_dsql_allocate_statement(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
      statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
      length: Word; statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: Word): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar;
      buffer_length: Short; buffer: PChar): ISC_STATUS;
    function isc_open_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_buffer: PChar): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_address: PChar): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar): PISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_get_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
      seg_buffer_length: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_event_block(event_buffer: PPChar; result_buffer: PPChar;
      id_count: Word; event_list: array of PChar): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS;
      buffer_length: Short; event_buffer: PChar; result_buffer: PChar);
    function isc_cancel_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
      event_buffer: PChar; event_function: TISC_CALLBACK;
      event_function_arg: PVoid): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    { Interbase Version 6 routines }
    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP;
      tm_date: PCTimeStructure);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure;
      ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure;
      ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure;
      ib_timestamp: PISC_TIMESTAMP);
    function isc_vax_integer(buffer: PChar; length: Short): ISC_LONG;
  end;

  {** Represents an interface to Interbase 6+ native API. }
  IZFirebird15PlainDriver = interface (IZInterbasePlainDriver)
    ['{AFCC45CF-CF6D-499B-8EC2-5C1737A59E30}']
  end;

  {** Represents class to Interbase 6+ native API. }
  TZFirebird15PlainDriver = class (TZAbstractObject, IZPlainDriver,
    IZInterbasePlainDriver, IZFirebird15PlainDriver)
  public
    constructor Create;

    function GetProtocol: string;
    function GetDescription: string;
    procedure Initialize;

    function isc_attach_database (status_vector: PISC_STATUS;
      db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
      parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
    function isc_detach_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_drop_database(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE): ISC_STATUS;
    function isc_database_info(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short;
      result_buffer: PChar): ISC_STATUS;
    function isc_array_gen_sdl(status_vector: PISC_STATUS;
      isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
      isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
    function isc_array_get_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    function isc_array_lookup_bounds(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_lookup_desc(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      table_name, column_name: PChar;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_set_desc(status_vector: PISC_STATUS;
      table_name: PChar; column_name: PChar;
      sql_dtype, sql_length, sql_dimensions: PShort;
      descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    function isc_array_put_slice(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
      array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
      source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    function isc_free(isc_arg1: PChar): ISC_LONG;
    function isc_sqlcode(status_vector: PISC_STATUS): ISC_LONG;
    procedure isc_sql_interprete(sqlcode: Short; buffer: PChar;
      buffer_length: Short);
    function isc_interprete(buffer: PChar; status_vector: PPISC_STATUS): ISC_STATUS;
    function isc_start_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PChar): ISC_STATUS;
    function isc_start_multiple(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
      teb_vector_address: PISC_TEB): ISC_STATUS;
    function isc_rollback_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_rollback_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_retaining(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_commit_transaction(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    function isc_dsql_allocate_statement(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_alloc_statement2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    function isc_dsql_describe(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_describe_bind(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute2(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
      in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_execute_immediate(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
      statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_fetch(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_free_statement(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    function isc_dsql_prepare(status_vector: PISC_STATUS;
      tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
      length: Word; statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    function isc_dsql_set_cursor_name(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: Word): ISC_STATUS;
    function isc_dsql_sql_info(status_vector: PISC_STATUS;
      stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar;
      buffer_length: Short; buffer: PChar): ISC_STATUS;
    function isc_open_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_buffer: PChar): ISC_STATUS;
    function isc_create_blob2(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
      blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
      bpb_address: PChar): ISC_STATUS;
    function isc_blob_info(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
      item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar): PISC_STATUS;
    function isc_close_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_cancel_blob(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    function isc_get_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
      seg_buffer_length: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_put_segment(status_vector: PISC_STATUS;
      blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
    function isc_event_block(event_buffer: PPChar; result_buffer: PPChar;
      id_count: Word; event_list: array of PChar): ISC_LONG;
    procedure isc_event_counts(status_vector: PISC_STATUS;
      buffer_length: Short; event_buffer: PChar; result_buffer: PChar);
    function isc_cancel_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    function isc_que_events(status_vector: PISC_STATUS;
      db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
      event_buffer: PChar; event_function: TISC_CALLBACK;
      event_function_arg: PVoid): ISC_STATUS;
    procedure isc_decode_date(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    procedure isc_encode_date(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    { Interbase Version 6 routines }
    procedure isc_decode_sql_date(ib_date: PISC_DATE; tm_date: PCTimeStructure);
    procedure isc_decode_sql_time(ib_time: PISC_TIME; tm_date: PCTimeStructure);
    procedure isc_decode_timestamp(ib_timestamp: PISC_TIMESTAMP;
      tm_date: PCTimeStructure);
    procedure isc_encode_sql_date(tm_date: PCTimeStructure;
      ib_date: PISC_DATE);
    procedure isc_encode_sql_time(tm_date: PCTimeStructure;
      ib_time: PISC_TIME);
    procedure isc_encode_timestamp(tm_date: PCTimeStructure;
      ib_timestamp: PISC_TIMESTAMP);
    function isc_vax_integer(buffer: PChar; length: Short): ISC_LONG;
  end;

  function XSQLDA_LENGTH(Value: LongInt): LongInt;

implementation

uses SysUtils, ZPlainInterbase5, ZPlainInterbase6, ZPlainFirebird10,
  ZPlainFirebird15;

function XSQLDA_LENGTH(Value: LongInt): LongInt;
begin
  Result := SizeOf(TXSQLDA) + ((Value - 1) * SizeOf(TXSQLVAR));
end;

{ IZInterbase6PlainDriver }

constructor TZInterbase6PlainDriver.Create;
begin

end;

function TZInterbase6PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Interbase 6+';
end;

function TZInterbase6PlainDriver.GetProtocol: string;
begin
  Result := 'interbase-6';
end;

procedure TZInterbase6PlainDriver.Initialize;
begin
  ZPlainInterbase6.LibraryLoader.LoadIfNeeded;
end;

function TZInterbase6PlainDriver.isc_array_gen_sdl(status_vector: PISC_STATUS;
  isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
  isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_array_gen_sdl(status_vector, isc_array_desc,
    isc_arg3, isc_arg4, isc_arg5);
end;

function TZInterbase6PlainDriver.isc_array_get_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; dest_array: PVoid;
  slice_length: ISC_LONG): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_array_get_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, dest_array, slice_length);
end;

function TZInterbase6PlainDriver.isc_array_lookup_bounds(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_array_lookup_bounds(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZInterbase6PlainDriver.isc_array_lookup_desc(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_array_lookup_desc(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZInterbase6PlainDriver.isc_array_put_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; source_array: PVoid;
  slice_length: PISC_LONG): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_array_put_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, source_array, slice_length);
end;

function TZInterbase6PlainDriver.isc_array_set_desc(status_vector: PISC_STATUS;
  table_name, column_name: PChar; sql_dtype, sql_length, sql_dimensions: PShort;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_array_set_desc(status_vector, table_name,
    column_name, sql_dtype, sql_length, sql_dimensions, descriptor);
end;

function TZInterbase6PlainDriver.isc_attach_database(status_vector: PISC_STATUS;
  db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
  parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_attach_database(status_vector, db_name_length,
    db_name, db_handle, parm_buffer_length, parm_buffer);
end;

function TZInterbase6PlainDriver.isc_blob_info(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PChar; result_buffer_length: Short;
  result_buffer: PChar): PISC_STATUS;
begin
  Result := PISC_STATUS(ZPlainInterbase6.isc_blob_info(status_vector, blob_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer));
end;

function TZInterbase6PlainDriver.isc_cancel_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_cancel_blob(status_vector, blob_handle);
end;

function TZInterbase6PlainDriver.isc_cancel_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_cancel_events(status_vector, db_handle,
    event_id);
end;

function TZInterbase6PlainDriver.isc_close_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_close_blob(status_vector, blob_handle);
end;

function TZInterbase6PlainDriver.isc_commit_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_commit_retaining(status_vector, tran_handle);
end;

function TZInterbase6PlainDriver.isc_commit_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_commit_transaction(status_vector, tran_handle);
end;

function TZInterbase6PlainDriver.isc_create_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_address: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_create_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_address);
end;

function TZInterbase6PlainDriver.isc_database_info(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PChar; result_buffer_length: Short;
  result_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_database_info(status_vector, db_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer);
end;

procedure TZInterbase6PlainDriver.isc_decode_date(ib_date: PISC_QUAD;
  tm_date: PCTimeStructure);
begin
  ZPlainInterbase6.isc_decode_date(ib_date, tm_date);
end;

procedure TZInterbase6PlainDriver.isc_decode_sql_date(ib_date: PISC_DATE;
  tm_date: PCTimeStructure);
begin
  ZPlainInterbase6.isc_decode_sql_date(ib_date, tm_date);
end;

procedure TZInterbase6PlainDriver.isc_decode_sql_time(ib_time: PISC_TIME;
  tm_date: PCTimeStructure);
begin
  ZPlainInterbase6.isc_decode_sql_time(ib_time, tm_date);
end;

procedure TZInterbase6PlainDriver.isc_decode_timestamp(
  ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
begin
  ZPlainInterbase6.isc_decode_timestamp(ib_timestamp, tm_date);
end;

function TZInterbase6PlainDriver.isc_detach_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_detach_database(status_vector, db_handle);
end;

function TZInterbase6PlainDriver.isc_drop_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_drop_database(status_vector, db_handle);
end;

function TZInterbase6PlainDriver.isc_dsql_alloc_statement2(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_alloc_statement2(status_vector,
    db_handle, stmt_handle);
end;

function TZInterbase6PlainDriver.isc_dsql_allocate_statement(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_allocate_statement(status_vector,
    db_handle, stmt_handle);
end;

function TZInterbase6PlainDriver.isc_dsql_describe(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_describe(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZInterbase6PlainDriver.isc_dsql_describe_bind(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_describe_bind(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZInterbase6PlainDriver.isc_dsql_execute(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_execute(status_vector, tran_handle,
    stmt_handle, dialect, xsqlda);
end;

function TZInterbase6PlainDriver.isc_dsql_execute_immediate(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  tran_handle: PISC_TR_HANDLE; length: Word; statement: PChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_execute_immediate(status_vector,
    db_handle, tran_handle, length, statement, dialect, xsqlda);
end;

function TZInterbase6PlainDriver.isc_dsql_execute2(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  dialect: Word; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_execute2(status_vector, tran_handle,
    stmt_handle, dialect, in_xsqlda, out_xsqlda);
end;

function TZInterbase6PlainDriver.isc_dsql_fetch(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_fetch(status_vector, stmt_handle, dialect,
    xsqlda);
end;

function TZInterbase6PlainDriver.isc_dsql_free_statement(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  options: Word): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_free_statement(status_vector, stmt_handle,
    options);
end;

function TZInterbase6PlainDriver.isc_dsql_prepare(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  stmt_handle: PISC_STMT_HANDLE; length: Word; statement: PChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_prepare(status_vector, tran_handle,
    stmt_handle, length, statement, dialect, xsqlda);
end;

function TZInterbase6PlainDriver.isc_dsql_set_cursor_name(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  cursor_name: PChar; _type: Word): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_set_cursor_name(status_vector,
    stmt_handle, cursor_name, _type);
end;

function TZInterbase6PlainDriver.isc_dsql_sql_info(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  item_length: Short; items: PChar; buffer_length: Short;
  buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_dsql_sql_info(status_vector, stmt_handle,
    item_length, items, buffer_length, buffer);
end;

procedure TZInterbase6PlainDriver.isc_encode_date(tm_date: PCTimeStructure;
  ib_date: PISC_QUAD);
begin
  ZPlainInterbase6.isc_encode_date(tm_date, ib_date);
end;

procedure TZInterbase6PlainDriver.isc_encode_sql_date(
  tm_date: PCTimeStructure; ib_date: PISC_DATE);
begin
  ZPlainInterbase6.isc_encode_sql_date(tm_date, ib_date);
end;

procedure TZInterbase6PlainDriver.isc_encode_sql_time(
  tm_date: PCTimeStructure; ib_time: PISC_TIME);
begin
  ZPlainInterbase6.isc_encode_sql_time(tm_date, ib_time);
end;

procedure TZInterbase6PlainDriver.isc_encode_timestamp(
  tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP);
begin
  ZPlainInterbase6.isc_encode_timestamp(tm_date, ib_timestamp);
end;

function TZInterbase6PlainDriver.isc_event_block(event_buffer,
  result_buffer: PPChar; id_count: Word;
  event_list: array of PChar): ISC_LONG;
begin
  Result := ZPlainInterbase6.isc_event_block(event_buffer, result_buffer,
    id_count, event_list);
end;

procedure TZInterbase6PlainDriver.isc_event_counts(
  status_vector: PISC_STATUS; buffer_length: Short; event_buffer,
  result_buffer: PChar);
begin
  ZPlainInterbase6.isc_event_counts(status_vector, buffer_length,
    event_buffer, result_buffer);
end;

function TZInterbase6PlainDriver.isc_free(isc_arg1: PChar): ISC_LONG;
begin
  Result := ZPlainInterbase6.isc_free(isc_arg1);
end;

function TZInterbase6PlainDriver.isc_get_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  actual_seg_length: PWord; seg_buffer_length: Word;
  seg_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_get_segment(status_vector, blob_handle,
    actual_seg_length, seg_buffer_length, seg_buffer);
end;

function TZInterbase6PlainDriver.isc_interprete(buffer: PChar;
  status_vector: PPISC_STATUS): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_interprete(buffer, status_vector);
end;

function TZInterbase6PlainDriver.isc_open_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_open_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_buffer);
end;

function TZInterbase6PlainDriver.isc_put_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_put_segment(status_vector, blob_handle,
    seg_buffer_len, seg_buffer);
end;

function TZInterbase6PlainDriver.isc_que_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
  event_buffer: PChar; event_function: TISC_CALLBACK;
  event_function_arg: PVoid): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_que_events(status_vector, db_handle,
    event_id, length, event_buffer, event_function, event_function_arg)
end;

function TZInterbase6PlainDriver.isc_rollback_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_rollback_retaining(status_vector, tran_handle);
end;

procedure TZInterbase6PlainDriver.isc_sql_interprete(sqlcode: Short;
  buffer: PChar; buffer_length: Short);
begin
  ZPlainInterbase6.isc_sql_interprete(sqlcode, buffer, buffer_length);
end;

function TZInterbase6PlainDriver.isc_sqlcode(
  status_vector: PISC_STATUS): ISC_LONG;
begin
  Result := ZPlainInterbase6.isc_sqlcode(status_vector);
end;

function TZInterbase6PlainDriver.isc_rollback_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_rollback_transaction(status_vector, tran_handle);
end;

function TZInterbase6PlainDriver.isc_start_multiple(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_start_multiple(status_vector, tran_handle,
    db_handle_count, teb_vector_address);
end;

function TZInterbase6PlainDriver.isc_start_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; db_handle: PISC_DB_HANDLE; tpb_length: Word;
  tpb_address: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase6.isc_start_transaction(status_vector, tran_handle,
    db_handle_count, db_handle, tpb_length, tpb_address);
end;

function TZInterbase6PlainDriver.isc_vax_integer(buffer: PChar;
  length: Short): ISC_LONG;
begin
  Result := ZPlainInterbase6.isc_vax_integer(buffer, length);
end;

{ TZInterbase5PlainDriver }

constructor TZInterbase5PlainDriver.Create;
begin

end;

function TZInterbase5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Interbase 5+';
end;

function TZInterbase5PlainDriver.GetProtocol: string;
begin
  Result := 'interbase-5';
end;

procedure TZInterbase5PlainDriver.Initialize;
begin
  ZPlainInterbase5.LibraryLoader.LoadIfNeeded;
end;

function TZInterbase5PlainDriver.isc_array_gen_sdl(status_vector: PISC_STATUS;
  isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
  isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_array_gen_sdl(status_vector, isc_array_desc,
    isc_arg3, isc_arg4, isc_arg5);
end;

function TZInterbase5PlainDriver.isc_array_get_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; dest_array: PVoid;
  slice_length: ISC_LONG): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_array_get_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, dest_array, slice_length);
end;

function TZInterbase5PlainDriver.isc_array_lookup_bounds(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_array_lookup_bounds(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZInterbase5PlainDriver.isc_array_lookup_desc(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_array_lookup_desc(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZInterbase5PlainDriver.isc_array_put_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; source_array: PVoid;
  slice_length: PISC_LONG): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_array_put_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, source_array, slice_length);
end;

function TZInterbase5PlainDriver.isc_array_set_desc(status_vector: PISC_STATUS;
  table_name, column_name: PChar; sql_dtype, sql_length, sql_dimensions: PShort;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_array_set_desc(status_vector, table_name,
    column_name, sql_dtype, sql_length, sql_dimensions, descriptor);
end;

function TZInterbase5PlainDriver.isc_attach_database(status_vector: PISC_STATUS;
  db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
  parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_attach_database(status_vector, db_name_length,
    db_name, db_handle, parm_buffer_length, parm_buffer);
end;

function TZInterbase5PlainDriver.isc_blob_info(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PChar; result_buffer_length: Short;
  result_buffer: PChar): PISC_STATUS;
begin
  Result := PISC_STATUS(ZPlainInterbase5.isc_blob_info(status_vector, blob_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer));
end;

function TZInterbase5PlainDriver.isc_cancel_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_cancel_blob(status_vector, blob_handle);
end;

function TZInterbase5PlainDriver.isc_cancel_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_cancel_events(status_vector, db_handle,
    event_id);
end;

function TZInterbase5PlainDriver.isc_close_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_close_blob(status_vector, blob_handle);
end;

function TZInterbase5PlainDriver.isc_commit_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_commit_retaining(status_vector, tran_handle);
end;

function TZInterbase5PlainDriver.isc_commit_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_commit_transaction(status_vector, tran_handle);
end;

function TZInterbase5PlainDriver.isc_create_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_address: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_create_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_address);
end;

function TZInterbase5PlainDriver.isc_database_info(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PChar; result_buffer_length: Short;
  result_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_database_info(status_vector, db_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer);
end;

procedure TZInterbase5PlainDriver.isc_decode_date(ib_date: PISC_QUAD;
  tm_date: PCTimeStructure);
begin
  ZPlainInterbase5.isc_decode_date(ib_date, tm_date);
end;

function TZInterbase5PlainDriver.isc_detach_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_detach_database(status_vector, db_handle);
end;

function TZInterbase5PlainDriver.isc_drop_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_drop_database(status_vector, db_handle);
end;

function TZInterbase5PlainDriver.isc_dsql_alloc_statement2(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_alloc_statement2(status_vector,
    db_handle, stmt_handle);
end;

function TZInterbase5PlainDriver.isc_dsql_allocate_statement(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_allocate_statement(status_vector,
    db_handle, stmt_handle);
end;

function TZInterbase5PlainDriver.isc_dsql_describe(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_describe(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZInterbase5PlainDriver.isc_dsql_describe_bind(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_describe_bind(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZInterbase5PlainDriver.isc_dsql_execute(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_execute(status_vector, tran_handle,
    stmt_handle, dialect, xsqlda);
end;

function TZInterbase5PlainDriver.isc_dsql_execute_immediate(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  tran_handle: PISC_TR_HANDLE; length: Word; statement: PChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_execute_immediate(status_vector,
    db_handle, tran_handle, length, statement, dialect, xsqlda);
end;

function TZInterbase5PlainDriver.isc_dsql_execute2(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  dialect: Word; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_execute2(status_vector, tran_handle,
    stmt_handle, dialect, in_xsqlda, out_xsqlda);
end;

function TZInterbase5PlainDriver.isc_dsql_fetch(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_fetch(status_vector, stmt_handle, dialect,
    xsqlda);
end;

function TZInterbase5PlainDriver.isc_dsql_free_statement(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  options: Word): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_free_statement(status_vector, stmt_handle,
    options);
end;

function TZInterbase5PlainDriver.isc_dsql_prepare(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  stmt_handle: PISC_STMT_HANDLE; length: Word; statement: PChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_prepare(status_vector, tran_handle,
    stmt_handle, length, statement, dialect, xsqlda);
end;

function TZInterbase5PlainDriver.isc_dsql_set_cursor_name(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  cursor_name: PChar; _type: Word): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_set_cursor_name(status_vector,
    stmt_handle, cursor_name, _type);
end;

function TZInterbase5PlainDriver.isc_dsql_sql_info(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  item_length: Short; items: PChar; buffer_length: Short;
  buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_dsql_sql_info(status_vector, stmt_handle,
    item_length, items, buffer_length, buffer);
end;

procedure TZInterbase5PlainDriver.isc_encode_date(tm_date: PCTimeStructure;
  ib_date: PISC_QUAD);
begin
  ZPlainInterbase5.isc_encode_date(tm_date, ib_date);
end;

function TZInterbase5PlainDriver.isc_event_block(event_buffer,
  result_buffer: PPChar; id_count: Word;
  event_list: array of PChar): ISC_LONG;
begin
  Result := ZPlainInterbase5.isc_event_block(event_buffer, result_buffer,
    id_count, event_list);
end;

procedure TZInterbase5PlainDriver.isc_event_counts(
  status_vector: PISC_STATUS; buffer_length: Short; event_buffer,
  result_buffer: PChar);
begin
  ZPlainInterbase5.isc_event_counts(status_vector, buffer_length,
    event_buffer, result_buffer);
end;

function TZInterbase5PlainDriver.isc_free(isc_arg1: PChar): ISC_LONG;
begin
  Result := ZPlainInterbase5.isc_free(isc_arg1);
end;

function TZInterbase5PlainDriver.isc_get_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  actual_seg_length: PWord; seg_buffer_length: Word;
  seg_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_get_segment(status_vector, blob_handle,
    actual_seg_length, seg_buffer_length, seg_buffer);
end;

function TZInterbase5PlainDriver.isc_interprete(buffer: PChar;
  status_vector: PPISC_STATUS): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_interprete(buffer, status_vector);
end;

function TZInterbase5PlainDriver.isc_open_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_open_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_buffer);
end;

function TZInterbase5PlainDriver.isc_put_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_put_segment(status_vector, blob_handle,
    seg_buffer_len, seg_buffer);
end;

function TZInterbase5PlainDriver.isc_que_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
  event_buffer: PChar; event_function: TISC_CALLBACK;
  event_function_arg: PVoid): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_que_events(status_vector, db_handle,
    event_id, length, event_buffer, event_function, event_function_arg)
end;

function TZInterbase5PlainDriver.isc_rollback_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_rollback_retaining(status_vector, tran_handle);
end;

procedure TZInterbase5PlainDriver.isc_sql_interprete(sqlcode: Short;
  buffer: PChar; buffer_length: Short);
begin
  ZPlainInterbase5.isc_sql_interprete(sqlcode, buffer, buffer_length);
end;

function TZInterbase5PlainDriver.isc_sqlcode(
  status_vector: PISC_STATUS): ISC_LONG;
begin
  Result := ZPlainInterbase5.isc_sqlcode(status_vector);
end;

function TZInterbase5PlainDriver.isc_rollback_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_rollback_transaction(status_vector, tran_handle);
end;

function TZInterbase5PlainDriver.isc_start_multiple(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_start_multiple(status_vector, tran_handle,
    db_handle_count, teb_vector_address);
end;

function TZInterbase5PlainDriver.isc_start_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; db_handle: PISC_DB_HANDLE; tpb_length: Word;
  tpb_address: PChar): ISC_STATUS;
begin
  Result := ZPlainInterbase5.isc_start_transaction(status_vector, tran_handle,
    db_handle_count, db_handle, tpb_length, tpb_address);
end;

function TZInterbase5PlainDriver.isc_vax_integer(buffer: PChar;
  length: Short): ISC_LONG;
begin
  Result := ZPlainInterbase5.isc_vax_integer(buffer, length);
end;

procedure TZInterbase5PlainDriver.isc_decode_sql_date(ib_date: PISC_DATE;
  tm_date: PCTimeStructure);
begin
  ZPlainInterbase5.isc_decode_sql_date_stub(ib_date, tm_date);
end;

procedure TZInterbase5PlainDriver.isc_decode_sql_time(ib_time: PISC_TIME;
  tm_date: PCTimeStructure);
begin
  ZPlainInterbase5.isc_decode_sql_time_stub(ib_time, tm_date);
end;

procedure TZInterbase5PlainDriver.isc_decode_timestamp(
  ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
begin
  ZPlainInterbase5.isc_decode_timestamp_stub(ib_timestamp, tm_date);
end;

procedure TZInterbase5PlainDriver.isc_encode_sql_date(
  tm_date: PCTimeStructure; ib_date: PISC_DATE);
begin
  ZPlainInterbase5.isc_encode_sql_date_stub(tm_date, ib_date);
end;

procedure TZInterbase5PlainDriver.isc_encode_sql_time(
  tm_date: PCTimeStructure; ib_time: PISC_TIME);
begin
  ZPlainInterbase5.isc_encode_sql_time_stub(tm_date, ib_time);
end;

procedure TZInterbase5PlainDriver.isc_encode_timestamp(
  tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP);
begin
  ZPlainInterbase5.isc_encode_timestamp_stub(tm_date, ib_timestamp);
end;


{ IZFirebirdPlainDriver }

constructor TZFirebird10PlainDriver.Create;
begin

end;

function TZFirebird10PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird';
end;

function TZFirebird10PlainDriver.GetProtocol: string;
begin
  Result := 'firebird-1.0';
end;

procedure TZFirebird10PlainDriver.Initialize;
begin
  ZPlainFirebird10.LibraryLoader.LoadIfNeeded;
end;

function TZFirebird10PlainDriver.isc_array_gen_sdl(status_vector: PISC_STATUS;
  isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
  isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_array_gen_sdl(status_vector, isc_array_desc,
    isc_arg3, isc_arg4, isc_arg5);
end;

function TZFirebird10PlainDriver.isc_array_get_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; dest_array: PVoid;
  slice_length: ISC_LONG): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_array_get_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, dest_array, slice_length);
end;

function TZFirebird10PlainDriver.isc_array_lookup_bounds(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_array_lookup_bounds(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZFirebird10PlainDriver.isc_array_lookup_desc(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_array_lookup_desc(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZFirebird10PlainDriver.isc_array_put_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; source_array: PVoid;
  slice_length: PISC_LONG): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_array_put_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, source_array, slice_length);
end;

function TZFirebird10PlainDriver.isc_array_set_desc(status_vector: PISC_STATUS;
  table_name, column_name: PChar; sql_dtype, sql_length, sql_dimensions: PShort;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_array_set_desc(status_vector, table_name,
    column_name, sql_dtype, sql_length, sql_dimensions, descriptor);
end;

function TZFirebird10PlainDriver.isc_attach_database(status_vector: PISC_STATUS;
  db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
  parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_attach_database(status_vector, db_name_length,
    db_name, db_handle, parm_buffer_length, parm_buffer);
end;

function TZFirebird10PlainDriver.isc_blob_info(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PChar; result_buffer_length: Short;
  result_buffer: PChar): PISC_STATUS;
begin
  Result := PISC_STATUS(ZPlainFirebird10.isc_blob_info(status_vector, blob_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer));
end;

function TZFirebird10PlainDriver.isc_cancel_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_cancel_blob(status_vector, blob_handle);
end;

function TZFirebird10PlainDriver.isc_cancel_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_cancel_events(status_vector, db_handle,
    event_id);
end;

function TZFirebird10PlainDriver.isc_close_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_close_blob(status_vector, blob_handle);
end;

function TZFirebird10PlainDriver.isc_commit_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_commit_retaining(status_vector, tran_handle);
end;

function TZFirebird10PlainDriver.isc_commit_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_commit_transaction(status_vector, tran_handle);
end;

function TZFirebird10PlainDriver.isc_create_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_address: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_create_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_address);
end;

function TZFirebird10PlainDriver.isc_database_info(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PChar; result_buffer_length: Short;
  result_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_database_info(status_vector, db_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer);
end;

procedure TZFirebird10PlainDriver.isc_decode_date(ib_date: PISC_QUAD;
  tm_date: PCTimeStructure);
begin
  ZPlainFirebird10.isc_decode_date(ib_date, tm_date);
end;

procedure TZFirebird10PlainDriver.isc_decode_sql_date(ib_date: PISC_DATE;
  tm_date: PCTimeStructure);
begin
  ZPlainFirebird10.isc_decode_sql_date(ib_date, tm_date);
end;

procedure TZFirebird10PlainDriver.isc_decode_sql_time(ib_time: PISC_TIME;
  tm_date: PCTimeStructure);
begin
  ZPlainFirebird10.isc_decode_sql_time(ib_time, tm_date);
end;

procedure TZFirebird10PlainDriver.isc_decode_timestamp(
  ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
begin
  ZPlainFirebird10.isc_decode_timestamp(ib_timestamp, tm_date);
end;

function TZFirebird10PlainDriver.isc_detach_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_detach_database(status_vector, db_handle);
end;

function TZFirebird10PlainDriver.isc_drop_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_drop_database(status_vector, db_handle);
end;

function TZFirebird10PlainDriver.isc_dsql_alloc_statement2(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_alloc_statement2(status_vector,
    db_handle, stmt_handle);
end;

function TZFirebird10PlainDriver.isc_dsql_allocate_statement(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_allocate_statement(status_vector,
    db_handle, stmt_handle);
end;

function TZFirebird10PlainDriver.isc_dsql_describe(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_describe(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZFirebird10PlainDriver.isc_dsql_describe_bind(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_describe_bind(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZFirebird10PlainDriver.isc_dsql_execute(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_execute(status_vector, tran_handle,
    stmt_handle, dialect, xsqlda);
end;

function TZFirebird10PlainDriver.isc_dsql_execute_immediate(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  tran_handle: PISC_TR_HANDLE; length: Word; statement: PChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_execute_immediate(status_vector,
    db_handle, tran_handle, length, statement, dialect, xsqlda);
end;

function TZFirebird10PlainDriver.isc_dsql_execute2(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  dialect: Word; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_execute2(status_vector, tran_handle,
    stmt_handle, dialect, in_xsqlda, out_xsqlda);
end;

function TZFirebird10PlainDriver.isc_dsql_fetch(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_fetch(status_vector, stmt_handle, dialect,
    xsqlda);
end;

function TZFirebird10PlainDriver.isc_dsql_free_statement(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  options: Word): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_free_statement(status_vector, stmt_handle,
    options);
end;

function TZFirebird10PlainDriver.isc_dsql_prepare(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  stmt_handle: PISC_STMT_HANDLE; length: Word; statement: PChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_prepare(status_vector, tran_handle,
    stmt_handle, length, statement, dialect, xsqlda);
end;

function TZFirebird10PlainDriver.isc_dsql_set_cursor_name(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  cursor_name: PChar; _type: Word): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_set_cursor_name(status_vector,
    stmt_handle, cursor_name, _type);
end;

function TZFirebird10PlainDriver.isc_dsql_sql_info(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  item_length: Short; items: PChar; buffer_length: Short;
  buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_dsql_sql_info(status_vector, stmt_handle,
    item_length, items, buffer_length, buffer);
end;

procedure TZFirebird10PlainDriver.isc_encode_date(tm_date: PCTimeStructure;
  ib_date: PISC_QUAD);
begin
  ZPlainFirebird10.isc_encode_date(tm_date, ib_date);
end;

procedure TZFirebird10PlainDriver.isc_encode_sql_date(
  tm_date: PCTimeStructure; ib_date: PISC_DATE);
begin
  ZPlainFirebird10.isc_encode_sql_date(tm_date, ib_date);
end;

procedure TZFirebird10PlainDriver.isc_encode_sql_time(
  tm_date: PCTimeStructure; ib_time: PISC_TIME);
begin
  ZPlainFirebird10.isc_encode_sql_time(tm_date, ib_time);
end;

procedure TZFirebird10PlainDriver.isc_encode_timestamp(
  tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP);
begin
  ZPlainFirebird10.isc_encode_timestamp(tm_date, ib_timestamp);
end;

function TZFirebird10PlainDriver.isc_event_block(event_buffer,
  result_buffer: PPChar; id_count: Word;
  event_list: array of PChar): ISC_LONG;
begin
  Result := ZPlainFirebird10.isc_event_block(event_buffer, result_buffer,
    id_count, event_list);
end;

procedure TZFirebird10PlainDriver.isc_event_counts(
  status_vector: PISC_STATUS; buffer_length: Short; event_buffer,
  result_buffer: PChar);
begin
  ZPlainFirebird10.isc_event_counts(status_vector, buffer_length,
    event_buffer, result_buffer);
end;

function TZFirebird10PlainDriver.isc_free(isc_arg1: PChar): ISC_LONG;
begin
  Result := ZPlainFirebird10.isc_free(isc_arg1);
end;

function TZFirebird10PlainDriver.isc_get_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  actual_seg_length: PWord; seg_buffer_length: Word;
  seg_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_get_segment(status_vector, blob_handle,
    actual_seg_length, seg_buffer_length, seg_buffer);
end;

function TZFirebird10PlainDriver.isc_interprete(buffer: PChar;
  status_vector: PPISC_STATUS): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_interprete(buffer, status_vector);
end;

function TZFirebird10PlainDriver.isc_open_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_open_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_buffer);
end;

function TZFirebird10PlainDriver.isc_put_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_put_segment(status_vector, blob_handle,
    seg_buffer_len, seg_buffer);
end;

function TZFirebird10PlainDriver.isc_que_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
  event_buffer: PChar; event_function: TISC_CALLBACK;
  event_function_arg: PVoid): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_que_events(status_vector, db_handle,
    event_id, length, event_buffer, event_function, event_function_arg)
end;

function TZFirebird10PlainDriver.isc_rollback_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_rollback_retaining(status_vector, tran_handle);
end;

procedure TZFirebird10PlainDriver.isc_sql_interprete(sqlcode: Short;
  buffer: PChar; buffer_length: Short);
begin
  ZPlainFirebird10.isc_sql_interprete(sqlcode, buffer, buffer_length);
end;

function TZFirebird10PlainDriver.isc_sqlcode(
  status_vector: PISC_STATUS): ISC_LONG;
begin
  Result := ZPlainFirebird10.isc_sqlcode(status_vector);
end;

function TZFirebird10PlainDriver.isc_rollback_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_rollback_transaction(status_vector, tran_handle);
end;

function TZFirebird10PlainDriver.isc_start_multiple(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_start_multiple(status_vector, tran_handle,
    db_handle_count, teb_vector_address);
end;

function TZFirebird10PlainDriver.isc_start_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; db_handle: PISC_DB_HANDLE; tpb_length: Word;
  tpb_address: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird10.isc_start_transaction(status_vector, tran_handle,
    db_handle_count, db_handle, tpb_length, tpb_address);
end;

function TZFirebird10PlainDriver.isc_vax_integer(buffer: PChar;
  length: Short): ISC_LONG;
begin
  Result := ZPlainFirebird10.isc_vax_integer(buffer, length);
end;


{ IZFirebird15PlainDriver }

constructor TZFirebird15PlainDriver.Create;
begin

end;

function TZFirebird15PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Firebird 1.5';
end;

function TZFirebird15PlainDriver.GetProtocol: string;
begin
  Result := 'firebird-1.5';
end;

procedure TZFirebird15PlainDriver.Initialize;
begin
  ZPlainFirebird15.LibraryLoader.LoadIfNeeded;
end;

function TZFirebird15PlainDriver.isc_array_gen_sdl(status_vector: PISC_STATUS;
  isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
  isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_array_gen_sdl(status_vector, isc_array_desc,
    isc_arg3, isc_arg4, isc_arg5);
end;

function TZFirebird15PlainDriver.isc_array_get_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; dest_array: PVoid;
  slice_length: ISC_LONG): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_array_get_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, dest_array, slice_length);
end;

function TZFirebird15PlainDriver.isc_array_lookup_bounds(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_array_lookup_bounds(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZFirebird15PlainDriver.isc_array_lookup_desc(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  trans_handle: PISC_TR_HANDLE; table_name, column_name: PChar;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_array_lookup_desc(status_vector, db_handle,
    trans_handle, table_name, column_name, descriptor);
end;

function TZFirebird15PlainDriver.isc_array_put_slice(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE; array_id: PISC_QUAD;
  descriptor: PISC_ARRAY_DESC; source_array: PVoid;
  slice_length: PISC_LONG): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_array_put_slice(status_vector, db_handle,
    trans_handle, array_id, descriptor, source_array, slice_length);
end;

function TZFirebird15PlainDriver.isc_array_set_desc(status_vector: PISC_STATUS;
  table_name, column_name: PChar; sql_dtype, sql_length, sql_dimensions: PShort;
  descriptor: PISC_ARRAY_DESC): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_array_set_desc(status_vector, table_name,
    column_name, sql_dtype, sql_length, sql_dimensions, descriptor);
end;

function TZFirebird15PlainDriver.isc_attach_database(status_vector: PISC_STATUS;
  db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
  parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_attach_database(status_vector, db_name_length,
    db_name, db_handle, parm_buffer_length, parm_buffer);
end;

function TZFirebird15PlainDriver.isc_blob_info(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PChar; result_buffer_length: Short;
  result_buffer: PChar): PISC_STATUS;
begin
  Result := PISC_STATUS(ZPlainFirebird15.isc_blob_info(status_vector, blob_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer));
end;

function TZFirebird15PlainDriver.isc_cancel_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_cancel_blob(status_vector, blob_handle);
end;

function TZFirebird15PlainDriver.isc_cancel_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_cancel_events(status_vector, db_handle,
    event_id);
end;

function TZFirebird15PlainDriver.isc_close_blob(status_vector: PISC_STATUS;
  blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_close_blob(status_vector, blob_handle);
end;

function TZFirebird15PlainDriver.isc_commit_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_commit_retaining(status_vector, tran_handle);
end;

function TZFirebird15PlainDriver.isc_commit_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_commit_transaction(status_vector, tran_handle);
end;

function TZFirebird15PlainDriver.isc_create_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_address: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_create_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_address);
end;

function TZFirebird15PlainDriver.isc_database_info(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
  item_list_buffer: PChar; result_buffer_length: Short;
  result_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_database_info(status_vector, db_handle,
    item_list_buffer_length, item_list_buffer, result_buffer_length,
    result_buffer);
end;

procedure TZFirebird15PlainDriver.isc_decode_date(ib_date: PISC_QUAD;
  tm_date: PCTimeStructure);
begin
  ZPlainFirebird15.isc_decode_date(ib_date, tm_date);
end;

procedure TZFirebird15PlainDriver.isc_decode_sql_date(ib_date: PISC_DATE;
  tm_date: PCTimeStructure);
begin
  ZPlainFirebird15.isc_decode_sql_date(ib_date, tm_date);
end;

procedure TZFirebird15PlainDriver.isc_decode_sql_time(ib_time: PISC_TIME;
  tm_date: PCTimeStructure);
begin
  ZPlainFirebird15.isc_decode_sql_time(ib_time, tm_date);
end;

procedure TZFirebird15PlainDriver.isc_decode_timestamp(
  ib_timestamp: PISC_TIMESTAMP; tm_date: PCTimeStructure);
begin
  ZPlainFirebird15.isc_decode_timestamp(ib_timestamp, tm_date);
end;

function TZFirebird15PlainDriver.isc_detach_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_detach_database(status_vector, db_handle);
end;

function TZFirebird15PlainDriver.isc_drop_database(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_drop_database(status_vector, db_handle);
end;

function TZFirebird15PlainDriver.isc_dsql_alloc_statement2(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_alloc_statement2(status_vector,
    db_handle, stmt_handle);
end;

function TZFirebird15PlainDriver.isc_dsql_allocate_statement(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_allocate_statement(status_vector,
    db_handle, stmt_handle);
end;

function TZFirebird15PlainDriver.isc_dsql_describe(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_describe(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZFirebird15PlainDriver.isc_dsql_describe_bind(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_describe_bind(status_vector, stmt_handle,
    dialect, xsqlda);
end;

function TZFirebird15PlainDriver.isc_dsql_execute(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_execute(status_vector, tran_handle,
    stmt_handle, dialect, xsqlda);
end;

function TZFirebird15PlainDriver.isc_dsql_execute_immediate(
  status_vector: PISC_STATUS; db_handle: PISC_DB_HANDLE;
  tran_handle: PISC_TR_HANDLE; length: Word; statement: PChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_execute_immediate(status_vector,
    db_handle, tran_handle, length, statement, dialect, xsqlda);
end;

function TZFirebird15PlainDriver.isc_dsql_execute2(status_vector: PISC_STATUS;
  tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
  dialect: Word; in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_execute2(status_vector, tran_handle,
    stmt_handle, dialect, in_xsqlda, out_xsqlda);
end;

function TZFirebird15PlainDriver.isc_dsql_fetch(status_vector: PISC_STATUS;
  stmt_handle: PISC_STMT_HANDLE; dialect: Word;
  xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_fetch(status_vector, stmt_handle, dialect,
    xsqlda);
end;

function TZFirebird15PlainDriver.isc_dsql_free_statement(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  options: Word): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_free_statement(status_vector, stmt_handle,
    options);
end;

function TZFirebird15PlainDriver.isc_dsql_prepare(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  stmt_handle: PISC_STMT_HANDLE; length: Word; statement: PChar;
  dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_prepare(status_vector, tran_handle,
    stmt_handle, length, statement, dialect, xsqlda);
end;

function TZFirebird15PlainDriver.isc_dsql_set_cursor_name(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  cursor_name: PChar; _type: Word): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_set_cursor_name(status_vector,
    stmt_handle, cursor_name, _type);
end;

function TZFirebird15PlainDriver.isc_dsql_sql_info(
  status_vector: PISC_STATUS; stmt_handle: PISC_STMT_HANDLE;
  item_length: Short; items: PChar; buffer_length: Short;
  buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_dsql_sql_info(status_vector, stmt_handle,
    item_length, items, buffer_length, buffer);
end;

procedure TZFirebird15PlainDriver.isc_encode_date(tm_date: PCTimeStructure;
  ib_date: PISC_QUAD);
begin
  ZPlainFirebird15.isc_encode_date(tm_date, ib_date);
end;

procedure TZFirebird15PlainDriver.isc_encode_sql_date(
  tm_date: PCTimeStructure; ib_date: PISC_DATE);
begin
  ZPlainFirebird15.isc_encode_sql_date(tm_date, ib_date);
end;

procedure TZFirebird15PlainDriver.isc_encode_sql_time(
  tm_date: PCTimeStructure; ib_time: PISC_TIME);
begin
  ZPlainFirebird15.isc_encode_sql_time(tm_date, ib_time);
end;

procedure TZFirebird15PlainDriver.isc_encode_timestamp(
  tm_date: PCTimeStructure; ib_timestamp: PISC_TIMESTAMP);
begin
  ZPlainFirebird15.isc_encode_timestamp(tm_date, ib_timestamp);
end;

function TZFirebird15PlainDriver.isc_event_block(event_buffer,
  result_buffer: PPChar; id_count: Word;
  event_list: array of PChar): ISC_LONG;
begin
  Result := ZPlainFirebird15.isc_event_block(event_buffer, result_buffer,
    id_count, event_list);
end;

procedure TZFirebird15PlainDriver.isc_event_counts(
  status_vector: PISC_STATUS; buffer_length: Short; event_buffer,
  result_buffer: PChar);
begin
  ZPlainFirebird15.isc_event_counts(status_vector, buffer_length,
    event_buffer, result_buffer);
end;

function TZFirebird15PlainDriver.isc_free(isc_arg1: PChar): ISC_LONG;
begin
  Result := ZPlainFirebird15.isc_free(isc_arg1);
end;

function TZFirebird15PlainDriver.isc_get_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  actual_seg_length: PWord; seg_buffer_length: Word;
  seg_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_get_segment(status_vector, blob_handle,
    actual_seg_length, seg_buffer_length, seg_buffer);
end;

function TZFirebird15PlainDriver.isc_interprete(buffer: PChar;
  status_vector: PPISC_STATUS): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_interprete(buffer, status_vector);
end;

function TZFirebird15PlainDriver.isc_open_blob2(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
  blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
  bpb_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_open_blob2(status_vector, db_handle,
    tran_handle, blob_handle, blob_id, bpb_length, bpb_buffer);
end;

function TZFirebird15PlainDriver.isc_put_segment(
  status_vector: PISC_STATUS; blob_handle: PISC_BLOB_HANDLE;
  seg_buffer_len: Word; seg_buffer: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_put_segment(status_vector, blob_handle,
    seg_buffer_len, seg_buffer);
end;

function TZFirebird15PlainDriver.isc_que_events(status_vector: PISC_STATUS;
  db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
  event_buffer: PChar; event_function: TISC_CALLBACK;
  event_function_arg: PVoid): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_que_events(status_vector, db_handle,
    event_id, length, event_buffer, event_function, event_function_arg)
end;

function TZFirebird15PlainDriver.isc_rollback_retaining(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_rollback_retaining(status_vector, tran_handle);
end;

procedure TZFirebird15PlainDriver.isc_sql_interprete(sqlcode: Short;
  buffer: PChar; buffer_length: Short);
begin
  ZPlainFirebird15.isc_sql_interprete(sqlcode, buffer, buffer_length);
end;

function TZFirebird15PlainDriver.isc_sqlcode(
  status_vector: PISC_STATUS): ISC_LONG;
begin
  Result := ZPlainFirebird15.isc_sqlcode(status_vector);
end;

function TZFirebird15PlainDriver.isc_rollback_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_rollback_transaction(status_vector, tran_handle);
end;

function TZFirebird15PlainDriver.isc_start_multiple(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; teb_vector_address: PISC_TEB): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_start_multiple(status_vector, tran_handle,
    db_handle_count, teb_vector_address);
end;

function TZFirebird15PlainDriver.isc_start_transaction(
  status_vector: PISC_STATUS; tran_handle: PISC_TR_HANDLE;
  db_handle_count: Short; db_handle: PISC_DB_HANDLE; tpb_length: Word;
  tpb_address: PChar): ISC_STATUS;
begin
  Result := ZPlainFirebird15.isc_start_transaction(status_vector, tran_handle,
    db_handle_count, db_handle, tpb_length, tpb_address);
end;

function TZFirebird15PlainDriver.isc_vax_integer(buffer: PChar;
  length: Short): ISC_LONG;
begin
  Result := ZPlainFirebird15.isc_vax_integer(buffer, length);
end;

end.

