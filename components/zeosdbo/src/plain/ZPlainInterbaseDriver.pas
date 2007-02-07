{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for Interbase            }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainInterbaseDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZCompatibility, ZPlainDriver,ZPlainFirebirdInterbaseConstants;

type
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

  function XSQLDA_LENGTH(Value: LongInt): LongInt;

implementation

uses SysUtils, ZPlainInterbase5, ZPlainInterbase6;

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



end.

