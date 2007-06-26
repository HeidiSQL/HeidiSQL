{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Delphi interface to gds32.dll               }
{                     from Firebird                       }
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

unit ZPlainFirebird15;

interface

{$I ZPlain.inc}

{$J+}

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  ZCompatibility, ZPlainLoader, ZPlainFirebirdInterbaseConstants,
  ZPlainFirebirdDriver;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS1_DLL_LOCATION   = 'fbclient15.dll';
  WINDOWS1_DLL_LOCATION_EMBEDDED = 'fbclientd15.dll';
  LINUX1_DLL_LOCATION   = 'libfbclient.so.15';
  LINUX1_IB_CRYPT_LOCATION = 'libcrypt.so.15';
  LINUX1_DLL_LOCATION_EMBEDDED = 'libfbembed.so.15';

type

{ ************** Plain API Function types definition ************* }

  { General database routines }

  Tisc_attach_database = function(status_vector: PISC_STATUS;
    db_name_length: Short; db_name: PChar; db_handle: PISC_DB_HANDLE;
    parm_buffer_length: Short; parm_buffer: PChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_detach_database = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_drop_database = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_database_info = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PChar; result_buffer_length: Short;
    result_buffer: PChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Array processing routines }
  Tisc_array_gen_sdl = function(status_vector: PISC_STATUS;
    isc_array_desc: PISC_ARRAY_DESC; isc_arg3: PShort;
    isc_arg4: PChar; isc_arg5: PShort): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_get_slice = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
    dest_array: PVoid; slice_length: ISC_LONG): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_lookup_bounds = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    table_name, column_name: PChar;
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_lookup_desc = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    table_name, column_name: PChar;
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_set_desc = function(status_vector: PISC_STATUS;
    table_name: PChar; column_name: PChar;
    sql_dtype, sql_length, sql_dimensions: PShort;
    descriptor: PISC_ARRAY_DESC): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_array_put_slice = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; trans_handle: PISC_TR_HANDLE;
    array_id: PISC_QUAD; descriptor: PISC_ARRAY_DESC;
    source_array: PVoid; slice_length: PISC_LONG): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_free = function(isc_arg1: PChar): ISC_LONG;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_sqlcode = function(status_vector: PISC_STATUS): ISC_LONG;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_sql_interprete = procedure(sqlcode: Short; buffer: PChar;
    buffer_length: Short); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_interprete = function(buffer: PChar; status_vector: PPISC_STATUS):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Transaction support routines }

  Tisc_start_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
    db_handle: PISC_DB_HANDLE; tpb_length: Word; tpb_address: PChar):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_start_multiple = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; db_handle_count: Short;
    teb_vector_address: PISC_TEB): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_rollback_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_rollback_retaining = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_commit_retaining = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_commit_transaction = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Dynamic SQL routines }

  Tisc_dsql_allocate_statement = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_alloc_statement2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; stmt_handle: PISC_STMT_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_describe = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_describe_bind = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
    xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute2 = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE; dialect: Word;
    in_xsqlda, out_xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_execute_immediate = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE; length: Word;
    statement: PChar; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_fetch = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; dialect: Word; xsqlda: PXSQLDA): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_free_statement = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; options: Word): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_prepare = function(status_vector: PISC_STATUS;
    tran_handle: PISC_TR_HANDLE; stmt_handle: PISC_STMT_HANDLE;
    length: Word; statement: PChar; dialect: Word; xsqlda: PXSQLDA):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_set_cursor_name = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; cursor_name: PChar; _type: Word): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_dsql_sql_info = function(status_vector: PISC_STATUS;
    stmt_handle: PISC_STMT_HANDLE; item_length: Short; items: PChar;
    buffer_length: Short; buffer: PChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Blob processing routines }

  Tisc_open_blob2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
    blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
    bpb_buffer: PChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_create_blob2 = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; tran_handle: PISC_TR_HANDLE;
    blob_handle: PISC_BLOB_HANDLE; blob_id: PISC_QUAD; bpb_length: Short;
    bpb_address: PChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_blob_info = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; item_list_buffer_length: Short;
    item_list_buffer: PChar; result_buffer_length: Short; result_buffer: PChar):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_close_blob = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_cancel_blob = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_get_segment = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; actual_seg_length: PWord;
    seg_buffer_length: Word; seg_buffer: PChar): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_put_segment = function(status_vector: PISC_STATUS;
    blob_handle: PISC_BLOB_HANDLE; seg_buffer_len: Word; seg_buffer: PChar):
    ISC_STATUS; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Event processing routines }

  Tisc_event_block = function(event_buffer: PPChar; result_buffer: PPChar;
    id_count: Word; event_list: array of PChar): ISC_LONG;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_event_counts = procedure(status_vector: PISC_STATUS;
    buffer_length: Short; event_buffer: PChar; result_buffer: PChar);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_cancel_events = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; event_id: PISC_LONG): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_que_events = function(status_vector: PISC_STATUS;
    db_handle: PISC_DB_HANDLE; event_id: PISC_LONG; length: Short;
    event_buffer: PChar; event_function: TISC_CALLBACK;
    event_function_arg: PVoid): ISC_STATUS;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Types convertion routines }

  Tisc_decode_date = procedure(ib_date: PISC_QUAD; tm_date: PCTimeStructure);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_date = procedure(tm_date: PCTimeStructure; ib_date: PISC_QUAD);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Interbase Version 6 routines }
  Tisc_decode_sql_date = procedure(ib_date: PISC_DATE;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_decode_sql_time = procedure(ib_time: PISC_TIME;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_decode_timestamp = procedure(ib_timestamp: PISC_TIMESTAMP;
    tm_date: PCTimeStructure); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_sql_date = procedure(tm_date: PCTimeStructure;
    ib_date: PISC_DATE); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_sql_time = procedure(tm_date: PCTimeStructure;
    ib_time: PISC_TIME); {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_encode_timestamp = procedure(tm_date: PCTimeStructure;
    ib_timestamp: PISC_TIMESTAMP);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tisc_vax_integer = function(buffer: PChar; length: Short): ISC_LONG;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

{ ************* Plain API Function variables definition ************ }

var
  { General database routines }
  isc_attach_database:  Tisc_attach_database;
  isc_detach_database:  Tisc_detach_database;
  isc_drop_database:    Tisc_drop_database;
  isc_database_info:    Tisc_database_info;
  isc_free:             Tisc_free;
  isc_sqlcode:          Tisc_sqlcode;
  isc_sql_interprete:   Tisc_sql_interprete;
  isc_interprete:       Tisc_interprete;

  { Transaction support routines }
  isc_start_transaction: Tisc_start_transaction;
  isc_start_multiple:   Tisc_start_multiple;
  isc_rollback_transaction: Tisc_rollback_transaction;
  isc_rollback_retaining: Tisc_rollback_retaining;
  isc_commit_transaction: Tisc_commit_transaction;
  isc_commit_retaining: Tisc_commit_retaining;

  { Dynamic SQL routines }
  isc_dsql_allocate_statement: Tisc_dsql_allocate_statement;
  isc_dsql_alloc_statement2: Tisc_dsql_alloc_statement2;
  isc_dsql_describe:    Tisc_dsql_describe;
  isc_dsql_describe_bind: Tisc_dsql_describe_bind;
  isc_dsql_execute:     Tisc_dsql_execute;
  isc_dsql_execute2:    Tisc_dsql_execute2;
  isc_dsql_execute_immediate: Tisc_dsql_execute_immediate;
  isc_dsql_fetch:       Tisc_dsql_fetch;
  isc_dsql_free_statement: Tisc_dsql_free_statement;
  isc_dsql_prepare:     Tisc_dsql_prepare;
  isc_dsql_set_cursor_name: Tisc_dsql_set_cursor_name;
  isc_dsql_sql_info:    Tisc_dsql_sql_info;

  { Array processing routines }
  isc_array_gen_sdl:    Tisc_array_gen_sdl;
  isc_array_get_slice:  Tisc_array_get_slice;
  isc_array_lookup_bounds: Tisc_array_lookup_bounds;
  isc_array_lookup_desc: Tisc_array_lookup_desc;
  isc_array_set_desc:   Tisc_array_set_desc;
  isc_array_put_slice:  Tisc_array_put_slice;

  { Blob processing routines }
  isc_open_blob2:       Tisc_open_blob2;
  isc_create_blob2:     Tisc_create_blob2;
  isc_blob_info:        Tisc_blob_info;
  isc_close_blob:       Tisc_close_blob;
  isc_cancel_blob:      Tisc_cancel_blob;
  isc_get_segment:      Tisc_get_segment;
  isc_put_segment:      Tisc_put_segment;

  { Event processing routines }
  isc_que_events:       Tisc_que_events;
  isc_event_counts:     Tisc_event_counts;
  isc_event_block:      Tisc_event_block;
  isc_cancel_events:    Tisc_cancel_events;

  { Types convertion routines }
  isc_encode_date:      Tisc_encode_date;
  isc_decode_date:      Tisc_decode_date;
  isc_vax_integer:      Tisc_vax_integer;

  isc_encode_sql_date:  Tisc_encode_sql_date;
  isc_decode_sql_date:  Tisc_decode_sql_date;

  isc_encode_sql_time:  Tisc_encode_sql_time;
  isc_decode_sql_time:  Tisc_decode_sql_time;

  isc_encode_timestamp: Tisc_encode_timestamp;
  isc_decode_timestamp: Tisc_decode_timestamp;

var
  LibraryLoader: TZNativeLibraryLoader;
  LibraryLoaderEmbedded: TZNativeLibraryLoader;

implementation

uses SysUtils, ZMessages,
{$IFNDEF UNIX}
  Windows;
{$ELSE}
  libc;
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF INTERBASE_CRYPT}
{$DEFINE ENABLE_INTERBASE_CRYPT}
{$ENDIF}
{$ENDIF}

type
  {** Implements a loader for Firebird native library. }
  TZFirebirdNativeLibraryLoader = class (TZNativeLibraryLoader)
{$IFDEF ENABLE_INTERBASE_CRYPT}
  private
    FPreLocations: TStringDynArray;
    FPreHandle: LongWord;
    FPreLoaded: Boolean;
    FPreRequared: boolean;
  protected
    function LoadNativeLibrary: Boolean; override;
    procedure FreeNativeLibrary; override;

  public
    constructor Create(PreLocations, Locations: array of string;
      PreRequared: boolean = False); overload;

    property PreHandle: LongWord read FPreHandle write FPreHandle;
    property PreLoaded: Boolean read FPreLoaded write FPreLoaded;
{$ENDIF}
  public
    function Load: Boolean; override;
  end;

{ TZFirebirdNativeLibraryLoader }

{**
  Creates this loader class and assignes main properties.
  @param Locations locations of native library on windows platform.
}
{$IFDEF ENABLE_INTERBASE_CRYPT}
constructor TZFirebirdNativeLibraryLoader.Create(PreLocations,
  Locations: array of string; PreRequared: boolean);
var
  I: Integer;
begin
  inherited Create(Locations);
  SetLength(FPreLocations, Length(PreLocations));
  for I := 0 to High(PreLocations) do
    FPreLocations[I] := PreLocations[I];
  FPreRequared := PreRequared;
end;
{$ENDIF}

{**
  Frees a previously loaded library.
}
{$IFDEF ENABLE_INTERBASE_CRYPT}
procedure TZFirebirdNativeLibraryLoader.FreeNativeLibrary;
begin
  inherited FreeNativeLibrary;
  if (PreHandle <> 0) and Loaded then
    FreeLibrary(PreHandle);
  PreHandle := 0;
  PreLoaded := False;
end;
{$ENDIF}

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZFirebirdNativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

  isc_sqlcode         := GetAddress('isc_sqlcode');
  isc_sql_interprete  := GetAddress('isc_sql_interprete');
  isc_interprete      := GetAddress('isc_interprete');
  isc_vax_integer     := GetAddress('isc_vax_integer');

  isc_array_gen_sdl   := GetAddress( 'isc_array_gen_sdl');
  isc_array_get_slice := GetAddress( 'isc_array_get_slice');
  isc_array_lookup_bounds := GetAddress( 'isc_array_lookup_bounds');
  isc_array_lookup_desc := GetAddress( 'isc_array_lookup_desc');
  isc_array_set_desc  := GetAddress( 'isc_array_set_desc');
  isc_array_put_slice := GetAddress( 'isc_array_put_slice');

  isc_blob_info       := GetAddress('isc_blob_info');
  isc_open_blob2      := GetAddress('isc_open_blob2');
  isc_close_blob      := GetAddress('isc_close_blob');
  isc_cancel_blob     := GetAddress('isc_cancel_blob');
  isc_get_segment     := GetAddress('isc_get_segment');
  isc_put_segment     := GetAddress('isc_put_segment');
  isc_create_blob2    := GetAddress('isc_create_blob2');
  isc_decode_date     := GetAddress('isc_decode_date');
  isc_encode_date     := GetAddress('isc_encode_date');
  isc_dsql_free_statement := GetAddress('isc_dsql_free_statement');
  isc_dsql_execute2   := GetAddress('isc_dsql_execute2');
  isc_dsql_execute    := GetAddress('isc_dsql_execute');
  isc_dsql_set_cursor_name := GetAddress('isc_dsql_set_cursor_name');
  isc_dsql_fetch      := GetAddress('isc_dsql_fetch');
  isc_dsql_sql_info   := GetAddress('isc_dsql_sql_info');
  isc_dsql_allocate_statement := GetAddress('isc_dsql_allocate_statement');
  isc_dsql_alloc_statement2 := GetAddress('isc_dsql_alloc_statement2');
  isc_dsql_prepare    := GetAddress('isc_dsql_prepare');
  isc_dsql_describe_bind := GetAddress('isc_dsql_describe_bind');
  isc_dsql_describe   := GetAddress('isc_dsql_describe');
  isc_dsql_execute_immediate := GetAddress('isc_dsql_execute_immediate');
  isc_drop_database   := GetAddress('isc_drop_database');
  isc_detach_database := GetAddress('isc_detach_database');
  isc_attach_database := GetAddress('isc_attach_database');
  isc_database_info   := GetAddress('isc_database_info');
  isc_start_multiple  := GetAddress('isc_start_multiple');
  isc_start_transaction := GetAddress('isc_start_transaction');
  isc_commit_transaction := GetAddress('isc_commit_transaction');

  isc_commit_retaining := GetAddress('isc_commit_retaining');
  isc_rollback_transaction := GetAddress('isc_rollback_transaction');
  isc_cancel_events   := GetAddress('isc_cancel_events');
  isc_que_events      := GetAddress('isc_que_events');
  isc_event_counts    := GetAddress('isc_event_counts');
  isc_event_block     := GetAddress('isc_event_block');
  isc_free            := GetAddress('isc_free');

  isc_rollback_retaining := GetAddress( 'isc_rollback_retaining');
  isc_decode_sql_date := GetAddress('isc_decode_sql_date');
  isc_decode_sql_time := GetAddress('isc_decode_sql_time');
  isc_decode_timestamp := GetAddress('isc_decode_timestamp');
  isc_encode_sql_date := GetAddress('isc_encode_sql_date');
  isc_encode_sql_time := GetAddress('isc_encode_sql_time');
  isc_encode_timestamp := GetAddress('isc_encode_timestamp');
end;

{**
  Loads a library module and initializes the handle.
  @return <code>True</code> is library was successfully loaded.
}
{$IFDEF ENABLE_INTERBASE_CRYPT}
function TZFirebirdNativeLibraryLoader.LoadNativeLibrary: Boolean;
var
  I: Integer;
  Location: string;
  TriedLocations: string;
begin
  if Length(FPreLocations) <> 0 then
  begin
    PreLoaded := False;
    Location := '';
    TriedLocations := '';
    if PreHandle = 0 then
    begin
      for I := 0 to High(FPreLocations) do
      begin
        Location := FPreLocations[I];
//        PreHandle := GetModuleHandle(PChar(Location));
//        if PreHandle = 0 then
//        begin
{$IFDEF UNIX}
  {$IFDEF FPC}
          PreHandle := ZCompatibility.LoadLibrary(PChar(Location));
  {$ELSE}
          PreHandle := HMODULE(dlopen(PChar(Location), RTLD_GLOBAL));
  {$ENDIF}
{$ELSE}
          PreHandle := LoadLibrary(PChar(Location));
{$ENDIF}
//        end;
        if PreHandle <> 0 then
        begin
          PreLoaded := True;
          Break;
        end;
        if TriedLocations <> '' then
          TriedLocations := TriedLocations + ', ';
        TriedLocations := TriedLocations + Location;
      end;
    end;

    if (not PreLoaded) and (FPreRequared) then
      raise Exception.Create(Format(SLibraryNotFound, [TriedLocations]));
  end;

  Result := inherited LoadNativeLibrary;
end;
{$ENDIF}

initialization
{$IFNDEF UNIX}
  LibraryLoader := TZFirebirdNativeLibraryLoader.Create(
    [WINDOWS1_DLL_LOCATION
		{$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
    	,WINDOWS2_DLL_LOCATION
		{$ENDIF}
    ]);
  LibraryLoaderEmbedded := TZFirebirdNativeLibraryLoader.Create(
    [WINDOWS1_DLL_LOCATION_EMBEDDED
		{$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
    	,WINDOWS2_DLL_LOCATION_EMBEDDED
		{$ENDIF}
    ]);

{$ELSE}
  {$IFDEF ENABLE_INTERBASE_CRYPT}
  	LibraryLoader := TZFirebirdNativeLibraryLoader.Create(
  	[LINUX1_IB_CRYPT_LOCATION
		{$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
	  	, LINUX2_IB_CRYPT_LOCATION
		{$ENDIF}
	  ],[LINUX1_DLL_LOCATION

		{$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
	  	,LINUX2_DLL_LOCATION
		{$ENDIF}
	  ]);

 		LibraryLoaderEmbedded := TZFirebirdNativeLibraryLoader.Create(
    [LINUX1_IB_CRYPT_LOCATION
		{$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
    	,LINUX2_IB_CRYPT_LOCATION
		{$ENDIF}
    ], [LINUX1_DLL_LOCATION_EMBEDDED
		{$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
    	,LINUX2_DLL_LOCATION_EMBEDDED
		{$ENDIF}
    ]);
  {$ELSE}
  	LibraryLoader := TZFirebirdNativeLibraryLoader.Create(
    [LINUX1_DLL_LOCATION
		{$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
    	,LINUX2_DLL_LOCATION
		{$ENDIF}
    ]);
  	LibraryLoaderEmbedded := TZFirebirdNativeLibraryLoader.Create(
    [LINUX1_DLL_LOCATION_EMBEDDED
		{$IFNDEF FIREBIRD_STRICT_DLL_LOADING}
    	,LINUX2_DLL_LOCATION_EMBEDDED
		{$ENDIF}
    ]);
 	{$ENDIF}
{$ENDIF}
finalization
  if Assigned(LibraryLoader) then
    LibraryLoader.Free;
  if Assigned(LibraryLoaderEmbedded) then
    LibraryLoaderEmbedded.Free;

end.
