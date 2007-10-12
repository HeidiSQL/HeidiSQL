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

unit ZPlainASA9;

interface

{$I ZPlain.inc}

{$J+}

uses Classes, ZPlainLoader, ZCompatibility, ZPlainASADriver;

{ ***************** Plain API Constants definition **************** }

const
  WINDOWS_DLL_LOCATION = 'DBLIB9.DLL';
  LINUX_DLL_LOCATION = 'libdblib9.so';

{ ****************** Plain API Types definition ***************** }

type
  TASABasic = function( sqlca: PZASASQLCA): Integer;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASABasicWithParam = function( sqlca: PZASASQLCA; Params: PChar): LongWord;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAfind_engine = function( sqlca: PZASASQLCA; Params: PChar): Word;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAAlloc_sqlda = function( NumVar: LongWord): PASASQLDA;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAfill_sqlda = function( Parameter: PASASQLDA): PASASQLDA;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAfill_s_sqlda = function( Parameter: PASASQLDA; MaxLength: Integer): PASASQLDA;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAProc_sqlda = procedure( Parameter: PASASQLDA);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_setConnect = procedure( sqlca: PZASASQLCA; ConnName: PChar);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_describe_cursor = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    Descriptor: PASASQLDA; SomeNumber: LongWord);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_prepare_into = procedure( sqlca: PZASASQLCA; UnKnown: PChar;
    ProgName: PChar; RecordStatementNum: PSmallInt; SqlStatement: PChar;
    Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; SomeNumber: LongWord);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_prepare_describe = procedure( sqlca: PZASASQLCA; UnKnown: PChar;
    ProgName: PChar; RecordStatementNum: PSmallInt; SqlStatement: PChar;
    Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
    LongNames: Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_select = procedure( sqlca: PZASASQLCA; UnKnown: PChar;
    ProgName: PChar; RecordStatementNum: PSmallInt; Descriptor1,
    Descriptor2: PASASQLDA);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_open = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    UnKnown: PChar; ProgName: PChar; RecordStatementNum: PSmallInt;
    Descriptor1: PASASQLDA; BlockSize: SmallInt; IsolationLvl: SmallInt;
    Options : Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_close = procedure( sqlca: PZASASQLCA; CursorName: PChar);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_fetch = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    Offset: Word; RelPositon: LongInt; Descriptor1: PASASQLDA;
    BlockSize: SmallInt; Options: Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_declare = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    UnKnown: PChar; ProgName: PChar; RecordStatementNum: PSmallInt;
    Options: Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_dropstmt = procedure( sqlca: PZASASQLCA; UnKnown: PChar;
    ProgName: PChar; RecordStatementNum: PSmallInt);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_describe = procedure( sqlca: PZASASQLCA; UnKnown: PChar;
    ProgName: PChar; RecordStatementNum: PSmallInt; Descriptor: PASASQLDA;
    WhatToDesc: Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_delete = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    UnKnown1: PChar; UnKnown2: PChar);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_update = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    Descriptor: PASASQLDA);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_execute_imm = procedure( sqlca: PZASASQLCA; SqlRecordStatement: PChar;
    UnKnown1: Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_put_into = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    Descriptor: PASASQLDA; UnKnown1: PASASQLDA);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_put_array = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    Descriptor: PASASQLDA; Into_sqlda: PASASQLDA; Rows: Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_commit = procedure( sqlca: PZASASQLCA; SomeNumber: LongWord);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_rollback = procedure( sqlca: PZASASQLCA; SomeNumber: LongWord);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_execute_into = procedure( sqlca: PZASASQLCA; UnKnown: PChar;
    ProgName: PChar; RecordStatementNum: PSmallInt; Descriptor1: PASASQLDA;
    Descriptor2: PASASQLDA);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_get_data = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    ColumnNumber: Word; Offset: LongInt; Descriptor1: PASASQLDA;
    Unknown: Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_explain = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    SomeNumber1: Word; Descriptor1: PASASQLDA);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASASQLCallback = procedure() {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdb_register_a_callback = procedure( sqlca: PZASASQLCA;
    CBIdx: integer; Proc: TASASQLCallback);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_setoption = procedure( sqlca: PZASASQLCA; Temporary: LongInt;
    User: PChar; Option: PChar; Descriptor: PASASQLDA);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_fetch_array = procedure( sqlca: PZASASQLCA; CursorName: PChar;
    Offset: Word; RelPositon: LongInt; Descriptor1: PASASQLDA;
    BlockSize: SmallInt; Options: Word; ArrayWidth: Word);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAerror_message = function( sqlca: PZASASQLCA; Buffer: PChar;
    MaxSize: Integer): PChar;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdbpp_resume = procedure( sqlca: PZASASQLCA; CursorName: PChar);
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

  TASAdb_cancel_request = function( sqlca: PZASASQLCA): Integer;
    {$IFNDEF LINUX} stdcall {$ELSE} cdecl {$ENDIF};

{ ************* Plain API Function variables definition ************ }

var
  sqlerror_message:       TASAerror_message;
  db_init:                TASABasic;
  db_fini:                TASABasic;
  db_string_connect:      TASABasicWithParam;
  db_string_disconnect:   TASABasicWithParam;
  db_find_engine:         TASABasicWithParam;
  db_start_engine:        TASABasicWithParam;
  db_stop_engine:         TASABasicWithParam;
  db_start_database:      TASABasicWithParam;
  db_stop_database:       TASABasicWithParam;
  alloc_sqlda:            TASAAlloc_sqlda;
  free_sqlda:             TASAProc_sqlda;
  free_sqlda_noind:       TASAProc_sqlda;
  fill_sqlda:             TASAfill_sqlda;
  free_filled_sqlda:      TASAProc_sqlda;
  fill_s_sqlda:           TASAfill_s_sqlda;
  dbpp_setConnect:        TASAdbpp_setConnect;
  dbpp_describe_cursor:   TASAdbpp_describe_cursor;
  dbpp_disconnect:        TASAdbpp_setConnect;
  dbpp_prepare_into:      TASAdbpp_prepare_into;
  dbpp_prepare_describe:  TASAdbpp_prepare_describe;
  dbpp_select:            TASAdbpp_select;
  dbpp_open:              TASAdbpp_open;
  dbpp_close:             TASAdbpp_close;
  dbpp_fetch:             TASAdbpp_fetch;
  dbpp_declare:           TASAdbpp_declare;
  dbpp_dropstmt:          TASAdbpp_dropstmt;
  dbpp_describe:          TASAdbpp_describe;
  dbpp_delete:            TASAdbpp_delete;
  dbpp_update:            TASAdbpp_update;
  dbpp_execute_imm:       TASAdbpp_execute_imm;
  dbpp_put_into:          TASAdbpp_put_into;
  dbpp_put_array:         TASAdbpp_put_array;
  dbpp_commit:            TASAdbpp_commit;
  dbpp_rollback:          TASAdbpp_rollback;
  dbpp_execute_into:      TASAdbpp_execute_into;
  dbpp_get_data:          TASAdbpp_get_data;
  dbpp_explain:           TASAdbpp_explain;
  db_register_a_callback: TASAdb_register_a_callback;
  dbpp_setoption:         TASAdbpp_setoption;
  dbpp_fetch_array:       TASAdbpp_fetch_array;
  dbpp_resume:            TASAdbpp_resume;
  db_cancel_request:      TASAdb_cancel_request;

var
  LibraryLoader: TZNativeLibraryLoader;

implementation

type
  {** Implements a loader for ASA native library. }
  TZASANativeLibraryLoader = class (TZNativeLibraryLoader)
  public
    function Load: Boolean; override;
  end;

{ TZASANativeLibraryLoader }

{**
  Loads a library module.
  @return <code>True</code> if library was successfully loaded.
}
function TZASANativeLibraryLoader.Load: Boolean;
begin
  Result := inherited Load;

  @sqlerror_message       := GetAddress('sqlerror_message');
  @db_init                := GetAddress('db_init');
  @db_fini                := GetAddress('db_fini');
  @db_string_connect      := GetAddress('db_string_connect');
  @db_string_disconnect   := GetAddress('db_string_disconnect');
  @db_find_engine         := GetAddress('db_find_engine');
  @db_start_engine        := GetAddress('db_start_engine');
  @db_stop_engine         := GetAddress('db_stop_engine');
  @db_start_database      := GetAddress('db_start_database');
  @db_stop_database       := GetAddress('db_stop_database');
  @alloc_sqlda            := GetAddress('alloc_sqlda');
  @fill_sqlda             := GetAddress('fill_sqlda');
  @fill_s_sqlda           := GetAddress('fill_s_sqlda');
  @free_filled_sqlda      := GetAddress('free_filled_sqlda');
  @free_sqlda             := GetAddress('free_sqlda');
  @free_sqlda_noind       := GetAddress('free_sqlda_noind');
  @dbpp_setConnect        := GetAddress('dbpp_setconnect');
  @dbpp_disconnect        := GetAddress('dbpp_disconnect');
  @dbpp_prepare_into      := GetAddress('dbpp_prepare_into');
  @dbpp_describe_cursor   := GetAddress('dbpp_describe_cursor');
  @dbpp_prepare_describe  := GetAddress('dbpp_prepare_describe');
  @dbpp_select            := GetAddress('dbpp_select');
  @dbpp_open              := GetAddress('dbpp_open');
  @dbpp_close             := GetAddress('dbpp_close');
  @dbpp_fetch             := GetAddress('dbpp_fetch');
  @dbpp_declare           := GetAddress('dbpp_declare');
  @dbpp_dropstmt          := GetAddress('dbpp_dropstmt');
  @dbpp_describe          := GetAddress('dbpp_describe');
  @dbpp_delete            := GetAddress('dbpp_delete');
  @dbpp_update            := GetAddress('dbpp_update');
  @dbpp_put_into          := GetAddress('dbpp_put_into');
  @dbpp_put_array         := GetAddress('dbpp_put_array');
  @dbpp_execute_imm       := GetAddress('dbpp_execute_imm');
  @dbpp_commit            := GetAddress('dbpp_commit');
  @dbpp_rollback          := GetAddress('dbpp_rollback');
  @dbpp_execute_into      := GetAddress('dbpp_execute_into');
  @dbpp_get_data          := GetAddress('dbpp_get_data');
  @dbpp_explain           := GetAddress('dbpp_explain');
  @dbpp_setoption         := GetAddress('dbpp_setoption');
  @dbpp_fetch_array       := GetAddress('dbpp_fetch_array');
  @db_register_a_callback := GetAddress('db_register_a_callback');
  @dbpp_resume            := GetAddress('dbpp_resume');
  @db_cancel_request      := GetAddress('db_cancel_request');      
end;

initialization
  LibraryLoader := nil;
{$IFNDEF LINUX}
  LibraryLoader := TZASANativeLibraryLoader.Create(
    [WINDOWS_DLL_LOCATION]);
{$ELSE}
  LibraryLoader := TZASANativeLibraryLoader.Create(
    [LINUX_DLL_LOCATION]);
{$ENDIF}

finalization
  if Assigned( LibraryLoader) then
    LibraryLoader.Free;
end.

