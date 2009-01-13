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

{$IFDEF MYSQL_5_API}
  {$DEFINE MYSQL_5_UP}
  {$DEFINE MYSQL_41_UP}
  {$DEFINE MYSQL_40_UP}
  {$DEFINE MYSQL_323_UP}
  {$DEFINE MYSQL_320_UP}
  {$DEFINE MYSQL_5_DOWN}
{$ENDIF}
{$IFDEF MYSQL_41_API}
  {$DEFINE MYSQL_41_UP}
  {$DEFINE MYSQL_40_UP}
  {$DEFINE MYSQL_323_UP}
  {$DEFINE MYSQL_320_UP}
  {$DEFINE MYSQL_41_DOWN}
  {$DEFINE MYSQL_5_DOWN}
{$ENDIF}
{$IFDEF MYSQL_40_API}
  {$DEFINE MYSQL_40_UP}
  {$DEFINE MYSQL_323_UP}
  {$DEFINE MYSQL_320_UP}
  {$DEFINE MYSQL_40_DOWN}
  {$DEFINE MYSQL_41_DOWN}
  {$DEFINE MYSQL_5_DOWN}
{$ENDIF}
{$IFDEF MYSQL_323_API}
  {$DEFINE MYSQL_323_UP}
  {$DEFINE MYSQL_320_UP}
  {$DEFINE MYSQL_323_DOWN}
  {$DEFINE MYSQL_40_DOWN}
  {$DEFINE MYSQL_41_DOWN}
  {$DEFINE MYSQL_5_DOWN}
{$ENDIF}
{$IFDEF MYSQL_320_API}
  {$DEFINE MYSQL_320_UP}
  {$DEFINE MYSQL_320_DOWN}
  {$DEFINE MYSQL_323_DOWN}
  {$DEFINE MYSQL_40_DOWN}
  {$DEFINE MYSQL_41_DOWN}
  {$DEFINE MYSQL_5_DOWN}
{$ENDIF}

{ ************** Load adresses of API Functions ************* }
  @api_rec.mysql_affected_rows          := GetAddress('mysql_affected_rows');
  @api_rec.mysql_character_set_name     := GetAddress('mysql_character_set_name');
  @api_rec.mysql_close                  := GetAddress('mysql_close');
  @api_rec.mysql_connect                := GetAddress('mysql_connect');
  @api_rec.mysql_create_db              := GetAddress('mysql_create_db');
  @api_rec.mysql_data_seek              := GetAddress('mysql_data_seek');
  @api_rec.mysql_debug                  := GetAddress('mysql_debug');
  @api_rec.mysql_drop_db                := GetAddress('mysql_drop_db');
  @api_rec.mysql_dump_debug_info        := GetAddress('mysql_dump_debug_info');
  @api_rec.mysql_eof                    := GetAddress('mysql_eof');
  @api_rec.mysql_errno                  := GetAddress('mysql_errno');
  @api_rec.mysql_error                  := GetAddress('mysql_error');
  @api_rec.mysql_escape_string          := GetAddress('mysql_escape_string');
  @api_rec.mysql_fetch_field            := GetAddress('mysql_fetch_field');
  @api_rec.mysql_fetch_field_direct     := GetAddress('mysql_fetch_field_direct');
  @api_rec.mysql_fetch_fields           := GetAddress('mysql_fetch_fields');
  @api_rec.mysql_fetch_lengths          := GetAddress('mysql_fetch_lengths');
  @api_rec.mysql_fetch_row              := GetAddress('mysql_fetch_row');
  @api_rec.mysql_field_seek             := GetAddress('mysql_field_seek');
  @api_rec.mysql_field_tell             := GetAddress('mysql_field_tell');
  @api_rec.mysql_free_result            := GetAddress('mysql_free_result');
  @api_rec.mysql_get_client_info        := GetAddress('mysql_get_client_info');
  @api_rec.mysql_get_host_info          := GetAddress('mysql_get_host_info');
  @api_rec.mysql_get_proto_info         := GetAddress('mysql_get_proto_info');
  @api_rec.mysql_get_server_info        := GetAddress('mysql_get_server_info');
  @api_rec.mysql_info                   := GetAddress('mysql_info');
  @api_rec.mysql_init                   := GetAddress('mysql_init');
  @api_rec.mysql_insert_id              := GetAddress('mysql_insert_id');
  @api_rec.mysql_kill                   := GetAddress('mysql_kill');
  @api_rec.mysql_list_dbs               := GetAddress('mysql_list_dbs');
  @api_rec.mysql_list_fields            := GetAddress('mysql_list_fields');
  @api_rec.mysql_list_processes         := GetAddress('mysql_list_processes');
  @api_rec.mysql_list_tables            := GetAddress('mysql_list_tables');
  @api_rec.mysql_num_fields             := GetAddress('mysql_num_fields');
  @api_rec.mysql_num_rows               := GetAddress('mysql_num_rows');
  @api_rec.mysql_options                := GetAddress('mysql_options');
  @api_rec.mysql_ping                   := GetAddress('mysql_ping');
  @api_rec.mysql_query                  := GetAddress('mysql_query');
  @api_rec.mysql_real_connect           := GetAddress('mysql_real_connect');
  @api_rec.mysql_real_escape_string     := GetAddress('mysql_real_escape_string');
  @api_rec.mysql_real_query             := GetAddress('mysql_real_query');
  @api_rec.mysql_refresh                := GetAddress('mysql_refresh');
  @api_rec.mysql_row_seek               := GetAddress('mysql_row_seek');
  @api_rec.mysql_row_tell               := GetAddress('mysql_row_tell');
  @api_rec.mysql_select_db              := GetAddress('mysql_select_db');
  @api_rec.mysql_shutdown               := GetAddress('mysql_shutdown');
  @api_rec.mysql_ssl_set                := GetAddress('mysql_ssl_set');
  @api_rec.mysql_stat                   := GetAddress('mysql_stat');
  @api_rec.mysql_store_result           := GetAddress('mysql_store_result');
  @api_rec.mysql_thread_id              := GetAddress('mysql_thread_id');
  @api_rec.mysql_use_result             := GetAddress('mysql_use_result');

  @api_rec.my_init                      := GetAddress('my_init');
  @api_rec.mysql_thread_init            := GetAddress('mysql_thread_init');
  @api_rec.mysql_thread_end             := GetAddress('mysql_thread_end');
  @api_rec.mysql_thread_safe            := GetAddress('mysql_thread_safe');

  @api_rec.mysql_server_init            := GetAddress('mysql_server_init');
  @api_rec.mysql_server_end             := GetAddress('mysql_server_end');

  {$IFDEF MYSQL_323_UP}
  @api_rec.mysql_change_user            := GetAddress('mysql_change_user');
  @api_rec.mysql_field_count            := GetAddress('mysql_field_count');
  {$ENDIF}

  {$IFDEF MYSQL_40_UP}
  @api_rec.mysql_get_client_version     := GetAddress('mysql_get_client_version');
  {BELOW are functions not implemented in PDO}
  @api_rec.mysql_send_query      := GetAddress('mysql_send_query');
  @api_rec.mysql_read_query_result := GetAddress('mysql_read_query_result');
  @api_rec.mysql_master_query    := GetAddress('mysql_master_query');
  @api_rec.mysql_master_send_query := GetAddress('mysql_master_send_query');
  @api_rec.mysql_slave_query     := GetAddress('mysql_slave_query');
  @api_rec.mysql_slave_send_query := GetAddress('mysql_slave_send_query');
  @api_rec.mysql_enable_rpl_parse := GetAddress('mysql_enable_rpl_parse');
  @api_rec.mysql_disable_rpl_parse := GetAddress('mysql_disable_rpl_parse');
  @api_rec.mysql_rpl_parse_enabled := GetAddress('mysql_rpl_parse_enabled');
  @api_rec.mysql_enable_reads_from_master := GetAddress('mysql_enable_reads_from_master');
  @api_rec.mysql_disable_reads_from_master := GetAddress('mysql_disable_reads_from_master');
  @api_rec.mysql_reads_from_master_enabled := GetAddress('mysql_reads_from_master_enabled');
  @api_rec.mysql_rpl_query_type  := GetAddress('mysql_rpl_query_type');
  @api_rec.mysql_rpl_probe       := GetAddress('mysql_rpl_probe');
  @api_rec.mysql_set_master      := GetAddress('mysql_set_master');
  @api_rec.mysql_add_slave       := GetAddress('mysql_add_slave');
  @api_rec.mysql_manager_init    := GetAddress('mysql_manager_init');
  @api_rec.mysql_manager_connect := GetAddress('mysql_manager_connect');
  @api_rec.mysql_manager_close   := GetAddress('mysql_manager_close');
  @api_rec.mysql_manager_command := GetAddress('mysql_manager_command');
  @api_rec.mysql_manager_fetch_line := GetAddress('mysql_manager_fetch_line');
  {$ENDIF}

  {$IFDEF MYSQL_41_UP}
  @api_rec.mysql_autocommit             := GetAddress('mysql_autocommit');
  @api_rec.mysql_commit                 := GetAddress('mysql_commit');
  @api_rec.mysql_get_server_version     := GetAddress('mysql_get_server_version');
  @api_rec.mysql_hex_string             := GetAddress('mysql_hex_string');
  @api_rec.mysql_more_results           := GetAddress('mysql_more_results');
  @api_rec.mysql_next_result            := GetAddress('mysql_next_result');
  @api_rec.mysql_rollback               := GetAddress('mysql_rollback');
  @api_rec.mysql_set_character_set      := GetAddress('mysql_set_character_set');
  @api_rec.mysql_set_server_option      := GetAddress('mysql_set_server_option');
  @api_rec.mysql_sqlstate               := GetAddress('mysql_sqlstate');
  @api_rec.mysql_warning_count          := GetAddress('mysql_warning_count');
  {API for PREPARED STATEMENTS}
  @api_rec.mysql_stmt_affected_rows     := GetAddress('mysql_stmt_affected_rows');
  @api_rec.mysql_stmt_attr_get          := GetAddress('mysql_stmt_attr_get');
  @api_rec.mysql_stmt_attr_set          := GetAddress('mysql_stmt_attr_set');
  @api_rec.mysql_stmt_bind_param        := GetAddress('mysql_stmt_bind_param');
  @api_rec.mysql_stmt_bind_result       := GetAddress('mysql_stmt_bind_result');
  @api_rec.mysql_stmt_close             := GetAddress('mysql_stmt_close');
  @api_rec.mysql_stmt_data_seek         := GetAddress('mysql_stmt_data_seek');
  @api_rec.mysql_stmt_errno             := GetAddress('mysql_stmt_errno');
  @api_rec.mysql_stmt_error             := GetAddress('mysql_stmt_error');
  @api_rec.mysql_stmt_execute           := GetAddress('mysql_stmt_execute');
  @api_rec.mysql_stmt_fetch             := GetAddress('mysql_stmt_fetch');
  @api_rec.mysql_stmt_fetch_column      := GetAddress('mysql_stmt_fetch_column');
  @api_rec.mysql_stmt_field_count       := GetAddress('mysql_stmt_field_count');
  @api_rec.mysql_stmt_free_result       := GetAddress('mysql_stmt_free_result');
  @api_rec.mysql_stmt_init              := GetAddress('mysql_stmt_init');
  @api_rec.mysql_stmt_insert_id         := GetAddress('mysql_stmt_insert_id');
  @api_rec.mysql_stmt_num_rows          := GetAddress('mysql_stmt_num_rows');
  @api_rec.mysql_stmt_param_count       := GetAddress('mysql_stmt_param_count');
  @api_rec.mysql_stmt_param_metadata    := GetAddress('mysql_stmt_param_metadata');
  @api_rec.mysql_stmt_prepare           := GetAddress('mysql_stmt_prepare');
  @api_rec.mysql_stmt_reset             := GetAddress('mysql_stmt_reset');
  @api_rec.mysql_stmt_result_metadata   := GetAddress('mysql_stmt_result_metadata');
  @api_rec.mysql_stmt_row_seek          := GetAddress('mysql_stmt_row_seek');
  @api_rec.mysql_stmt_row_tell          := GetAddress('mysql_stmt_row_tell');
  @api_rec.mysql_stmt_send_long_data    := GetAddress('mysql_stmt_send_long_data');
  @api_rec.mysql_stmt_sqlstate          := GetAddress('mysql_stmt_sqlstate');
  @api_rec.mysql_stmt_store_result      := GetAddress('mysql_stmt_store_result');
  {$ENDIF}

  {$IFDEF MYSQL_5_UP}
  @api_rec.mysql_get_character_set_info := GetAddress('mysql_get_character_set_info');
  {$ENDIF}

{****************  UNDEFINE EVERYTHING SET IN THIS INCLUDE *************}
  {$UNDEF MYSQL_5_UP}
  {$UNDEF MYSQL_5_DOWN}
  {$UNDEF MYSQL_41_UP}
  {$UNDEF MYSQL_41_DOWN}
  {$UNDEF MYSQL_40_UP}
  {$UNDEF MYSQL_40_DOWN}
  {$UNDEF MYSQL_323_UP}
  {$UNDEF MYSQL_323_DOWN}
  {$UNDEF MYSQL_320_UP}
  {$UNDEF MYSQL_320_DOWN}

