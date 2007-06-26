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

{ ************* Plain API Function variables definition ************ }
  mysql_affected_rows:          Tmysql_affected_rows;	          {mysql 3.2}
  mysql_character_set_name:     Tmysql_character_set_name;      {mysql 3.2}
  mysql_close:                  Tmysql_close;                   {mysql 3.2}
  mysql_connect:                Tmysql_connect;                 {mysql 3.2} {deprecated for mysql_real_connect}
  mysql_create_db:              Tmysql_create_db;               {mysql 3.2} {deprecated for mysql_query}
  mysql_data_seek:              Tmysql_data_seek;               {mysql 3.2}
  mysql_debug:                  Tmysql_debug;                   {mysql 3.2}
  mysql_drop_db:                Tmysql_drop_db;                 {mysql 3.2} {deprecated for mysql_query}
  mysql_dump_debug_info:        Tmysql_dump_debug_info;         {mysql 3.2}
  mysql_eof:                    Tmysql_eof;                     {mysql 3.2} {deprecated for mysql_error/mysql_errno}
  mysql_errno:                  Tmysql_errno;	                {mysql 3.2}
  mysql_error:                  Tmysql_error;                   {mysql 3.2}
  mysql_escape_string:          Tmysql_escape_string;           {mysql 3.2} {deprecated for mysql_real_escape_string}
  mysql_fetch_field:            Tmysql_fetch_field;             {mysql 3.2}
  mysql_fetch_field_direct:     Tmysql_fetch_field_direct;      {mysql 3.2}
  mysql_fetch_fields:           Tmysql_fetch_fields;            {mysql 3.2}
  mysql_fetch_lengths:          Tmysql_fetch_lengths;           {mysql 3.2}
  mysql_fetch_row:              Tmysql_fetch_row;               {mysql 3.2}
  mysql_field_seek:             Tmysql_field_seek;              {mysql 3.2}
  mysql_field_tell:             Tmysql_field_tell;              {mysql 3.2}
  mysql_free_result:            Tmysql_free_result;             {mysql 3.2}
  mysql_get_client_info:        Tmysql_get_client_info;         {mysql 3.2}
  mysql_get_host_info:          Tmysql_get_host_info;           {mysql 3.2}
  mysql_get_proto_info:         Tmysql_get_proto_info;          {mysql 3.2}
  mysql_get_server_info:        Tmysql_get_server_info;         {mysql 3.2}
  mysql_info:                   Tmysql_info;                    {mysql 3.2}
  mysql_init:                   Tmysql_init;                    {mysql 3.2}
  mysql_insert_id:              Tmysql_insert_id;               {mysql 3.2}
  mysql_kill:                   Tmysql_kill;                    {mysql 3.2}
  mysql_list_dbs:               Tmysql_list_dbs;                {mysql 3.2}
  mysql_list_fields:            Tmysql_list_fields;             {mysql 3.2}
  mysql_list_processes:         Tmysql_list_processes;          {mysql 3.2}
  mysql_list_tables:            Tmysql_list_tables;             {mysql 3.2}
  mysql_num_fields:             Tmysql_num_fields;              {mysql 3.2}
  mysql_num_rows:               Tmysql_num_rows;                {mysql 3.2}
  mysql_options:                Tmysql_options;                 {mysql 3.2}
  mysql_ping:                   Tmysql_ping;	                   {mysql 3.2}
  mysql_query:                  Tmysql_query;                   {mysql 3.2} {deprecated for mysql_real_query}
  mysql_real_connect:           Tmysql_real_connect;            {mysql 3.2}
  mysql_real_escape_string:     Tmysql_real_escape_string;      {mysql 3.2}
  mysql_real_query:             Tmysql_real_query;              {mysql 3.2}
  mysql_refresh:                Tmysql_refresh;                 {mysql 3.2}
  mysql_row_seek:               Tmysql_row_seek;                {mysql 3.2}
  mysql_row_tell:               Tmysql_row_tell;                {mysql 3.2}
  mysql_select_db:              Tmysql_select_db;               {mysql 3.2}
  mysql_shutdown:               Tmysql_shutdown;                {mysql 3.2} {new argument 4.1}
  mysql_ssl_set:                Tmysql_ssl_set;                 {mysql 3.2}
  mysql_stat:                   Tmysql_stat;                    {mysql 3.2}
  mysql_store_result:           Tmysql_store_result;            {mysql 3.2}
  mysql_thread_id:              Tmysql_thread_id;               {mysql 3.2}
  mysql_use_result:             Tmysql_use_result;              {mysql 3.2}

  {API for THREADED FUNCTIONS }
  my_init:                      Tmy_init;                       {mysql 3.2}
  mysql_thread_init:            Tmysql_thread_init;             {mysql 3.2}
  mysql_thread_end:             Tmysql_thread_end;              {mysql 3.2}
  mysql_thread_safe:            tmysql_thread_safe;             {mysql 3.2}

  {API for EMBEDDED SERVER  }
  mysql_server_init:            Tmysql_server_init;             {mysql 3.2}
  mysql_server_end:             Tmysql_server_end;              {mysql 3.2}

  {$IFDEF MYSQL_323_UP}
  mysql_change_user:            Tmysql_change_user;             {mysql 3.23}
  mysql_field_count:            Tmysql_field_count;             {mysql 3.22}
  {$ENDIF}

  {$IFDEF MYSQL_40_UP}
  mysql_get_client_version:     Tmysql_get_client_version;      {mysql 4.0}
  {BELOW are functions not implemented in PDO}
  mysql_send_query:     Tmysql_send_query;
  mysql_read_query_result: Tmysql_read_query_result;
  mysql_master_query:   Tmysql_master_query;
  mysql_master_send_query: Tmysql_master_send_query;
  mysql_slave_query:    Tmysql_slave_query;
  mysql_slave_send_query: Tmysql_slave_send_query;
  mysql_enable_rpl_parse: Tmysql_enable_rpl_parse;
  mysql_disable_rpl_parse: Tmysql_disable_rpl_parse;
  mysql_rpl_parse_enabled: Tmysql_rpl_parse_enabled;
  mysql_enable_reads_from_master: Tmysql_enable_reads_from_master;
  mysql_disable_reads_from_master: Tmysql_disable_reads_from_master;
  mysql_reads_from_master_enabled: Tmysql_reads_from_master_enabled;
  mysql_rpl_query_type: Tmysql_rpl_query_type;
  mysql_rpl_probe:      Tmysql_rpl_probe;
  mysql_set_master:     Tmysql_set_master;
  mysql_add_slave:      Tmysql_add_slave;
  mysql_manager_init:   Tmysql_manager_init;
  mysql_manager_connect: Tmysql_manager_connect;
  mysql_manager_close:  Tmysql_manager_close;
  mysql_manager_command: Tmysql_manager_command;
  mysql_manager_fetch_line: Tmysql_manager_fetch_line;
  {$ENDIF}

  {$IFDEF MYSQL_41_UP}
  mysql_autocommit:             Tmysql_autocommit;              {mysql 4.1}
  mysql_commit:                 Tmysql_commit;                  {mysql 4.1}
  mysql_get_server_version:     Tmysql_get_server_version;      {mysql 4.1}
  mysql_hex_string:             Tmysql_hex_string;              {mysql 4.1.8}
  mysql_more_results:           Tmysql_more_results;            {mysql 4.1}
  mysql_next_result:            Tmysql_next_result;             {mysql 4.1}
  mysql_rollback:               Tmysql_rollback;                {mysql 4.1}
  mysql_set_character_set:      Tmysql_set_character_set;       {mysql 4.1.13}
  mysql_set_server_option:      Tmysql_set_server_option;       {mysql 4.1}
  mysql_sqlstate:               Tmysql_sqlstate;                {mysql 4.1}
  mysql_warning_count:          Tmysql_warning_count;           {mysql 4.1}
  {API for PREPARED STATEMENTS}
  mysql_stmt_affected_rows:     Tmysql_stmt_affected_rows;      {mysql 4.1.0}
  mysql_stmt_attr_get:          Tmysql_stmt_attr_get;           {mysql 4.1.2}
  mysql_stmt_attr_set:          Tmysql_stmt_attr_set;           {mysql 4.1.2} {augmented 5.0.2/6}
  mysql_stmt_bind_param:        Tmysql_stmt_bind_param;         {mysql 4.1.2}
  mysql_stmt_bind_result:       Tmysql_stmt_bind_result;        {mysql 4.1.2}
  mysql_stmt_close:             Tmysql_stmt_close;              {mysql 4.1.0}
  mysql_stmt_data_seek:         Tmysql_stmt_data_seek;          {mysql 4.1.1}
  mysql_stmt_errno:             Tmysql_stmt_errno;              {mysql 4.1.0}
  mysql_stmt_error:             Tmysql_stmt_error;              {mysql 4.1.0}
  mysql_stmt_execute:           Tmysql_stmt_execute;            {mysql 4.1.2}
  mysql_stmt_fetch:             Tmysql_stmt_fetch;              {mysql 4.1.2}
  mysql_stmt_fetch_column:      Tmysql_stmt_fetch_column;       {mysql 4.1.2}
  mysql_stmt_field_count:       Tmysql_stmt_field_count;        {mysql 4.1.3}
  mysql_stmt_free_result:       Tmysql_stmt_free_result;        {mysql 4.1.1}
  mysql_stmt_init:              Tmysql_stmt_init;               {mysql 4.1.2}
  mysql_stmt_insert_id:         Tmysql_stmt_insert_id;          {mysql 4.1.2}
  mysql_stmt_num_rows:          Tmysql_stmt_num_rows;           {mysql 4.1.1}
  mysql_stmt_param_count:       Tmysql_stmt_param_count;        {mysql 4.1.2}
  mysql_stmt_param_metadata:    Tmysql_stmt_param_metadata;     {mysql 4.1.2}
  mysql_stmt_prepare:           Tmysql_stmt_prepare;            {mysql 4.1.2}
  mysql_stmt_reset:             Tmysql_stmt_reset;              {mysql 4.1.1}
  mysql_stmt_result_metadata:   Tmysql_stmt_result_metadata;    {mysql 4.1.2}
  mysql_stmt_row_seek:          Tmysql_stmt_row_seek;           {mysql 4.1.1}
  mysql_stmt_row_tell:          Tmysql_stmt_row_tell;           {mysql 4.1.1}
  mysql_stmt_send_long_data:    Tmysql_stmt_send_long_data;     {mysql 4.1.2}
  mysql_stmt_sqlstate:          Tmysql_stmt_sqlstate;           {mysql 4.1.1}
  mysql_stmt_store_result:      Tmysql_stmt_store_result;       {mysql 4.1.0}
  {$ENDIF}

  {$IFDEF MYSQL_5_UP}
  mysql_get_character_set_info: Tmysql_get_character_set_info;  {mysql 5.0.10}
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

