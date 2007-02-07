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

{$IFDEF LOAD_MYSQL_API_FUNC}
{ ************** Plain API Function types definition ************* }

  { Functions to get information from the MYSQL and MYSQL_RES structures
    Should definitely be used if one uses shared libraries. }
  Tmysql_affected_rows          = function(Handle: PMYSQL): Int64;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_character_set_name     = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_close                  = procedure(Handle: PMYSQL);                                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_connect                = function(Handle: PMYSQL; const Host, User, Passwd: PChar): PMYSQL;   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_create_db              = function(Handle: PMYSQL; const Db: PChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_data_seek              = procedure(Result: PMYSQL_RES; Offset: Int64);                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_debug                  = procedure(Debug: PChar);                                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_drop_db                = function(Handle: PMYSQL; const Db: PChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_dump_debug_info        = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_eof                    = function(Result: PMYSQL_RES): Byte;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_errno                  = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_error                  = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_escape_string          = function(PTo, PFrom: PChar; Len: Cardinal): Cardinal;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_field            = function(Result: PMYSQL_RES): PMYSQL_FIELD;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_field_direct     = function(Result: PMYSQL_RES; FieldNo: Cardinal): PMYSQL_FIELD;       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_fields           = function(Result: PMYSQL_RES): PMYSQL_FIELD;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_lengths          = function(Result: PMYSQL_RES): PLongInt;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_fetch_row              = function(Result: PMYSQL_RES): PMYSQL_ROW;                            {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_seek             = function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_field_tell             = function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_free_result            = procedure(Result: PMYSQL_RES);                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_client_info        = function: PChar;                                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_host_info          = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_proto_info         = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_server_info        = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_info                   = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_init                   = function(Handle: PMYSQL): PMYSQL;                                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_insert_id              = function(Handle: PMYSQL): Int64;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_kill                   = function(Handle: PMYSQL; Pid: LongInt): Integer;                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_dbs               = function(Handle: PMYSQL; Wild: PChar): PMYSQL_RES;                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_fields            = function(Handle: PMYSQL; const Table, Wild: PChar): PMYSQL_RES;      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_processes         = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_list_tables            = function(Handle: PMYSQL; const Wild: PChar): PMYSQL_RES;             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_num_fields             = function(Result: PMYSQL_RES): Cardinal;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_num_rows               = function(Result: PMYSQL_RES): Int64;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_options                = function(Handle: PMYSQL; Option: TMySqlOption; const Arg: PChar): Integer;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_ping                   = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_query                  = function(Handle: PMYSQL; const Query: PChar): Integer;               {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_connect           = function(Handle: PMYSQL; const Host, User, Passwd, Db: PChar;
                                           Port: Cardinal; const UnixSocket: PChar; ClientFlag: Cardinal): PMYSQL;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_escape_string     = function(Handle: PMYSQL; PTo: PChar; const PFrom: PChar; length: Cardinal): Cardinal;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_real_query             = function(Handle: PMYSQL; const Query: PChar; Length: Cardinal): Integer;
                                                                                                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_refresh                = function(Handle: PMYSQL; Options: Cardinal): Integer;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_row_seek               = function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS;      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_row_tell               = function(Result: PMYSQL_RES): PMYSQL_ROWS;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_select_db              = function(Handle: PMYSQL; const Db: PChar): Integer;                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_ssl_set                = function(Handle: PMYSQL; const key, cert, CA, CApath, cipher:
                                  PChar): Byte;                                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stat                   = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_store_result           = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_thread_id              = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_use_result             = function(Handle: PMYSQL): PMYSQL_RES;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Set up and bring down a thread; these function should be called for each thread in an application which
    opens at least one MySQL connection.  All uses of the connection(s) should be between these function calls. }
  Tmy_init                      = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_init            = function: Byte;                                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_end             = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_thread_safe            = function: Cardinal;                                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Set up and bring down the server; to ensure that applications will work when linked against either the
    standard client library or the embedded server library, these functions should be called. }
  Tmysql_server_init            = function(Argc: Integer; Argv, Groups: Pointer): Integer;            {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_server_end             = procedure;                                                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  {$IFDEF MYSQL_323_UP}
  Tmysql_change_user            = function(mysql: PMYSQL; const user: PChar; const passwd: PChar; const db: PChar): Byte;
  Tmysql_field_count            = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  {$ENDIF}

  {$IFDEF MYSQL_40_UP}
  Tmysql_get_client_version     = function: Cardinal;                                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
{BELOW are functions not implemented in PDO}
  Tmysql_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_read_query_result = function(mysql: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Perform query on master }
  Tmysql_master_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_master_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Perform query on slave }
  Tmysql_slave_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_slave_send_query = function(mysql: PMYSQL; const query: PChar;
    length: Cardinal): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Enable/disable parsing of all queries to decide
    if they go on master or slave }
  Tmysql_enable_rpl_parse = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_disable_rpl_parse = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Get the value of the parse flag }
  Tmysql_rpl_parse_enabled = function(mysql: PMYSQL): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Enable/disable reads from master }
  Tmysql_enable_reads_from_master = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_disable_reads_from_master = procedure(mysql: PMYSQL);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Get the value of the master read flag }
  Tmysql_reads_from_master_enabled = function(mysql: PMYSQL): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_rpl_query_type = function(const query: PChar; len: Integer):
    TMySqlRplType; {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Discover the master and its slaves }
  Tmysql_rpl_probe = function(mysql: PMYSQL): Byte;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  { Set the master, close/free the old one, if it is not a pivot }
  Tmysql_set_master = function(mysql: PMYSQL; const host: PChar;
    port: Cardinal; const user: PChar; const passwd: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_add_slave = function(mysql: PMYSQL; const host: PChar;
    port: Cardinal; const user: PChar; const passwd: PChar): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_init = function(con: PMYSQL_MANAGER): PMYSQL_MANAGER;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_connect = function(con: PMYSQL_MANAGER; const host: PChar;
    const user: PChar; const passwd: PChar; port: Cardinal): PMYSQL_MANAGER;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_close = procedure(con: PMYSQL_MANAGER);
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_command = function(con: PMYSQL_MANAGER; const cmd: PChar;
    cmd_len: Integer): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};

  Tmysql_manager_fetch_line = function(con: PMYSQL_MANAGER; res_buf: PChar;
    res_buf_size: Integer): Integer;
    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  {$ENDIF}

  {$IFDEF MYSQL_40_DOWN}
  Tmysql_shutdown               = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  {$ENDIF}

  {$IFDEF MYSQL_41_UP}
  Tmysql_autocommit             = function(Handle: PMYSQL; const mode: Byte): Byte;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_commit                 = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_get_server_version     = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_hex_string             = function(PTo, PFrom: Pchar; Len: Cardinal): Cardinal;                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_more_results           = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_next_result            = function(Handle: PMYSQL): Integer;                                   {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_rollback               = function(Handle: PMYSQL): Byte;                                      {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_set_character_set      = function(Handle: PMYSQL; csname: PChar): Integer;                    {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_set_server_option      = function(Handle: PMYSQL; Option: TMysqlSetOption): Integer;          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_shutdown               = function(Handle: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer;
  Tmysql_sqlstate               = function(Handle: PMYSQL): PChar;                                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_warning_count          = function(Handle: PMYSQL): Cardinal;                                  {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
{BELOW are new PREPARED STATEMENTS}
  Tmysql_stmt_affected_rows     = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_attr_get          = function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
                                  arg: PChar): Integer;                                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_attr_set          = function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
                                  const arg: PChar): Integer;                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_bind_param        = function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND2): Byte;              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_bind_result       = function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND2): Byte;              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_close             = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_data_seek         = procedure(stmt: PMYSQL_STMT; offset: Int64);                       {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_errno             = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_error             = function(stmt: PMYSQL_STMT): PChar;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_execute           = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_fetch             = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_fetch_column      = function(stmt: PMYSQL_STMT; bind: PMYSQL_BIND2; column: Cardinal;
                                  offset: Cardinal): Integer;                                        {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_field_count       = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_free_result       = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_init              = function(Handle: PMYSQL): PMYSQL_STMT;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_insert_id         = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_num_rows          = function(stmt: PMYSQL_STMT): Int64;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_param_count       = function(stmt: PMYSQL_STMT): Cardinal;                             {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_param_metadata    = function(stmt: PMYSQL_STMT): PMYSQL_RES;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_prepare           = function(stmt: PMYSQL_STMT; const query: PChar; length: Cardinal):
                                  Integer;                                                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_reset             = function(stmt: PMYSQL_STMT): Byte;                                 {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_result_metadata   = function(stmt: PMYSQL_STMT): PMYSQL_RES;                           {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_row_seek          = function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS;     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_row_tell          = function(stmt: PMYSQL_STMT): PMYSQL_ROWS;                          {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_send_long_data    = function(stmt: PMYSQL_STMT; parameter_number: Cardinal; const
                                  data: PChar; length: Cardinal): Byte;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_sqlstate          = function(stmt: PMYSQL_STMT): PChar;                                {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  Tmysql_stmt_store_result      = function(stmt: PMYSQL_STMT): Integer;                              {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  {$ENDIF}

  {$IFDEF MYSQL_5_UP}
  Tmysql_get_character_set_info = procedure(Handle: PMYSQL; cs: PMY_CHARSET_INFO);                     {$IFNDEF UNIX} stdcall {$ELSE} cdecl {$ENDIF};
  {$ENDIF}

{$ENDIF}

{$IFDEF MYSQL_API_VAR}
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

{$ENDIF}

{$IFDEF LOAD_MYSQL_API_REC}
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

