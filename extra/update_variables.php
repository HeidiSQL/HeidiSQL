<?php
$vars = 'auto_increment_increment 	numeric 	GLOBAL | SESSION
auto_increment_offset 	numeric 	GLOBAL | SESSION
autocommit 	boolean 	GLOBAL | SESSION
automatic_sp_privileges 	boolean 	GLOBAL
big_tables 	boolean 	GLOBAL | SESSION
binlog_cache_size 	numeric 	GLOBAL
binlog_checksum 	string 	GLOBAL
binlog_direct_non_transactional_updates 	boolean 	GLOBAL | SESSION
binlog_format 	enumeration 	GLOBAL | SESSION
binlog_row_image=image_type 	enumeration 	GLOBAL | SESSION
binlog_rows_query_log_events 	boolean 	GLOBAL | SESSION
binlog_stmt_cache_size 	numeric 	GLOBAL
bulk_insert_buffer_size 	numeric 	GLOBAL | SESSION
character_set_client 	string 	GLOBAL | SESSION
character_set_connection 	string 	GLOBAL | SESSION
character_set_database 	string 	GLOBAL | SESSION
character_set_filesystem 	string 	GLOBAL | SESSION
character_set_results 	string 	GLOBAL | SESSION
character_set_server 	string 	GLOBAL | SESSION
collation_connection 	string 	GLOBAL | SESSION
collation_database 	string 	GLOBAL | SESSION
collation_server 	string 	GLOBAL | SESSION
completion_type 	numeric 	GLOBAL | SESSION
concurrent_insert 	boolean 	GLOBAL
connect_timeout 	numeric 	GLOBAL
debug 	string 	GLOBAL | SESSION
debug_sync 	string 	SESSION
default_storage_engine 	enumeration 	GLOBAL | SESSION
default_tmp_storage_engine 	enumeration 	GLOBAL | SESSION
default_week_format 	numeric 	GLOBAL | SESSION
delay_key_write 	enumeration 	GLOBAL
delayed_insert_limit 	numeric 	GLOBAL
delayed_insert_timeout 	numeric 	GLOBAL
delayed_queue_size 	numeric 	GLOBAL
div_precision_increment 	numeric 	GLOBAL | SESSION
end_markers_in_json 	boolean 	GLOBAL | SESSION
engine_condition_pushdown 	boolean 	GLOBAL | SESSION
eq_range_index_dive_limit 	numeric 	GLOBAL | SESSION
event_scheduler 	enumeration 	GLOBAL
expire_logs_days 	numeric 	GLOBAL
flush 	boolean 	GLOBAL
flush_time 	numeric 	GLOBAL
foreign_key_checks 	boolean 	GLOBAL | SESSION
ft_boolean_syntax 	string 	GLOBAL
general_log 	boolean 	GLOBAL
general_log_file 	filename 	GLOBAL
group_concat_max_len 	numeric 	GLOBAL | SESSION
gtid_next 	enumeration 	SESSION
host_cache_size 	numeric 	GLOBAL
identity 	numeric 	SESSION
init_connect 	string 	GLOBAL
init_slave 	string 	GLOBAL
innodb_adaptive_flushing 	boolean 	GLOBAL
innodb_adaptive_hash_index 	boolean 	GLOBAL
innodb_adaptive_max_sleep_delay 	numeric 	GLOBAL
innodb_analyze_is_persistent 	boolean 	GLOBAL
innodb_api_enable_binlog 	boolean 	GLOBAL
innodb_api_enable_mdl 	boolean 	GLOBAL
innodb_api_trx_level 	boolean 	GLOBAL
innodb_autoextend_increment 	numeric 	GLOBAL
innodb_buffer_pool_dump_at_shutdown 	boolean 	GLOBAL
innodb_buffer_pool_dump_now 	boolean 	GLOBAL
innodb_buffer_pool_filename 	boolean 	GLOBAL
innodb_buffer_pool_load_abort 	boolean 	GLOBAL
innodb_buffer_pool_load_at_startup 	boolean 	GLOBAL
innodb_buffer_pool_load_now 	boolean 	GLOBAL
innodb_change_buffer_max_size 	numeric 	GLOBAL
innodb_change_buffering 	enumeration 	GLOBAL
innodb_checksum_algorithm 	enumeration 	GLOBAL
innodb_commit_concurrency 	numeric 	GLOBAL
innodb_concurrency_tickets 	numeric 	GLOBAL
innodb_fast_shutdown 	numeric 	GLOBAL
innodb_file_format 	string 	GLOBAL
innodb_file_format_max 	string 	GLOBAL
innodb_file_per_table 	boolean 	GLOBAL
innodb_flush_log_at_trx_commit 	enumeration 	GLOBAL
innodb_flush_neighbors 	boolean 	GLOBAL
innodb_ft_aux_table 	string 	GLOBAL
innodb_ft_cache_size 	numeric 	GLOBAL
innodb_ft_enable_stopword 	boolean 	GLOBAL
innodb_ft_max_token_size 	numeric 	GLOBAL
innodb_ft_min_token_size 	numeric 	GLOBAL
innodb_ft_num_word_optimize 	numeric 	GLOBAL
innodb_ft_server_stopword_table 	string 	GLOBAL
innodb_ft_sort_pll_degree 	numeric 	GLOBAL
innodb_ft_user_stopword_table 	string 	GLOBAL
innodb_io_capacity 	numeric 	GLOBAL
innodb_large_prefix 	boolean 	GLOBAL
innodb_lock_wait_timeout 	numeric 	GLOBAL | SESSION
innodb_lru_scan_depth 	numeric 	GLOBAL
innodb_max_dirty_pages_pct 	numeric 	GLOBAL
innodb_max_purge_lag 	numeric 	GLOBAL
innodb_monitor_disable 	string 	GLOBAL
innodb_monitor_enable 	string 	GLOBAL
innodb_monitor_reset 	string 	GLOBAL
innodb_monitor_reset_all 	string 	GLOBAL
innodb_old_blocks_pct 	numeric 	GLOBAL
innodb_old_blocks_time 	numeric 	GLOBAL
innodb_optimize_fulltext_only 	boolean 	GLOBAL
innodb_print_all_deadlocks 	boolean 	GLOBAL
innodb_random_read_ahead 	boolean 	GLOBAL
innodb_read_ahead_threshold 	numeric 	GLOBAL
innodb_replication_delay 	numeric 	GLOBAL
innodb_rollback_segments 	numeric 	GLOBAL
innodb_sort_buffer_size 	numeric 	GLOBAL
innodb_spin_wait_delay 	numeric 	GLOBAL
innodb_stats_method 	enumeration 	GLOBAL | SESSION
innodb_stats_on_metadata 	boolean 	GLOBAL
innodb_stats_persistent_sample_pages 	numeric 	GLOBAL
innodb_stats_sample_pages 	numeric 	GLOBAL
innodb_stats_transient_sample_pages 	numeric 	GLOBAL
innodb_strict_mode 	boolean 	GLOBAL | SESSION
innodb_support_xa 	boolean 	GLOBAL | SESSION
innodb_sync_spin_loops 	numeric 	GLOBAL
innodb_table_locks 	boolean 	GLOBAL | SESSION
innodb_thread_concurrency 	numeric 	GLOBAL
innodb_thread_sleep_delay 	numeric 	GLOBAL
innodb_undo_directory 	string 	GLOBAL
innodb_undo_logs 	numeric 	GLOBAL
innodb_undo_tablespaces 	numeric 	GLOBAL
insert_id 	numeric 	SESSION
interactive_timeout 	numeric 	GLOBAL | SESSION
join_buffer_size 	numeric 	GLOBAL | SESSION
keep_files_on_create 	boolean 	GLOBAL | SESSION
key_buffer_size 	numeric 	GLOBAL
key_cache_age_threshold 	numeric 	GLOBAL
key_cache_block_size 	numeric 	GLOBAL
key_cache_division_limit 	numeric 	GLOBAL
last_insert_id 	numeric 	SESSION
lc_messages 	string 	GLOBAL | SESSION
lc_time_names 	string 	GLOBAL | SESSION
local_infile 	boolean 	GLOBAL
lock_wait_timeout 	numeric 	GLOBAL | SESSION
log 	string 	GLOBAL
log_output 	set 	GLOBAL
log_queries_not_using_indexes 	boolean 	GLOBAL
log_slow_queries 	boolean 	GLOBAL
log_throttle_queries_not_using_indexes 	numeric 	GLOBAL
log_warnings 	numeric 	GLOBAL
long_query_time 	numeric 	GLOBAL | SESSION
low_priority_updates 	boolean 	GLOBAL | SESSION
master_verify_checksum 	boolean 	GLOBAL
max_allowed_packet 	numeric 	GLOBAL
max_binlog_cache_size 	numeric 	GLOBAL
max_binlog_size 	numeric 	GLOBAL
max_binlog_stmt_cache_size 	numeric 	GLOBAL
max_connect_errors 	numeric 	GLOBAL
max_connections 	numeric 	GLOBAL
max_delayed_threads 	numeric 	GLOBAL | SESSION
max_error_count 	numeric 	GLOBAL | SESSION
max_heap_table_size 	numeric 	GLOBAL | SESSION
max_insert_delayed_threads 	numeric 	GLOBAL | SESSION
max_join_size 	numeric 	GLOBAL | SESSION
max_length_for_sort_data 	numeric 	GLOBAL | SESSION
max_prepared_stmt_count 	numeric 	GLOBAL
max_relay_log_size 	numeric 	GLOBAL
max_seeks_for_key 	numeric 	GLOBAL | SESSION
max_sort_length 	numeric 	GLOBAL | SESSION
max_sp_recursion_depth 	numeric 	GLOBAL | SESSION
max_tmp_tables 	numeric 	GLOBAL | SESSION
max_user_connections 	numeric 	GLOBAL | SESSION
max_write_lock_count 	numeric 	GLOBAL
min_examined_row_limit 	numeric 	GLOBAL | SESSION
myisam_data_pointer_size 	numeric 	GLOBAL
myisam_max_sort_file_size 	numeric 	GLOBAL
myisam_repair_threads 	numeric 	GLOBAL | SESSION
myisam_sort_buffer_size 	numeric 	GLOBAL | SESSION
myisam_stats_method 	enumeration 	GLOBAL | SESSION
myisam_use_mmap 	boolean 	GLOBAL
net_buffer_length 	numeric 	GLOBAL | SESSION
net_read_timeout 	numeric 	GLOBAL | SESSION
net_retry_count 	numeric 	GLOBAL | SESSION
net_write_timeout 	numeric 	GLOBAL | SESSION
new 	boolean 	GLOBAL | SESSION
old_alter_table 	boolean 	GLOBAL | SESSION
old_passwords 	boolean 	GLOBAL | SESSION
optimizer_join_cache_level 	numeric 	GLOBAL | SESSION
optimizer_prune_level 	boolean 	GLOBAL | SESSION
optimizer_search_depth 	numeric 	GLOBAL | SESSION
optimizer_switch 	set 	GLOBAL | SESSION
optimizer_trace 	string 	GLOBAL | SESSION
optimizer_trace_features 	string 	GLOBAL | SESSION
optimizer_trace_limit 	numeric 	GLOBAL | SESSION
optimizer_trace_max_mem_size 	numeric 	GLOBAL | SESSION
optimizer_trace_offset 	numeric 	GLOBAL | SESSION
preload_buffer_size 	numeric 	GLOBAL | SESSION
profiling 	boolean 	GLOBAL | SESSION
profiling_history_size 	numeric 	GLOBAL | SESSION
pseudo_thread_id 	numeric 	SESSION
query_alloc_block_size 	numeric 	GLOBAL | SESSION
query_cache_limit 	numeric 	GLOBAL
query_cache_min_res_unit 	numeric 	GLOBAL
query_cache_size 	numeric 	GLOBAL
query_cache_type 	enumeration 	GLOBAL | SESSION
query_cache_wlock_invalidate 	boolean 	GLOBAL | SESSION
query_prealloc_size 	numeric 	GLOBAL | SESSION
rand_seed1 	numeric 	SESSION
rand_seed2 	numeric 	SESSION
range_alloc_block_size 	numeric 	GLOBAL | SESSION
read_buffer_size 	numeric 	GLOBAL | SESSION
read_only 	boolean 	GLOBAL
read_rnd_buffer_size 	numeric 	GLOBAL | SESSION
relay_log_purge 	boolean 	GLOBAL
relay_log_recovery 	boolean 	GLOBAL
rpl_semi_sync_master_enabled 	boolean 	GLOBAL
rpl_semi_sync_master_timeout 	numeric 	GLOBAL
rpl_semi_sync_master_trace_level 	numeric 	GLOBAL
rpl_semi_sync_master_wait_no_slave 	boolean 	GLOBAL
rpl_semi_sync_slave_enabled 	boolean 	GLOBAL
rpl_semi_sync_slave_trace_level 	numeric 	GLOBAL
secure_auth 	boolean 	GLOBAL
server_id 	numeric 	GLOBAL
slave_compressed_protocol 	boolean 	GLOBAL
slave_exec_mode 	enumeration 	GLOBAL
slave_net_timeout 	numeric 	GLOBAL
slave_parallel_workers 	numeric 	GLOBAL
slave_sql_verify_checksum 	boolean 	GLOBAL
slave_transaction_retries 	numeric 	GLOBAL
slow_launch_time 	numeric 	GLOBAL
slow_query_log 	boolean 	GLOBAL
slow_query_log_file 	filename 	GLOBAL
sort_buffer_size 	numeric 	GLOBAL | SESSION
sql_auto_is_null 	boolean 	GLOBAL | SESSION
sql_big_selects 	boolean 	GLOBAL | SESSION
sql_big_tables 	boolean 	GLOBAL | SESSION
sql_buffer_result 	boolean 	GLOBAL | SESSION
sql_log_bin 	boolean 	GLOBAL | SESSION
sql_log_off 	boolean 	GLOBAL | SESSION
sql_low_priority_updates 	boolean 	GLOBAL | SESSION
sql_max_join_size 	numeric 	GLOBAL | SESSION
sql_mode 	set 	GLOBAL | SESSION
sql_notes 	boolean 	GLOBAL | SESSION
sql_quote_show_create 	boolean 	GLOBAL | SESSION
sql_safe_updates 	boolean 	GLOBAL | SESSION
sql_select_limit 	numeric 	GLOBAL | SESSION
sql_slave_skip_counter 	numeric 	GLOBAL
sql_warnings 	boolean 	GLOBAL | SESSION
storage_engine 	enumeration 	GLOBAL | SESSION
stored_program_cache 	numeric 	GLOBAL
sync_binlog 	numeric 	GLOBAL
sync_frm 	boolean 	GLOBAL
sync_master_info 	numeric 	GLOBAL
sync_relay_log 	numeric 	GLOBAL
sync_relay_log_info 	numeric 	GLOBAL
table_definition_cache 	numeric 	GLOBAL
table_open_cache 	numeric 	GLOBAL
thread_cache_size 	numeric 	GLOBAL
time_zone 	string 	GLOBAL | SESSION
timed_mutexes 	boolean 	GLOBAL
timestamp 	numeric 	SESSION
tmp_table_size 	numeric 	GLOBAL | SESSION
transaction_alloc_block_size 	numeric 	GLOBAL | SESSION
transaction_prealloc_size 	numeric 	GLOBAL | SESSION
tx_isolation 	enumeration 	GLOBAL | SESSION
tx_read_only 	boolean 	GLOBAL | SESSION
unique_checks 	boolean 	GLOBAL | SESSION
updatable_views_with_limit 	boolean 	GLOBAL | SESSION
wait_timeout 	numeric 	GLOBAL | SESSION';

$vars = explode("\r\n", $vars);
foreach($vars as $v)
{
	preg_match('#^(\S+)\s+(\S+)\s+(.+)$#', $v, $matches);
	switch($matches[2])
	{
		case 'numeric': $vartype='vtNumeric'; break;
		case 'string': $vartype='vtString'; break;
		case 'boolean': $vartype='vtBoolean'; break;
		case 'enumeration': $vartype='vtEnum'; break;
	}
	switch($matches[3])
	{
		case 'GLOBAL': $varscope='vsGlobal'; break;
		case 'SESSION': $varscope='vsSession'; break;
		case 'GLOBAL | SESSION': $varscope='vsBoth'; break;
	}
	echo "    (
      Name: '".$matches[1]."';
      VarType: ".$vartype.";
      VarScope: ".$varscope.";".($vartype=='vtEnum' ? "\r\n      EnumValues: '';" : "")."
    ),\r\n";
}

?>
