<?php
$vars = 'auto_increment_increment 	Yes 	Yes 	Yes 	Both 	Yes
auto_increment_offset 	Yes 	Yes 	Yes 	Both 	Yes
autocommit 	Yes 	Yes 	Yes 	Both 	Yes
automatic_sp_privileges 	  	  	Yes 	Global 	Yes
back_log 	Yes 	Yes 	Yes 	Global 	No
basedir 	Yes 	Yes 	Yes 	Global 	No
big-tables 	Yes 	Yes 	  	  	Yes
- Variable: big_tables 	  	  	Yes 	Both 	Yes
bind-address 	Yes 	Yes 	Yes 	Global 	No
binlog_cache_size 	Yes 	Yes 	Yes 	Global 	Yes
binlog_checksum 	  	  	Yes 	Global 	Yes
binlog_direct_non_transactional_updates 	Yes 	Yes 	Yes 	Both 	Yes
binlog-format 	Yes 	Yes 	  	  	Yes
- Variable: binlog_format 	  	  	Yes 	Both 	Yes
binlog_row_image 	Yes 	  	Yes 	Both 	Yes
binlog_stmt_cache_size 	Yes 	Yes 	Yes 	Global 	Yes
bulk_insert_buffer_size 	Yes 	Yes 	Yes 	Both 	Yes
character_set_client 	  	  	Yes 	Both 	Yes
character_set_connection 	  	  	Yes 	Both 	Yes
character_set_database[a] 	  	  	Yes 	Both 	Yes
character-set-filesystem 	Yes 	Yes 	  	  	Yes
- Variable: character_set_filesystem 	  	  	Yes 	Both 	Yes
character_set_results 	  	  	Yes 	Both 	Yes
character-set-server 	Yes 	Yes 	  	  	Yes
- Variable: character_set_server 	  	  	Yes 	Both 	Yes
character_set_system 	  	  	Yes 	Global 	No
character-sets-dir 	Yes 	Yes 	  	  	No
- Variable: character_sets_dir 	  	  	Yes 	Global 	No
collation_connection 	  	  	Yes 	Both 	Yes
collation_database[b] 	  	  	Yes 	Both 	Yes
collation-server 	Yes 	Yes 	  	  	Yes
- Variable: collation_server 	  	  	Yes 	Both 	Yes
completion_type 	Yes 	Yes 	Yes 	Both 	Yes
concurrent_insert 	Yes 	Yes 	Yes 	Global 	Yes
connect_timeout 	Yes 	Yes 	Yes 	Global 	Yes
datadir 	Yes 	Yes 	Yes 	Global 	No
date_format 	  	  	Yes 	Global 	No
datetime_format 	  	  	Yes 	Global 	No
debug 	Yes 	Yes 	Yes 	Both 	Yes
debug_sync 	  	  	Yes 	Session 	Yes
default-storage-engine 	Yes 	Yes 	  	  	Yes
- Variable: default_storage_engine 	  	  	Yes 	Both 	Yes
default_tmp_storage_engine 	Yes 	Yes 	Yes 	Both 	Yes
default_week_format 	Yes 	Yes 	Yes 	Both 	Yes
delay-key-write 	Yes 	Yes 	  	  	Yes
- Variable: delay_key_write 	  	  	Yes 	Global 	Yes
delayed_insert_limit 	Yes 	Yes 	Yes 	Global 	Yes
delayed_insert_timeout 	Yes 	Yes 	Yes 	Global 	Yes
delayed_queue_size 	Yes 	Yes 	Yes 	Global 	Yes
disable_gtid_unsafe_statements 	Yes 	Yes 	Yes 	Global 	No
disable-gtid-unsafe-statements 	Yes 	Yes 	Yes 	Global 	No
div_precision_increment 	Yes 	Yes 	Yes 	Both 	Yes
end_markers_in_json 	  	  	Yes 	Both 	Yes
engine-condition-pushdown 	Yes 	Yes 	  	  	Yes
- Variable: engine_condition_pushdown 	  	  	Yes 	Both 	Yes
eq_range_index_dive_limit 	Yes 	Yes 	Yes 	Both 	Yes
error_count 	  	  	Yes 	Session 	No
event-scheduler 	Yes 	Yes 	  	  	Yes
- Variable: event_scheduler 	  	  	Yes 	Global 	Yes
expire_logs_days 	Yes 	Yes 	Yes 	Global 	Yes
external_user 	  	  	Yes 	Session 	No
flush 	Yes 	Yes 	Yes 	Global 	Yes
flush_time 	Yes 	Yes 	Yes 	Global 	Yes
foreign_key_checks 	  	  	Yes 	Both 	Yes
ft_boolean_syntax 	Yes 	Yes 	Yes 	Global 	Yes
ft_max_word_len 	Yes 	Yes 	Yes 	Global 	No
ft_min_word_len 	Yes 	Yes 	Yes 	Global 	No
ft_query_expansion_limit 	Yes 	Yes 	Yes 	Global 	No
ft_stopword_file 	Yes 	Yes 	Yes 	Global 	No
general-log 	Yes 	Yes 	  	  	Yes
- Variable: general_log 	  	  	Yes 	Global 	Yes
general_log_file 	Yes 	Yes 	Yes 	Global 	Yes
group_concat_max_len 	Yes 	Yes 	Yes 	Both 	Yes
gtid_done 	  	  	Yes 	Both 	No
gtid_lost 	  	  	Yes 	Global 	No
gtid_mode 	  	  	Yes 	Global 	No
gtid-mode 	Yes 	Yes 	  	  	No
- Variable: gtid_mode 	  	  	Yes 	Global 	No
gtid_next 	  	  	Yes 	Session 	Yes
gtid_owned 	  	  	Yes 	Both 	No
have_compress 	  	  	Yes 	Global 	No
have_crypt 	  	  	Yes 	Global 	No
have_csv 	  	  	Yes 	Global 	No
have_dynamic_loading 	  	  	Yes 	Global 	No
have_geometry 	  	  	Yes 	Global 	No
have_innodb 	  	  	Yes 	Global 	No
have_ndbcluster 	  	  	Yes 	Global 	No
have_openssl 	  	  	Yes 	Global 	No
have_partitioning 	  	  	Yes 	Global 	No
have_profiling 	  	  	Yes 	Global 	No
have_query_cache 	  	  	Yes 	Global 	No
have_rtree_keys 	  	  	Yes 	Global 	No
have_ssl 	  	  	Yes 	Global 	No
have_symlink 	  	  	Yes 	Global 	No
host_cache_size 	Yes 	Yes 	Yes 	Global 	Yes
hostname 	  	  	Yes 	Global 	No
identity 	  	  	Yes 	Session 	Yes
ignore-builtin-innodb 	Yes 	Yes 	  	  	No
- Variable: ignore_builtin_innodb 	  	  	Yes 	Global 	No
init_connect 	Yes 	Yes 	Yes 	Global 	Yes
init-file 	Yes 	Yes 	  	  	No
- Variable: init_file 	  	  	Yes 	Global 	No
init_slave 	Yes 	Yes 	Yes 	Global 	Yes
innodb_adaptive_flushing 	Yes 	Yes 	Yes 	Global 	Yes
innodb_adaptive_hash_index 	Yes 	Yes 	Yes 	Global 	Yes
innodb_adaptive_max_sleep_delay 	Yes 	Yes 	Yes 	Global 	Yes
innodb_additional_mem_pool_size 	Yes 	Yes 	Yes 	Global 	No
innodb_analyze_is_persistent 	Yes 	Yes 	Yes 	Global 	Yes
innodb_api_enable_binlog 	Yes 	Yes 	Yes 	Global 	Yes
innodb_api_enable_mdl 	Yes 	Yes 	Yes 	Global 	Yes
innodb_api_trx_level 	Yes 	Yes 	Yes 	Global 	Yes
innodb_autoextend_increment 	Yes 	Yes 	Yes 	Global 	Yes
innodb_autoinc_lock_mode 	Yes 	Yes 	Yes 	Global 	No
innodb_buffer_pool_dump_at_shutdown 	Yes 	Yes 	Yes 	Global 	Yes
innodb_buffer_pool_dump_now 	Yes 	Yes 	Yes 	Global 	Yes
innodb_buffer_pool_filename 	Yes 	Yes 	Yes 	Global 	Yes
innodb_buffer_pool_instances 	Yes 	Yes 	Yes 	Global 	No
innodb_buffer_pool_load_abort 	Yes 	Yes 	Yes 	Global 	Yes
innodb_buffer_pool_load_at_startup 	Yes 	Yes 	Yes 	Global 	Yes
innodb_buffer_pool_load_now 	Yes 	Yes 	Yes 	Global 	Yes
innodb_buffer_pool_size 	Yes 	Yes 	Yes 	Global 	No
innodb_change_buffer_max_size 	Yes 	Yes 	Yes 	Global 	Yes
innodb_change_buffering 	Yes 	Yes 	Yes 	Global 	Yes
innodb_checksum_algorithm 	Yes 	Yes 	Yes 	Global 	Yes
innodb_checksums 	Yes 	Yes 	Yes 	Global 	No
innodb_commit_concurrency 	Yes 	Yes 	Yes 	Global 	Yes
innodb_concurrency_tickets 	Yes 	Yes 	Yes 	Global 	Yes
innodb_data_file_path 	Yes 	Yes 	Yes 	Global 	No
innodb_data_home_dir 	Yes 	Yes 	Yes 	Global 	No
innodb_doublewrite 	Yes 	Yes 	Yes 	Global 	No
innodb_fast_shutdown 	Yes 	Yes 	Yes 	Global 	Yes
innodb_file_format 	Yes 	Yes 	Yes 	Global 	Yes
innodb_file_format_check 	Yes 	Yes 	Yes 	Global 	No
innodb_file_format_max 	Yes 	Yes 	Yes 	Global 	Yes
innodb_file_per_table 	Yes 	Yes 	Yes 	Global 	Yes
innodb_flush_log_at_trx_commit 	Yes 	Yes 	Yes 	Global 	Yes
innodb_flush_method 	Yes 	Yes 	Yes 	Global 	No
innodb_flush_neighbors 	Yes 	Yes 	Yes 	Global 	Yes
innodb_force_load_corrupted 	Yes 	Yes 	Yes 	Global 	No
innodb_force_recovery 	Yes 	Yes 	Yes 	Global 	No
innodb_ft_aux_table 	Yes 	Yes 	Yes 	Global 	Yes
innodb_ft_cache_size 	Yes 	Yes 	Yes 	Global 	Yes
innodb_ft_enable_stopword 	Yes 	Yes 	Yes 	Global 	Yes
innodb_ft_max_token_size 	Yes 	Yes 	Yes 	Global 	Yes
innodb_ft_min_token_size 	Yes 	Yes 	Yes 	Global 	Yes
innodb_ft_num_word_optimize 	Yes 	Yes 	Yes 	Global 	Yes
innodb_ft_server_stopword_table 	Yes 	Yes 	Yes 	Global 	Yes
innodb_ft_sort_pll_degree 	Yes 	Yes 	Yes 	Global 	Yes
innodb_ft_user_stopword_table 	Yes 	Yes 	Yes 	Global 	Yes
innodb_io_capacity 	Yes 	Yes 	Yes 	Global 	Yes
innodb_large_prefix 	Yes 	Yes 	Yes 	Global 	Yes
innodb_lock_wait_timeout 	Yes 	Yes 	Yes 	Both 	Yes
innodb_locks_unsafe_for_binlog 	Yes 	Yes 	Yes 	Global 	No
innodb_log_buffer_size 	Yes 	Yes 	Yes 	Global 	No
innodb_log_file_size 	Yes 	Yes 	Yes 	Global 	No
innodb_log_files_in_group 	Yes 	Yes 	Yes 	Global 	No
innodb_log_group_home_dir 	Yes 	Yes 	Yes 	Global 	No
innodb_lru_scan_depth 	Yes 	Yes 	Yes 	Global 	Yes
innodb_max_dirty_pages_pct 	Yes 	Yes 	Yes 	Global 	Yes
innodb_max_purge_lag 	Yes 	Yes 	Yes 	Global 	Yes
innodb_mirrored_log_groups 	Yes 	Yes 	Yes 	Global 	No
innodb_monitor_disable 	Yes 	Yes 	Yes 	Global 	Yes
innodb_monitor_enable 	Yes 	Yes 	Yes 	Global 	Yes
innodb_monitor_reset 	Yes 	Yes 	Yes 	Global 	Yes
innodb_monitor_reset_all 	Yes 	Yes 	Yes 	Global 	Yes
innodb_old_blocks_pct 	Yes 	Yes 	Yes 	Global 	Yes
innodb_old_blocks_time 	Yes 	Yes 	Yes 	Global 	Yes
innodb_open_files 	Yes 	Yes 	Yes 	Global 	No
innodb_optimize_fulltext_only 	Yes 	Yes 	Yes 	Global 	Yes
innodb_page_size 	Yes 	Yes 	Yes 	Global 	No
innodb_print_all_deadlocks 	Yes 	Yes 	Yes 	Global 	Yes
innodb_purge_batch_size 	Yes 	Yes 	Yes 	Global 	No
innodb_purge_threads 	Yes 	Yes 	Yes 	Global 	No
innodb_random_read_ahead 	Yes 	Yes 	Yes 	Global 	Yes
innodb_read_ahead_threshold 	Yes 	Yes 	Yes 	Global 	Yes
innodb_read_io_threads 	Yes 	Yes 	Yes 	Global 	No
innodb_replication_delay 	Yes 	Yes 	Yes 	Global 	Yes
innodb_rollback_on_timeout 	Yes 	Yes 	Yes 	Global 	No
innodb_rollback_segments 	Yes 	Yes 	Yes 	Global 	Yes
innodb_sort_buffer_size 	Yes 	Yes 	Yes 	Global 	Yes
innodb_spin_wait_delay 	Yes 	Yes 	Yes 	Global 	Yes
innodb_stats_method 	Yes 	Yes 	Yes 	Both 	Yes
innodb_stats_on_metadata 	Yes 	Yes 	Yes 	Global 	Yes
innodb_stats_persistent_sample_pages 	Yes 	Yes 	Yes 	Global 	Yes
innodb_stats_sample_pages 	Yes 	Yes 	Yes 	Global 	Yes
innodb_stats_transient_sample_pages 	Yes 	Yes 	Yes 	Global 	Yes
innodb_strict_mode 	Yes 	Yes 	Yes 	Both 	Yes
innodb_support_xa 	Yes 	Yes 	Yes 	Both 	Yes
innodb_sync_array_size 	Yes 	Yes 	Yes 	Global 	No
innodb_sync_spin_loops 	Yes 	Yes 	Yes 	Global 	Yes
innodb_table_locks 	Yes 	Yes 	Yes 	Both 	Yes
innodb_thread_concurrency 	Yes 	Yes 	Yes 	Global 	Yes
innodb_thread_sleep_delay 	Yes 	Yes 	Yes 	Global 	Yes
innodb_undo_directory 	Yes 	Yes 	Yes 	Global 	Yes
innodb_undo_logs 	Yes 	Yes 	Yes 	Global 	Yes
innodb_undo_tablespaces 	Yes 	Yes 	Yes 	Global 	Yes
innodb_use_native_aio 	Yes 	Yes 	Yes 	Global 	No
innodb_use_sys_malloc 	Yes 	Yes 	Yes 	Global 	No
innodb_version 	  	  	Yes 	Global 	No
innodb_write_io_threads 	Yes 	Yes 	Yes 	Global 	No
insert_id 	  	  	Yes 	Session 	Yes
interactive_timeout 	Yes 	Yes 	Yes 	Both 	Yes
join_buffer_size 	Yes 	Yes 	Yes 	Both 	Yes
keep_files_on_create 	Yes 	Yes 	Yes 	Both 	Yes
key_buffer_size 	Yes 	Yes 	Yes 	Global 	Yes
key_cache_age_threshold 	Yes 	Yes 	Yes 	Global 	Yes
key_cache_block_size 	Yes 	Yes 	Yes 	Global 	Yes
key_cache_division_limit 	Yes 	Yes 	Yes 	Global 	Yes
language 	Yes 	Yes 	Yes 	Global 	No
large_files_support 	  	  	Yes 	Global 	No
large_page_size 	  	  	Yes 	Global 	No
large-pages 	Yes 	Yes 	  	  	No
- Variable: large_pages 	  	  	Yes 	Global 	No
last_insert_id 	  	  	Yes 	Session 	Yes
lc-messages 	Yes 	Yes 	  	  	Yes
- Variable: lc_messages 	  	  	Yes 	Both 	Yes
lc-messages-dir 	Yes 	Yes 	  	  	No
- Variable: lc_messages_dir 	  	  	Yes 	Global 	No
lc_time_names 	  	  	Yes 	Both 	Yes
license 	  	  	Yes 	Global 	No
local-infile 	Yes 	Yes 	  	  	Yes
- Variable: local_infile 	  	  	Yes 	Global 	Yes
lock_wait_timeout 	Yes 	Yes 	Yes 	Both 	Yes
locked_in_memory 	  	  	Yes 	Global 	No
log 	Yes 	Yes 	Yes 	Global 	Yes
log_bin 	  	  	Yes 	Global 	No
log-bin 	Yes 	Yes 	Yes 	Global 	No
log_bin_basename 	  	  	Yes 	Global 	No
log-error 	Yes 	Yes 	  	  	No
- Variable: log_error 	  	  	Yes 	Global 	No
log-output 	Yes 	Yes 	  	  	Yes
- Variable: log_output 	  	  	Yes 	Global 	Yes
log-queries-not-using-indexes 	Yes 	Yes 	  	  	Yes
- Variable: log_queries_not_using_indexes 	  	  	Yes 	Global 	Yes
log-slave-updates 	Yes 	Yes 	  	  	No
- Variable: log_slave_updates 	  	  	Yes 	Global 	No
log-slow-queries 	Yes 	Yes 	  	  	Yes
- Variable: log_slow_queries 	  	  	Yes 	Global 	Yes
log_throttle_queries_not_using_indexes 	  	  	Yes 	Global 	Yes
log-warnings 	Yes 	Yes 	  	  	Yes
- Variable: log_warnings 	  	  	Yes 	Global 	Yes
long_query_time 	Yes 	Yes 	Yes 	Both 	Yes
low-priority-updates 	Yes 	Yes 	  	  	Yes
- Variable: low_priority_updates 	  	  	Yes 	Both 	Yes
lower_case_file_system 	Yes 	Yes 	Yes 	Global 	No
lower_case_table_names 	Yes 	Yes 	Yes 	Global 	No
master_info_repository 	  	  	Yes 	Global 	No
master_verify_checksum 	  	  	Yes 	Global 	Yes
max_allowed_packet 	Yes 	Yes 	Yes 	Global 	Yes
max_binlog_cache_size 	Yes 	Yes 	Yes 	Global 	Yes
max_binlog_size 	Yes 	Yes 	Yes 	Global 	Yes
max_binlog_stmt_cache_size 	Yes 	Yes 	Yes 	Global 	Yes
max_connect_errors 	Yes 	Yes 	Yes 	Global 	Yes
max_connections 	Yes 	Yes 	Yes 	Global 	Yes
max_delayed_threads 	Yes 	Yes 	Yes 	Both 	Yes
max_error_count 	Yes 	Yes 	Yes 	Both 	Yes
max_heap_table_size 	Yes 	Yes 	Yes 	Both 	Yes
max_insert_delayed_threads 	  	  	Yes 	Both 	Yes
max_join_size 	Yes 	Yes 	Yes 	Both 	Yes
max_length_for_sort_data 	Yes 	Yes 	Yes 	Both 	Yes
max_prepared_stmt_count 	Yes 	Yes 	Yes 	Global 	Yes
max_relay_log_size 	Yes 	Yes 	Yes 	Global 	Yes
max_seeks_for_key 	Yes 	Yes 	Yes 	Both 	Yes
max_sort_length 	Yes 	Yes 	Yes 	Both 	Yes
max_sp_recursion_depth 	Yes 	Yes 	Yes 	Both 	Yes
max_tmp_tables 	Yes 	Yes 	Yes 	Both 	Yes
max_user_connections 	Yes 	Yes 	Yes 	Both 	Yes
max_write_lock_count 	Yes 	Yes 	Yes 	Global 	Yes
memlock 	Yes 	Yes 	Yes 	Global 	No
metadata_locks_cache_size 	  	  	Yes 	Global 	No
min-examined-row-limit 	Yes 	Yes 	Yes 	Both 	Yes
myisam_data_pointer_size 	Yes 	Yes 	Yes 	Global 	Yes
myisam_max_sort_file_size 	Yes 	Yes 	Yes 	Global 	Yes
myisam_mmap_size 	Yes 	Yes 	Yes 	Global 	No
myisam_recover_options 	  	  	Yes 	Global 	No
myisam_repair_threads 	Yes 	Yes 	Yes 	Both 	Yes
myisam_sort_buffer_size 	Yes 	Yes 	Yes 	Both 	Yes
myisam_stats_method 	Yes 	Yes 	Yes 	Both 	Yes
myisam_use_mmap 	Yes 	Yes 	Yes 	Global 	Yes
named_pipe 	  	  	Yes 	Global 	No
net_buffer_length 	Yes 	Yes 	Yes 	Both 	Yes
net_read_timeout 	Yes 	Yes 	Yes 	Both 	Yes
net_retry_count 	Yes 	Yes 	Yes 	Both 	Yes
net_write_timeout 	Yes 	Yes 	Yes 	Both 	Yes
new 	Yes 	Yes 	Yes 	Both 	Yes
old 	Yes 	Yes 	Yes 	Global 	No
old-alter-table 	Yes 	Yes 	  	  	Yes
- Variable: old_alter_table 	  	  	Yes 	Both 	Yes
old-passwords 	Yes 	Yes 	  	  	Yes
- Variable: old_passwords 	  	  	Yes 	Both 	Yes
open-files-limit 	Yes 	Yes 	  	  	No
- Variable: open_files_limit 	  	  	Yes 	Global 	No
optimizer_join_cache_level 	Yes 	Yes 	Yes 	Both 	Yes
optimizer_prune_level 	Yes 	Yes 	Yes 	Both 	Yes
optimizer_search_depth 	Yes 	Yes 	Yes 	Both 	Yes
optimizer_switch 	Yes 	Yes 	Yes 	Both 	Yes
optimizer_trace 	  	  	Yes 	Both 	Yes
optimizer_trace_features 	  	  	Yes 	Both 	Yes
optimizer_trace_limit 	  	  	Yes 	Both 	Yes
optimizer_trace_max_mem_size 	  	  	Yes 	Both 	Yes
optimizer_trace_offset 	  	  	Yes 	Both 	Yes
partition 	Yes 	Yes 	  	  	No
- Variable: have_partitioning 	  	  	Yes 	Global 	No
performance_schema 	Yes 	Yes 	Yes 	Global 	No
performance_schema_accounts_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_digests_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_events_stages_history_long_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_events_stages_history_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_events_statements_history_long_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_events_statements_history_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_events_waits_history_long_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_events_waits_history_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_hosts_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_cond_classes 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_cond_instances 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_file_classes 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_file_handles 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_file_instances 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_mutex_classes 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_mutex_instances 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_rwlock_classes 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_rwlock_instances 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_socket_classes 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_socket_instances 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_stage_classes 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_statement_classes 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_table_handles 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_table_instances 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_thread_classes 	Yes 	Yes 	Yes 	Global 	No
performance_schema_max_thread_instances 	Yes 	Yes 	Yes 	Global 	No
performance_schema_setup_actors_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_setup_objects_size 	Yes 	Yes 	Yes 	Global 	No
performance_schema_users_size 	Yes 	Yes 	Yes 	Global 	No
pid-file 	Yes 	Yes 	  	  	No
- Variable: pid_file 	  	  	Yes 	Global 	No
plugin_dir 	Yes 	Yes 	Yes 	Global 	No
port 	Yes 	Yes 	Yes 	Global 	No
preload_buffer_size 	Yes 	Yes 	Yes 	Both 	Yes
profiling 	  	  	Yes 	Both 	Yes
profiling_history_size 	Yes 	Yes 	Yes 	Both 	Yes
protocol_version 	  	  	Yes 	Global 	No
proxy_user 	  	  	Yes 	Session 	No
pseudo_thread_id 	  	  	Yes 	Session 	Yes
query_alloc_block_size 	Yes 	Yes 	Yes 	Both 	Yes
query_cache_limit 	Yes 	Yes 	Yes 	Global 	Yes
query_cache_min_res_unit 	Yes 	Yes 	Yes 	Global 	Yes
query_cache_size 	Yes 	Yes 	Yes 	Global 	Yes
query_cache_type 	Yes 	Yes 	Yes 	Both 	Yes
query_cache_wlock_invalidate 	Yes 	Yes 	Yes 	Both 	Yes
query_prealloc_size 	Yes 	Yes 	Yes 	Both 	Yes
rand_seed1 	  	  	Yes 	Session 	Yes
rand_seed2 	  	  	Yes 	Session 	Yes
range_alloc_block_size 	Yes 	Yes 	Yes 	Both 	Yes
read_buffer_size 	Yes 	Yes 	Yes 	Both 	Yes
read_only 	Yes 	Yes 	Yes 	Global 	Yes
read_rnd_buffer_size 	Yes 	Yes 	Yes 	Both 	Yes
relay_log_basename 	  	  	Yes 	Global 	No
relay-log-index 	Yes 	Yes 	  	  	No
- Variable: relay_log_index 	  	  	Yes 	Both 	No
relay_log_index 	Yes 	Yes 	Yes 	Global 	No
relay_log_info_file 	Yes 	Yes 	Yes 	Global 	No
relay_log_info_repository 	  	  	Yes 	Global 	No
relay_log_purge 	Yes 	Yes 	Yes 	Global 	Yes
relay_log_recovery 	Yes 	Yes 	Yes 	Global 	Yes
relay_log_space_limit 	Yes 	Yes 	Yes 	Global 	No
report-host 	Yes 	Yes 	  	  	No
- Variable: report_host 	  	  	Yes 	Global 	No
report-password 	Yes 	Yes 	  	  	No
- Variable: report_password 	  	  	Yes 	Global 	No
report-port 	Yes 	Yes 	  	  	No
- Variable: report_port 	  	  	Yes 	Global 	No
report-user 	Yes 	Yes 	  	  	No
- Variable: report_user 	  	  	Yes 	Global 	No
rpl_semi_sync_master_enabled 	  	  	Yes 	Global 	Yes
rpl_semi_sync_master_timeout 	  	  	Yes 	Global 	Yes
rpl_semi_sync_master_trace_level 	  	  	Yes 	Global 	Yes
rpl_semi_sync_master_wait_no_slave 	  	  	Yes 	Global 	Yes
rpl_semi_sync_slave_enabled 	  	  	Yes 	Global 	Yes
rpl_semi_sync_slave_trace_level 	  	  	Yes 	Global 	Yes
secure-auth 	Yes 	Yes 	  	  	Yes
- Variable: secure_auth 	  	  	Yes 	Global 	Yes
secure-file-priv 	Yes 	Yes 	  	  	No
- Variable: secure_file_priv 	  	  	Yes 	Global 	No
server-id 	Yes 	Yes 	  	  	Yes
- Variable: server_id 	  	  	Yes 	Global 	Yes
server_uuid 	  	  	Yes 	Global 	No
shared_memory 	  	  	Yes 	Global 	No
shared_memory_base_name 	  	  	Yes 	Global 	No
skip-external-locking 	Yes 	Yes 	  	  	No
- Variable: skip_external_locking 	  	  	Yes 	Global 	No
skip-name-resolve 	Yes 	Yes 	  	  	No
- Variable: skip_name_resolve 	  	  	Yes 	Global 	No
skip-networking 	Yes 	Yes 	  	  	No
- Variable: skip_networking 	  	  	Yes 	Global 	No
skip-show-database 	Yes 	Yes 	  	  	No
- Variable: skip_show_database 	  	  	Yes 	Global 	No
slave_compressed_protocol 	Yes 	Yes 	Yes 	Global 	Yes
slave_exec_mode 	  	  	Yes 	Global 	Yes
slave-load-tmpdir 	Yes 	Yes 	  	  	No
- Variable: slave_load_tmpdir 	  	  	Yes 	Global 	No
slave-net-timeout 	Yes 	Yes 	  	  	Yes
- Variable: slave_net_timeout 	  	  	Yes 	Global 	Yes
slave_parallel_workers 	  	  	Yes 	Global 	Yes
slave-skip-errors 	Yes 	Yes 	  	  	No
- Variable: slave_skip_errors 	  	  	Yes 	Global 	No
slave_sql_verify_checksum 	  	  	Yes 	Global 	Yes
slave_transaction_retries 	Yes 	Yes 	Yes 	Global 	Yes
slave_type_conversions 	Yes 	Yes 	Yes 	Global 	No
slow_launch_time 	Yes 	Yes 	Yes 	Global 	Yes
slow-query-log 	Yes 	Yes 	  	  	Yes
- Variable: slow_query_log 	  	  	Yes 	Global 	Yes
slow_query_log_file 	Yes 	Yes 	Yes 	Global 	Yes
socket 	Yes 	Yes 	Yes 	Global 	No
sort_buffer_size 	Yes 	Yes 	Yes 	Both 	Yes
sql_auto_is_null 	  	  	Yes 	Both 	Yes
sql_big_selects 	  	  	Yes 	Both 	Yes
sql_big_tables 	  	  	Yes 	Both 	Yes
sql_buffer_result 	  	  	Yes 	Both 	Yes
sql_log_bin 	  	  	Yes 	Both 	Yes
sql_log_off 	  	  	Yes 	Both 	Yes
sql_low_priority_updates 	  	  	Yes 	Both 	Yes
sql_max_join_size 	  	  	Yes 	Both 	Yes
sql-mode 	Yes 	Yes 	  	  	Yes
- Variable: sql_mode 	  	  	Yes 	Both 	Yes
sql_notes 	  	  	Yes 	Both 	Yes
sql_quote_show_create 	  	  	Yes 	Both 	Yes
sql_safe_updates 	  	  	Yes 	Both 	Yes
sql_select_limit 	  	  	Yes 	Both 	Yes
sql_slave_skip_counter 	  	  	Yes 	Global 	Yes
sql_warnings 	  	  	Yes 	Both 	Yes
ssl-ca 	Yes 	Yes 	  	  	No
- Variable: ssl_ca 	  	  	Yes 	Global 	No
ssl-capath 	Yes 	Yes 	  	  	No
- Variable: ssl_capath 	  	  	Yes 	Global 	No
ssl-cert 	Yes 	Yes 	  	  	No
- Variable: ssl_cert 	  	  	Yes 	Global 	No
ssl-cipher 	Yes 	Yes 	  	  	No
- Variable: ssl_cipher 	  	  	Yes 	Global 	No
ssl-crl 	Yes 	Yes 	  	  	No
- Variable: ssl_crl 	  	  	Yes 	Global 	No
ssl-crlpath 	Yes 	Yes 	  	  	No
- Variable: ssl_crlpath 	  	  	Yes 	Global 	No
ssl-key 	Yes 	Yes 	  	  	No
- Variable: ssl_key 	  	  	Yes 	Global 	No
storage_engine 	  	  	Yes 	Both 	Yes
stored_program_cache 	Yes 	Yes 	Yes 	Global 	Yes
sync_binlog 	Yes 	Yes 	Yes 	Global 	Yes
sync_frm 	Yes 	Yes 	Yes 	Global 	Yes
sync_master_info 	Yes 	Yes 	Yes 	Global 	Yes
sync_relay_log 	Yes 	Yes 	Yes 	Global 	Yes
sync_relay_log_info 	Yes 	Yes 	Yes 	Global 	Yes
system_time_zone 	  	  	Yes 	Global 	No
table_definition_cache 	Yes 	Yes 	Yes 	Global 	Yes
table_open_cache 	Yes 	Yes 	Yes 	Global 	Yes
thread_cache_size 	Yes 	Yes 	Yes 	Global 	Yes
thread_concurrency 	Yes 	Yes 	Yes 	Global 	No
thread_handling 	Yes 	Yes 	Yes 	Global 	No
thread_stack 	Yes 	Yes 	Yes 	Global 	No
time_format 	  	  	Yes 	Global 	No
time_zone 	Yes 	Yes 	Yes 	Both 	Yes
timed_mutexes 	Yes 	Yes 	Yes 	Global 	Yes
timestamp 	  	  	Yes 	Session 	Yes
tmp_table_size 	Yes 	Yes 	Yes 	Both 	Yes
tmpdir 	Yes 	Yes 	Yes 	Global 	No
transaction_alloc_block_size 	Yes 	Yes 	Yes 	Both 	Yes
transaction_prealloc_size 	Yes 	Yes 	Yes 	Both 	Yes
tx_isolation 	  	  	Yes 	Both 	Yes
tx_read_only 	  	  	Yes 	Both 	Yes
unique_checks 	  	  	Yes 	Both 	Yes
updatable_views_with_limit 	Yes 	Yes 	Yes 	Both 	Yes
version 	  	  	Yes 	Global 	No
version_comment 	  	  	Yes 	Global 	No
version_compile_machine 	  	  	Yes 	Global 	No
version_compile_os 	  	  	Yes 	Global 	No
wait_timeout 	Yes 	Yes 	Yes 	Both 	Yes
warning_count 	  	  	Yes 	Session 	No';

$vars = explode("\r\n", $vars);
$i = 0;
foreach($vars as $v)
{
	// auto_increment_increment 	Yes 	Yes 	Yes 	Both 	Yes
	// version_compile_machine 	  	  	Yes 	Global 	No
	preg_match('#^(\-\s+Variable\:\s+)?(\S+)\s{2}(Yes|No|\s)\s{2}(Yes|No|\s)\s{2}(Yes|No|\s)\s{2}(Global|Session|Both|\s)\s{2}(Yes|No|\s)$#', $v, $matches);
	#	var_dump($matches);
	$name = $matches[2];
	$scope = strtoupper(trim($matches[6]));
	if(empty($scope) || strpos($name, '-')!==false)
		continue;
	switch($scope)
	{
		case 'GLOBAL': $varscope='vsGlobal'; break;
		case 'SESSION': $varscope='vsSession'; break;
		case 'BOTH': $varscope='vsBoth'; break;
	}
	echo "    (
      Name: '".$name."';
      IsDynamic: ".($matches[7]=='Yes' ? 'True' : 'False').";
      VarScope: ".$varscope.";
    ),\r\n";
    $i++;
	//	break;
}

echo $i;
?>
