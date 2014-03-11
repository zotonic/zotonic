%%% Copyright (C) 2014 - Andreas Stenius <kaos@astekk.se>.  All rights reserved.
%% Error codes as documented at the PostgreSQL site:
%% http://www.postgresql.org/docs/9.3/interactive/errcodes-appendix.html

-module(pgsql_errors).
-author("Andreas Stenius <kaos@astekk.se>").

-export([describe_code/1]).

describe_code(<<Class:2/binary, Condition:3/binary>>) ->
    describe_code(Class, Condition).

describe_code(<<"00">>, Condition) -> successful_completion(Condition);
describe_code(<<"01">>, Condition) -> warning(Condition);
describe_code(<<"02">>, Condition) -> no_data(Condition);
describe_code(<<"03">>, Condition) -> sql_statement_not_yet_complete(Condition);
describe_code(<<"08">>, Condition) -> connection_exception(Condition);
describe_code(<<"09">>, Condition) -> triggered_action_exception(Condition);
describe_code(<<"0A">>, Condition) -> feature_not_supported(Condition);
describe_code(<<"0B">>, Condition) -> invalid_transaction_initiation(Condition);
describe_code(<<"0F">>, Condition) -> locator_exception(Condition);
describe_code(<<"0L">>, Condition) -> invalid_grantor(Condition);
describe_code(<<"0P">>, Condition) -> invalid_role_specification(Condition);
describe_code(<<"0Z">>, Condition) -> diagnostics_exception(Condition);
describe_code(<<"20">>, Condition) -> case_not_found(Condition);
describe_code(<<"21">>, Condition) -> cardinality_violation(Condition);
describe_code(<<"22">>, Condition) -> data_exception(Condition);
describe_code(<<"23">>, Condition) -> integrity_constraint_violation(Condition);
describe_code(<<"24">>, Condition) -> invalid_cursor_state(Condition);
describe_code(<<"25">>, Condition) -> invalid_transaction_state(Condition);
describe_code(<<"26">>, Condition) -> invalid_sql_statement_name(Condition);
describe_code(<<"27">>, Condition) -> triggered_data_change_violation(Condition);
describe_code(<<"28">>, Condition) -> invalid_authorization_specification(Condition);
describe_code(<<"2B">>, Condition) -> dependent_privilege_descriptors_still_exist(Condition);
describe_code(<<"2D">>, Condition) -> invalid_transaction_termination(Condition);
describe_code(<<"2F">>, Condition) -> sql_routine_exception(Condition);
describe_code(<<"34">>, Condition) -> invalid_cursor_name(Condition);
describe_code(<<"38">>, Condition) -> external_routine_exception(Condition);
describe_code(<<"39">>, Condition) -> external_routine_invocation_exception(Condition);
describe_code(<<"3B">>, Condition) -> savepoint_exception(Condition);
describe_code(<<"3D">>, Condition) -> invalid_catalog_name(Condition);
describe_code(<<"3F">>, Condition) -> invalid_schema_name(Condition);
describe_code(<<"40">>, Condition) -> transaction_rollback(Condition);
describe_code(<<"42">>, Condition) -> syntax_error_or_access_rule_violation(Condition);
describe_code(<<"44">>, Condition) -> with_check_option_violation(Condition);
describe_code(<<"53">>, Condition) -> insufficient_resources(Condition);
describe_code(<<"54">>, Condition) -> program_limit_exceeded(Condition);
describe_code(<<"55">>, Condition) -> object_not_in_prerequisite_state(Condition);
describe_code(<<"57">>, Condition) -> operator_intervention(Condition);
describe_code(<<"58">>, Condition) -> system_error(Condition);
describe_code(<<"F0">>, Condition) -> config_file_error(Condition);
describe_code(<<"HV">>, Condition) -> fdw_error(Condition);
describe_code(<<"P0">>, Condition) -> plpgsql_error(Condition);
describe_code(<<"XX">>, Condition) -> internal_error(Condition);
describe_code(Class, Condition) -> {Class, Condition}.


%% Class 00 — Successful Completion
successful_completion(<<"000">>) -> successful_completion;
successful_completion(Condition) -> {successful_completion, Condition}.

%% Class 01 — Warning
warning(<<"000">>) -> warning;
warning(<<"00C">>) -> {warning, dynamic_result_sets_returned};
warning(<<"008">>) -> {warning, implicit_zero_bit_padding};
warning(<<"003">>) -> {warning, null_value_eliminated_in_set_function};
warning(<<"007">>) -> {warning, privilege_not_granted};
warning(<<"006">>) -> {warning, privilege_not_revoked};
warning(<<"004">>) -> {warning, string_data_right_truncation};
warning(<<"P01">>) -> {warning, deprecated_feature};
warning(Condition) -> {warning, Condition}.

%% Class 02 — No Data (this is also a warning class per the SQL standard)
no_data(<<"000">>) -> no_data;
no_data(<<"001">>) -> {no_data, no_additional_dynamic_result_sets_returned};
no_data(Condition) -> {no_data, Condition}.

%% Class 03 — SQL Statement Not Yet Complete
sql_statement_not_yet_complete(<<"000">>) -> sql_statement_not_yet_complete;
sql_statement_not_yet_complete(Condition) -> {sql_statement_not_yet_complete, Condition}.

%% Class 08 — Connection Exception
connection_exception(<<"000">>) -> connection_exception;
connection_exception(<<"003">>) -> {connection_exception, connection_does_not_exist};
connection_exception(<<"006">>) -> {connection_exception, connection_failure};
connection_exception(<<"001">>) -> {connection_exception, sqlclient_unable_to_establish_sqlconnection};
connection_exception(<<"004">>) -> {connection_exception, sqlserver_rejected_establishment_of_sqlconnection};
connection_exception(<<"007">>) -> {connection_exception, transaction_resolution_unknown};
connection_exception(<<"P01">>) -> {connection_exception, protocol_violation};
connection_exception(Condition) -> {connection_exception, Condition}.

%% Class 09 — Triggered Action Exception
triggered_action_exception(<<"000">>) -> triggered_action_exception;
triggered_action_exception(Condition) -> {triggered_action_exception, Condition}.

%% Class 0A — Feature Not Supported
feature_not_supported(<<"000">>) -> feature_not_supported;
feature_not_supported(Condition) -> {feature_not_supported, Condition}.

%% Class 0B — Invalid Transaction Initiation
invalid_transaction_initiation(<<"000">>) -> invalid_transaction_initiation;
invalid_transaction_initiation(Condition) -> {invalid_transaction_initiation, Condition}.

%% Class 0F — Locator Exception
locator_exception(<<"000">>) -> locator_exception;
locator_exception(<<"001">>) -> {locator_exception, invalid_locator_specification};
locator_exception(Condition) -> {locator_exception, Condition}.

%% Class 0L — Invalid Grantor
invalid_grantor(<<"000">>) -> invalid_grantor;
invalid_grantor(<<"P01">>) -> {invalid_grantor, invalid_grant_operation};
invalid_grantor(Condition) -> {invalid_grantor, Condition}.

%% Class 0P — Invalid Role Specification
invalid_role_specification(<<"000">>) -> invalid_role_specification;
invalid_role_specification(Condition) -> {invalid_role_specification, Condition}.

%% Class 0Z — Diagnostics Exception
diagnostics_exception(<<"000">>) -> diagnostics_exception;
diagnostics_exception(<<"002">>) -> {diagnostics_exception, stacked_diagnostics_accessed_without_active_handler};
diagnostics_exception(Condition) -> {diagnostics_exception, Condition}.

%% Class 20 — Case Not Found
case_not_found(<<"000">>) -> case_not_found;
case_not_found(Condition) -> {case_not_found, Condition}.

%% Class 21 — Cardinality Violation
cardinality_violation(<<"000">>) -> cardinality_violation;
cardinality_violation(Condition) -> {cardinality_violation, Condition}.

%% Class 22 — Data Exception
data_exception(<<"000">>) -> data_exception;
data_exception(<<"02E">>) -> {data_exception, array_subscript_error};
data_exception(<<"021">>) -> {data_exception, character_not_in_repertoire};
data_exception(<<"008">>) -> {data_exception, datetime_field_overflow};
data_exception(<<"012">>) -> {data_exception, division_by_zero};
data_exception(<<"005">>) -> {data_exception, error_in_assignment};
data_exception(<<"00B">>) -> {data_exception, escape_character_conflict};
data_exception(<<"022">>) -> {data_exception, indicator_overflow};
data_exception(<<"015">>) -> {data_exception, interval_field_overflow};
data_exception(<<"01E">>) -> {data_exception, invalid_argument_for_logarithm};
data_exception(<<"014">>) -> {data_exception, invalid_argument_for_ntile_function};
data_exception(<<"016">>) -> {data_exception, invalid_argument_for_nth_value_function};
data_exception(<<"01F">>) -> {data_exception, invalid_argument_for_power_function};
data_exception(<<"01G">>) -> {data_exception, invalid_argument_for_width_bucket_function};
data_exception(<<"018">>) -> {data_exception, invalid_character_value_for_cast};
data_exception(<<"007">>) -> {data_exception, invalid_datetime_format};
data_exception(<<"019">>) -> {data_exception, invalid_escape_character};
data_exception(<<"00D">>) -> {data_exception, invalid_escape_octet};
data_exception(<<"025">>) -> {data_exception, invalid_escape_sequence};
data_exception(<<"P06">>) -> {data_exception, nonstandard_use_of_escape_character};
data_exception(<<"010">>) -> {data_exception, invalid_indicator_parameter_value};
data_exception(<<"023">>) -> {data_exception, invalid_parameter_value};
data_exception(<<"01B">>) -> {data_exception, invalid_regular_expression};
data_exception(<<"01W">>) -> {data_exception, invalid_row_count_in_limit_clause};
data_exception(<<"01X">>) -> {data_exception, invalid_row_count_in_result_offset_clause};
data_exception(<<"009">>) -> {data_exception, invalid_time_zone_displacement_value};
data_exception(<<"00C">>) -> {data_exception, invalid_use_of_escape_character};
data_exception(<<"00G">>) -> {data_exception, most_specific_type_mismatch};
data_exception(<<"004">>) -> {data_exception, null_value_not_allowed};
data_exception(<<"002">>) -> {data_exception, null_value_no_indicator_parameter};
data_exception(<<"003">>) -> {data_exception, numeric_value_out_of_range};
data_exception(<<"026">>) -> {data_exception, string_data_length_mismatch};
data_exception(<<"001">>) -> {data_exception, string_data_right_truncation};
data_exception(<<"011">>) -> {data_exception, substring_error};
data_exception(<<"027">>) -> {data_exception, trim_error};
data_exception(<<"024">>) -> {data_exception, unterminated_c_string};
data_exception(<<"00F">>) -> {data_exception, zero_length_character_string};
data_exception(<<"P01">>) -> {data_exception, floating_point_exception};
data_exception(<<"P02">>) -> {data_exception, invalid_text_representation};
data_exception(<<"P03">>) -> {data_exception, invalid_binary_representation};
data_exception(<<"P04">>) -> {data_exception, bad_copy_file_format};
data_exception(<<"P05">>) -> {data_exception, untranslatable_character};
data_exception(<<"00L">>) -> {data_exception, not_an_xml_document};
data_exception(<<"00M">>) -> {data_exception, invalid_xml_document};
data_exception(<<"00N">>) -> {data_exception, invalid_xml_content};
data_exception(<<"00S">>) -> {data_exception, invalid_xml_comment};
data_exception(<<"00T">>) -> {data_exception, invalid_xml_processing_instruction};
data_exception(Condition) -> {data_exception, Condition}.

%% Class 23 — Integrity Constraint Violation
integrity_constraint_violation(<<"000">>) -> integrity_constraint_violation;
integrity_constraint_violation(<<"001">>) -> {integrity_constraint_violation, restrict_violation};
integrity_constraint_violation(<<"502">>) -> {integrity_constraint_violation, not_null_violation};
integrity_constraint_violation(<<"503">>) -> {integrity_constraint_violation, foreign_key_violation};
integrity_constraint_violation(<<"505">>) -> {integrity_constraint_violation, unique_violation};
integrity_constraint_violation(<<"514">>) -> {integrity_constraint_violation, check_violation};
integrity_constraint_violation(<<"P01">>) -> {integrity_constraint_violation, exclusion_violation};
integrity_constraint_violation(Condition) -> {integrity_constraint_violation, Condition}.

%% Class 24 — Invalid Cursor State
invalid_cursor_state(<<"000">>) -> invalid_cursor_state;
invalid_cursor_state(Condition) -> {invalid_cursor_state, Condition}.

%% Class 25 — Invalid Transaction State
invalid_transaction_state(<<"000">>) -> invalid_transaction_state;
invalid_transaction_state(<<"001">>) -> {invalid_transaction_state, active_sql_transaction};
invalid_transaction_state(<<"002">>) -> {invalid_transaction_state, branch_transaction_already_active};
invalid_transaction_state(<<"008">>) -> {invalid_transaction_state, held_cursor_requires_same_isolation_level};
invalid_transaction_state(<<"003">>) -> {invalid_transaction_state, inappropriate_access_mode_for_branch_transaction};
invalid_transaction_state(<<"004">>) -> {invalid_transaction_state, inappropriate_isolation_level_for_branch_transaction};
invalid_transaction_state(<<"005">>) -> {invalid_transaction_state, no_active_sql_transaction_for_branch_transaction};
invalid_transaction_state(<<"006">>) -> {invalid_transaction_state, read_only_sql_transaction};
invalid_transaction_state(<<"007">>) -> {invalid_transaction_state, schema_and_data_statement_mixing_not_supported};
invalid_transaction_state(<<"P01">>) -> {invalid_transaction_state, no_active_sql_transaction};
invalid_transaction_state(<<"P02">>) -> {invalid_transaction_state, in_failed_sql_transaction};
invalid_transaction_state(Condition) -> {invalid_transaction_state, Condition}.

%% Class 26 — Invalid SQL Statement Name
invalid_sql_statement_name(<<"000">>) -> invalid_sql_statement_name;
invalid_sql_statement_name(Condition) -> {invalid_sql_statement_name, Condition}.

%% Class 27 — Triggered Data Change Violation
triggered_data_change_violation(<<"000">>) -> triggered_data_change_violation;
triggered_data_change_violation(Condition) -> {triggered_data_change_violation, Condition}.

%% Class 28 — Invalid Authorization Specification
invalid_authorization_specification(<<"000">>) -> invalid_authorization_specification;
invalid_authorization_specification(<<"P01">>) -> {invalid_authorization_specification, invalid_password};
invalid_authorization_specification(Condition) -> {invalid_authorization_specification, Condition}.

%% Class 2B — Dependent Privilege Descriptors Still Exist
dependent_privilege_descriptors_still_exist(<<"000">>) -> dependent_privilege_descriptors_still_exist;
dependent_privilege_descriptors_still_exist(<<"P01">>) -> {dependent_privilege_descriptors_still_exist, dependent_objects_still_exist};
dependent_privilege_descriptors_still_exist(Condition) -> {dependent_privilege_descriptors_still_exist, Condition}.

%% Class 2D — Invalid Transaction Termination
invalid_transaction_termination(<<"000">>) -> invalid_transaction_termination;
invalid_transaction_termination(Condition) -> {invalid_transaction_termination, Condition}.

%% Class 2F — SQL Routine Exception
sql_routine_exception(<<"000">>) -> sql_routine_exception;
sql_routine_exception(<<"005">>) -> {sql_routine_exception, function_executed_no_return_statement};
sql_routine_exception(<<"002">>) -> {sql_routine_exception, modifying_sql_data_not_permitted};
sql_routine_exception(<<"003">>) -> {sql_routine_exception, prohibited_sql_statement_attempted};
sql_routine_exception(<<"004">>) -> {sql_routine_exception, reading_sql_data_not_permitted};
sql_routine_exception(Condition) -> {sql_routine_exception, Condition}.

%% Class 34 — Invalid Cursor Name
invalid_cursor_name(<<"000">>) -> invalid_cursor_name;
invalid_cursor_name(Condition) -> {invalid_cursor_name, Condition}.

%% Class 38 — External Routine Exception
external_routine_exception(<<"000">>) -> external_routine_exception;
external_routine_exception(<<"001">>) -> {external_routine_exception, containing_sql_not_permitted};
external_routine_exception(<<"002">>) -> {external_routine_exception, modifying_sql_data_not_permitted};
external_routine_exception(<<"003">>) -> {external_routine_exception, prohibited_sql_statement_attempted};
external_routine_exception(<<"004">>) -> {external_routine_exception, reading_sql_data_not_permitted};
external_routine_exception(Condition) -> {external_routine_exception, Condition}.

%% Class 39 — External Routine Invocation Exception
external_routine_invocation_exception(<<"000">>) -> external_routine_invocation_exception;
external_routine_invocation_exception(<<"001">>) -> {external_routine_invocation_exception, invalid_sqlstate_returned};
external_routine_invocation_exception(<<"004">>) -> {external_routine_invocation_exception, null_value_not_allowed};
external_routine_invocation_exception(<<"P01">>) -> {external_routine_invocation_exception, trigger_protocol_violated};
external_routine_invocation_exception(<<"P02">>) -> {external_routine_invocation_exception, srf_protocol_violated};
external_routine_invocation_exception(Condition) -> {external_routine_invocation_exception, Condition}.

%% Class 3B — Savepoint Exception
savepoint_exception(<<"000">>) -> savepoint_exception;
savepoint_exception(<<"001">>) -> {savepoint_exception, invalid_savepoint_specification};
savepoint_exception(Condition) -> {savepoint_exception, Condition}.

%% Class 3D — Invalid Catalog Name
invalid_catalog_name(<<"000">>) -> invalid_catalog_name;
invalid_catalog_name(Condition) -> {invalid_catalog_name, Condition}.

%% Class 3F — Invalid Schema Name
invalid_schema_name(<<"000">>) -> invalid_schema_name;
invalid_schema_name(Condition) -> {invalid_schema_name, Condition}.

%% Class 40 — Transaction Rollback
transaction_rollback(<<"000">>) -> transaction_rollback;
transaction_rollback(<<"002">>) -> {transaction_rollback, transaction_integrity_constraint_violation};
transaction_rollback(<<"001">>) -> {transaction_rollback, serialization_failure};
transaction_rollback(<<"003">>) -> {transaction_rollback, statement_completion_unknown};
transaction_rollback(<<"P01">>) -> {transaction_rollback, deadlock_detected};
transaction_rollback(Condition) -> {transaction_rollback, Condition}.

%% Class 42 — Syntax Error or Access Rule Violation
syntax_error_or_access_rule_violation(<<"000">>) -> syntax_error_or_access_rule_violation;
syntax_error_or_access_rule_violation(<<"601">>) -> {syntax_error_or_access_rule_violation, syntax_error};
syntax_error_or_access_rule_violation(<<"501">>) -> {syntax_error_or_access_rule_violation, insufficient_privilege};
syntax_error_or_access_rule_violation(<<"846">>) -> {syntax_error_or_access_rule_violation, cannot_coerce};
syntax_error_or_access_rule_violation(<<"803">>) -> {syntax_error_or_access_rule_violation, grouping_error};
syntax_error_or_access_rule_violation(<<"P20">>) -> {syntax_error_or_access_rule_violation, windowing_error};
syntax_error_or_access_rule_violation(<<"P19">>) -> {syntax_error_or_access_rule_violation, invalid_recursion};
syntax_error_or_access_rule_violation(<<"830">>) -> {syntax_error_or_access_rule_violation, invalid_foreign_key};
syntax_error_or_access_rule_violation(<<"602">>) -> {syntax_error_or_access_rule_violation, invalid_name};
syntax_error_or_access_rule_violation(<<"622">>) -> {syntax_error_or_access_rule_violation, name_too_long};
syntax_error_or_access_rule_violation(<<"939">>) -> {syntax_error_or_access_rule_violation, reserved_name};
syntax_error_or_access_rule_violation(<<"804">>) -> {syntax_error_or_access_rule_violation, datatype_mismatch};
syntax_error_or_access_rule_violation(<<"P18">>) -> {syntax_error_or_access_rule_violation, indeterminate_datatype};
syntax_error_or_access_rule_violation(<<"P21">>) -> {syntax_error_or_access_rule_violation, collation_mismatch};
syntax_error_or_access_rule_violation(<<"P22">>) -> {syntax_error_or_access_rule_violation, indeterminate_collation};
syntax_error_or_access_rule_violation(<<"809">>) -> {syntax_error_or_access_rule_violation, wrong_object_type};
syntax_error_or_access_rule_violation(<<"703">>) -> {syntax_error_or_access_rule_violation, undefined_column};
syntax_error_or_access_rule_violation(<<"883">>) -> {syntax_error_or_access_rule_violation, undefined_function};
syntax_error_or_access_rule_violation(<<"P01">>) -> {syntax_error_or_access_rule_violation, undefined_table};
syntax_error_or_access_rule_violation(<<"P02">>) -> {syntax_error_or_access_rule_violation, undefined_parameter};
syntax_error_or_access_rule_violation(<<"704">>) -> {syntax_error_or_access_rule_violation, undefined_object};
syntax_error_or_access_rule_violation(<<"701">>) -> {syntax_error_or_access_rule_violation, duplicate_column};
syntax_error_or_access_rule_violation(<<"P03">>) -> {syntax_error_or_access_rule_violation, duplicate_cursor};
syntax_error_or_access_rule_violation(<<"P04">>) -> {syntax_error_or_access_rule_violation, duplicate_database};
syntax_error_or_access_rule_violation(<<"723">>) -> {syntax_error_or_access_rule_violation, duplicate_function};
syntax_error_or_access_rule_violation(<<"P05">>) -> {syntax_error_or_access_rule_violation, duplicate_prepared_statement};
syntax_error_or_access_rule_violation(<<"P06">>) -> {syntax_error_or_access_rule_violation, duplicate_schema};
syntax_error_or_access_rule_violation(<<"P07">>) -> {syntax_error_or_access_rule_violation, duplicate_table};
syntax_error_or_access_rule_violation(<<"712">>) -> {syntax_error_or_access_rule_violation, duplicate_alias};
syntax_error_or_access_rule_violation(<<"710">>) -> {syntax_error_or_access_rule_violation, duplicate_object};
syntax_error_or_access_rule_violation(<<"702">>) -> {syntax_error_or_access_rule_violation, ambiguous_column};
syntax_error_or_access_rule_violation(<<"725">>) -> {syntax_error_or_access_rule_violation, ambiguous_function};
syntax_error_or_access_rule_violation(<<"P08">>) -> {syntax_error_or_access_rule_violation, ambiguous_parameter};
syntax_error_or_access_rule_violation(<<"P09">>) -> {syntax_error_or_access_rule_violation, ambiguous_alias};
syntax_error_or_access_rule_violation(<<"P10">>) -> {syntax_error_or_access_rule_violation, invalid_column_reference};
syntax_error_or_access_rule_violation(<<"611">>) -> {syntax_error_or_access_rule_violation, invalid_column_definition};
syntax_error_or_access_rule_violation(<<"P11">>) -> {syntax_error_or_access_rule_violation, invalid_cursor_definition};
syntax_error_or_access_rule_violation(<<"P12">>) -> {syntax_error_or_access_rule_violation, invalid_database_definition};
syntax_error_or_access_rule_violation(<<"P13">>) -> {syntax_error_or_access_rule_violation, invalid_function_definition};
syntax_error_or_access_rule_violation(<<"P14">>) -> {syntax_error_or_access_rule_violation, invalid_prepared_statement_definition};
syntax_error_or_access_rule_violation(<<"P15">>) -> {syntax_error_or_access_rule_violation, invalid_schema_definition};
syntax_error_or_access_rule_violation(<<"P16">>) -> {syntax_error_or_access_rule_violation, invalid_table_definition};
syntax_error_or_access_rule_violation(<<"P17">>) -> {syntax_error_or_access_rule_violation, invalid_object_definition};
syntax_error_or_access_rule_violation(Condition) -> {syntax_error_or_access_rule_violation, Condition}.

%% Class 44 — WITH CHECK OPTION Violation
with_check_option_violation(<<"000">>) -> with_check_option_violation;
with_check_option_violation(Condition) -> {with_check_option_violation, Condition}.

%% Class 53 — Insufficient Resources
insufficient_resources(<<"000">>) -> insufficient_resources;
insufficient_resources(<<"100">>) -> {insufficient_resources, disk_full};
insufficient_resources(<<"200">>) -> {insufficient_resources, out_of_memory};
insufficient_resources(<<"300">>) -> {insufficient_resources, too_many_connections};
insufficient_resources(<<"400">>) -> {insufficient_resources, configuration_limit_exceeded};
insufficient_resources(Condition) -> {insufficient_resources, Condition}.

%% Class 54 — Program Limit Exceeded
program_limit_exceeded(<<"000">>) -> program_limit_exceeded;
program_limit_exceeded(<<"001">>) -> {program_limit_exceeded, statement_too_complex};
program_limit_exceeded(<<"011">>) -> {program_limit_exceeded, too_many_columns};
program_limit_exceeded(<<"023">>) -> {program_limit_exceeded, too_many_arguments};
program_limit_exceeded(Condition) -> {program_limit_exceeded, Condition}.

%% Class 55 — Object Not In Prerequisite State
object_not_in_prerequisite_state(<<"000">>) -> object_not_in_prerequisite_state;
object_not_in_prerequisite_state(<<"006">>) -> {object_not_in_prerequisite_state, object_in_use};
object_not_in_prerequisite_state(<<"P02">>) -> {object_not_in_prerequisite_state, cant_change_runtime_param};
object_not_in_prerequisite_state(<<"P03">>) -> {object_not_in_prerequisite_state, lock_not_available};
object_not_in_prerequisite_state(Condition) -> {object_not_in_prerequisite_state, Condition}.

%% Class 57 — Operator Intervention
operator_intervention(<<"000">>) -> operator_intervention;
operator_intervention(<<"014">>) -> {operator_intervention, query_canceled};
operator_intervention(<<"P01">>) -> {operator_intervention, admin_shutdown};
operator_intervention(<<"P02">>) -> {operator_intervention, crash_shutdown};
operator_intervention(<<"P03">>) -> {operator_intervention, cannot_connect_now};
operator_intervention(<<"P04">>) -> {operator_intervention, database_dropped};
operator_intervention(Condition) -> {operator_intervention, Condition}.

%% Class 58 — System Error (errors external to PostgreSQL itself)
system_error(<<"000">>) -> system_error;
system_error(<<"030">>) -> {system_error, io_error};
system_error(<<"P01">>) -> {system_error, undefined_file};
system_error(<<"P02">>) -> {system_error, duplicate_file};
system_error(Condition) -> {system_error, Condition}.

%% Class F0 — Configuration File Error
config_file_error(<<"000">>) -> config_file_error;
config_file_error(<<"001">>) -> {config_file_error, lock_file_exists};
config_file_error(Condition) -> {config_file_error, Condition}.

%% Class HV — Foreign Data Wrapper Error (SQL/MED)
fdw_error(<<"000">>) -> fdw_error;
fdw_error(<<"005">>) -> {fdw_error, fdw_column_name_not_found};
fdw_error(<<"002">>) -> {fdw_error, fdw_dynamic_parameter_value_needed};
fdw_error(<<"010">>) -> {fdw_error, fdw_function_sequence_error};
fdw_error(<<"021">>) -> {fdw_error, fdw_inconsistent_descriptor_information};
fdw_error(<<"024">>) -> {fdw_error, fdw_invalid_attribute_value};
fdw_error(<<"007">>) -> {fdw_error, fdw_invalid_column_name};
fdw_error(<<"008">>) -> {fdw_error, fdw_invalid_column_number};
fdw_error(<<"004">>) -> {fdw_error, fdw_invalid_data_type};
fdw_error(<<"006">>) -> {fdw_error, fdw_invalid_data_type_descriptors};
fdw_error(<<"091">>) -> {fdw_error, fdw_invalid_descriptor_field_identifier};
fdw_error(<<"00B">>) -> {fdw_error, fdw_invalid_handle};
fdw_error(<<"00C">>) -> {fdw_error, fdw_invalid_option_index};
fdw_error(<<"00D">>) -> {fdw_error, fdw_invalid_option_name};
fdw_error(<<"090">>) -> {fdw_error, fdw_invalid_string_length_or_buffer_length};
fdw_error(<<"00A">>) -> {fdw_error, fdw_invalid_string_format};
fdw_error(<<"009">>) -> {fdw_error, fdw_invalid_use_of_null_pointer};
fdw_error(<<"014">>) -> {fdw_error, fdw_too_many_handles};
fdw_error(<<"001">>) -> {fdw_error, fdw_out_of_memory};
fdw_error(<<"00P">>) -> {fdw_error, fdw_no_schemas};
fdw_error(<<"00J">>) -> {fdw_error, fdw_option_name_not_found};
fdw_error(<<"00K">>) -> {fdw_error, fdw_reply_handle};
fdw_error(<<"00Q">>) -> {fdw_error, fdw_schema_not_found};
fdw_error(<<"00R">>) -> {fdw_error, fdw_table_not_found};
fdw_error(<<"00L">>) -> {fdw_error, fdw_unable_to_create_execution};
fdw_error(<<"00M">>) -> {fdw_error, fdw_unable_to_create_reply};
fdw_error(<<"00N">>) -> {fdw_error, fdw_unable_to_establish_connection};
fdw_error(Condition) -> {fdw_error, Condition}.

%% Class P0 — PL/pgSQL Error
plpgsql_error(<<"000">>) -> plpgsql_error;
plpgsql_error(<<"001">>) -> {plpgsql_error, raise_exception};
plpgsql_error(<<"002">>) -> {plpgsql_error, no_data_found};
plpgsql_error(<<"003">>) -> {plpgsql_error, too_many_rows};
plpgsql_error(Condition) -> {plpgsql_error, Condition}.

%% Class XX — Internal Error
internal_error(<<"000">>) -> internal_error;
internal_error(<<"001">>) -> {internal_error, data_corrupted};
internal_error(<<"002">>) -> {internal_error, index_corrupted};
internal_error(Condition) -> {internal_error, Condition}.
