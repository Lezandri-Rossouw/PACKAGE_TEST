**"* use this source file for your ABAP unit test classes
*CLASS lcl_td_param_mapping_badi DEFINITION FINAL CREATE PUBLIC.
*
*  PUBLIC SECTION.
*
*    INTERFACES /egs/if_prs_param_mapping.
*
*ENDCLASS.
*
*CLASS lcl_td_param_mapping_badi IMPLEMENTATION.
*
*  METHOD /egs/if_prs_param_mapping~param_mapping.
*    CASE is_as-funco.
*
*      WHEN '_XX'.
*        CASE is_as-parm1.
*          WHEN 'NET'.
*            rv_result = 'SA/NET'.
*          WHEN 'TAX'.
*            rv_result = 'SA/PAYE'.
*          WHEN 'UIF'.
*            rv_result = 'SA/UIF'.
*
*        ENDCASE.
*    ENDCASE.
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS lcl_plan_setup DEFINITION FINAL CREATE PUBLIC.
*  PUBLIC SECTION.
*    INTERFACES /egs/if_prs_calc_plan_builder.
*
*    ALIASES composite_builder FOR /egs/if_prs_calc_plan_builder~composite_builder.
*
*ENDCLASS.
*
*CLASS lcl_plan_setup IMPLEMENTATION.
*
*  METHOD /egs/if_prs_calc_plan_builder~composite_builder.
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " SETUP INPUT VALUES AND REFERENCES
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
*    DATA:
*      lv_distribution_unit_value TYPE decfloat16,
*      " TODO: variable is assigned but never used (ABAP cleaner)
*      lo_value_fa                TYPE REF TO /egs/cl_enc_fin_amount,
*      lo_age                     TYPE REF TO /egs/cl_enc_fin_amount,
*      lv_input_value             TYPE decfloat16 VALUE 12000,
*      " TODO: variable is assigned but never used (ABAP cleaner)
*      lo_current_period_fa       TYPE REF TO /egs/cl_enc_fin_amount,
*      lv_current_period          TYPE i                                                        VALUE 1,
*      " TODO: variable is assigned but never used (ABAP cleaner)
*      lo_target_period_fa        TYPE REF TO /egs/cl_enc_fin_amount,
*      lv_target_period           TYPE i                                                        VALUE 12,
*      " TODO: variable is assigned but never used (ABAP cleaner)
*      lo_distribution_unit       TYPE REF TO /egs/cl_enc_fin_amount,
*      lr_tax_tiers               TYPE REF TO /egs/cl_util_calculations=>ty_tab_graduated_table,
*      lr_tax_rebates_tiers       TYPE REF TO /egs/cl_util_calculations=>ty_tab_graduated_table.
*    DATA lt_commands TYPE TABLE OF REF TO /egs/cl_cmd_base.
*
*    lv_distribution_unit_value = CONV #( '0.01' ).
*    lo_value_fa = /egs/cl_enc_fin_amount=>create( lv_input_value ).
*    lo_age = /egs/cl_enc_fin_amount=>create( 32 ).
*    lo_current_period_fa = /egs/cl_enc_fin_amount=>create( lv_current_period ).
*    lo_target_period_fa = /egs/cl_enc_fin_amount=>create( lv_target_period ).
*    lo_distribution_unit = /egs/cl_enc_fin_amount=>create( lv_distribution_unit_value ).
*
*    " UIF CALCULATION VALUES
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA(lo_monthly_period_count_units) = NEW /egs/cl_enc_number( 12 ).
*    DATA(lo_uif_ceiling_units) = NEW /egs/cl_enc_number( 17712 ).
*
*    DATA(lo_uif_rate_units) = NEW /egs/cl_enc_number( 1 ).
*    " TAX CALCULATION VALUES
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    CREATE DATA lr_tax_tiers.
*    lr_tax_tiers->* = VALUE /egs/cl_util_calculations=>ty_tab_graduated_table( ( min = 1817000 value = 45 )
*                                                                               ( min = 857900  value = 41 )
*                                                                               ( min = 673000  value = 39 )
*                                                                               ( min = 512800  value = 36 )
*                                                                               ( min = 370500  value = 31 )
*                                                                               ( min = 237100  value = 26 )
*                                                                               ( min = 0       value = 18 ) ).
*
*    CREATE DATA lr_tax_rebates_tiers.
*    lr_tax_rebates_tiers->* = VALUE /egs/cl_util_calculations=>ty_tab_graduated_table( ( min = 75  value = 3145 )
*                                                                                       ( min = 65  value = 9444 )
*                                                                                       ( min = 0   value = 17235 ) ).
*
*    " END SETUP INPUT VALUES AND REFERENCES
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
*    lt_commands =
*      VALUE #( ( NEW
*                 /egs/cl_cmd_verify_entry( 'AGE' ) )
*
*               ( NEW /egs/cl_cmd_verify_entry( 'PERIODIC_GROSS_SALARY' ) )
*
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = 'CURRENT_PERIOD'
*                                         iv_data = lo_current_period_fa ) )
*
*                              ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = 'TARGET_PERIOD'
*                                         iv_data = lo_target_period_fa ) )
*
*
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = 'DIST_UNIT'
*                                         iv_data = lo_distribution_unit ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = 'TAX_TIERS'
*                                         iv_data = lr_tax_tiers->* ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = 'TAX_REBATES_TIERS'
*                                         iv_data = lr_tax_rebates_tiers->* ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = 'UIF_CEILING'
*                                         iv_data = lo_uif_ceiling_units ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = 'UIF_RATE'
*                                         iv_data = lo_uif_rate_units ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key           = 'PERIODIC_GROSS_SALARY'
*                                      iv_current_period_key  = 'CURRENT_PERIOD'
*                                      iv_target_period_key   = 'TARGET_PERIOD'
*                                      iv_projected_value_key = 'ANNUAL_SALARY'
*                                      iv_distribution_unit_key = 'DIST_UNIT' ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key           = 'ANNUAL_SALARY'
*                                      iv_current_period_key  = 'TARGET_PERIOD'
*                                      iv_target_period_key   = 'CURRENT_PERIOD'
*                                      iv_projected_value_key = 'MONTHLY_SALARY'
*                                      iv_distribution_unit_key = 'DIST_UNIT' ) )
*
*               ( NEW
*                 /egs/cl_cmd_ltd_percentage( iv_result_key     = 'MONTHLY_UIF'
*                                             iv_value_key      = 'MONTHLY_SALARY'
*                                             iv_percentage_key = 'UIF_RATE'
*                                             iv_ceiling_key    = 'UIF_CEILING' ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key           = 'MONTHLY_UIF'
*                                      iv_current_period_key  = 'CURRENT_PERIOD'
*                                      iv_target_period_key   = 'TARGET_PERIOD'
*                                      iv_projected_value_key = 'ANNUAL_UIF'
*                                      iv_distribution_unit_key = 'DIST_UNIT' ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key           = 'ANNUAL_UIF'
*                                      iv_current_period_key  = 'TARGET_PERIOD'
*                                      iv_target_period_key   = 'CURRENT_PERIOD'
*                                      iv_projected_value_key = 'PERIODIC_UIF'
*                                      iv_distribution_unit_key = 'DIST_UNIT' ) )
*
*               ( NEW
*                 /egs/cl_cmd_sum_tiers( iv_result_key = 'TAX_REBATES'
*                                        iv_value_key  = 'AGE'
*                                        iv_tiers_key  = 'TAX_REBATES_TIERS' ) )
*
*               ( NEW
*                 /egs/cl_cmd_tax_by_tiers( iv_result_key = 'ANNUAL_GROSS_TAX'
*                                           iv_value_key  = 'ANNUAL_SALARY'
*                                           iv_tiers_key  = 'TAX_TIERS' ) )
*
*               ( NEW
*                 /egs/cl_cmd_floored_diff( iv_result_key = 'ANNUAL_NET_PAYE'
*                                           iv_num1_key   = 'ANNUAL_GROSS_TAX'
*                                           iv_num2_key   = 'TAX_REBATES' ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key           = 'ANNUAL_NET_PAYE'
*                                      iv_current_period_key  = 'CURRENT_PERIOD'
*                                      iv_target_period_key   = 'TARGET_PERIOD'
*                                      iv_projected_value_key = 'PERIODIC_NET_PAYE'
*                                      iv_distribution_unit_key = 'DIST_UNIT' ) )
*
*               ( NEW
*                 /egs/cl_cmd_deductions( iv_result_key      = 'PERIODIC_NET_SALARY'
*                                         iv_num1_key        = 'PERIODIC_GROSS_SALARY'
*                                         it_deductions_keys = VALUE /egs/if_cmd_base=>ty_string_table(
*                                                                        ( CONV string( 'PERIODIC_NET_PAYE' ) )
*                                                                        ( CONV string( 'PERIODIC_UIF' ) ) ) ) )
*
*               ( NEW /egs/cl_cmd_fetch_data( 'PERIODIC_NET_SALARY' ) )
*
*               ( NEW /egs/cl_cmd_fetch_data( 'PERIODIC_NET_PAYE' ) )
*
*               ( NEW /egs/cl_cmd_fetch_data( 'PERIODIC_UIF' ) ) ).
*
*    ro_result = NEW #( iv_list_name = 'SA NET SALARY CALCULATION'
*                       it_children  = lt_commands ).
*  ENDMETHOD.
*
*ENDCLASS.
*
*CLASS lcl_td_any_country_net DEFINITION FINAL CREATE PUBLIC.
*
*  PUBLIC SECTION.
*    CONSTANTS: c_fixed_income_key TYPE string VALUE 'FIXED INCOME VALUES',
*               c_salary_name      TYPE string VALUE 'SALARY'.
*    INTERFACES /egs/if_prs_input_mapping.
*    METHODS: set_id
*      IMPORTING
*        iv_id TYPE int4.
*  PRIVATE SECTION.
*    DATA: mv_id TYPE int4.
*
*
*
*ENDCLASS.
*
*CLASS lcl_td_any_country_net IMPLEMENTATION.
*
*  METHOD /egs/if_prs_input_mapping~get_amount_data.
*    CONSTANTS c_amount TYPE decfloat34 VALUE 1000.
*
*    DATA: lt_wdt        TYPE /egs/cl_util_fin_calculations=>ty_tab_weight_distribution,
*          lt_named_amts TYPE /egs/if_prs_calc_adaptor=>ty_tab_named_amount_validity.
*
*    " Setup Amounts
*
*    " lt_named_amounts TYPE /egs/if_prs_calc_adaptor=>ty_tab_mapped_amt_validity.
*
*    lt_wdt = VALUE #( weight = 1
*                      ( date = '20240110' )
*                      ( date = '20240120' )
*                      ( date = '20240130' ) ).
*
*    lt_named_amts = VALUE #( ( amount           = /egs/cl_enc_fin_amount=>create( c_amount )
*                               begin_date       = '20240101'
*                               end_date         = '20240201'
*                               weight_distr_tab = lt_wdt
*                               name             = c_salary_name ) ).
*
*    et_amount_map = VALUE #( ( key = c_fixed_income_key name = c_salary_name ) ).
*    et_weighted_income = lt_named_amts.
*  ENDMETHOD.
*
*  METHOD /egs/if_prs_input_mapping~get_dynamic_data.
*    CONSTANTS c_age TYPE i VALUE 21.
*
*    ro_param_store = NEW #( ).
*    ro_param_store->add_entry( iv_data = c_age
*                               iv_key  = 'AGE' ).
*  ENDMETHOD.
*
*  METHOD /egs/if_prs_input_mapping~get_metric_data.
*    DATA ls_metric TYPE /egs/if_prs_input_mapping=>ty_struc_metric.
*
*    ls_metric = VALUE #( key        = 'PERIODIC_NET_SALARY'
*                         start_date = '20240101'
*                         end_date   = '20240201'
*                         value      = REF #( 'SALARY' ) ) ##no_text.
*
*    INSERT ls_metric INTO TABLE rt_metric_table.
*  ENDMETHOD.
*
*  METHOD /egs/if_prs_input_mapping~get_period_data.
*    ev_payroll_period_begin_date = '20240101'. "  Disclosure / Reporting period
*    ev_payroll_period_end_date = '20250101'.   "
*    ev_calc_period_begin_date = '20240101'.    " Payroll calculation period / Processing period
*    ev_calc_period_end_date = '20240201'.      "
*    et_period_dates = VALUE #( ).
*  ENDMETHOD.
*
*  METHOD /egs/if_prs_input_mapping~get_personnel_data.
*    rv_personnel_data = mv_id.
*  ENDMETHOD.
*
*  METHOD /egs/if_prs_input_mapping~get_plan_name.
*    DATA(lo_test_class) = NEW lcl_plan_setup( ).
*
*    DATA(lo_class_desc) = cl_abap_classdescr=>describe_by_object_ref( lo_test_class ).
*
*    rv_class_name = lo_class_desc->get_relative_name( ).
*  ENDMETHOD.
*
*  METHOD /egs/if_prs_input_mapping~get_storage_branch_name.
*    rv_result = 'SA'.
*  ENDMETHOD.
*
*  METHOD /egs/if_prs_input_mapping~map_global_names_to_keys.
*    rt_global_datamap = VALUE #(
*           ( global_var_name = 'iv_test_variable' datastore_key = 'TEST KEY' is_changeable = abap_true ) ) ##no_text.
*  ENDMETHOD.
*
*  METHOD /egs/if_prs_input_mapping~update_globals_with_results.
*    RETURN.
*  ENDMETHOD.
*
*  METHOD set_id.
*    mv_id = iv_id.
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*CLASS ltcl_performance DEFINITION DEFERRED.
*CLASS /egs/cl_performance_test DEFINITION LOCAL FRIENDS ltcl_performance.
*
*CLASS ltcl_performance DEFINITION FINAL FOR TESTING
*  DURATION LONG
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    DATA: mo_cut           TYPE REF TO /egs/cl_performance_test,
*          mo_store         TYPE REF TO /egs/cl_bts_store,
*          mo_head_worker   TYPE REF TO /egs/cl_enc_string,
*          mo_eot           TYPE REF TO /egs/cl_enc_end_of_time,
*          mo_salary_arr    TYPE REF TO /egs/cl_enc_array,
*          mo_match_pattern TYPE REF TO /egs/cl_enc_array,
*          mo_worker_id_arr TYPE REF TO /egs/cl_enc_array.
*
*    METHODS:
*      setup                        RAISING cx_static_check,
*      test_5 FOR TESTING RAISING cx_static_check,
*      match_query_0    ,
*      match_query_1    ,
*      match_query_2 ,
*      match_query_3 ,
*      match_query_4 ,
*      match_query_5 ,
*      match_query_6 ,
*      match_query_7 .
*ENDCLASS.
*
*CLASS ltcl_performance IMPLEMENTATION.
*  METHOD setup.
*    DATA: lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000'.
*    DATA(lo_initial_now) = NEW /egs/cl_enc_timestamp( lv_start_ts ).
*    mo_cut = NEW #( NEW /egs/cl_prs_calc_adaptor( ) ).
*    mo_head_worker = NEW #( 'WORKER' ).
*    mo_eot = /egs/cl_enc_end_of_time=>get_instance( ).
*    mo_salary_arr = NEW #( ).
*    mo_salary_arr->push( 10000 ).
*    mo_worker_id_arr = NEW #( ).
*    mo_match_pattern  = NEW #( ).
*
*    mo_store = NEW /egs/cl_bts_store( ii_clock = NEW /egs/cl_bts_sim_clock( lo_initial_now )
*                                   ii_encoder = NEW /egs/cl_enc_encoder( ) ).
*  ENDMETHOD.
*
*  METHOD match_query_0.
*    DATA: lv_contract_strt     TYPE timestampl VALUE '20240601000000.0000000',
*          lv_start_timespan_ts TYPE timestampl VALUE '20240701000000.0000000',
*          lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000'.
*
*    mo_store->create_branch( io_branch_name = NEW /egs/cl_enc_string( 'DEMO' )
*                            io_tx_time     = NEW /egs/cl_enc_timestamp( lv_start_ts ) ).
*    DATA(lo_slots_worker_1) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_1->push( 232 ).
*
*    DATA(lo_builder) = mo_store->build_transaction( ).
*
*    lo_builder->specify_id_key( io_head = mo_head_worker  io_slots = lo_slots_worker_1 ).
*
*    lo_builder->add_split( io_validity_range = NEW /egs/cl_enc_temp_range( io_from_ts = NEW /egs/cl_enc_timestamp( lv_contract_strt )
*                                                                           io_to_ts = mo_eot )
*                           io_data           = mo_salary_arr ).
*
*    lo_builder->done( ).
*
*  ENDMETHOD.
*
*  METHOD match_query_1.
*    DATA: lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000',
*          lv_start_timespan_ts TYPE timestampl VALUE '20240701000000.0000000'.
*
*    mo_store->set_branch( io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                      io_tx_time     = NEW /egs/cl_enc_timestamp( lv_start_ts ) ).
*    DATA(lo_slots_worker_1) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_1->push( 232 ).
*
*    mo_worker_id_arr->push( mo_head_worker ).
*    mo_worker_id_arr->push( lo_slots_worker_1 ).
*    mo_match_pattern->push( mo_worker_id_arr ).
*
*
*    DATA(lv_query_result) = mo_store->get_query_ref_asof( io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                                                          io_asof_ts     = NEW /egs/cl_enc_timestamp( lv_start_timespan_ts ) ).
*
*    DATA(lt_result) = mo_store->match_query( iv_query_ref         = lv_query_result
*                                                     io_match_patterns    = mo_match_pattern
*                                                     iv_include_timelines = abap_true
*                                                     iv_include_bindings  = abap_true ).
*
*  ENDMETHOD.
*
*  METHOD match_query_2.
*    DATA: lv_contract_strt     TYPE timestampl VALUE '20240701000000.0000000',
*          lv_start_timespan_ts TYPE timestampl VALUE '20240801000000.0000000',
*          lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000'.
*
*    mo_store->set_branch( io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                          io_tx_time     = NEW /egs/cl_enc_timestamp( lv_start_ts ) ).
*    DATA(lo_slots_worker_2) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_2->push( 434 ).
*
*    DATA(lo_builder) = mo_store->build_transaction( ).
*
*    lo_builder->specify_id_key( io_head  = mo_head_worker
*                                io_slots = lo_slots_worker_2 ).
*
*    lo_builder->add_split(
*        io_validity_range = NEW /egs/cl_enc_temp_range( io_from_ts = NEW /egs/cl_enc_timestamp( lv_contract_strt )
*                                                        io_to_ts   = mo_eot )
*        io_data           = mo_salary_arr ).
*
*    lo_builder->done( ).
*
*
*
*  ENDMETHOD.
*
*  METHOD match_query_3.
*    DATA: lv_contract_strt     TYPE timestampl VALUE '20240801000000.0000000',
*          lv_start_timespan_ts TYPE timestampl VALUE '20240801000000.0000000',
*          lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000'.
*    DATA(lo_slots_worker_2) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_2->push( 434 ).
*    mo_store->set_branch( io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                          io_tx_time     = NEW /egs/cl_enc_timestamp( lv_start_ts ) ).
*
*    mo_worker_id_arr->clear( ).
*    mo_match_pattern->clear( ).
*    mo_worker_id_arr->push( mo_head_worker ).
*    mo_worker_id_arr->push( lo_slots_worker_2 ).
*    mo_match_pattern->push( mo_worker_id_arr ).
*
*    DATA(lv_query_result) = mo_store->get_query_ref_asof(
*                                io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                                io_asof_ts     = NEW /egs/cl_enc_timestamp( lv_start_timespan_ts ) ).
*
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA(lt_result) = mo_store->match_query( iv_query_ref         = lv_query_result
*                                             io_match_patterns    = mo_match_pattern
*                                             iv_include_timelines = abap_true
*                                             iv_include_bindings  = abap_true ).
*
*  ENDMETHOD.
*
*  METHOD match_query_4.
*    DATA: lv_contract_strt     TYPE timestampl VALUE '20240801000000.0000000',
*          lv_start_timespan_ts TYPE timestampl VALUE '20240901000000.0000000',
*          lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000'.
*
*    mo_store->set_branch( io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                          io_tx_time     = NEW /egs/cl_enc_timestamp( lv_start_ts ) ).
*    DATA(lo_slots_worker_3) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_3->push( 555 ).
*
*    DATA(lo_builder) = mo_store->build_transaction( ).
*
*    lo_builder->specify_id_key( io_head  = mo_head_worker
*                                io_slots = lo_slots_worker_3 ).
*
*    lo_builder->add_split(
*        io_validity_range = NEW /egs/cl_enc_temp_range( io_from_ts = NEW /egs/cl_enc_timestamp( lv_contract_strt )
*                                                        io_to_ts   = mo_eot )
*        io_data           = mo_salary_arr ).
*
*    lo_builder->done( ).
*  ENDMETHOD.
*
*  METHOD match_query_5.
*    DATA: lv_contract_strt     TYPE timestampl VALUE '20240701000000.0000000',
*          lv_start_timespan_ts TYPE timestampl VALUE '20240901000000.0000000',
*          lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000'.
*    DATA(lo_slots_worker_2) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_2->push( 555 ).
*    mo_store->set_branch( io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                          io_tx_time     = NEW /egs/cl_enc_timestamp( lv_start_ts ) ).
*
*    mo_worker_id_arr->clear( ).
*    mo_match_pattern->clear( ).
*    mo_worker_id_arr->push( mo_head_worker ).
*    mo_worker_id_arr->push( lo_slots_worker_2 ).
*    mo_match_pattern->push( mo_worker_id_arr ).
*
*    DATA(lv_query_result) = mo_store->get_query_ref_asof(
*                                io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                                io_asof_ts     = NEW /egs/cl_enc_timestamp( lv_start_timespan_ts ) ).
*
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA(lt_result) = mo_store->match_query( iv_query_ref         = lv_query_result
*                                             io_match_patterns    = mo_match_pattern
*                                             iv_include_timelines = abap_true
*                                             iv_include_bindings  = abap_true ).
*
*  ENDMETHOD.
*
*  METHOD match_query_6.
*    DATA: lv_contract_strt     TYPE timestampl VALUE '20240901000000.0000000',
*          lv_start_timespan_ts TYPE timestampl VALUE '20240901000000.0000000',
*          lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000'.
*
*    mo_store->set_branch( io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                          io_tx_time     = NEW /egs/cl_enc_timestamp( lv_start_ts ) ).
*    DATA(lo_slots_worker_3) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_3->push( 888 ).
*
*    DATA(lo_builder) = mo_store->build_transaction( ).
*
*    lo_builder->specify_id_key( io_head  = mo_head_worker
*                                io_slots = lo_slots_worker_3 ).
*
*    lo_builder->add_split(
*        io_validity_range = NEW /egs/cl_enc_temp_range( io_from_ts = NEW /egs/cl_enc_timestamp( lv_contract_strt )
*                                                        io_to_ts   = mo_eot )
*        io_data           = mo_salary_arr ).
*
*    lo_builder->done( ).
*
*    DATA(lo_slots_worker_4) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_4->push( 775 ).
*
*    lo_builder = mo_store->build_transaction( ).
*
*    lo_builder->specify_id_key( io_head  = mo_head_worker
*                                io_slots = lo_slots_worker_4 ).
*
*    lo_builder->add_split(
*        io_validity_range = NEW /egs/cl_enc_temp_range( io_from_ts = NEW /egs/cl_enc_timestamp( lv_contract_strt )
*                                                        io_to_ts   = mo_eot )
*        io_data           = mo_salary_arr ).
*
*    lo_builder->done( ).
*
*    DATA(lo_slots_worker_5) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_5->push( 999 ).
*
*    lo_builder = mo_store->build_transaction( ).
*
*    lo_builder->specify_id_key( io_head  = mo_head_worker
*                                io_slots = lo_slots_worker_5 ).
*
*    lo_builder->add_split(
*        io_validity_range = NEW /egs/cl_enc_temp_range( io_from_ts = NEW /egs/cl_enc_timestamp( lv_contract_strt )
*                                                        io_to_ts   = mo_eot )
*        io_data           = mo_salary_arr ).
*
*    lo_builder->done( ).
*
*    DATA(lo_slots_worker_6) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_6->push( 412 ).
*
*    lo_builder = mo_store->build_transaction( ).
*
*    lo_builder->specify_id_key( io_head  = mo_head_worker
*                                io_slots = lo_slots_worker_6 ).
*
*    lo_builder->add_split(
*        io_validity_range = NEW /egs/cl_enc_temp_range( io_from_ts = NEW /egs/cl_enc_timestamp( lv_contract_strt )
*                                                        io_to_ts   = mo_eot )
*        io_data           = mo_salary_arr ).
*
*    lo_builder->done( ).
*
*    DATA(lo_slots_worker_7) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_6->push( 211 ).
*
*    lo_builder = mo_store->build_transaction( ).
*
*    lo_builder->specify_id_key( io_head  = mo_head_worker
*                                io_slots = lo_slots_worker_7 ).
*
*    lo_builder->add_split(
*        io_validity_range = NEW /egs/cl_enc_temp_range( io_from_ts = NEW /egs/cl_enc_timestamp( lv_contract_strt )
*                                                        io_to_ts   = mo_eot )
*        io_data           = mo_salary_arr ).
*
*    lo_builder->done( ).
*  ENDMETHOD.
*
*
*  METHOD match_query_7.
*    DATA: lv_contract_strt     TYPE timestampl VALUE '20240701000000.0000000',
*          lv_start_timespan_ts TYPE timestampl VALUE '20241001000000.0000000',
*          lv_start_ts          TYPE timestampl VALUE '20240508000000.0000000'.
*    DATA(lo_slots_worker_2) = NEW /egs/cl_enc_array( ).
*    lo_slots_worker_2->push( 888 ).
*    mo_store->set_branch( io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                          io_tx_time     = NEW /egs/cl_enc_timestamp( lv_start_ts ) ).
*
*    mo_worker_id_arr->clear( ).
*    mo_match_pattern->clear( ).
*    mo_worker_id_arr->push( mo_head_worker ).
*    mo_worker_id_arr->push( lo_slots_worker_2 ).
*    mo_match_pattern->push( mo_worker_id_arr ).
*
*    DATA(lv_query_result) = mo_store->get_query_ref_asof(
*                                io_branch_name = NEW /egs/cl_enc_string( 'SIMULATED-DEMO' )
*                                io_asof_ts     = NEW /egs/cl_enc_timestamp( lv_start_timespan_ts ) ).
*
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA(lt_result) = mo_store->match_query( iv_query_ref         = lv_query_result
*                                             io_match_patterns    = mo_match_pattern
*                                             iv_include_timelines = abap_true
*                                             iv_include_bindings  = abap_true ).
*  ENDMETHOD.
*
*  METHOD test_5.
*    DATA: li_tb_badi_isolate      TYPE REF TO /egs/if_util_isolate_badi,
*          li_td_mock_initial_badi TYPE REF TO /egs/if_prs_input_mapping,
*          lt_filter_table         TYPE /egs/if_util_isolate_badi=>ty_t_filter,
*          lr_map_name             TYPE REF TO data,
*          lo_config_store         TYPE REF TO /egs/cl_util_paramstore,
*          lv_id                   TYPE int4.
*
*    " Mock Input Mapping
*    " Test double setup
*    li_tb_badi_isolate ?= cl_abap_testdouble=>create( '/EGS/IF_UTIL_ISOLATE_BADI' ).
*    DATA(ls_schema_parmeters) = VALUE pcal_as( funco = '_XX'
*                                               parm1 = 'NET' ).
*    mo_cut->mo_calc_adaptor->setup_ds_mapping_badi_filter( ls_schema_parmeters ).
*    DO 1 TIMES.
*
*      lv_id = sy-index.
*
*      DATA(lo_td_net) = NEW lcl_td_any_country_net( ).
*      lo_td_net->set_id( lv_id ).
*      DATA(li_td_datastore_mapping) = CAST /egs/if_prs_input_mapping( lo_td_net ).
*
*
*      " Setup Datastore mapping BADI Filter
*      " Setup DS Mapping BADI using Mapping BADI Filter
*      " Get Globals Mapping BADI
*
*      DATA(li_mock_param_mapping) = CAST /egs/if_prs_param_mapping( NEW lcl_td_param_mapping_badi( ) ).
*
*
*
*
*      DATA(lo_td_param_mapping_td) = NEW lcl_td_param_mapping_badi( ).
*
*      cl_abap_testdouble=>configure_call( li_tb_badi_isolate )->set_parameter( name  = 'CV_BADI'
*                                                                               value = lo_td_param_mapping_td ).
*
*      li_tb_badi_isolate->get_badi( EXPORTING iv_badi_name = /egs/if_prs_calc_adaptor=>c_param_mapping
*                                    CHANGING  cv_badi      = li_td_mock_initial_badi ).
*
*      " Mock Input Mapping BADI call
*      cl_abap_testdouble=>configure_call( li_tb_badi_isolate )->set_parameter(
*                                                                 name  = 'CV_BADI'
*                                                                 value = li_td_datastore_mapping )->set_parameter(
*                                                                     name  = 'IT_FILTER'
*                                                                     value = lt_filter_table ).
*
*      " Initial call
*      li_tb_badi_isolate->get_badi( EXPORTING iv_badi_name = /egs/if_prs_calc_adaptor=>c_datastore_mapping
*                                              it_filter    = lt_filter_table
*                                    CHANGING  cv_badi      = li_td_mock_initial_badi ).
*
*      mo_cut->mo_calc_adaptor->setup_mapping( ).
*
*      " Map Global variables to config store entries
*
*      lo_config_store = NEW #( ).
*      lo_config_store->add_entry( iv_key = 'PERNR' iv_data = lv_id ).
*      mo_cut->mo_calc_adaptor->get_input_data_from_paramstore( lo_config_store ).
*
*
*      mo_cut->mo_calc_adaptor->process_input_data( ).
*
*      mo_cut->mo_calc_adaptor->prepare_slices( ).
*
*      " Run do_calc
*      mo_cut->mo_calc_adaptor->do_calculation( ).
*
*      DATA(lt_result) = mo_cut->mo_calc_adaptor->get_result_slice_context_data( ).
*    ENDDO.
*  ENDMETHOD.
*
*ENDCLASS.
