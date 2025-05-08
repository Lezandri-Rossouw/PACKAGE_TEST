CLASS /egs/cl_pri_bi_input_mapping DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: c_fixed_income_key TYPE string VALUE 'FIXED INCOME VALUES',
               c_salary_name      TYPE string VALUE 'SALARY'.

    INTERFACES /egs/if_prs_input_mapping.

    ALIASES: get_amount_data             FOR /egs/if_prs_input_mapping~get_amount_data,
             get_dynamic_data            FOR /egs/if_prs_input_mapping~get_dynamic_data,
             get_metric_data             FOR /egs/if_prs_input_mapping~get_metric_data,
             get_period_data             FOR /egs/if_prs_input_mapping~get_period_data,
             get_personnel_data          FOR /egs/if_prs_input_mapping~get_personnel_data,
             get_plan_name               FOR /egs/if_prs_input_mapping~get_plan_name,
             map_global_names_to_keys    FOR /egs/if_prs_input_mapping~map_global_names_to_keys,
             update_globals_with_results FOR /egs/if_prs_input_mapping~update_globals_with_results,
             get_storage_branch_name     FOR /egs/if_prs_input_mapping~get_storage_branch_name.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.

CLASS /egs/cl_pri_bi_input_mapping IMPLEMENTATION.

  METHOD get_storage_branch_name.
    rv_result = 'AA'.
  ENDMETHOD.

  METHOD get_amount_data.
    CONSTANTS c_amount TYPE decfloat34 VALUE 1000.

    DATA: lt_wdt        TYPE /egs/cl_util_fin_calculations=>ty_tab_weight_distribution,
          lt_named_amts TYPE /egs/if_prs_calc_adaptor=>ty_tab_named_amount_validity.

    " Setup Amounts

    " lt_named_amounts TYPE /egs/if_prs_calc_adaptor=>ty_tab_mapped_amt_validity.

    lt_wdt = VALUE #( weight = 1
                      ( date = '20240110' )
                      ( date = '20240120' )
                      ( date = '20240130' ) ).

    lt_named_amts = VALUE #( ( amount           = /egs/cl_enc_fin_amount=>create( c_amount )
                               begin_date       = '20240101'
                               end_date         = '20240201'
                               weight_distr_tab = lt_wdt
                               name             = c_salary_name ) ).

*   lt_wdt = VALUE #( weight = 1
*                      ( date = '20240210' )
*                      ( date = '20240220' )
*                      ( date = '20240228' ) ).
*
*    lt_named_amts = VALUE #( ( amount           = /egs/cl_enc_fin_amount=>create( c_amount )
*                               begin_date       = '20240201'
*                               end_date         = '20240301'
*                               weight_distr_tab = lt_wdt
*                               name             = c_salary_name ) ).

    " lt_named_amounts = VALUE #( ( key     = lcl_consts=>c_fixed_income_values_key
    "                              amounts = lt_named_amts ) ).

    et_amount_map = VALUE #( ( key = c_fixed_income_key name = c_salary_name ) ).
    et_weighted_income = lt_named_amts.
  ENDMETHOD.

  METHOD get_dynamic_data.
    CONSTANTS c_age TYPE i VALUE 21.

    ro_param_store = NEW #( ).
    ro_param_store->add_entry( iv_data = c_age
                               iv_key  = 'AGE' ).
  ENDMETHOD.

  METHOD get_metric_data.
    CONSTANTS c_company_name_key TYPE string VALUE 'COMPANY'.

    DATA ls_metric TYPE /egs/if_prs_input_mapping=>ty_struc_metric.

    ls_metric = VALUE #( key        = c_company_name_key
                         start_date = '20240101'
                         end_date   = '20240115'
                         value      = REF #( 'Company_1' ) ) ##NO_TEXT.

    INSERT ls_metric INTO TABLE rt_metric_table.

    ls_metric = VALUE #( key        = c_company_name_key
                         start_date = '20240115'
                         end_date   = '20240201'
                         value      = REF #( 'Company_2' ) ) ##NO_TEXT.

    INSERT ls_metric INTO TABLE rt_metric_table.

*
*        ls_metric = VALUE #( key        = c_company_name_key
*                         start_date = '20240201'
*                         end_date   = '20240215'
*                         value      = REF #( 'Company_1' ) ) ##no_text.
*
*    INSERT ls_metric INTO TABLE rt_metric_table.
*
*    ls_metric = VALUE #( key        = c_company_name_key
*                         start_date = '20240215'
*                         end_date   = '20240301'
*                         value      = REF #( 'Company_2' ) ) ##no_text.
*
*    INSERT ls_metric INTO TABLE rt_metric_table.
  ENDMETHOD.

  METHOD get_period_data.
    ev_payroll_period_begin_date = '20240101'.
    ev_payroll_period_end_date = '20250101'.
    ev_calc_period_begin_date = '20240101'.
    ev_calc_period_end_date = '20240201'.
    et_period_dates = VALUE #( ).

*    ev_payroll_period_begin_date = '20240101'.
*    ev_payroll_period_end_date = '20250101'.
*    ev_calc_period_begin_date = '20240201'.
*    ev_calc_period_end_date = '20240301'.
*    et_period_dates = VALUE #( ).
  ENDMETHOD.

  METHOD get_personnel_data.
    DATA(lr_pernr) = io_global_datastore->get_entry( 'PERNR' ).

    ASSIGN lr_pernr->* TO FIELD-SYMBOL(<lv_pernr>).
    rv_personnel_data = <lv_pernr>.

  ENDMETHOD.

  METHOD get_plan_name.
    rv_class_name = '/EGS/CL_PRI_BASIC_PRORATE_PLAN'.
  ENDMETHOD.

  METHOD map_global_names_to_keys.
    rt_global_datamap = VALUE #( ( global_var_name = 'pernr-pernr' datastore_key = 'PERNR' )
                                 ( global_var_name = 'rt[]'        datastore_key = 'RT' )
                                 ( global_var_name = 't549q'       datastore_key = 'T549Q' ) ).
  ENDMETHOD.

  METHOD update_globals_with_results.
    " TODO: implement.
  ENDMETHOD.

ENDCLASS.
