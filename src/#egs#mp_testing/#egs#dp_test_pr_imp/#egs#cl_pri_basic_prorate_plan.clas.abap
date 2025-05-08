CLASS /egs/cl_pri_basic_prorate_plan DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES /egs/if_prs_calc_plan_builder.

    ALIASES composite_builder FOR /egs/if_prs_calc_plan_builder~composite_builder.

  PRIVATE SECTION.
    CONSTANTS: c_company_name_key             TYPE string VALUE 'COMPANY',
               c_age_key                      TYPE string VALUE 'AGE',
               c_fixed_income_values_key      TYPE string VALUE 'FIXED INCOME VALUES',
               c_dates_table_key              TYPE string VALUE 'DISCLOSURE PERIOD DATES TABLE',
               c_slice_begin_key              TYPE string VALUE /egs/cl_prs_dyn_calculator=>c_slice_begin_date,
               c_slice_end_key                TYPE string VALUE /egs/cl_prs_dyn_calculator=>c_slice_end_date,
               c_fixed_income_ytd_key         TYPE string VALUE 'FIXED INCOME YTD',
               c_fixed_income_salary_name_key TYPE string VALUE 'SALARY',
               c_fixed_income_ot_name_key     TYPE string VALUE 'OVERTIME',
               c_monthly_uif_key              TYPE string VALUE 'C_monthly_uif' ##NO_TEXT,
               c_current_period_key           TYPE string VALUE 'C_current_period' ##NO_TEXT,
               c_annual_net_paye_key          TYPE string VALUE 'C_annual_net_paye' ##NO_TEXT,
               c_target_period_key            TYPE string VALUE 'C_target_period' ##NO_TEXT,
               c_periodic_net_paye_key        TYPE string VALUE 'c_periodic_net_paye' ##NO_TEXT,
               c_dist_unit_key                TYPE string VALUE 'c_dist_unit' ##NO_TEXT,
               c_periodic_gross_salary_key    TYPE string VALUE 'c_periodic_gross_salary' ##NO_TEXT,
               c_periodic_net_salary_key      TYPE string VALUE 'c_periodic_net_salary' ##NO_TEXT,
               c_periodic_uif_key             TYPE string VALUE 'c_periodic_uif' ##NO_TEXT,
               c_composite_name               TYPE string VALUE 'SA NET SALARY CALCULATION' ##NO_TEXT,
               c_tax_tiers_key                TYPE string VALUE 'c_tax_tiers' ##NO_TEXT,
               c_annual_salary_key            TYPE string VALUE 'c_annual_salary' ##NO_TEXT,
               c_annual_gross_tax_key         TYPE string VALUE 'c_annual_gross_tax' ##NO_TEXT,
               c_tax_rebates_key              TYPE string VALUE 'c_tax_rebates' ##NO_TEXT,
               c_tax_rebates_tiers_key        TYPE string VALUE 'c_tax_rebates_tiers' ##NO_TEXT,
               c_annual_uif_key               TYPE string VALUE 'c_annual_uif_key' ##NO_TEXT,
               c_uif_ceiling_key              TYPE string VALUE 'c_uif_ceiling_key' ##NO_TEXT,
               c_uif_rate_key                 TYPE string VALUE 'c_uif_rate_key' ##NO_TEXT,
               c_monthly_salary_key           TYPE string VALUE 'c_monthly_salary_key' ##NO_TEXT.
ENDCLASS.

CLASS /egs/cl_pri_basic_prorate_plan IMPLEMENTATION.

  METHOD /egs/if_prs_calc_plan_builder~composite_builder.
*    DATA: lv_distribution_unit_value TYPE decfloat16,
*    DATA: lr_tax_tiers               TYPE REF TO /egs/cl_util_calculations=>ty_tab_graduated_table,
*    DATA: lr_tax_rebates_tiers       TYPE REF TO /egs/cl_util_calculations=>ty_tab_graduated_table.
*    DATA lt_commands TYPE TABLE OF REF TO /egs/cl_cmd_base.

    ro_result = NEW /egs/cl_cmd_composite( 'Test' ) ##NO_TEXT.

    ro_result->add( NEW /egs/cl_cmd_define_metric( c_company_name_key ) ).

    ro_result->add( NEW /egs/cl_cmd_prorate_sum( iv_result_key        = c_fixed_income_ytd_key
                                                 iv_amounts_table_key = c_fixed_income_values_key
                                                 iv_slice_end         = c_slice_end_key
                                                 iv_slice_start       = c_slice_begin_key ) ).

    ro_result->add( NEW /egs/cl_cmd_fetch_data( iv_data_key = c_fixed_income_ytd_key ) ).

*
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*    " SETUP INPUT VALUES AND REFERENCES
*    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*
*    lv_distribution_unit_value = CONV #( '0.01' ).
*
**    DATA(lo_value_fa) = /egs/cl_enc_fin_amount=>create( lv_input_value ).
**    DATA(lo_age) = /egs/cl_enc_fin_amount=>create( 32 ).
*    DATA(lo_current_period_fa) = /egs/cl_enc_fin_amount=>create( lv_current_period ).
**    DATA(lo_target_period_fa) = /egs/cl_enc_fin_amount=>create( lv_target_period ).
*    DATA(lo_distribution_unit) = /egs/cl_enc_fin_amount=>create( lv_distribution_unit_value ).
**    " UIF CALCULATION VALUES
**    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
**    " TODO: variable is assigned but never used (ABAP cleaner)
**    DATA(lo_monthly_period_count_units) = /egs/cl_enc_fin_amount=>create( 12 ).
*    DATA(lo_uif_ceiling_units) = /egs/cl_enc_fin_amount=>create( 17712 ).
*    "
*    DATA(lo_uif_rate_units) = /egs/cl_enc_fin_amount=>create( 1 ).
**    " TAX CALCULATION VALUES
**    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
**    CREATE DATA lr_tax_tiers.
**    lr_tax_tiers->* = VALUE /egs/cl_util_calculations=>ty_tab_graduated_table( ( min = 1817000 value = 45 )
**                                                                               ( min = 857900  value = 41 )
**                                                                               ( min = 673000  value = 39 )
**                                                                               ( min = 512800  value = 36 )
**                                                                               ( min = 370500  value = 31 )
**                                                                               ( min = 237100  value = 26 )
**                                                                               ( min = 0       value = 18 ) ).
**
**    CREATE DATA lr_tax_rebates_tiers.
**    lr_tax_rebates_tiers->* = VALUE /egs/cl_util_calculations=>ty_tab_graduated_table( ( min = 75  value = 3145 )
**                                                                                       ( min = 65  value = 9444 )
**                                                                                       ( min = 0   value = 17235 ) ).
**
**    " END SETUP INPUT VALUES AND REFERENCES
**    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
**
*    lt_commands =
*      VALUE #( ( NEW /egs/cl_cmd_define_metric( c_company_name_key ) )
*
**               ( NEW
**                 /egs/cl_cmd_store_data( iv_key  = 'AGE'
**                                         iv_data = lo_age ) )
*               ( NEW /egs/cl_cmd_define_input( iv_data_key = c_age_key ) )
*
*               ( NEW /egs/cl_cmd_define_input( iv_data_key = c_dates_table_key ) )
*
*               ( NEW /egs/cl_cmd_define_static_inp( iv_data_key = c_fixed_income_values_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = C_current_period_key
*                                         iv_data = lo_current_period_fa ) )
*
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = c_dist_unit_key
*                                         iv_data = lo_distribution_unit ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = c_tax_tiers_key
*                                         iv_data = lr_tax_tiers->* ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = c_tax_rebates_tiers_key
*                                         iv_data = lr_tax_rebates_tiers->* ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = c_uif_ceiling_key
*                                         iv_data = lo_uif_ceiling_units ) )
*
*               ( NEW
*                 /egs/cl_cmd_store_data( iv_key  = c_uif_rate_key
*                                         iv_data = lo_uif_rate_units ) )
*
*               " Add Count dates
*               ( NEW
*                 /egs/cl_cmd_count(
*                 iv_container_key = c_dates_table_key
*                 iv_result_key  = c_target_period_key ) )
*
*               " Add Prorate Fixed income
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key             = c_periodic_gross_salary_key
*                                      iv_current_period_key    = C_current_period_key
*                                      iv_target_period_key     = C_target_period_key
*                                      iv_projected_value_key   = c_annual_salary_key
*                                      iv_distribution_unit_key = c_dist_unit_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key             = c_annual_salary_key
*                                      iv_current_period_key    = C_target_period_key
*                                      iv_target_period_key     = C_current_period_key
*                                      iv_projected_value_key   = c_monthly_salary_key
*                                      iv_distribution_unit_key = c_dist_unit_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_ltd_percentage( iv_result_key     = C_monthly_uif_key
*                                             iv_value_key      = c_monthly_salary_key
*                                             iv_percentage_key = c_uif_rate_key
*                                             iv_ceiling_key    = c_uif_ceiling_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key             = C_monthly_uif_key
*                                      iv_current_period_key    = C_current_period_key
*                                      iv_target_period_key     = C_target_period_key
*                                      iv_projected_value_key   = c_annual_uif_key
*                                      iv_distribution_unit_key = c_dist_unit_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key             = c_annual_uif_key
*                                      iv_current_period_key    = C_target_period_key
*                                      iv_target_period_key     = C_current_period_key
*                                      iv_projected_value_key   = c_periodic_uif_key
*                                      iv_distribution_unit_key = c_dist_unit_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_sum_tiers( iv_result_key = c_tax_rebates_key
*                                        iv_value_key  = c_age_key
*                                        iv_tiers_key  = c_tax_rebates_tiers_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_tax_by_tiers( iv_result_key = c_annual_gross_tax_key
*                                           iv_value_key  = c_annual_salary_key
*                                           iv_tiers_key  = c_tax_tiers_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_floored_diff( iv_result_key = C_annual_net_paye_key
*                                           iv_num1_key   = c_annual_gross_tax_key
*                                           iv_num2_key   = c_tax_rebates_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_project( iv_value_key             = C_annual_net_paye_key
*                                      iv_current_period_key    = C_current_period_key
*                                      iv_target_period_key     = C_target_period_key
*                                      iv_projected_value_key   = c_periodic_net_paye_key
*                                      iv_distribution_unit_key = c_dist_unit_key ) )
*
*               ( NEW
*                 /egs/cl_cmd_deductions( iv_result_key      = c_periodic_net_salary_key
*                                         iv_num1_key        = c_periodic_gross_salary_key
*                                         it_deductions_keys = VALUE /egs/if_cmd_base=>ty_string_table(
*                                                                        ( c_periodic_net_paye_key )
*                                                                        ( c_periodic_uif_key ) ) ) )
*
*               ( NEW /egs/cl_cmd_fetch_data( c_periodic_net_salary_key ) )
*
*               ( NEW /egs/cl_cmd_fetch_data( c_periodic_net_paye_key ) )
*
*               ( NEW /egs/cl_cmd_fetch_data( c_periodic_uif_key ) ) ).
*
** TODO: uncomment to apply new plan to result
**    ro_result = NEW #( iv_list_name = c_composite_name
**                       it_children  = lt_commands ).
  ENDMETHOD.

ENDCLASS.
