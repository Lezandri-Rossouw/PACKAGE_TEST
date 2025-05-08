*"* use this source file for your ABAP unit test classes
CLASS ltcl_sa_tax_plan DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_datastore_entry_val,
             key   TYPE string,
             value TYPE string,
           END OF ty_datastore_entry_val,

           ty_tab_datastore_entry_vals TYPE STANDARD TABLE OF ty_datastore_entry_val.

    METHODS first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_sa_tax_plan IMPLEMENTATION.

  METHOD first_test.
    DATA: lo_fetch_visitor            TYPE REF TO /egs/cl_vst_fetch,
          lo_datastore                TYPE REF TO /egs/cl_enc_datastore,
          lo_dates_tab                TYPE REF TO /egs/cl_enc_array,
          lt_fixed_income_values      TYPE /egs/cl_util_fin_calculations=>ty_tab_amount_validity,
          lo_plan                     TYPE REF TO /egs/cl_cmd_base,
          lv_company_name             TYPE string                                                VALUE 'Company 1',
          lv_age                      TYPE i                                                     VALUE 21,
          lv_disclosure_period_begin  TYPE dats                                                  VALUE '20240101',
          lv_disclosure_period_end    TYPE dats                                                  VALUE '20250101',
          lv_current_month_begin_date TYPE dats                                                  VALUE '20240101',
          lv_next_month_begin_date    TYPE dats                                                  VALUE '20240201',
          lv_slice_period_begin       TYPE dats                                                  VALUE '20240101',
          lv_slice_period_end         TYPE dats                                                  VALUE '20240201',
          " TODO: variable is assigned but never used (ABAP cleaner)
          lt_datastore_values         TYPE ty_tab_datastore_entry_vals,
          " TODO: variable is assigned but never used (ABAP cleaner)
          lt_result_values            TYPE ty_tab_datastore_entry_vals.

    lo_fetch_visitor = NEW #( ).
    lo_datastore = NEW #( ).

    lo_dates_tab = NEW #( ).

    lt_fixed_income_values = VALUE #( ( begin_date       = '20240101'
                                        end_date         = '20240111'
                                        amount           = /egs/cl_enc_fin_amount=>create( 10000 )
                                        weight_distr_tab = VALUE #( ( date = '20240101' weight = 1 ) ) ) ).

    lo_plan = /egs/cl_pri_sa_tax_plan=>composite_builder( ).

    " Set metrics
    lo_datastore->set( iv_data = NEW /egs/cl_enc_string( lv_company_name )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_company_name_key ) ).

    " Set dynamic inputs
    lo_datastore->set( iv_data = /egs/cl_enc_fin_amount=>create( lv_age )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_age_key ) ).

    lo_datastore->set( iv_data = lo_dates_tab
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_dates_table_key ) ).

    lo_datastore->set( iv_data = /egs/cl_util_enc_helper=>encode_amount_validation_table( lt_fixed_income_values )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_fixed_income_values_key ) ).

*  c_disclosure_period_begin_date
    lo_datastore->set( iv_data = NEW /egs/cl_enc_date( lv_disclosure_period_begin )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_disclosure_period_begin_date ) ).

*  c_disclosure_period_end_date
    lo_datastore->set( iv_data = NEW /egs/cl_enc_date( lv_disclosure_period_end )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_disclosure_period_end_date ) ).

*  c_current_month_begin_date
    lo_datastore->set( iv_data = NEW /egs/cl_enc_date( lv_current_month_begin_date )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_current_month_begin_date ) ).

*  c_month_after_begin_date
    lo_datastore->set( iv_data = NEW /egs/cl_enc_date( lv_next_month_begin_date )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_next_month_begin_date ) ).

*  c_slice_begin_key
    lo_datastore->set( iv_data = NEW /egs/cl_enc_date( lv_slice_period_begin )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_slice_begin_key ) ).
*  c_slice_end_key
    lo_datastore->set( iv_data = NEW /egs/cl_enc_date( lv_slice_period_end )
                       iv_key  = NEW /egs/cl_enc_string( /egs/cl_pri_sa_tax_plan=>c_slice_end_key ) ).

    lo_plan->execute( lo_datastore ).
    lo_plan->accept( lo_fetch_visitor ).

    LOOP AT lo_datastore->to_table( ) INTO DATA(ls_entry).
      APPEND VALUE #( key   = ls_entry-key->to_string( )
                      value = ls_entry-data->to_string( ) ) TO lt_datastore_values.
    ENDLOOP.

    DATA(lo_results) = lo_fetch_visitor->get_datastore( ).

    LOOP AT lo_results->to_table( ) INTO ls_entry.
      APPEND VALUE #( key   = ls_entry-key->to_string( )
                      value = ls_entry-data->to_string( ) ) TO lt_result_values.
    ENDLOOP.

    WRITE 'done'.

  ENDMETHOD.

ENDCLASS.
