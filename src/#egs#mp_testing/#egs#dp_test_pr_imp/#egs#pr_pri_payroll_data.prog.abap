*&---------------------------------------------------------------------*
*& Report /egs/pr_pri_payroll_data
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /egs/pr_pri_payroll_data.

CLASS lcl_available_pernr_list DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES ty_t_pernr_list TYPE SORTED TABLE OF pernr_d WITH UNIQUE KEY table_line.

    METHODS constructor
      IMPORTING iv_number_count   TYPE i
                iv_start_at_pernr TYPE pernr_d
                ii_sql_actions    TYPE REF TO /egs/if_bts_sql_actions.

    METHODS get_pernr_list
      RETURNING VALUE(rt_result) TYPE ty_t_pernr_list.

  PRIVATE SECTION.

    DATA: mv_number_count   TYPE i,
          mv_start_at_pernr TYPE pernr_d,
          mi_sql_actions    TYPE REF TO /egs/if_bts_sql_actions.

    METHODS is_number_available
      IMPORTING iv_pernr         TYPE pernr_d
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

CLASS lcl_available_pernr_list IMPLEMENTATION.

  METHOD constructor.

    mv_number_count   = iv_number_count.
    mv_start_at_pernr = iv_start_at_pernr.
    mi_sql_actions    = ii_sql_actions.

  ENDMETHOD.

  METHOD get_pernr_list.

    DATA lv_pernr TYPE pernr_d.

    lv_pernr = mv_start_at_pernr.

    DO.

      IF is_number_available( lv_pernr ).
        mv_number_count = mv_number_count - 1.
        INSERT lv_pernr INTO TABLE rt_result.
      ENDIF.

      IF mv_number_count = 0.
        EXIT.
      ENDIF.

      lv_pernr = lv_pernr + 1.

    ENDDO.

  ENDMETHOD.

  METHOD is_number_available.

    DATA lt_where TYPE /egs/if_bts_sql_actions=>ty_tab_where.

    lt_where = VALUE #( ( operator = '=' name = 'PERNR'  value = REF #( iv_pernr ) ) ).

    TRY.
        mi_sql_actions->select_where( iv_table_name = 'PA0003'
                                      iv_order_by   = 'PERNR'
                                      it_where      = lt_where ).
        rv_result = abap_false.
      CATCH cx_static_check.
        rv_result = abap_true.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_td_select DEFINITION FINAL CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.

    INTERFACES /egs/if_bts_sql_actions PARTIALLY IMPLEMENTED.

  PRIVATE SECTION.

    DATA mv_count TYPE i.

ENDCLASS.

CLASS ltcl_td_select IMPLEMENTATION.

  METHOD /egs/if_bts_sql_actions~select_where.

    mv_count = mv_count + 1.

    IF mv_count <> 1.
      /egs/cl_util_validation=>raise_error( iv_msg_id = /egs/cl_util_validation=>c_util_msg_id
                                            iv_msg_no = '000'
                                            iv_text_1 = |First random employee number exists| ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_pernr_list DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS first_test    FOR TESTING RAISING cx_static_check.
    METHODS pernr_exsists FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_pernr_list IMPLEMENTATION.

  METHOD first_test.

    CONSTANTS c_record_count TYPE i VALUE 10000.

    DATA(lo_cut) = NEW lcl_available_pernr_list( iv_start_at_pernr = 1
                                                 iv_number_count   = c_record_count
                                                 ii_sql_actions    = NEW /egs/cl_bts_sql_actions( ) ).

    cl_abap_unit_assert=>assert_equals( exp = c_record_count
                                        act = lines( lo_cut->get_pernr_list( ) ) ).

  ENDMETHOD.

  METHOD pernr_exsists.

    DATA(lo_cut) = NEW lcl_available_pernr_list( iv_start_at_pernr = 1
                                                 iv_number_count   = 10
                                                 ii_sql_actions    = NEW ltcl_td_select( ) ).

    cl_abap_unit_assert=>assert_equals( exp = 10
                                        act = lines( lo_cut->get_pernr_list( ) ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_employee_data DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS get_mt_prelp
      RETURNING VALUE(rt_result) TYPE hrpad_prelp_tab.

    METHODS add_data
      IMPORTING it_pnnnn TYPE STANDARD TABLE.

  PRIVATE SECTION.

    DATA mt_prelp TYPE hrpad_prelp_tab.

    METHODS pnnnn_to_prelp_tab
      IMPORTING it_pnnnn         TYPE STANDARD TABLE
      RETURNING VALUE(rt_result) TYPE prelp_tab.

ENDCLASS.

CLASS lcl_employee_data IMPLEMENTATION.

  METHOD get_mt_prelp.

    rt_result = mt_prelp.

  ENDMETHOD.

  METHOD pnnnn_to_prelp_tab.

    cl_hr_pnnnn_type_cast=>pnnnn_to_prelp_tab( EXPORTING pnnnn_tab = it_pnnnn
                                               IMPORTING prelp_tab = rt_result ).

  ENDMETHOD.

  METHOD add_data.

    APPEND LINES OF pnnnn_to_prelp_tab( it_pnnnn ) TO mt_prelp.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_employee_data DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS first_test FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_employee_data IMPLEMENTATION.

  METHOD first_test.

    TYPES ty_t_p0000 TYPE STANDARD TABLE OF p0000 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey.

    DATA: lt_0000     TYPE ty_t_p0000,
          lt_expected TYPE prelp_tab.

    lt_0000 = VALUE #( ( infty = '0000'
                         endda = '99991231'
                         begda = '20240301'
                         massn = '01'
                         stat2 = '3'
                         stat3 = '1' ) ).

    DATA(lo_cut) = NEW lcl_employee_data( ).

    lo_cut->add_data( lt_0000 ).

    cl_hr_pnnnn_type_cast=>pnnnn_to_prelp_tab( EXPORTING pnnnn_tab = lt_0000
                                               IMPORTING prelp_tab = lt_expected ).

    cl_abap_unit_assert=>assert_equals( exp = lt_expected
                                        act = lo_cut->get_mt_prelp( ) ).

  ENDMETHOD.

ENDCLASS.

INTERFACE lif_employee.

  TYPES: ty_t_p0000 TYPE STANDARD TABLE OF p0000 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey,
         ty_t_p0001 TYPE STANDARD TABLE OF p0001 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey,
         ty_t_p0002 TYPE STANDARD TABLE OF p0002 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey,
         ty_t_p0003 TYPE STANDARD TABLE OF p0003 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey,
         ty_t_p0007 TYPE STANDARD TABLE OF p0007 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey,
         ty_t_p0008 TYPE STANDARD TABLE OF p0008 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey,
         ty_t_p0009 TYPE STANDARD TABLE OF p0009 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey,
         ty_t_p0149 TYPE STANDARD TABLE OF p0149 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey,
         ty_t_p0150 TYPE STANDARD TABLE OF p0150 WITH UNIQUE SORTED KEY pskey COMPONENTS pskey.

  METHODS get_mo_employee_data RETURNING VALUE(ro_result) TYPE REF TO lcl_employee_data.
  METHODS get_mv_number_count  RETURNING VALUE(rv_result) TYPE i.

ENDINTERFACE.

CLASS lcl_sa_employee DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES lif_employee.

    ALIASES: ty_t_p0000           FOR lif_employee~ty_t_p0000,
             ty_t_p0001           FOR lif_employee~ty_t_p0001,
             ty_t_p0002           FOR lif_employee~ty_t_p0002,
             ty_t_p0003           FOR lif_employee~ty_t_p0003,
             ty_t_p0007           FOR lif_employee~ty_t_p0007,
             ty_t_p0008           FOR lif_employee~ty_t_p0008,
             ty_t_p0009           FOR lif_employee~ty_t_p0009,
             ty_t_p0149           FOR lif_employee~ty_t_p0149,
             ty_t_p0150           FOR lif_employee~ty_t_p0150,
             get_mo_employee_data FOR lif_employee~get_mo_employee_data,
             get_mv_number_count  FOR lif_employee~get_mv_number_count.

    METHODS constructor
      IMPORTING iv_employee_count TYPE i
                iv_start_at_pernr TYPE pernr_d DEFAULT 1.

  PRIVATE SECTION.

    DATA: mv_employee_count       TYPE i,
          mv_start_at_pernr       TYPE pernr_d,
          mo_available_pernr_list TYPE REF TO lcl_available_pernr_list,
          mo_employee_data        TYPE REF TO lcl_employee_data.

    METHODS add_data
      IMPORTING iv_target_pernr TYPE pernr_d.

ENDCLASS.

CLASS lcl_sa_employee IMPLEMENTATION.

  METHOD constructor.

    mv_employee_count = iv_employee_count.
    mv_start_at_pernr = iv_start_at_pernr.

    mo_available_pernr_list = NEW lcl_available_pernr_list( iv_start_at_pernr = mv_start_at_pernr
                                                            iv_number_count   = mv_employee_count
                                                            ii_sql_actions    = NEW /egs/cl_bts_sql_actions( ) ).

    mo_employee_data = NEW lcl_employee_data( ).

    LOOP AT mo_available_pernr_list->get_pernr_list( ) INTO DATA(lv_target_pernr).
      add_data( lv_target_pernr ).
    ENDLOOP.

  ENDMETHOD.

  METHOD add_data.

    CONSTANTS: c_ename TYPE string VALUE 'User Epi' ##NO_TEXT,
               c_nachn TYPE string VALUE 'Epi' ##NO_TEXT,
               c_vorna TYPE string VALUE 'User' ##NO_TEXT.

    DATA:
      lt_0000 TYPE ty_t_p0000,
      lt_0001 TYPE ty_t_p0001,
      lt_0002 TYPE ty_t_p0002,
      lt_0003 TYPE ty_t_p0003,
      lt_0007 TYPE ty_t_p0007,
      lt_0008 TYPE ty_t_p0008,
      lt_0009 TYPE ty_t_p0009,
      lt_0149 TYPE ty_t_p0149,
      lt_0150 TYPE ty_t_p0150.

    lt_0000 = VALUE #( ( pernr = iv_target_pernr
                         infty = '0000'
                         endda = '99991231'
                         begda = '20240301'
                         massn = '01'
                         stat2 = '3'
                         stat3 = '1' ) ).

    lt_0001 = VALUE #( ( pernr = iv_target_pernr
                         infty = '0001'
                         endda = '99991231'
                         begda = '20240301'
                         bukrs = 'ZA01'
                         werks = 'ZA01'
                         persg = '1'
                         persk = 'W0'
                         vdsk1 = 'ZA01'
                         btrtl = '0001'
                         abkrs = 'W0'
                         plans = '99999999'
                         sname = 'USER EPI'
                         ename = c_ename
                         otype = 'S'
                         sbmod = 'ZA01' ) ).

    lt_0002 = VALUE #( ( pernr = iv_target_pernr
                         infty = '0002'
                         endda = '99991231'
                         begda = '19811221'
                         nachn = c_nachn
                         vorna = c_vorna
                         knznm = '00'
                         anred = '3'
                         gbdat = '19811221'
                         natio = 'ZA'
                         sprsl = 'E'
                         gbjhr = '1981'
                         gbmon = '12'
                         gbtag = '21'
                         nchmc = 'DU SUD'
                         vnamc = 'AFRIQUE' ) ).

    lt_0003 = VALUE #( ( pernr = iv_target_pernr
                         infty = '0003'
                         endda = '99991231'
                         begda = '18000101'
                         viekn = '16' ) ).

    lt_0007 = VALUE #( ( pernr = iv_target_pernr
                         infty = '0007'
                         endda = '99991231'
                         begda = '20240301'
                         zterf = '0'
                         schkz = 'WNORM'
                         empct = '100.00'
                         mostd = '173.33'
                         wostd = '40.00'
                         arbst = '8.00'
                         wkwdy = '5.00'
                         jrstd = '2080.00' ) ).

    lt_0008 = VALUE #( ( subty = '0'
                         pernr = iv_target_pernr
                         infty = '0008'
                         endda = '99991231'
                         begda = '20240301'
                         trfar = '01'
                         trfgb = '01'
                         trfgr = 'E BAND'
                         trfst = '01'
                         stvor = '00000000'
                         waers = 'ZAR'
                         bsgrd = '100.00'
                         divgv = '173.33'
                         lga01 = 'M020'
                         bet01 = '10000.00' ) ).

    lt_0009 = VALUE #( ( pernr = iv_target_pernr
                         infty = '0009'
                         endda = '99991231'
                         begda = '20240301'
                         waers = 'ZAR'
                         zlsch = 'C'
                         banks = 'ZA' ) ).

    lt_0149 = VALUE #( ( pernr = iv_target_pernr
                         infty = '0149'
                         endda = '99991231'
                         begda = '20240301'
                         fzamt = '99'
                         pfind = 'FT'
                         txpro = '6'
                         waers = 'ZAR' ) ).

    lt_0150 = VALUE #( ( pernr = iv_target_pernr
                         infty = '0150'
                         subty = 'MAIN'
                         endda = '99991231'
                         begda = '20240301'
                         kvkz1 = '0'
                         rvkz1 = '0'
                         avkz1 = '1' ) ).

    mo_employee_data->add_data( lt_0000 ).
    mo_employee_data->add_data( lt_0001 ).
    mo_employee_data->add_data( lt_0002 ).
    mo_employee_data->add_data( lt_0003 ).
    mo_employee_data->add_data( lt_0007 ).
    mo_employee_data->add_data( lt_0008 ).
    mo_employee_data->add_data( lt_0009 ).
    mo_employee_data->add_data( lt_0149 ).
    mo_employee_data->add_data( lt_0150 ).

  ENDMETHOD.

  METHOD lif_employee~get_mo_employee_data.

    ro_result = mo_employee_data.

  ENDMETHOD.

  METHOD lif_employee~get_mv_number_count.

    rv_result = mv_employee_count.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_infotypes DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING ii_employee TYPE REF TO lif_employee.

    METHODS insert_infotype.



  PRIVATE SECTION.

    CONSTANTS c_tclas TYPE tclas VALUE 'A'.

    DATA: mi_employee          TYPE REF TO lif_employee,
          mi_masterdata_buffer TYPE REF TO if_hrpa_masterdata_buffer.

    METHODS insert_infotyp_record
      IMPORTING is_data TYPE prelp.

ENDCLASS.

CLASS lcl_infotypes IMPLEMENTATION.

  METHOD constructor.

    mi_employee = ii_employee.
    cl_hrpa_masterdata_factory=>get_masterdata_buffer( IMPORTING masterdata_buffer = mi_masterdata_buffer ).

  ENDMETHOD.

  METHOD insert_infotype.

    LOOP AT mi_employee->get_mo_employee_data( )->get_mt_prelp( ) INTO DATA(ls_data_0003) WHERE infty = '0003'.
      insert_infotyp_record( is_data = ls_data_0003  ).
    ENDLOOP.

    LOOP AT mi_employee->get_mo_employee_data( )->get_mt_prelp( ) INTO DATA(ls_data_rest) WHERE infty <> '0003'.
      insert_infotyp_record( is_data = ls_data_rest  ).
    ENDLOOP.

  ENDMETHOD.

  METHOD insert_infotyp_record.

    DATA: lr_data      TYPE REF TO data,
          li_container TYPE REF TO if_hrpa_infotype_container.

    DATA(lv_infotype_type) = |P{ is_data-infty }|.

    TRY.

        CREATE DATA lr_data TYPE (lv_infotype_type).
        ASSIGN lr_data->* TO FIELD-SYMBOL(<lv_data>).

        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn( EXPORTING prelp = is_data
                                               IMPORTING pnnnn = <lv_data> ).

        cl_hrpa_infotype_container=>get_instance( EXPORTING tclas          = c_tclas
                                                            primary_record = <lv_data>
                                                  IMPORTING container      = li_container ).

        mi_masterdata_buffer->insert( container       = li_container
                                      no_ale          = abap_true
                                      no_workflow     = abap_true
                                      no_infotype_log = abap_true ).

        mi_masterdata_buffer->flush( abap_false ).

      CATCH cx_dynamic_check
            cx_static_check INTO DATA(lo_exception).

        WRITE lo_exception->get_longtext( ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_insert DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS first_test FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_insert IMPLEMENTATION.

  METHOD first_test.

    TRY.
        NEW lcl_infotypes( ii_employee = NEW lcl_sa_employee( 10 ) )->insert_infotype( ).
      CATCH cx_dynamic_check
            cx_static_check INTO DATA(lo_exception).
        cl_abap_unit_assert=>fail( msg    = 'Unit test should not have failed.'
                                   detail = lo_exception->get_longtext( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_run DEFINITION FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING iv_employee_count TYPE i
                iv_start_at_pernr TYPE pernr_d.

ENDCLASS.

CLASS lcl_run IMPLEMENTATION.

  METHOD constructor.

    TRY.
        DATA(lo_sa_employee) = NEW lcl_sa_employee( iv_employee_count = iv_employee_count
                                                    iv_start_at_pernr = iv_start_at_pernr ).

        DATA(lo_infotype) = NEW lcl_infotypes( ii_employee = lo_sa_employee ).
        lo_infotype->insert_infotype( ).

      CATCH cx_dynamic_check
            cx_static_check INTO DATA(lo_exception).

        WRITE lo_exception->get_longtext( ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

PARAMETERS p_count TYPE i DEFAULT 10.
PARAMETERS p_start TYPE pernr_d DEFAULT 1.

START-OF-SELECTION.

  NEW lcl_run( iv_employee_count = p_count
               iv_start_at_pernr = p_start ).
