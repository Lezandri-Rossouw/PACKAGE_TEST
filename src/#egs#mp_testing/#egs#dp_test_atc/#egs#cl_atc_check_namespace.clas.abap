CLASS /egs/cl_atc_check_namespace DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_scan
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS run REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS c_my_name   TYPE seoclsname VALUE '/EGS/CL_ATC_CHECK_NAMESPACE' ##NO_TEXT.
    CONSTANTS c_namespace TYPE sci_errc   VALUE 'NAMESPACE' ##NO_TEXT.

    METHODS add_message
      IMPORTING iv_test        TYPE seoclsname DEFAULT c_my_name
                iv_code        TYPE sci_errc
                iv_kind        TYPE sci_errty  DEFAULT c_error
                iv_text        TYPE string
                iv_comment     TYPE sci_pcom   OPTIONAL
                iv_alt_comment TYPE sci_pcom   OPTIONAL.

ENDCLASS.

CLASS /egs/cl_atc_check_namespace IMPLEMENTATION.

  METHOD add_message.

    INSERT VALUE #( test     = iv_test
                    code     = iv_code
                    kind     = iv_kind
                    text     = iv_text
                    pcom     = iv_comment
                    pcom_alt = iv_alt_comment ) INTO TABLE scimessages.

  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).

    description = 'Check Names'.
    category = '/EGS/CL_ATC_CATEGORY_ROOT'.
    version = '001'.

    add_obj_type( 'CLAS' ).
    add_obj_type( 'DEVC' ).
    add_obj_type( 'DOMA' ).
    add_obj_type( 'DTEL' ).
    add_obj_type( 'ENHS' ).
    add_obj_type( 'ENQU' ).
    add_obj_type( 'INTF' ).
    add_obj_type( 'MSAG' ).
    add_obj_type( 'SOTR' ).
    add_obj_type( 'TABL' ).

    add_message( iv_code = c_namespace
                 iv_text = CONV #( 'Obj &1 type &2 expected namespace: "&3".'(001) )
                 iv_alt_comment = 'NEEDED' ).

  ENDMETHOD.

  METHOD run.

*    IF object_name(5) = '/EGS/'.
*    ELSEIF object_name(1) = 'Z'. " and in local package
*    ELSE.
*    ENDIF.

    SELECT SINGLE devclass FROM tadir
      WHERE pgmid = 'R3TR' AND object = @object_type AND obj_name = @object_name
      INTO @DATA(lv_package).

    SELECT SINGLE * FROM tdevc WHERE devclass = @lv_package INTO @DATA(ls_tdevc).

    inform( p_test    = c_my_name
            p_code    = c_namespace
            p_param_1 = object_name
            p_param_2 = object_type
            p_param_3 = 'ZZZZ' ).

  ENDMETHOD.

ENDCLASS.
