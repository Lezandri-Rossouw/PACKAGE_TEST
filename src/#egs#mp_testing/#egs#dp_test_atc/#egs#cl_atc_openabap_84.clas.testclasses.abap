CLASS ltcl_check_84 DEFINITION DEFERRED.
CLASS /egs/cl_atc_openabap_84 DEFINITION LOCAL FRIENDS ltcl_check_84.

CLASS ltcl_check_84 DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS first_test               FOR TESTING RAISING cx_static_check.
    METHODS class_excluded_via_setup FOR TESTING RAISING cx_static_check.
    METHODS exception_class          FOR TESTING RAISING cx_static_check.
    METHODS export_import            FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_check_84 IMPLEMENTATION.

  METHOD first_test.

    DATA(lo_cut) = NEW /egs/cl_atc_openabap_84( ).

    lo_cut->mt_classes                 = VALUE #( ).
    lo_cut->object_name                = '/EGS/CL_ATC_OA_84_EXAMPLE'.
    lo_cut->mv_read_only_allowed       = abap_false.
    lo_cut->mv_exclude_alias_constants = abap_false.
    lo_cut->mv_exclude_cx_classes      = abap_false.

    lo_cut->run( ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lo_cut->mt_attributes ) ).

    lo_cut->mv_exclude_alias_constants = abap_true.

    lo_cut->run( ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( lo_cut->mt_attributes ) ).

  ENDMETHOD.

  METHOD class_excluded_via_setup.

    DATA(lo_cut) = NEW /egs/cl_atc_openabap_84( ).

    lo_cut->mt_classes                 = VALUE #( ( low = '/EGS/CL_TEST_OPEN_ABAP_84' option = 'EQ' sign = 'I' ) ).
    lo_cut->object_name                = '/EGS/CL_TEST_OPEN_ABAP_84'.
    lo_cut->mv_read_only_allowed       = abap_false.
    lo_cut->mv_exclude_alias_constants = abap_true.
    lo_cut->mv_exclude_cx_classes      = abap_false.

    lo_cut->run( ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lines( lo_cut->mt_attributes ) ).

  ENDMETHOD.

  METHOD exception_class.

    DATA(lo_cut) = NEW /egs/cl_atc_openabap_84( ).

    lo_cut->mt_classes                 = VALUE #( ).
    lo_cut->object_name                = 'CX_ROOT'.
    lo_cut->mv_read_only_allowed       = abap_false.
    lo_cut->mv_exclude_alias_constants = abap_true.
    lo_cut->mv_exclude_cx_classes      = abap_false.

    lo_cut->run( ).

    cl_abap_unit_assert=>assert_equals( exp = 4
                                        act = lines( lo_cut->mt_attributes ) ).

    lo_cut->mv_exclude_cx_classes = abap_true.

    lo_cut->run( ).
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lines( lo_cut->mt_attributes ) ).

  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( NEW /egs/cl_atc_openabap_84( ) ).
  ENDMETHOD.

ENDCLASS.
