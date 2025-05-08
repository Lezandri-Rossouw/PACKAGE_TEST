CLASS ltcl_test DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS first_test FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD first_test.

    DATA(lo_cut) = NEW /egs/cl_atc_oa_84_example( ).

    cl_abap_unit_assert=>assert_equals( exp = /egs/if_atc_oa_84_example=>c_cx_prefix
                                        act = lo_cut->get_class_refix( 40 ) ).

    cl_abap_unit_assert=>assert_equals( exp = /egs/if_atc_oa_84_example=>c_cl_prefix
                                        act = lo_cut->get_class_refix( 0 ) ).

  ENDMETHOD.

ENDCLASS.
