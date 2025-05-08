CLASS /egs/cl_atc_oa_84_example DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES /egs/if_atc_oa_84_example.

    ALIASES: c_cl_category_exception FOR /egs/if_atc_oa_84_example~c_cl_category_exception,
             c_cl_prefix             FOR /egs/if_atc_oa_84_example~c_cl_prefix,
             c_cx_prefix             FOR /egs/if_atc_oa_84_example~c_cx_prefix,
             gv_variable             FOR /egs/if_atc_oa_84_example~mv_variable.

    METHODS get_class_refix
      IMPORTING iv_category      TYPE seocategry
      RETURNING VALUE(rv_result) TYPE string.
ENDCLASS.

CLASS /egs/cl_atc_oa_84_example IMPLEMENTATION.

  METHOD get_class_refix.

    IF iv_category = c_cl_category_exception.
      rv_result = 'CX_'.
    ELSE.
      rv_result = 'CL_'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
