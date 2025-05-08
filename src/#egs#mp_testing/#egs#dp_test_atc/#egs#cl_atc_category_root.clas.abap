CLASS /egs/cl_atc_category_root DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_category_root FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS /egs/cl_atc_category_root IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    description = 'EPI-USE Global services'.
    category    = 'CL_CI_CATEGORY_TOP'.
    position    = '000'.

  ENDMETHOD.

ENDCLASS.
