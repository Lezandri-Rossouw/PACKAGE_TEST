CLASS /egs/cl_atc_openabap_84 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.
    METHODS run                         REDEFINITION.
    METHODS get_attributes              REDEFINITION.
    METHODS put_attributes              REDEFINITION.
    METHODS if_ci_test~query_attributes REDEFINITION.

  PROTECTED SECTION.

    DATA: mt_classes                 TYPE zaoc_seoclsname_range_tt,
          mv_read_only_allowed       TYPE flag,
          mv_exclude_alias_constants TYPE flag,
          mv_exclude_cx_classes      TYPE flag,
          mt_attributes              TYPE /egs/cl_util_class_metadata=>ty_t_vseocompdf.

  PRIVATE SECTION.

    METHODS is_class_excluded_in_atc_setup
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS is_exception_class
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS delete_alias_constant
      IMPORTING is_component     TYPE vseocompdf
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

CLASS /egs/cl_atc_openabap_84 IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '084'.

    has_documentation = abap_false.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'INTF' ).
    add_obj_type( 'CLAS' ).

    insert_scimessage( iv_code = '001'
                       iv_text = 'No public attributes, &1'(m01) ).

    description = '084 - GS: No Public attributes and ignore inherited CONSTANTS'(001).
    myname = '/EGS/CL_ATC_OPENABAP_84'.

  ENDMETHOD.

  METHOD get_attributes.

    EXPORT mv_errty                   = mv_errty
           mt_classes                 = mt_classes
           mv_read_only_allowed       = mv_read_only_allowed
           mv_exclude_alias_constants = mv_exclude_alias_constants
           mv_exclude_cx_classes      = mv_exclude_cx_classes TO DATA BUFFER p_attributes.

  ENDMETHOD.

  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' '' ##NO_TEXT.
    zzaoc_fill_att mt_classes 'Skip Classes' 'S' ##NO_TEXT.
    zzaoc_fill_att mv_read_only_allowed 'Allow READ-ONLY attributes' '' ##NO_TEXT.
    zzaoc_fill_att mv_exclude_alias_constants 'Exclude alias constants' '' ##NO_TEXT.
    zzaoc_fill_att mv_exclude_cx_classes 'Exclude check classes' '' ##NO_TEXT.
    zzaoc_popup.

  ENDMETHOD.

  METHOD put_attributes.

    IMPORT mv_errty                   = mv_errty
           mt_classes                 = mt_classes
           mv_read_only_allowed       = mv_read_only_allowed
           mv_exclude_alias_constants = mv_exclude_alias_constants
           mv_exclude_cx_classes      = mv_exclude_cx_classes FROM DATA BUFFER p_attributes. "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD run.

    CLEAR mt_attributes.

    DATA(lv_class_name) = CONV seoclsname( object_name ).

    IF is_class_excluded_in_atc_setup( ).
      RETURN.
    ENDIF.

    IF is_exception_class( ) AND mv_exclude_cx_classes = abap_true.
      RETURN.
    ENDIF.

    TRY.
        mt_attributes = /egs/cl_util_class_metadata=>get_private_attributes( lv_class_name ).
      CATCH cx_static_check.
        RETURN.
    ENDTRY.

    LOOP AT mt_attributes ASSIGNING FIELD-SYMBOL(<ls_result>).
      IF <ls_result>-attdecltyp = /egs/cl_util_class_metadata=>c_constant.
        DELETE mt_attributes.
      ELSEIF delete_alias_constant( <ls_result> ).
        DELETE mt_attributes.
      ENDIF.
    ENDLOOP.

    LOOP AT mt_attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>).
      inform( p_test    = myname
              p_kind    = mv_errty
              p_code    = '001'
              p_param_1 = <ls_attribute>-cmpname ).
    ENDLOOP.

  ENDMETHOD.

  METHOD delete_alias_constant.

    IF mv_exclude_alias_constants = abap_false.
      rv_result = abap_false.
    ELSEIF is_component-refclsname IS INITIAL.
      rv_result = abap_false.
    ELSE.
      TRY.
          DATA(lv_declaration_type) = /egs/cl_util_class_metadata=>get_component(
                                          iv_class_name     = is_component-refclsname
                                          iv_component_name = is_component-refcmpname )-attdecltyp.
          IF lv_declaration_type = /egs/cl_util_class_metadata=>c_constant.
            rv_result = abap_true.
          ELSE.
            rv_result = abap_false.
          ENDIF.
        CATCH cx_static_check.
          rv_result = abap_false.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD is_class_excluded_in_atc_setup.

    IF lines( mt_classes ) > 0 AND object_name IN mt_classes.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD is_exception_class.

    DATA(lv_class_name) = CONV seoclsname( object_name ).

    TRY.
        DATA(ls_defenition) = /egs/cl_util_class_metadata=>get_definition( lv_class_name ).
      CATCH cx_static_check.
        rv_result = abap_false.
    ENDTRY.

    IF ls_defenition-category = seoc_category_exception.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
