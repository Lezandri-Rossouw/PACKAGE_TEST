CLASS /egs/cl_pri_bi_param_mapping DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_badi_interface.
    INTERFACES /egs/if_prs_param_mapping.

    ALIASES: param_mapping FOR /egs/if_prs_param_mapping~param_mapping.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /egs/cl_pri_bi_param_mapping IMPLEMENTATION.

  METHOD /egs/if_prs_param_mapping~param_mapping.

    CASE is_as-funco.

      WHEN '_XX'.
        CASE is_as-parm1.
          WHEN 'NET'.
            rv_result = 'SA/NET'.
          WHEN 'TAX'.
            rv_result = 'SA/PAYE'.
          WHEN 'UIF'.
            rv_result = 'SA/UIF'.

        ENDCASE.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
