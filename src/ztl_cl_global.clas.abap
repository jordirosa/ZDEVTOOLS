CLASS ztl_cl_global DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: t_range_sign TYPE char1,
           t_range_option TYPE char2.

    CONSTANTS: c_msgid_ztools TYPE arbgb VALUE 'ZTOOLS',

               c_rng_sgn_include TYPE t_range_sign VALUE 'I',
               c_rng_sgn_exclude TYPE t_range_sign VALUE 'E',
               c_rng_opt_equals TYPE t_range_option VALUE 'EQ',
               c_rng_opt_between TYPE t_range_option VALUE 'BT',
               c_rng_opt_contains_pattern TYPE t_range_option VALUE 'CP',

               c_comment_line TYPE c VALUE '*',

               c_regex_clean_comments TYPE string VALUE '^([^"]*)'.

    CLASS-METHODS: check_parameter_value_valid
      IMPORTING
        i_value TYPE ANY
        i_valid_values TYPE TABLE
        i_parameter_name TYPE string OPTIONAL
      RAISING
        zcx_invalid_parameters.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_regex_i_value_parameter TYPE string VALUE '^.*I_VALUE.*= *([^ ]*)'.
ENDCLASS.



CLASS ZTL_CL_GLOBAL IMPLEMENTATION.


  METHOD check_parameter_value_valid.
    DATA: lcx_invalid_parameters TYPE REF TO zcx_invalid_parameters,

          li_stack TYPE cl_abap_get_call_stack=>call_stack_internal,

          lwa_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry,
          li_formatted_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack,

          lwa_report TYPE string,
          lit_report TYPE rswsourcet,

          l_parameter_name TYPE sychar50.

    IF NOT i_value IN i_valid_values.
      IF NOT i_parameter_name IS SUPPLIED.
        CALL METHOD cl_abap_get_call_stack=>get_call_stack
          RECEIVING
            stack = li_stack.

        CALL METHOD cl_abap_get_call_stack=>format_call_stack_with_struct
          EXPORTING
            stack           = li_stack
          RECEIVING
            formatted_stack = li_formatted_stack.

        READ TABLE li_formatted_stack INTO lwa_formatted_stack INDEX 2.
        IF sy-subrc = 0.
          READ REPORT lwa_formatted_stack-includename INTO lit_report.

          LOOP AT lit_report FROM lwa_formatted_stack-line INTO lwa_report.
            IF lwa_report(1) = c_comment_line.
              CONTINUE.
            ENDIF.

            TRANSLATE lwa_report TO UPPER CASE.
            FIND REGEX c_regex_clean_comments IN lwa_report SUBMATCHES lwa_report.

            FIND REGEX c_regex_i_value_parameter IN lwa_report SUBMATCHES l_parameter_name.

            IF sy-subrc = 0 OR lwa_report CA '.'.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.
        l_parameter_name = i_parameter_name.
      ENDIF.

      CREATE OBJECT lcx_invalid_parameters
        EXPORTING
          i_msgid = ztl_cl_global=>c_msgid_ztools
          i_msgno = '002'
          i_msgv1 = l_parameter_name.

      RAISE EXCEPTION lcx_invalid_parameters.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
