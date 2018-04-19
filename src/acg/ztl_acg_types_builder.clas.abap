CLASS ztl_acg_types_builder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: c_prefix_local  TYPE string VALUE 'LT_',
               c_prefix_global TYPE string VALUE 'T_'.

    CLASS-METHODS class_constructor.

    CLASS-METHODS generate_name
      IMPORTING
        i_scope       TYPE ztl_acg_global=>t_scope
        i_name        TYPE string
      RETURNING
        VALUE(r_name) TYPE string.

    METHODS constructor
      IMPORTING
        i_name TYPE string.

    METHODS get_name
      RETURNING
        VALUE(r_result) TYPE string.
    METHODS set_name
      IMPORTING
        i_m_name TYPE string.

    METHODS set_type_internal_type
      IMPORTING
        i_typekind TYPE abap_typekind DEFAULT cl_abap_typedescr=>typekind_char
        i_length   TYPE i DEFAULT 1
        i_decimals TYPE i OPTIONAL.

    METHODS generate_statement
      IMPORTING
        i_scope      TYPE ztl_acg_global=>t_scope
      CHANGING
        c_sourcecode TYPE rswsourcet.


  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: allowed_abap_typekinds TYPE RANGE OF abap_typekind,
                allowed_len_abap_typekinds TYPE RANGE OF abap_typekind,
                allowed_dec_abap_typekinds TYPE RANGE OF abap_typekind.

    DATA: m_name     TYPE string,

          m_typekind TYPE abap_typekind,
          m_length   TYPE i,
          m_decimals TYPE i.
ENDCLASS.



CLASS ztl_acg_types_builder IMPLEMENTATION.

  METHOD class_constructor.
    DATA: lwa_abap_typekind_range LIKE LINE OF ztl_acg_types_builder=>allowed_abap_typekinds.

    APPEND_ALLOWED_ABAP_TYPEKIND cl_abap_typedescr=>typekind_char.
    APPEND_ALLOWED_ABAP_TYPEKIND cl_abap_typedescr=>typekind_int.
    APPEND_ALLOWED_ABAP_TYPEKIND cl_abap_typedescr=>typekind_float.
    APPEND_ALLOWED_ABAP_TYPEKIND cl_abap_typedescr=>typekind_packed.

    APPEND_ALLOW_LEN_ABAP_TYPEKIND cl_abap_typedescr=>typekind_char.
    APPEND_ALLOW_LEN_ABAP_TYPEKIND cl_abap_typedescr=>typekind_packed.

    APPEND_ALLOW_DEC_ABAP_TYPEKIND cl_abap_typedescr=>typekind_packed.
  ENDMETHOD.


  METHOD constructor.
    me->m_name = i_name.

    me->m_typekind = cl_abap_typedescr=>typekind_char.
    me->m_length = 1.
    me->m_decimals = -1.
  ENDMETHOD.


  METHOD get_name.
    r_result = me->m_name.
  ENDMETHOD.


  METHOD set_name.
    me->m_name = i_m_name.
  ENDMETHOD.


  METHOD generate_name.
    DATA: l_prefix TYPE string.

    CASE i_scope.
      WHEN ztl_acg_global=>c_scope_local.
        l_prefix = c_prefix_local.
      WHEN ztl_acg_global=>c_scope_global.
        l_prefix = c_prefix_global.
    ENDCASE.

    CONCATENATE l_prefix
                i_name
    INTO r_name.
    TRANSLATE r_name TO LOWER CASE.
  ENDMETHOD.


  METHOD set_type_internal_type.
    IF i_typekind IN ztl_acg_types_builder=>allowed_abap_typekinds.
      me->m_typekind = i_typekind.
      IF i_typekind IN ztl_acg_types_builder=>allowed_len_abap_typekinds.
        me->m_length = i_length.
      ELSE.
        me->m_length = -1.
      ENDIF.
      IF i_typekind IN ztl_acg_types_builder=>allowed_dec_abap_typekinds.
        me->m_decimals = i_decimals.
      ELSE.
        me->m_decimals = -1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD generate_statement.
    DATA: l_name            TYPE string,
          l_typekind        TYPE abap_typekind,
          l_length_string   TYPE string,
          l_decimals_string TYPE string,

          l_sourcecode      TYPE string.

    CALL METHOD ztl_acg_types_builder=>generate_name
      EXPORTING
        i_scope = i_scope
        i_name  = me->m_name
      RECEIVING
        r_name  = l_name.

    l_length_string = me->m_length.
    l_typekind = me->m_typekind.
    TRANSLATE l_typekind TO LOWER CASE.
    CONCATENATE ztl_acg_global=>c_keyword_types
                l_name
                ztl_acg_global=>c_keyword_type
                l_typekind
    INTO l_sourcecode SEPARATED BY space.
    CONDENSE l_sourcecode.

    IF me->m_length > 1 AND me->m_typekind IN ztl_acg_types_builder=>allowed_len_abap_typekinds.
      l_length_string = me->m_length.
      CONCATENATE l_sourcecode
                  ztl_acg_global=>c_keyword_length
                  l_length_string
      INTO l_sourcecode SEPARATED BY space.
      CONDENSE l_sourcecode.
    ENDIF.

    IF me->m_decimals > 0 AND me->m_typekind IN ztl_acg_types_builder=>allowed_dec_abap_typekinds.
      l_decimals_string = me->m_decimals.
      CONCATENATE l_sourcecode
                  ztl_acg_global=>c_keyword_decimals
                  l_decimals_string
      INTO l_sourcecode SEPARATED BY space.
      CONDENSE l_sourcecode.
    ENDIF.

    CONCATENATE l_sourcecode
                ztl_acg_global=>c_operator_end_statement
    INTO l_sourcecode.

    APPEND l_sourcecode TO c_sourcecode.
  ENDMETHOD.

ENDCLASS.
