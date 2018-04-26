CLASS ztl_cl_message_pool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: ztl_i_dynamic_type_conversion.

    TYPES: t_message_type TYPE sychar01,
           t_message_var  TYPE sychar50,

           BEGIN OF t_message_related_data,
             data   TYPE REF TO data,
             child  TYPE REF TO data,
           END OF t_message_related_data,

           BEGIN OF t_message_info,
             related_data TYPE t_message_related_data,
             id           TYPE arbgb,
             number       TYPE msgnr,
             v1           TYPE t_message_var,
             v2           TYPE t_message_var,
             v3           TYPE t_message_var,
             v4           TYPE t_message_var,
             type         TYPE t_message_type,
             text         TYPE string,
           END OF t_message_info,

           t_message_info_tab TYPE STANDARD TABLE OF t_message_info.

    CONSTANTS: BEGIN OF e_message_types,
                 sucess  TYPE t_message_type VALUE 'S',
                 info    TYPE t_message_type VALUE 'I',
                 warning TYPE t_message_type VALUE 'W',
                 error   TYPE t_message_type VALUE 'E',
                 abend   TYPE t_message_type VALUE 'A',
               END OF e_message_types.

    CLASS-METHODS class_constructor.

    METHODS add_message
      IMPORTING
        iv_type         TYPE t_message_type
        iv_id           TYPE arbgb
        iv_number       TYPE msgnr
        iv_v1           TYPE t_message_var OPTIONAL
        iv_v2           TYPE t_message_var OPTIONAL
        iv_v3           TYPE t_message_var OPTIONAL
        iv_v4           TYPE t_message_var OPTIONAL
        iv_related_data TYPE any OPTIONAL
      RAISING
        zcx_invalid_parameters.

    METHODS add_message_from_dynamic
      IMPORTING
        iv_message_data TYPE any
        iv_related_data TYPE any OPTIONAL
      RAISING
        zcx_invalid_parameters
        zcx_dyncnv_config_missing.

    METHODS get_messages
      EXPORTING
        ev_messages TYPE t_message_info_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF e_conversion_destinations,
                 message_pool_message_info TYPE abap_abstypename VALUE '\CLASS=ZTL_CL_MESSAGE_POOL\TYPE=T_MESSAGE_INFO',
               END OF e_conversion_destinations.

    CLASS-DATA: valid_message_types TYPE RANGE OF t_message_type.

    DATA: m_message_info TYPE t_message_info_tab.

    METHODS helper_add_message_info
      IMPORTING
        iv_related_data TYPE any OPTIONAL
      CHANGING
        ic_message_info TYPE t_message_info
      RAISING
        zcx_invalid_parameters.
ENDCLASS.



CLASS ztl_cl_message_pool IMPLEMENTATION.


  METHOD ztl_i_dynamic_type_conversion~convert.
    FIELD-SYMBOLS: <lo_ztl_cl_message_pool> TYPE REF TO ztl_cl_message_pool.

    DATA: li_messages TYPE ztl_cl_message_pool=>t_message_info_tab.

    ASSIGN iv_source TO <lo_ztl_cl_message_pool>.

    CALL METHOD <lo_ztl_cl_message_pool>->get_messages
      IMPORTING
        ev_messages = li_messages.

    CASE iv_destination_type.
      WHEN e_conversion_destinations-message_pool_message_info.
        IF iv_destination_is_table = abap_true.
          ev_destination = li_messages.
        ELSE.
          READ TABLE li_messages INTO ev_destination INDEX 1.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD add_message.
    DATA: lwa_message_info TYPE ztl_cl_message_pool=>t_message_info.

    lwa_message_info-type = iv_type.
    lwa_message_info-id = iv_id.
    lwa_message_info-number = iv_number.
    lwa_message_info-v1 = iv_v1.
    lwa_message_info-v2 = iv_v2.
    lwa_message_info-v3 = iv_v3.
    lwa_message_info-v4 = iv_v4.

    CALL METHOD helper_add_message_info
      EXPORTING
        iv_related_data = iv_related_data
      CHANGING
        ic_message_info = lwa_message_info.
  ENDMETHOD.


  METHOD add_message_from_dynamic.
    FIELD-SYMBOLS: <lit_table> TYPE ANY TABLE,
                   <lwa_line>  TYPE any.

    DATA: lo_typedescr     TYPE REF TO cl_abap_typedescr,

          lwa_message_info TYPE t_message_info,
          li_message_info  TYPE t_message_info_tab.

    CALL METHOD cl_abap_typedescr=>describe_by_data
      EXPORTING
        p_data      = iv_message_data
      RECEIVING
        p_descr_ref = lo_typedescr.

    CASE lo_typedescr->type_kind.
      WHEN cl_abap_typedescr=>typekind_oref.
        CALL METHOD ztl_cl_dynamic_type_conversion=>dynamic_conversion
          EXPORTING
            iv_source      = iv_message_data
          IMPORTING
            ev_destination = li_message_info.

        LOOP AT li_message_info INTO lwa_message_info.
          CALL METHOD helper_add_message_info
            EXPORTING
              iv_related_data = iv_related_data
            CHANGING
              ic_message_info = lwa_message_info.
        ENDLOOP.

      WHEN cl_abap_typedescr=>typekind_table.
        ASSIGN iv_message_data TO <lit_table>.

        LOOP AT <lit_table> INTO <lwa_line>.
          CALL METHOD ztl_cl_dynamic_type_conversion=>dynamic_conversion
            EXPORTING
              iv_source      = <lwa_line>
            IMPORTING
              ev_destination = lwa_message_info.

          CALL METHOD helper_add_message_info
            EXPORTING
              iv_related_data = iv_related_data
            CHANGING
              ic_message_info = lwa_message_info.
        ENDLOOP.

      WHEN OTHERS.
        CALL METHOD ztl_cl_dynamic_type_conversion=>dynamic_conversion
          EXPORTING
            iv_source      = iv_message_data
          IMPORTING
            ev_destination = lwa_message_info.

        CALL METHOD helper_add_message_info
          EXPORTING
            iv_related_data = iv_related_data
          CHANGING
            ic_message_info = lwa_message_info.
    ENDCASE.
  ENDMETHOD.


  METHOD class_constructor.
    DATA: lwa_valid_message_types LIKE LINE OF ztl_cl_message_pool=>valid_message_types.

    append_equals valid_message_types ztl_cl_message_pool=>e_message_types-sucess.
    append_equals valid_message_types ztl_cl_message_pool=>e_message_types-info.
    append_equals valid_message_types ztl_cl_message_pool=>e_message_types-warning.
    append_equals valid_message_types ztl_cl_message_pool=>e_message_types-error.
    append_equals valid_message_types ztl_cl_message_pool=>e_message_types-abend.
  ENDMETHOD.


  METHOD helper_add_message_info.
    FIELD-SYMBOLS: <lwa_related_data> TYPE t_message_related_data,
                   <lwa_related_data_aux> TYPE t_message_related_data,
                   <lwa_data> TYPE ANY,
                   <lwa_data_aux> TYPE ANY.

    DATA: l_related_data TYPE t_message_related_data,
          l_related_data_aux TYPE t_message_related_data.

*   We check the value for type parameter.
    CALL METHOD ztl_cl_global=>check_parameter_value_valid
      EXPORTING
        iv_value        = ic_message_info-type
        it_valid_values = ztl_cl_message_pool=>valid_message_types.

*   If we receive related data.
    IF iv_related_data IS SUPPLIED.
*     We assign the current related data to an aux variable.
      l_related_data = ic_message_info-related_data.
*     We current related data.
      CLEAR ic_message_info-related_data.

*     We create a variable on related_data-data pointer and assign it to
*     field-symbol to assign the value of received iv_related_data.
      CREATE DATA ic_message_info-related_data-data LIKE iv_related_data.
      ASSIGN ic_message_info-related_data-data->* TO <lwa_data>.

      <lwa_data> = iv_related_data.

*     If we have related data on our aux variable we need to move it to
*     the current related data as a child.
      IF NOT l_related_data IS INITIAL.
        ASSIGN l_related_data TO <lwa_related_data>.
        ASSIGN ic_message_info-related_data TO <lwa_related_data_aux>.
        WHILE sy-subrc = 0.
          CREATE DATA <lwa_related_data_aux>-child TYPE t_message_related_data.
          ASSIGN <lwa_related_data_aux>-child->* TO <lwa_related_data_aux>.

          ASSIGN <lwa_related_data>-data->* TO <lwa_data>.
          CREATE DATA <lwa_related_data_aux>-data LIKE <lwa_data>.
          ASSIGN <lwa_related_data_aux>-data->* TO <lwa_data_aux>.

          <lwa_data_aux> = <lwa_data>.

          ASSIGN <lwa_related_data>-child->* TO <lwa_related_data>.
        ENDWHILE.
      ENDIF.
    ENDIF.

*   We build the message text.
    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        language               = sy-langu
        msg_id                 = ic_message_info-id
        msg_no                 = ic_message_info-number
        msg_var1               = ic_message_info-v1
        msg_var2               = ic_message_info-v2
        msg_var3               = ic_message_info-v3
        msg_var4               = ic_message_info-v4
      IMPORTING
        msg_text               = ic_message_info-text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

*   Message is ok, we add it to our member attribute.
    APPEND ic_message_info TO m_message_info.
  ENDMETHOD.

  METHOD get_messages.
    ev_messages = me->m_message_info.
  ENDMETHOD.

ENDCLASS.
