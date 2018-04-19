CLASS ztl_cl_message_pool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: t_message_type TYPE sychar01,
           t_message_var TYPE sychar50,

           BEGIN OF t_message_info,
             id TYPE arbgb,
             number TYPE msgnr,
             v1 TYPE t_message_var,
             v2 TYPE t_message_var,
             v3 TYPE t_message_var,
             v4 TYPE t_message_var,
             type TYPE t_message_type,
           END OF t_message_info,

           t_message_info_tab TYPE STANDARD TABLE OF t_message_info.

    CONSTANTS: c_msg_type_sucess TYPE t_message_type VALUE 'S',
               c_msg_type_info TYPE t_message_type VALUE 'I',
               c_msg_type_warning TYPE t_message_type VALUE 'W',
               c_msg_type_error TYPE t_message_type VALUE 'E',
               c_msg_type_abend TYPE t_message_type VALUE 'A'.

    CLASS-METHODS class_constructor.

    METHODS add_message
      IMPORTING
        i_type TYPE t_message_type
        i_id TYPE arbgb
        i_number TYPE msgnr
        i_v1 TYPE t_message_var OPTIONAL
        i_v2 TYPE t_message_var OPTIONAL
        i_v3 TYPE t_message_var OPTIONAL
        i_v4 TYPE t_message_var OPTIONAL
      RAISING
        zcx_invalid_parameters.

    METHODS add_message_from_dynamic
      IMPORTING
        i_message_info TYPE ANY
      RAISING
        zcx_invalid_parameters.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: valid_message_types TYPE RANGE OF t_message_type.

    DATA: m_message_info TYPE t_message_info_tab.
ENDCLASS.



CLASS ZTL_CL_MESSAGE_POOL IMPLEMENTATION.


  METHOD class_constructor.
    DATA: lwa_valid_message_types LIKE LINE OF ztl_cl_message_pool=>valid_message_types.

    APPEND_EQUALS valid_message_types ztl_cl_message_pool=>c_msg_type_sucess.
    APPEND_EQUALS valid_message_types ztl_cl_message_pool=>c_msg_type_info.
    APPEND_EQUALS valid_message_types ztl_cl_message_pool=>c_msg_type_warning.
    APPEND_EQUALS valid_message_types ztl_cl_message_pool=>c_msg_type_error.
    APPEND_EQUALS valid_message_types ztl_cl_message_pool=>c_msg_type_abend.
  ENDMETHOD.


  METHOD add_message.
    DATA: lwa_message_info TYPE ztl_cl_message_pool=>t_message_info.

    CALL METHOD ztl_cl_global=>check_parameter_value_valid
      EXPORTING
        i_value        = i_type
        i_valid_values = ztl_cl_message_pool=>valid_message_types.

    lwa_message_info-type = i_type.
    lwa_message_info-id = i_id.
    lwa_message_info-number = i_number.
    lwa_message_info-v1 = i_v1.
    lwa_message_info-v2 = i_v2.
    lwa_message_info-v3 = i_v3.
    lwa_message_info-v4 = i_v4.
    APPEND lwa_message_info TO m_message_info.
  ENDMETHOD.


  METHOD add_message_from_dynamic.
    DATA: lwa_message_info TYPE t_message_info.

    CALL METHOD ztl_cl_dynamic_type_conversion=>dynamic_conversion
      EXPORTING
        i_source      = i_message_info
      IMPORTING
        e_destination = lwa_message_info.

    CALL METHOD ztl_cl_global=>check_parameter_value_valid
      EXPORTING
        i_value          = lwa_message_info-id
        i_valid_values   = ztl_cl_message_pool=>valid_message_types.

    APPEND lwa_message_info TO m_message_info.
  ENDMETHOD.
ENDCLASS.
