CLASS ztl_cl_dynamic_type_conversion DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF e_conversion_type,
                 direct_assigment TYPE ztle_dyncnv_conv_type VALUE 'D',
                 move_corresponding TYPE ztle_dyncnv_conv_type VALUE 'M',
                 class TYPE ztle_dyncnv_conv_type VALUE 'C',
               END OF e_conversion_type.

    CLASS-METHODS dynamic_conversion
      IMPORTING
        iv_source TYPE ANY
      EXPORTING
        ev_destination TYPE ANY
      RAISING
        zcx_dyncnv_config_missing.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: t_ztlt_dyncnv_0001_tab TYPE STANDARD TABLE OF ztlt_dyncnv_0001
                                  WITH DEFAULT KEY.
ENDCLASS.



CLASS ztl_cl_dynamic_type_conversion IMPLEMENTATION.
  METHOD dynamic_conversion.
    DATA: lo_dyn_type_conversion TYPE REF TO ztl_i_dynamic_type_conversion,

          lo_src_type TYPE REF TO cl_abap_typedescr,
          lo_src_type_table TYPE REF TO cl_abap_tabledescr,
          lo_dst_type TYPE REF TO cl_abap_typedescr,
          lo_dst_type_table TYPE REF TO cl_abap_tabledescr,

          l_src_is_table TYPE abap_bool,
          l_dst_is_table TYPE abap_bool,

          lcx_dyncnv_config_missing TYPE REF TO zcx_dyncnv_config_missing,

          l_config_missing TYPE abap_bool,

          l_src_type_name TYPE string,
          l_dst_type_name TYPE string,

          lwa_dyncnv_0001 TYPE ztlt_dyncnv_0001.

*   Get type of source data
    CALL METHOD cl_abap_typedescr=>describe_by_data
      EXPORTING
        p_data      = iv_source
      RECEIVING
        p_descr_ref = lo_src_type.

    CASE lo_src_type->type_kind.
*     When source data is an object ref (instance of a class)
*     we must get the type descriptor using describe_by_object_ref.
      WHEN cl_abap_typedescr=>typekind_oref.
        CALL METHOD cl_abap_typedescr=>describe_by_object_ref
          EXPORTING
            p_object_ref = iv_source
          RECEIVING
            p_descr_ref  = lo_src_type.

*     When source data is a table we will use the line type and
*     we will mark a flag for later table treatment.
      WHEN cl_abap_typedescr=>typekind_table.
        l_src_is_table = abap_true.

        lo_src_type_table ?= lo_src_type.

        CALL METHOD lo_src_type_table->get_table_line_type
          RECEIVING
            p_descr_ref = lo_src_type.
    ENDCASE.

*   Get type of destination data
    CALL METHOD cl_abap_typedescr=>describe_by_data
      EXPORTING
        p_data      = ev_destination
      RECEIVING
        p_descr_ref = lo_dst_type.

    CASE lo_dst_type->type_kind.
*     When destination data is an object ref (instance of a class)
*     we must get the type descriptor using describe_by_object_ref.
      WHEN cl_abap_typedescr=>typekind_oref.
        CALL METHOD cl_abap_typedescr=>describe_by_object_ref
          EXPORTING
            p_object_ref = ev_destination
          RECEIVING
            p_descr_ref  = lo_dst_type.

*     When destination data is a table we will use the line type and
*     we will mark a flag for later table treatment.
      WHEN cl_abap_typedescr=>typekind_table.
        l_dst_is_table = abap_true.

        lo_dst_type_table ?= lo_dst_type.

        CALL METHOD lo_dst_type_table->get_table_line_type
          RECEIVING
            p_descr_ref = lo_dst_type.
    ENDCASE.

*   If source and target structure are the same type no conversion
*   is needed.
    IF lo_src_type->absolute_name = lo_dst_type->absolute_name.
      ev_destination = iv_source.
*   Else we will convert.
    ELSE.
*     We select the conversion configuration from ZTLT_DYNCNV_0001.
      SELECT SINGLE *
      FROM ztlt_dyncnv_0001
      INTO lwa_dyncnv_0001
      WHERE src_type = lo_src_type->absolute_name
        AND dst_type = lo_dst_type->absolute_name.

*     If there's conversion configuration.
      IF sy-subrc = 0.
*       If conversion configuration is active.
        IF lwa_dyncnv_0001-active = abap_true.
          CASE lwa_dyncnv_0001-conversion_type.
*           When conversion type is direct_assigment we make a direct assignment.
            WHEN ztl_cl_dynamic_type_conversion=>e_conversion_type-direct_assigment.
              ev_destination = iv_source.
*           When conversion type is move_corresponding we make the move corresponding.
            WHEN ztl_cl_dynamic_type_conversion=>e_conversion_type-move_corresponding.
              MOVE-CORRESPONDING iv_source TO ev_destination.
*           When conversion type is class we call method convert for a new instance of the
*           configured class.
            WHEN ztl_cl_dynamic_type_conversion=>e_conversion_type-class.
              CREATE OBJECT lo_dyn_type_conversion TYPE (lwa_dyncnv_0001-conversion_class).

              IF lo_dyn_type_conversion IS BOUND.
                CALL METHOD lo_dyn_type_conversion->convert
                  EXPORTING
                    iv_source               = iv_source
                    iv_source_type          = lo_src_type->absolute_name
                    iv_source_is_table      = l_src_is_table
                    iv_destination_type     = lo_dst_type->absolute_name
                    iv_destination_is_table = l_dst_is_table
                  IMPORTING
                    ev_destination = ev_destination.
              ENDIF.
          ENDCASE.
*       If conversion configuration is not active.
        ELSE.
*         We mark configuration missing flag.
          l_config_missing = abap_true.
        ENDIF.
*     If there's not conversion configuration.
      ELSE.
*       We insert a line in configuration table in inactive state.
        lwa_dyncnv_0001-src_type = lo_src_type->absolute_name.
        lwa_dyncnv_0001-dst_type = lo_dst_type->absolute_name.
        lwa_dyncnv_0001-conversion_type = e_conversion_type-direct_assigment.
        CLEAR lwa_dyncnv_0001-conversion_class.
        lwa_dyncnv_0001-active = abap_false.

        INSERT ztlt_dyncnv_0001 FROM lwa_dyncnv_0001.
*       The exception does rollback we would lose the line if we
*       don't do commit work (TO DO: To avoid commit data from other
*       programs we must do the commit in a new LUW)
        COMMIT WORK.

*       We mark configuration missing flag.
        l_config_missing = abap_true.
      ENDIF.

*     If configuration is missing we raise an exception.
      IF l_config_missing = abap_true.
        CALL METHOD lo_src_type->get_relative_name
          RECEIVING
            p_relative_name = l_src_type_name.

        CALL METHOD lo_dst_type->get_relative_name
          RECEIVING
            p_relative_name = l_dst_type_name.

        CREATE OBJECT lcx_dyncnv_config_missing
          EXPORTING
            i_source_type = l_src_type_name
            i_destination_type = l_dst_type_name.

        RAISE EXCEPTION lcx_dyncnv_config_missing.
      ENDIF.
    ENDIF.


  ENDMETHOD.

ENDCLASS.
