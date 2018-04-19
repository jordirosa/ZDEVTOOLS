CLASS ztl_cl_dynamic_type_conversion DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: c_conversion_type_direct TYPE ztle_dyncnv_conv_type VALUE 'D',
               c_conversion_type_move TYPE ztle_dyncnv_conv_type VALUE 'M',
               c_conversion_type_class TYPE ztle_dyncnv_conv_type VALUE 'C'.

    CLASS-METHODS dynamic_conversion
      IMPORTING
        i_source TYPE ANY
      EXPORTING
        e_destination TYPE ANY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: t_ztlt_dyncnv_0001_tab TYPE STANDARD TABLE OF ztlt_dyncnv_0001
                                  WITH DEFAULT KEY.
ENDCLASS.



CLASS ztl_cl_dynamic_type_conversion IMPLEMENTATION.
  METHOD dynamic_conversion.
    DATA: lo_dyn_type_conversion TYPE REF TO ztl_i_dynamic_type_conversion,

          lo_src_type TYPE REF TO cl_abap_typedescr,
          lo_dst_type TYPE REF TO cl_abap_typedescr,

          lcx_dyncnv_config_missing TYPE REF TO zcx_dyncnv_config_missing,

          l_config_missing TYPE abap_bool,

          l_src_type_name TYPE string,
          l_dst_type_name TYPE string,

          lwa_dyncnv_0001 TYPE ztlt_dyncnv_0001.

    CALL METHOD cl_abap_typedescr=>describe_by_data
      EXPORTING
        p_data      = i_source
      RECEIVING
        p_descr_ref = lo_src_type.

    CALL METHOD cl_abap_typedescr=>describe_by_data
      EXPORTING
        p_data      = e_destination
      RECEIVING
        p_descr_ref = lo_dst_type.

    IF lo_src_type->absolute_name = lo_dst_type->absolute_name.
      e_destination = i_source.
    ELSE.
      SELECT SINGLE *
      FROM ztlt_dyncnv_0001
      INTO lwa_dyncnv_0001
      WHERE src_type = lo_src_type->absolute_name
        AND dst_type = lo_dst_type->absolute_name.

      IF sy-subrc = 0.
        IF lwa_dyncnv_0001-active = abap_true.
          CASE lwa_dyncnv_0001-conversion_type.
            WHEN ztl_cl_dynamic_type_conversion=>c_conversion_type_direct.
              e_destination = i_source.
            WHEN ztl_cl_dynamic_type_conversion=>c_conversion_type_move.
              MOVE-CORRESPONDING i_source TO e_destination.
            WHEN ztl_cl_dynamic_type_conversion=>c_conversion_type_class.
              CREATE OBJECT lo_dyn_type_conversion TYPE (lwa_dyncnv_0001-conversion_class).

              IF lo_dyn_type_conversion IS BOUND.
                CALL METHOD lo_dyn_type_conversion->convert
                  EXPORTING
                    i_source      = i_source
                  IMPORTING
                    e_destination = e_destination.
              ENDIF.
          ENDCASE.
        ELSE.
          l_config_missing = abap_true.
        ENDIF.
      ELSE.
        lwa_dyncnv_0001-src_type = lo_src_type->absolute_name.
        lwa_dyncnv_0001-dst_type = lo_dst_type->absolute_name.
        lwa_dyncnv_0001-conversion_type = c_conversion_type_direct.
        CLEAR lwa_dyncnv_0001-conversion_class.
        lwa_dyncnv_0001-active = abap_false.

        INSERT ztlt_dyncnv_0001 FROM lwa_dyncnv_0001.
        COMMIT WORK.

        l_config_missing = abap_true.
      ENDIF.

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
