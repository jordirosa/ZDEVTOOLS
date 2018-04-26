interface ZTL_I_DYNAMIC_TYPE_CONVERSION
  public .
    METHODS convert
      IMPORTING
        iv_source TYPE ANY
        iv_source_type TYPE abap_abstypename
        iv_source_is_table TYPE abap_bool
        iv_destination_type TYPE abap_abstypename
        iv_destination_is_table TYPE abap_bool
      EXPORTING
        ev_destination TYPE ANY.
endinterface.
