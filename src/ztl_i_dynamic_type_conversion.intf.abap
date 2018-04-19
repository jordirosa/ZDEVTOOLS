interface ZTL_I_DYNAMIC_TYPE_CONVERSION
  public .
    METHODS convert
      IMPORTING
        i_source TYPE ANY
      EXPORTING
        e_destination TYPE ANY.
endinterface.
