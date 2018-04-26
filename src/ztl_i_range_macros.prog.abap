*&---------------------------------------------------------------------*
*& Include ztl_i_range_macros
*&---------------------------------------------------------------------*
DEFINE append_equals.
  lwa_&1-sign = ztl_cl_global=>e_range_signs-include.
  lwa_&1-option = ztl_cl_global=>e_range_options-equals.
  lwa_&1-low = &2.

  APPEND lwa_&1 TO &1.
END-OF-DEFINITION.
