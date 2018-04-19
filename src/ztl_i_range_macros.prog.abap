*&---------------------------------------------------------------------*
*& Include ztl_i_range_macros
*&---------------------------------------------------------------------*
DEFINE append_equals.
  lwa_&1-sign = ztl_cl_global=>c_rng_sgn_include.
  lwa_&1-option = ztl_cl_global=>c_rng_opt_equals.
  lwa_&1-low = &2.

  APPEND lwa_&1 TO &1.
END-OF-DEFINITION.
