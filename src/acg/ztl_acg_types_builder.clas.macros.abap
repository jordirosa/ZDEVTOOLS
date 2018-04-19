*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
DEFINE append_allowed_abap_typekind.
  CLEAR lwa_abap_typekind_range.
  lwa_abap_typekind_range-sign = 'I'.
  lwa_abap_typekind_range-option = 'EQ'.
  lwa_abap_typekind_range-low = &1.
  APPEND lwa_abap_typekind_range TO ztl_acg_types_builder=>allowed_abap_typekinds.
END-OF-DEFINITION.

DEFINE append_allow_dec_abap_typekind.
  CLEAR lwa_abap_typekind_range.
  lwa_abap_typekind_range-sign = 'I'.
  lwa_abap_typekind_range-option = 'EQ'.
  lwa_abap_typekind_range-low = &1.
  APPEND lwa_abap_typekind_range TO ztl_acg_types_builder=>allowed_dec_abap_typekinds.
END-OF-DEFINITION.

DEFINE append_allow_len_abap_typekind.
  CLEAR lwa_abap_typekind_range.
  lwa_abap_typekind_range-sign = 'I'.
  lwa_abap_typekind_range-option = 'EQ'.
  lwa_abap_typekind_range-low = &1.
  APPEND lwa_abap_typekind_range TO ztl_acg_types_builder=>allowed_len_abap_typekinds.
END-OF-DEFINITION.
