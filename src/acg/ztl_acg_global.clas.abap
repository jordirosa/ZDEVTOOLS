CLASS ztl_acg_global DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: t_scope TYPE numc1.

    CONSTANTS: c_scope_local TYPE t_scope VALUE '1',
               c_scope_global TYPE t_scope VALUE '2',

               c_operator_end_statement TYPE string VALUE '.',

               c_keyword_type TYPE string VALUE 'TYPE',
               c_keyword_types TYPE string VALUE 'TYPES',
               c_keyword_length TYPE string VALUE 'LENGTH',
               c_keyword_decimals TYPE string VALUE 'DECIMALS'.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZTL_ACG_GLOBAL IMPLEMENTATION.
ENDCLASS.
