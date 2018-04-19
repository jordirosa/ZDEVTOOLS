CLASS zcx_dyncnv_config_missing DEFINITION
  INHERITING FROM cx_static_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_t100_message.

METHODS constructor
  IMPORTING
    i_source_type TYPE string
    i_destination_type TYPE string
    i_previous TYPE REF TO cx_root OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_dyncnv_config_missing IMPLEMENTATION.
  METHOD constructor. ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    CLEAR me->textid.
    if_t100_message~t100key-msgid = ztl_cl_global=>c_msgid_ztools.
    if_t100_message~t100key-msgno = '003'.
    if_t100_message~t100key-attr1 = i_source_type.
    if_t100_message~t100key-attr2 = i_destination_type.
    if_t100_message~t100key-attr3 = space.
    if_t100_message~t100key-attr4 = space.

  ENDMETHOD.
ENDCLASS.
