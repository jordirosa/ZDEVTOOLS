CLASS zcx_invalid_parameters DEFINITION
  INHERITING FROM cx_static_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    interfaces: if_t100_message.

METHODS constructor
  IMPORTING
    i_msgid TYPE arbgb OPTIONAL
    i_msgno TYPE msgnr OPTIONAL
    i_msgv1 TYPE sychar50 OPTIONAL
    i_msgv2 TYPE sychar50 OPTIONAL
    i_msgv3 TYPE sychar50 OPTIONAL
    i_msgv4 TYPE sychar50 OPTIONAL
    i_previous TYPE REF TO cx_root OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_INVALID_PARAMETERS IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).

    CLEAR me->textid.
    IF i_msgid IS SUPPLIED AND
       i_msgno IS SUPPLIED.
      if_t100_message~t100key-msgid = i_msgid.
      if_t100_message~t100key-msgno = i_msgno.
      if_t100_message~t100key-attr1 = i_msgv1.
      if_t100_message~t100key-attr2 = i_msgv2.
      if_t100_message~t100key-attr3 = i_msgv3.
      if_t100_message~t100key-attr4 = i_msgv4.
    ELSE.
      if_t100_message~t100key-msgid = ztl_cl_global=>c_msgid_ztools.
      if_t100_message~t100key-msgno = '001'.
      if_t100_message~t100key-attr1 = space.
      if_t100_message~t100key-attr2 = space.
      if_t100_message~t100key-attr3 = space.
      if_t100_message~t100key-attr4 = space.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
