*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTLT_DYNCNV_SM30
*   generation date: 25.04.2018 at 23:53:35
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTLT_DYNCNV_SM30   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
