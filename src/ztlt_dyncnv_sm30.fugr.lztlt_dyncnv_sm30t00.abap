*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 25.04.2018 at 23:53:38
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTLT_DYNCNV_0001................................*
DATA:  BEGIN OF STATUS_ZTLT_DYNCNV_0001              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTLT_DYNCNV_0001              .
CONTROLS: TCTRL_ZTLT_DYNCNV_0001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTLT_DYNCNV_0001              .
TABLES: ZTLT_DYNCNV_0001               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
