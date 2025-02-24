*&---------------------------------------------------------------------*
*& Report ZGER_ALV_004
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGER_ALV_004.


TYPE-POOLS: SLIS.
TYPES: UD_STRUCT TYPE ZGER_MATNRMAKTX.
"    BEGIN OF UD_STRUCT,
"        MATNR TYPE MATNR,
"        MAKTX TYPE MAKTX,
"    END OF UD_STRUCT.
TABLES: MARA, MAKT.

DATA GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
DATA GT_OUTTAB TYPE UD_STRUCT OCCURS 0 WITH HEADER LINE.
DATA G_REPID TYPE SY-REPID.

INITIALIZATION.
    g_repid = sy-repid.
    PERFORM FIELDCAT_INIT USING GT_FIELDCAT[].

START-OF-SELECTION.
    PERFORM SELECT_DATA TABLES GT_OUTTAB.
END-OF-SELECTION.

*megjelenítés
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
        I_CALLBACK_PROGRAM = G_REPID
        IT_FIELDCAT = GT_FIELDCAT[]
*           I_SAVE = 'A'
    TABLES
        T_OUTTAB = GT_OUTTAB.

*&---------------------------------------------------------------------*
*&      Form FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       mezőkatalógus inicializálás
*----------------------------------------------------------------------*
*  -->  RT_FIELDCAT
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT
    USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
    DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
    DATA: POS TYPE I VALUE 1.

    CLEAR LS_FIELDCAT.
    LS_FIELDCAT-COL_POS = POS.
    LS_FIELDCAT-FIELDNAME = 'MATNR'.
    LS_FIELDCAT-REF_FIELDNAME = 'MATNR'.
    LS_FIELDCAT-REF_TABNAME = 'MARA'.
    LS_FIELDCAT-KEY = 'X'.
    APPEND LS_FIELDCAT TO RT_FIELDCAT.
    CLEAR LS_FIELDCAT.
    POS = POS + 1.
    LS_FIELDCAT-COL_POS = POS.
    LS_FIELDCAT-FIELDNAME = 'MAKTX'.
    LS_FIELDCAT-REF_FIELDNAME = 'MAKTX'.
    LS_FIELDCAT-REF_TABNAME = 'MAKT'.
    LS_FIELDCAT-SELTEXT_L = 'ANYAG MEGNEVEZÉSE'.
    APPEND LS_FIELDCAT TO RT_FIELDCAT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       szelekció szubrutin
*----------------------------------------------------------------------*
*  -->  RT_OUTTAB
*----------------------------------------------------------------------*
FORM SELECT_DATA
  TABLES RT_OUTTAB LIKE GT_OUTTAB[].
  SELECT MARA~MATNR MAKT~MAKTX
    INTO (RT_OUTTAB-MATNR, RT_OUTTAB-MAKTX) UP TO 10 ROWS
    FROM MARA INNER JOIN MAKT ON MARA~MATNR = MAKT~MATNR
    WHERE MAKT~SPRAS EQ 'EN'.
    APPEND RT_OUTTAB.
  ENDSELECT.
ENDFORM.  "select_data