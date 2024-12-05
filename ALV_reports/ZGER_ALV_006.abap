*&---------------------------------------------------------------------*
*& Report ZGER_ALV_006
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGER_ALV_006.

TYPE-POOLS: SLIS.
TYPES UD_STRUCT TYPE ZGER_MATNRMAKTX.
TYPES UD_STRUCT2 TYPE ZGER_STRUCT_KESZLET.
TABLES: MARA, MAKT.

DATA GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
DATA GT_OUTTAB TYPE UD_STRUCT OCCURS 0 WITH HEADER LINE.
DATA G_REPID TYPE SY-REPID.
DATA G_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER-COMMAND'.
DATA: BT_STRUCT2 TYPE TABLE OF UD_STRUCT2,
      BS_STRUCT2 TYPE UD_STRUCT2.
DATA GT_OUTTAB2 TYPE BT_STRUCT2.       
PARAMETERS P_SPRAS TYPE SPRAS DEFAULT 'EN'.

INITIALIZATION. "mezőkatalógus inicializálása.
  G_REPID = SY-REPID.
  PERFORM FIELDCAT_INIT USING GT_FIELDCAT[].

START-OF-SELECTION.
  PERFORM SELECT_DATA TABLES GT_OUTTAB.
END-OF-SELECTION.

*megjelenítés
CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
        I_CALLBACK_PROGRAM = G_REPID
        IT_FIELDCAT = GT_FIELDCAT[]
        I_SAVE = 'A'
        I_CALLBACK_USER_COMMAND = G_USER_COMMAND
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

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        I_PROGRAM_NAME = G_REPID
        I_INTERNAL_TABNAME = 'GT_OUTTAB'
        I_STRUCTURE_NAME = 'ZGER_MATNRMAKTX'
      CHANGING
        CT_FIELDCAT = RT_FIELDCAT.
ENDFORM.   "FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       szelekció szubrutin
*----------------------------------------------------------------------*
*  -->  RT_OUTTAB
*----------------------------------------------------------------------*
FORM SELECT_DATA
  TABLES RT_OUTTAB LIKE GT_OUTTAB[].
  SELECT MARA~MATNR MAKT~MAKTX MAKT~SPRAS
    INTO (RT_OUTTAB-MATNR, RT_OUTTAB-MAKTX, RT_OUTTAB-SPRAS)
    FROM MARA INNER JOIN MAKT ON MARA~MATNR = MAKT~MATNR
    WHERE MAKT~SPRAS EQ P_SPRAS.
    APPEND RT_OUTTAB.
  ENDSELECT.
ENDFORM.  "SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       felhasználó parancsa (duplaklikk, ikonra katt, stb...)
*----------------------------------------------------------------------*
*  -->  R_UCOMM        text
*  -->  RS_SELFIELD    text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE R_UCOMM.
    WHEN '&IC1'.  "duplaklikk
      READ TABLE GT_OUTTAB INTO GT_OUTTAB INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        SUBMIT RM07MWRKK WITH MATNR EQ GT_OUTTAB-MATNR AND RETURN.
      ENDIF.
      CLEAR R_UCOMM.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_KESZLET

*----------------------------------------------------------------------*
FORM SELECT_DATA_KESZLET.

  LOOP AT GT_OUTTAB INTO UD_STRUCT.
    SELECT LABST INSME SPEME
    FROM MARD
    INTO (BS_STRUCT2-LABST, BS_STRUCT2-INSME, BS_STRUCT2-SPEME)
    WHERE BS_STRUCT2-MATNR EQ UD_STRUCT-MATNR.
    IF sy-subrc = 0.
      APPEND BS_UDSTRUCT2 TO BT_STRUCT2.
    ENDIF.
    ENDSELECT.
  ENDLOOP.
ENDFORM.