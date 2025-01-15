*&---------------------------------------------------------------------*
*& Report ZGER_ALV_010_STOCK
*&---------------------------------------------------------------------*
*& This report displays the materials (MATNR) basic data and warehouse 
*& stock levels in an ALV format. The ALV table shows the material number, 
*& description, and other details. Double-clicking opens a pop-up table 
*& to view the specific warehouse stocks (unrestricted, quality inspection, 
*& blocked) and their totals by factory.
*&---------------------------------------------------------------------*
REPORT ZGER_ALV_010_STOCK.

TYPE-POOLS: SLIS.
TABLES: MARA, MAKT.

DATA GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

DATA GT_OUTTAB TYPE TABLE OF ZGER_KESZLET_MAIN.
DATA GT_OUTTAB2 TYPE TABLE OF ZGER_STRUCT_KESZLET.
DATA LS_OUTTAB TYPE ZGER_KESZLET_MAIN.
DATA LS_OUTTAB2 TYPE ZGER_STRUCT_KESZLET.

DATA G_REPID TYPE SY-REPID.

DATA G_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER-COMMAND'.

" Szelekciós opciók
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-001.
  PARAMETERS P_SPRAS TYPE SPRAS DEFAULT 'HU'.
  SELECT-OPTIONS: so_matnr FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK sel.

INITIALIZATION. "mezőkatalógus inicializálása.
  G_REPID = SY-REPID.
  PERFORM FIELDCAT_INIT USING GT_FIELDCAT[].

START-OF-SELECTION.
  PERFORM SELECT_DATA USING GT_OUTTAB.
  PERFORM SELECT_DATA_KESZLET TABLES GT_OUTTAB
                                     GT_OUTTAB2.
END-OF-SELECTION.

*megjelenítés
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
        I_CALLBACK_PROGRAM = G_REPID
        IT_FIELDCAT = GT_FIELDCAT[]
        I_SAVE = 'A'
        I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
    TABLES
        T_OUTTAB = GT_OUTTAB.

*&---------------------------------------------------------------------*
*&      Form FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       mezőkatalógus inicializálás
*----------------------------------------------------------------------*
*  -->  RT_FIELDCAT
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
    DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        I_PROGRAM_NAME = G_REPID
        I_INTERNAL_TABNAME = 'GT_OUTTAB'
        I_STRUCTURE_NAME = 'ZGER_KESZLET_MAIN'
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
FORM SELECT_DATA USING XT_OUTTAB TYPE STANDARD TABLE.

  DATA: XS_OUTTAB TYPE ZGER_KESZLET_MAIN.
  SELECT MARA~MATNR MARA~BRGEW MARA~GEWEI MAKT~MAKTX MAKT~SPRAS
    INTO (XS_OUTTAB-MATNR, XS_OUTTAB-BRGEW, XS_OUTTAB-GEWEI, XS_OUTTAB-MAKTX, XS_OUTTAB-SPRAS)
    FROM MARA INNER JOIN MAKT ON MARA~MATNR = MAKT~MATNR
    WHERE MAKT~SPRAS EQ P_SPRAS AND MARA~MATNR IN so_matnr.
    APPEND XS_OUTTAB TO XT_OUTTAB.
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
      READ TABLE GT_OUTTAB INTO LS_OUTTAB INDEX RS_SELFIELD-TABINDEX.
      IF SY-SUBRC = 0.
        DATA: XT_POPUP TYPE TABLE OF ZGER_STRUCT_KESZLET,
              XS_POPUP TYPE ZGER_STRUCT_KESZLET.

        SELECT MATNR WERKS LABST INSME SPEME
        INTO TABLE XT_POPUP
        FROM MARD
        WHERE MATNR = LS_OUTTAB-MATNR.

        " Készlet számítása a pop-up táblához
        LOOP AT XT_POPUP INTO XS_POPUP.
          XS_POPUP-KESZLET = XS_POPUP-LABST + XS_POPUP-INSME + XS_POPUP-SPEME.
          MODIFY XT_POPUP FROM XS_POPUP TRANSPORTING KESZLET.
        ENDLOOP.

          CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
            EXPORTING
              I_TITLE = 'POP-UP'
              I_ZEBRA = 'X'
              I_TABNAME = 'LT_POPUP'
              I_STRUCTURE_NAME = 'ZGER_STRUCT_KESZLET'
            TABLES
              T_OUTTAB = XT_POPUP.
      ENDIF.
      CLEAR R_UCOMM.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form SELECT_DATA_KESZLET
*&---------------------------------------------------------------------*
*       készlet adatokat összegyűjtő szubrutin
*----------------------------------------------------------------------*
FORM SELECT_DATA_KESZLET TABLES XT_OUTTAB  STRUCTURE ZGER_KESZLET_MAIN
                                XT_OUTTAB2 STRUCTURE ZGER_STRUCT_KESZLET.

  DATA: XS_OUTTAB   TYPE ZGER_KESZLET_MAIN,
        XS_OUTTAB2  TYPE ZGER_STRUCT_KESZLET.

  LOOP AT XT_OUTTAB INTO XS_OUTTAB.
    SELECT LABST INSME SPEME
    INTO (XS_OUTTAB2-LABST, XS_OUTTAB2-INSME, XS_OUTTAB2-SPEME)
    FROM MARD
    WHERE MATNR EQ XS_OUTTAB-MATNR.
    XS_OUTTAB-KESZLET = XS_OUTTAB2-LABST + XS_OUTTAB2-INSME + XS_OUTTAB2-SPEME.
    IF sy-subrc = 0.
    MODIFY XT_OUTTAB FROM XS_OUTTAB.
    ENDIF.
    ENDSELECT.
  ENDLOOP.
ENDFORM.