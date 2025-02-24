*&---------------------------------------------------------------------*
*& Report ZGER_ALV_006
*&---------------------------------------------------------------------*
*& This report displays material (MATNR) data and inventory details in 
*& an ALV format. The main table shows material numbers and descriptions. 
*& On double-clicking a row, a pop-up ALV table displays the selected 
*& material's stock quantities (unrestricted, quality inspection, and 
*& blocked stock) along with their total. The report supports multi-language 
*& material descriptions.
*& seltext_s, seltext_m, and seltext_l are customized.
*& You can hide Created On (ERSDA) field by selecting the "Hide 
*&---------------------------------------------------------------------*
REPORT ZGER_ALV_006_NOHEADER_TANANYAG .

TYPE-POOLS: SLIS.
TABLES: MARA, MAKT.

DATA GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

DATA GT_OUTTAB TYPE TABLE OF ZGER_MATNRMAKTX.
DATA GT_OUTTAB2 TYPE TABLE OF ZGER_STRUCT_KESZLET.
DATA LS_OUTTAB TYPE ZGER_MATNRMAKTX.
DATA LS_OUTTAB2 TYPE ZGER_STRUCT_KESZLET.

DATA G_REPID TYPE SY-REPID.

DATA G_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER-COMMAND'.

" Szelekciós opciók
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-001.
  PARAMETERS: P_SPRAS TYPE SPRAS DEFAULT 'HU'.
  SELECT-OPTIONS: so_matnr FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK se2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: P_REJT AS CHECKBOX DEFAULT ''.
SELECTION-SCREEN END OF BLOCK se2.

INITIALIZATION. "mezőkatalógus inicializálása.
  G_REPID = SY-REPID.


START-OF-SELECTION.
  PERFORM SELECT_DATA TABLES GT_OUTTAB.
  PERFORM SELECT_DATA_KESZLET TABLES GT_OUTTAB
                                     GT_OUTTAB2.
  PERFORM FIELDCAT_INIT USING GT_FIELDCAT[].
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
        I_STRUCTURE_NAME = 'ZGER_MATNRMAKTX'
      CHANGING
        CT_FIELDCAT = RT_FIELDCAT.
    LOOP AT RT_FIELDCAT INTO LS_FIELDCAT.
      IF LS_FIELDCAT-fieldname = 'MAKTX'.
        LS_FIELDCAT-seltext_s = 'Megn.'.
        LS_FIELDCAT-seltext_m = 'Megnevezés'.
        LS_FIELDCAT-seltext_l = 'Anyag megnevezése'.
        LS_FIELDCAT-reptext_ddic = 'Anyag megnevezése'.
        MODIFY RT_FIELDCAT FROM LS_FIELDCAT INDEX sy-tabix TRANSPORTING seltext_s reptext_ddic.

      ELSEIF LS_FIELDCAT-fieldname = 'MATNR'.
        LS_FIELDCAT-seltext_s = 'Anyag.'.
        LS_FIELDCAT-seltext_m = 'Anyag.'.
        LS_FIELDCAT-seltext_l = 'Anyag.'.
        LS_FIELDCAT-reptext_ddic = 'Anyag.'.
        MODIFY RT_FIELDCAT FROM LS_FIELDCAT INDEX sy-tabix .

      ELSEIF LS_FIELDCAT-fieldname = 'SPRAS'.
        LS_FIELDCAT-seltext_s = 'Nyelv'.
        LS_FIELDCAT-seltext_m = 'Nyelv'.
        LS_FIELDCAT-seltext_l = 'Nyelv'.
        LS_FIELDCAT-reptext_ddic = 'Nyelv'.
        LS_FIELDCAT-tech = p_rejt.
        MODIFY RT_FIELDCAT FROM LS_FIELDCAT INDEX sy-tabix .
      ENDIF.

    ENDLOOP.
ENDFORM.   "FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       szelekció szubrutin
*----------------------------------------------------------------------*
*  -->  RT_OUTTAB
*----------------------------------------------------------------------*
FORM SELECT_DATA TABLES XT_OUTTAB LIKE GT_OUTTAB.

  DATA: XS_OUTTAB TYPE ZGER_MATNRMAKTX.
  SELECT MARA~MATNR MAKT~MAKTX MAKT~SPRAS MARA~ERSDA
    INTO (XS_OUTTAB-MATNR, XS_OUTTAB-MAKTX, XS_OUTTAB-SPRAS, XS_OUTTAB-ERSDA)
    FROM MARA INNER JOIN MAKT ON MARA~MATNR = MAKT~MATNR
    WHERE MAKT~SPRAS EQ P_SPRAS.
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

        SELECT MATNR LABST INSME SPEME
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
FORM SELECT_DATA_KESZLET TABLES XT_OUTTAB  STRUCTURE ZGER_MATNRMAKTX
                                XT_OUTTAB2 STRUCTURE ZGER_STRUCT_KESZLET.

  DATA: XS_OUTTAB   TYPE ZGER_MATNRMAKTX,
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