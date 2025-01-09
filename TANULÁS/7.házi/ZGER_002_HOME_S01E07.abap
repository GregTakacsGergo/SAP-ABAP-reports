*&---------------------------------------------------------------------*
*& Report ZGER_002_HOME_S01E07
*& inkonzisztens elnevezés:ZGER_001_HOME_S01E07 kéne legyen,
*& mert 001 jelöli a riport mélységet, 002 az include-ok mélységét,
*& 003 a funkciók mélységét...
*& a bt, bs, bv - a belső_tábla-struktúra-változót jelöli!
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zger_002_home_s01e07.




TABLES: vbrk, vbrp, mara, mard, makt.
DATA: bv_spras TYPE spras.
DATA: bt_home07 TYPE TABLE OF zger_003_home07,
      bs_home07 TYPE zger_003_home07,
      bt_vbrk TYPE TABLE OF vbrk,
      bt_vbrp TYPE TABLE OF vbrp,
      bs_vbrp TYPE vbrp,
      bt_mara TYPE TABLE OF mara,
      bt_makt TYPE TABLE OF makt,
      bt_mard TYPE TABLE OF mard,
      bs_mard TYPE mard.

SELECTION-SCREEN BEGIN OF BLOCK selection_screen1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_werks TYPE mard-werks DEFAULT '1000',
            p_lgort TYPE mard-lgort DEFAULT '0001'.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_vbeln FOR vbrk-vbeln DEFAULT '0090005177' TO '0090005185',
                s_fkart FOR vbrk-fkart DEFAULT 'F2' OBLIGATORY,
                s_matnr FOR vbrp-matnr.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK rb WITH FRAME TITLE TEXT-001.
PARAMETERS: rb_delet RADIOBUTTON GROUP rad1,
            rb_inser RADIOBUTTON GROUP rad1,
            rb_modif RADIOBUTTON GROUP rad1,
            rb_updat RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK rb.
SELECTION-SCREEN END OF BLOCK selection_screen1.
*&---------------------------------------------------------------------*
*&  Include           ZGER_003_HOME_S01E07_FORM
*&---------------------------------------------------------------------*
*  -->értékelt korlátlan használatú készlet
*  -->nettó súly
*  <--számolt érték
*----------------------------------------------------------------------*
FORM szamolt USING p_labst TYPE mard-labst
                   p_ntgew TYPE vbrp-ntgew
             CHANGING p_szamolt TYPE f.

  p_szamolt = p_labst * p_ntgew.
  WRITE:/'számolt érték mard-labst*vbrp-ntgew:', p_szamolt.
  IF p_szamolt > '999999999999'.
    p_szamolt = '999999999999'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_records
*&---------------------------------------------------------------------*
FORM process_records.
  DATA bv_message TYPE string.
  IF rb_inser = 'X'.
    " Insert records a ZGER_003_home07 táblába
    LOOP AT bt_home07 INTO bs_home07.
      INSERT ZGER_003_home07 FROM bs_home07.
      IF sy-subrc = 0.
        bv_message = 'Record inserted successfully.'.
      ELSE.
        bv_message = 'Error inserting record.'.
      WRITE: / bv_message.
      ENDIF.
    ENDLOOP.
  ELSEIF rb_modif = 'X'.
    " Modify records a ZGER_003_home07 táblán
    MODIFY ZGER_003_home07 FROM TABLE bt_home07.
    IF sy-subrc = 0.
      bv_message = 'Record modified successfully.'.
    ELSE.
      bv_message = 'Error modifying record.'.
    ENDIF.
    WRITE: / bv_message.
  ENDIF.
ENDFORM.

START-OF-SELECTION.

bv_spras = sy-langu.

SELECT * FROM zger_003_home07 INTO TABLE bt_home07.
SELECT * FROM vbrk INTO TABLE bt_vbrk.
SELECT * FROM vbrp INTO TABLE bt_vbrp.
SELECT * FROM mara INTO TABLE bt_mara.
SELECT * FROM makt INTO TABLE bt_makt WHERE spras = bv_spras.
SELECT * FROM mard INTO TABLE bt_mard.
*nyelv szerinti szűrt találatok
*LOOP AT bt_makt INTO DATA(bs_makt).
*  WRITE: / 'Talált rekordok a nyelv alapján:', bs_makt-matnr, bs_makt-maktx.
*ENDLOOP.
*mard-labst*vbrp-ntgew számítása
DATA: bv_labst TYPE mard-labst,
      bv_ntgew TYPE vbrp-ntgew,
      bv_szamolt TYPE f.

READ TABLE bt_mard INTO bs_mard INDEX 1.
IF sy-subrc = 0.
  bv_labst = bs_mard-labst.
ELSE.
  WRITE: / 'MARD tábla olvasása sikertelen.'.
ENDIF.

READ TABLE bt_vbrp INTO bs_vbrp INDEX 1.
IF sy-subrc = 0.
  bv_ntgew = bs_vbrp-ntgew.
ELSE.
  WRITE: / 'VBRP tábla olvasása sikertelen.'.
ENDIF.

IF sy-subrc = 0 AND bv_labst IS NOT INITIAL AND bv_ntgew IS NOT INITIAL.
  WRITE: / 'Calling szamolt with', bv_labst, 'and', bv_ntgew.
  PERFORM szamolt USING bv_labst bv_ntgew
                  CHANGING bv_szamolt.
ENDIF.

PERFORM process_records.
IF bt_home07 IS INITIAL.
  WRITE: / 'bt_home07 is empty, nothing to insert.'.
  RETURN.
ENDIF.
IF sy-subrc = 0.
  WRITE: / 'process_records executed successfully.'.
ELSE.
  WRITE: / 'Error in executing process_records.'.
ENDIF.