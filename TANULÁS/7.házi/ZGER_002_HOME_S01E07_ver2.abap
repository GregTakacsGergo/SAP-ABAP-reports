*&---------------------------------------------------------------------*
*& Report ZGER_002_HOME_S01E07
*&---------------------------------------------------------------------*
REPORT zger_002_home_s01e07.

TABLES: vbrk, vbrp, mara, mard, makt.
DATA: bv_spras TYPE spras.
DATA: bt_home07 TYPE TABLE OF zger_003_home07,  " Internal table to hold records
      bs_home07 TYPE zger_003_home07,          " Work area for ZGER_003_HOME07 records
      bt_vbrk TYPE TABLE OF vbrk,              " Table to hold VBRK records
      bt_vbrp TYPE TABLE OF vbrp,              " Table to hold VBRP records
      bs_vbrp TYPE vbrp,
      bt_mara TYPE TABLE OF mara,
      bt_makt TYPE TABLE OF makt,
      bt_mard TYPE TABLE OF mard,
      bs_mard TYPE mard.

SELECTION-SCREEN BEGIN OF BLOCK selection_screen1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_werks TYPE mard-werks DEFAULT '1000',
            p_lgort TYPE mard-lgort DEFAULT '0001'.
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

START-OF-SELECTION.
  PERFORM fetch_data.          " Fill bt_home07 with data based on selection
  PERFORM process_records.      " Process the records based on the radio button selection

*&---------------------------------------------------------------------*
*& Form fetch_data
*&---------------------------------------------------------------------*
FORM fetch_data.
  CLEAR: bt_home07, bt_vbrk, bt_vbrp, bt_mara, bt_makt, bt_mard.
  bv_spras = sy-langu.

  " 1. Fetch records based on selection criteria
  SELECT * FROM vbrk INTO TABLE bt_vbrk WHERE vbeln IN s_vbeln AND fkart IN s_fkart.
  SELECT * FROM vbrp INTO TABLE bt_vbrp WHERE matnr IN s_matnr.
  SELECT * FROM mara INTO TABLE bt_mara.
  SELECT * FROM makt INTO TABLE bt_makt WHERE spras = bv_spras.
  SELECT * FROM mard INTO TABLE bt_mard WHERE werks = p_werks AND lgort = p_lgort.

  " 2. Populate bt_home07 with calculated SZAMOLT values
  LOOP AT bt_vbrp INTO bs_vbrp.
    READ TABLE bt_mard INTO bs_mard WITH KEY matnr = bs_vbrp-matnr.
    IF sy-subrc = 0.
      " Calculation of SZAMOLT = LABST * NTGEW with overflow check
      DATA(lv_szamolt) = bs_mard-labst * bs_vbrp-ntgew.
      IF lv_szamolt > 999999999999.
        lv_szamolt = 999999999999.
      ENDIF.

      " Populate bs_home07 with required fields
      CLEAR bs_home07.
      bs_home07-vbeln = bs_vbrp-vbeln.
      bs_home07-posnr = bs_vbrp-posnr.
      bs_home07-matnr = bs_vbrp-matnr.
      bs_home07-fkimg = bs_vbrp-fkimg.
      bs_home07-vrkme = bs_vbrp-vrkme.
      bs_home07-ntgew = bs_vbrp-ntgew.
      bs_home07-gewei = bs_vbrp-gewei.
      bs_home07-szamolt = lv_szamolt.
      
      " Populate additional fields from other tables
      READ TABLE bt_mara INTO DATA(bs_mara) WITH KEY matnr = bs_vbrp-matnr.
      IF sy-subrc = 0.
        bs_home07-groes = bs_mara-groes.
      ENDIF.

      READ TABLE bt_makt INTO DATA(bs_makt) WITH KEY matnr = bs_vbrp-matnr.
      IF sy-subrc = 0.
        bs_home07-maktx = bs_makt-maktx.
      ENDIF.

      APPEND bs_home07 TO bt_home07.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form process_records
*&---------------------------------------------------------------------*
FORM process_records.
  DATA lv_message TYPE string.

  " Check radio button selection and perform respective action
  IF rb_inser = 'X'.
    LOOP AT bt_home07 INTO bs_home07.
      INSERT zger_003_home07 FROM bs_home07.
      IF sy-subrc = 0.
        lv_message = 'Record inserted successfully.'.
      ELSE.
        lv_message = 'Error inserting record.'.
      ENDIF.
      WRITE: / lv_message.
    ENDLOOP.

  ELSEIF rb_modif = 'X'.
    MODIFY zger_003_home07 FROM TABLE bt_home07.
    IF sy-subrc = 0.
      lv_message = 'Record modified successfully.'.
    ELSE.
      lv_message = 'Error modifying record.'.
    ENDIF.
    WRITE: / lv_message.

  ELSEIF rb_delet = 'X'.
    LOOP AT bt_home07 INTO bs_home07.
      DELETE FROM zger_003_home07 WHERE vbeln = bs_home07-vbeln AND posnr = bs_home07-posnr.
      IF sy-subrc = 0.
        lv_message = 'Record deleted successfully.'.
      ELSE.
        lv_message = 'Error deleting record.'.
      ENDIF.
      WRITE: / lv_message.
    ENDLOOP.

  ELSEIF rb_updat = 'X'.
    LOOP AT bt_home07 INTO bs_home07.
      UPDATE zger_003_home07 SET fkimg = bs_home07-fkimg, ntgew = bs_home07-ntgew WHERE vbeln = bs_home07-vbeln AND posnr = bs_home07-posnr.
      IF sy-subrc = 0.
        lv_message = 'Record updated successfully.'.
      ELSE.
        lv_message = 'Error updating record.'.
      ENDIF.
      WRITE: / lv_message.
    ENDLOOP.

  ENDIF.
ENDFORM.
