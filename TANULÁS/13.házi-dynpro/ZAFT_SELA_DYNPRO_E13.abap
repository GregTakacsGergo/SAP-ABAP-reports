
*&---------------------------------------------------------------------*
*& Module Pool      ZAFT_SELA_DYNPRO_E13
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zaft_sela_dynpro_e13.

TABLES: ekko, ekpo.

DATA: gt_ekko         TYPE TABLE OF ekko,
      gs_ekko         TYPE ekko,
      gt_ekpo         TYPE TABLE OF ekpo,
      gs_ekpo         TYPE ekpo,
      wa_ekko         TYPE ekko,
      wa_ekpo         TYPE ekpo,
      p_ebeln         TYPE ebeln,
      p_netpr         TYPE netpr,
      ls_user_address TYPE addr3_val,
      lv_full_name    TYPE addr3_val-name_text,
      gv_total_price  TYPE ekpo-netwr,
      gv_item_count   TYPE i,
      g_okcode_100    TYPE syucomm,
      ok_code         TYPE syucomm.
DATA: gv_selected_row TYPE sy-tabix.
DATA: display_empty_tc        TYPE c.

TYPES: BEGIN OF ty_tc,
         sel   TYPE c,           "Egyszeres jelölő sor miatt kell
         ebelp TYPE ekpo-ebelp,  "Beszerzési tételszám
         matnr TYPE ekpo-matnr,  "Anyagszám
         txz01 TYPE ekpo-txz01,  "Tétel rövid leírása
         menge TYPE ekpo-menge,  "Megrendelt mennyiség
         meins TYPE ekpo-meins,  "Mennyiségi egység
         netpr TYPE ekpo-netpr,  "Nettó ár
         waers TYPE ekko-waers,  "Pénznem
       END OF ty_tc.
       
DATA: lt_tc                TYPE TABLE OF ty_tc,        " Table Control belső tábla
      lt_tc_full           TYPE TABLE OF ty_tc,   " Eredeti adatok tárolására
      wa_tc                TYPE ty_tc,
      kizart_tetelek_szama TYPE i.
CONTROLS: TETELADATOK TYPE TABLEVIEW USING SCREEN 400. " ki kellett ide hozni, mert az include-ból nem találta meg a PERFORM get_mat_data!
*----------------------------------------------------------------------SUBROUTINES---------------------------------------------------------------------------------

FORM read_data.
  " visszaadja a szelekciónál beírt ebeln-t a 200-as screenen
  SELECT SINGLE * FROM ekko INTO gs_ekko
  WHERE ebeln EQ p_ebeln.

  SELECT * FROM ekpo INTO TABLE gt_ekpo
    WHERE ebeln EQ p_ebeln.
ENDFORM.

FORM get_item_data_form.
  IF lt_tc IS INITIAL AND display_empty_tc = 'X'. " ha a szűrés alatt nincs több érték, üres legyen majd a table control
    CLEAR lt_tc.
  ELSEIF lt_tc IS INITIAL. "AND empty_tc = 'X'.
    SELECT ekpo~ebelp, ekpo~matnr, ekpo~txz01, ekpo~menge, ekpo~meins, ekpo~netpr, ekko~waers
    INTO CORRESPONDING FIELDS OF TABLE @lt_tc
    FROM ekpo
    JOIN ekko ON ekpo~ebeln = ekko~ebeln
    WHERE ekpo~ebeln = @p_ebeln.

    lt_tc_full = lt_tc.
  ENDIF.
ENDFORM.

FORM filter_table_control.
    DATA: lt_filtered       TYPE TABLE OF ty_tc,
          lv_excluded_count TYPE i.
    " Ha van szűrési feltétel
    IF p_netpr IS NOT INITIAL.
  
      LOOP AT lt_tc INTO DATA(ls_tc) WHERE netpr >= p_netpr." másoljuk ki a kizárt tételeket
        APPEND ls_tc TO lt_filtered.
      ENDLOOP.
  
      DELETE lt_tc WHERE netpr >= p_netpr. " Csak az alacsonyabb értékűeket hagyjuk meg
  
      lv_excluded_count = lines( lt_filtered ). " Írjuk ki a kizárt tételek számát a kimeneti mezőbe
      kizart_tetelek_szama = lv_excluded_count.
  
      IF lt_tc IS INITIAL.
        display_empty_tc = 'X'.
      ENDIF.
  
      CLEAR p_netpr.
    ELSEIF p_netpr IS INITIAL.
  
      lt_tc = lt_tc_full. " Ha nincs szűrés, töltsük vissza az eredeti adatokat
      CLEAR kizart_tetelek_szama.
    ENDIF.
  ENDFORM.

FORM get_mat_data USING lv_index.
    DATA: lv_matnr TYPE matnr.
  
    " Kijelölt sorból vegyük ki a MATNR értéket
    READ TABLE lt_tc INTO wa_tc INDEX lv_index.
    IF sy-subrc = 0 AND wa_tc-matnr IS NOT INITIAL.
      lv_matnr = wa_tc-matnr.
      SET PARAMETER ID 'MAT' FIELD lv_matnr.
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
    ELSE.
      MESSAGE 'Nincs anyagszám(MATNR)!' TYPE 'I'.
    ENDIF.
  ENDFORM.
*----------------------------------------------------------------------FLOW LOGIC INCLUDE MODULES-------------------------------------------------------------------

INCLUDE zaft_sela_dynpro_e13_statuso01.

INCLUDE zaft_sela_dynpro_e13_exit_0i01.

INCLUDE zaft_sela_dynpro_e13_user_ci01.

INCLUDE zaft_sela_dynpro_e13_statuso02.

INCLUDE zaft_sela_dynpro_e13_exit_0i02.

INCLUDE zaft_sela_dynpro_e13_user_ci02.

INCLUDE zaft_sela_dynpro_e13_displao01.

INCLUDE zaft_sela_dynpro_e13_read_do01.

INCLUDE zaft_sela_dynpro_e13_f4_ebei01.

INCLUDE zaft_sela_dynpro_e13_get_heo01.

INCLUDE zaft_sela_dynpro_e13_statuso03.

INCLUDE zaft_sela_dynpro_e13_exit_0i03.

INCLUDE zaft_sela_dynpro_e13_user_ci03.

INCLUDE zaft_sela_dynpro_e13_statuso04.

INCLUDE zaft_sela_dynpro_e13_user_ci04.

INCLUDE zaft_sela_dynpro_e13_get_ito01.

INCLUDE zaft_sela_dynpro_e13_get_ito02.

INCLUDE zaft_sela_dynpro_e13_exit_0i04.

*&SPWizard: Data incl. inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE zaft_sela_dynpro_e13_tc .