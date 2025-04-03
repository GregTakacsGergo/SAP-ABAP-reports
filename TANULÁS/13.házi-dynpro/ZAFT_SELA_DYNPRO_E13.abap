
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
DATA: lt_tc      TYPE TABLE OF ty_tc,        " Table Control belső tábla
      lt_tc_full TYPE TABLE OF ty_tc,   " Eredeti adatok tárolására
      wa_tc      type ty_tc.

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
  DATA: lt_filtered TYPE TABLE OF ty_tc.
  " Ha van szűrési feltétel
  IF p_netpr IS NOT INITIAL.
    DELETE lt_tc WHERE netpr >= p_netpr. " Csak az alacsonyabb értékűeket hagyjuk meg
    IF lt_tc IS INITIAL.
      display_empty_tc = 'X'.
    ENDIF.
    CLEAR p_netpr.
  ELSEIF p_netpr IS INITIAL.
    " Ha nincs szűrés, töltsük vissza az eredeti adatokat
      lt_tc = lt_tc_full.
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