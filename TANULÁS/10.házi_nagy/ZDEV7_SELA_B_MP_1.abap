*&---------------------------------------------------------------------*
*& Module Pool      ZAFT_SELA_B_MP_1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
PROGRAM zaft_sela_b_mp_1.

*----------------------------------------------------------------DB-DECLARATIONS------------------------------------------------------------
TABLES: ekpo, ekko, zdev7_ekkoekpo.
DATA it_zdev7_ekkoekpo TYPE TABLE OF zdev7_ekkoekpo.
DATA it_zdev7_ekkoekpo_origin TYPE TABLE OF zdev7_ekkoekpo.
DATA gt_outtab TYPE TABLE OF zdev7_013_ekkoekpo. " ez egy munka-struktúra amely a stílus elemek miatt lett létrehozva
DATA ls_outtab TYPE zdev7_013_ekkoekpo.

*---------------------------------------------------------------OTHER-DECLARATIONS-----------------------------------------------------------
INCLUDE <cl_alv_control>.

DATA g_repid TYPE sy-repid.
DATA gt_fieldcat TYPE lvc_t_fcat.
DATA g_user_command TYPE slis_formname VALUE 'USER-COMMAND'.
DATA layout TYPE lvc_s_layo.
DATA g_status TYPE slis_formname VALUE 'SET_STATUS'.
DATA go_grid TYPE REF TO cl_gui_alv_grid.


DATA ok_copy TYPE sy-ucomm.
DATA ok_code TYPE sy-ucomm.
DATA rb_alv(1) TYPE c VALUE 'X'.
DATA rb_upl(1) TYPE c.
DATA: p_xloekz TYPE c LENGTH 1.

*DATA: gt_ebeln      TYPE RANGE OF ekpo-ebeln,
*      gt_ebelp      TYPE RANGE OF ekpo-ebelp,
*      gt_bukrs      TYPE RANGE OF ekko-bukrs,
*      gt_bstyp      TYPE RANGE OF ekko-bstyp,
*      gt_bsart      TYPE RANGE OF ekko-bsart,
*
*      gt2_ebeln      TYPE RANGE OF ekpo-ebeln,
*      gt2_ebelp      TYPE RANGE OF ekpo-ebelp,
*      gt2_bukrs      TYPE RANGE OF ekko-bukrs,
*      gt2_bstyp      TYPE RANGE OF ekko-bstyp,
*      gt2_bsart      TYPE RANGE OF ekko-bsart,
*
*      so_ebeln_low  TYPE ebeln,
*      so_ebeln_high TYPE ebeln,
*      so_ebelp_low  TYPE ebelp,
*      so_ebelp_high TYPE ebelp,
*      so_bukrs_low  TYPE bukrs,
*      so_bukrs_high TYPE bukrs,
*      so_bstyp_low  TYPE bstyp,
*      so_bstyp_high TYPE bstyp,
*      so_bsart_low  TYPE bsart,
*      so_bsart_high TYPE bsart.
*----------------------------------------------------------------SUBSCREEN-SELECTION-SCREEN--------------------------------------------------
SELECTION-SCREEN: BEGIN OF SCREEN 200 AS SUBSCREEN.

  SELECT-OPTIONS: gt_ebeln FOR ekpo-ebeln ,"DEFAULT '4500106625' TO '4500106631' ,
                  gt_ebelp FOR ekpo-ebelp,
                  gt_bukrs FOR ekko-bukrs,
                  gt_bstyp FOR ekko-bstyp,
                  gt_bsart FOR ekko-bsart.

SELECTION-SCREEN: END OF SCREEN 200.
"AT SELECTION-SCREEN ON VALUE-REQUEST FOR gt_ebeln.
"  CLEAR: gt_ebeln, gt_ebelp, gt_bukrs, gt_bstyp, gt_bsart.
"  REFRESH: gt_ebeln, gt_ebelp, gt_bukrs, gt_bstyp, gt_bsart.

"SELECTION-SCREEN: BEGIN OF SCREEN 300 AS SUBSCREEN.
"  PARAMETERS: p_xloekz AS CHECKBOX USER-COMMAND a.
"  PARAMETERS: rb_upl RADIOBUTTON GROUP g1,
"              rb_alv RADIOBUTTON GROUP g1 .
"SELECTION-SCREEN: END OF SCREEN 300.
*--------------------------------------------------------------SUBROUTINES------------------------------------------------------------------

FORM upload_zdev7ekkoekpo USING xt_ekkoekpo TYPE STANDARD TABLE.
  DELETE FROM  zdev7_ekkoekpo.
  SELECT a~ebeln, a~bukrs, a~bstyp,
       a~bsart, a~aedat,
       b~ebelp,
       b~txz01, b~matnr, b~ematn, b~bukrs AS bukrs2,
       b~werks, b~lgort, b~matkl, b~infnr, b~idnlf,
       b~ktmng, b~menge, b~meins, b~netwr
    INTO CORRESPONDING FIELDS OF TABLE @xt_ekkoekpo
    FROM ekko AS a
    INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
    WHERE     a~ebeln IN @gt_ebeln
          AND b~ebelp IN @gt_ebelp
          AND a~bukrs IN @gt_bukrs
          AND a~bstyp IN @gt_bstyp
          AND a~bsart IN @gt_bsart.
  INSERT zdev7_ekkoekpo FROM TABLE @xt_ekkoekpo.
ENDFORM.

FORM fetch_data_zdev7ekkoekpo USING xt_outtab TYPE STANDARD TABLE
                             CHANGING backup_outtab TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <fs_outtab> LIKE ls_outtab.
  DATA: xv_diff   TYPE char1,
        ls_backup TYPE zdev7_ekkoekpo.

  SELECT a~ebeln, a~bukrs, CAST( a~bstyp AS CHAR ) AS bstyp,
         a~bsart, a~aedat,
         CAST( b~ebelp AS NUMC ) AS ebelp,
         b~txz01, b~matnr, b~ematn, b~bukrs AS bukrs2,
         b~werks, b~lgort, b~matkl, b~infnr, b~idnlf,
         b~ktmng, b~menge, b~meins, b~netwr,  b~loekz AS xloekz
    INTO CORRESPONDING FIELDS OF TABLE @xt_outtab
    FROM ekko AS a
    INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
    LEFT JOIN zdev7_ekkoekpo AS z ON a~ebeln = z~ebeln AND b~ebelp = z~ebelp
    WHERE a~ebeln IN @gt_ebeln
      AND b~ebelp IN @gt_ebelp
      AND a~bukrs IN @gt_bukrs
      AND a~bstyp IN @gt_bstyp
      AND a~bsart IN @gt_bsart.
  "  CLEAR xt_outtab.
  " elmentjük az eredeti szelekciót

  CLEAR backup_outtab.
  LOOP AT xt_outtab ASSIGNING FIELD-SYMBOL(<fs_xt>).
    "DATA(ls_backup) = backup_outtab.  " Új rekord inicializálása
    MOVE-CORRESPONDING <fs_xt> TO ls_backup.  " Csak a közös mezőket másoljuk
    APPEND ls_backup TO backup_outtab.
  ENDLOOP.
"  LOOP AT xt_outtab ASSIGNING <fs_outtab>.
"    " Meghívjuk a funkciós modult, hogy kiderüljön, van-e eltérés
"    CALL FUNCTION 'ZDEV7_013_HOME_S01E10'
"      EXPORTING
"        iv_ebeln      = <fs_outtab>-ebeln
"      IMPORTING
"        ev_difference = xv_diff.
"    IF p_xloekz = 'X'.
"      <fs_outtab>-xloekz = 'X'.

"      IF <fs_outtab>-xloekz = 'X'.

"        <fs_outtab>-lamp_icon = '@0A@'.  "Piros lámpa ha van törlés előjegyezve
"      ELSEIF  xv_diff = 'X'.
"        <fs_outtab>-lamp_icon = '@09@'. " sárga lámpa ha van eltérés
"      ELSEIF  xv_diff = ''.
"        <fs_outtab>-lamp_icon = '@08@'. " Zöld lámpa (értékek egyeznek)
"      ENDIF .
"      PERFORM sor_szinezes USING   <fs_outtab>-matnr
"                                   <fs_outtab>-netwr
"                          CHANGING <fs_outtab>-color.
"      PERFORM netwr_kezdeti_allithatosag USING <fs_outtab>-xloekz
"                                      ls_outtab
"                                      CHANGING <fs_outtab>-celltab.
"    ELSEIF p_xloekz = ''.
"      <fs_outtab>-xloekz = ''.
"      IF <fs_outtab>-xloekz = ''.
"        <fs_outtab>-lamp_icon = '@08@'.  "zöld lámpa ha van törlés előjegyezve
"      ELSEIF  xv_diff = 'X'.
"        <fs_outtab>-lamp_icon = '@09@'. " sárga lámpa ha van eltérés
"      ELSEIF  xv_diff = ''.
"        <fs_outtab>-lamp_icon = '@08@'. " Zöld lámpa (értékek egyeznek)
"      ENDIF .
"      PERFORM sor_szinezes USING   <fs_outtab>-matnr
"                                   <fs_outtab>-netwr
"                          CHANGING <fs_outtab>-color.
"      PERFORM netwr_kezdeti_allithatosag USING <fs_outtab>-xloekz
"                                      ls_outtab
"                                      CHANGING <fs_outtab>-celltab.
"    ENDIF.
"  ENDLOOP.
ENDFORM.

FORM sor_szinezes USING    xv_matnr TYPE matnr
                          xv_netwr TYPE netwr
                  CHANGING cv_color TYPE char4.
  IF xv_matnr IS NOT INITIAL AND xv_netwr = 0.
    ls_outtab-color = 'C710'. " PIROS
  ELSEIF xv_netwr IS NOT INITIAL.
    cv_color = 'C410'. " kék
  ELSEIF xv_netwr = 0.
    ls_outtab-color = 'C310'. " SÁRGA
  ELSEIF xv_matnr IS NOT INITIAL.
    ls_outtab-color = 'C210'. " ZÖLD
  ENDIF.
ENDFORM.

FORM netwr_kezdeti_allithatosag USING xw_xloekz TYPE eloek
                                      xw_output LIKE ls_outtab
                                CHANGING celltab TYPE lvc_t_styl.

  DATA: xt_celltab TYPE lvc_t_styl,
        xs_celltab TYPE lvc_s_styl.
  IF xw_xloekz = 'X'.
    xs_celltab-fieldname = 'NETWR'.
    xs_celltab-style = alv_style_disabled.
  ELSEIF xw_xloekz = ''.
    xs_celltab-fieldname = 'NETWR'.
    xs_celltab-style = alv_style_enabled.
  ENDIF.
  APPEND xs_celltab TO celltab.
  " Ha van formázandó mező, beállítjuk a cellatábla mezőbe
  IF xt_celltab IS NOT INITIAL.
    xw_output-celltab = xt_celltab.
  ENDIF.
ENDFORM.

FORM fieldcat_layout_init USING xt_fieldcat TYPE lvc_t_fcat.
  DATA: xs_fieldcat TYPE lvc_s_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_internal_tabname = 'GT_OUTTAB'
      i_structure_name   = 'ZDEV7_013_EKKOEKPO'
      i_bypassing_buffer = 'X'
    CHANGING
      ct_fieldcat        = xt_fieldcat.
  LOOP AT xt_fieldcat INTO xs_fieldcat.
    CASE xs_fieldcat-fieldname.
      WHEN 'LAMP_ICON'. " Új ikon mező
        xs_fieldcat-icon = 'X'.
        xs_fieldcat-scrtext_s = 'Stát.'.
        xs_fieldcat-scrtext_m = 'Státusz ikon'.
        xs_fieldcat-scrtext_l = 'Rekord státusza'.
        MODIFY xt_fieldcat FROM xs_fieldcat INDEX sy-tabix.
      WHEN 'NETWR'.
        xs_fieldcat-edit = 'X'.
        xs_fieldcat-style = alv_style_font_bold.
        MODIFY xt_fieldcat FROM xs_fieldcat INDEX sy-tabix.
      WHEN 'XLOEKZ'.
        xs_fieldcat-edit = 'X'.
        xs_fieldcat-checkbox = 'X'.
        xs_fieldcat-outputlen = 1.
        xs_fieldcat-hotspot = 'X'.
        xs_fieldcat-scrtext_s = 'Törl.'.
        xs_fieldcat-scrtext_m = 'Törlési e.'.
        xs_fieldcat-scrtext_l = 'Törlési előjegyzés'.
        MODIFY xt_fieldcat FROM xs_fieldcat INDEX sy-tabix.
      WHEN 'COLOR'.
        xs_fieldcat-tech = 'X'.
        MODIFY xt_fieldcat FROM xs_fieldcat INDEX sy-tabix.
      WHEN OTHERS.
        xs_fieldcat-edit = ''.
        MODIFY xt_fieldcat FROM xs_fieldcat INDEX sy-tabix.
    ENDCASE.
  ENDLOOP.
  layout-info_fname = 'COLOR'.
  layout-stylefname = 'CELLTAB'.
ENDFORM.
"DATA: ls_range LIKE   rt_range.  " Egyetlen sor a range táblából
"FORM load_range_values USING rt_range TYPE range of ekpo-ebeln
""
"                             rv_low   TYPE any
"                             rv_high  TYPE any.
"   DATA: ls_range like  rt_range.

"  CLEAR: rv_low, rv_high.
"  READ TABLE rt_range INTO ls_range INDEX 1.
"  IF sy-subrc = 0.
"    rv_low  = ls_range-low.
"    rv_high = ls_range-high.
"  ENDIF.
"ENDFORM.


"FORM save_range_values USING rt_range TYPE STANDARD TABLE
"                             rv_low   TYPE any
"                             rv_high  TYPE any.
"  CLEAR rt_range.
"  IF rv_low IS NOT INITIAL OR rv_high IS NOT INITIAL.
"    APPEND INITIAL LINE TO rt_range ASSIGNING FIELD-SYMBOL(<fs>).
"    <fs>-sign   = 'I'.
"    <fs>-option = 'BT'.  " Between – tartományos keresés
"    <fs>-low    = rv_low.
"    <fs>-high   = rv_high.
"  ENDIF.
"ENDFORM.

INCLUDE zaft_sela_b_mp_1_status_010o01.

INCLUDE zaft_sela_b_mp_1_exit_0100i01.

INCLUDE zaft_sela_b_mp_1_user_commai01.

INCLUDE zaft_sela_b_mp_1_status_010o02.

INCLUDE zaft_sela_b_mp_1_pbo_101o01.

INCLUDE zaft_sela_b_mp_1_user_commai02.

"INCLUDE zaft_sela_b_mp_1_exit_101i01.


INCLUDE zaft_sela_b_mp_1_pbo_load_ro01.

INCLUDE zaft_sela_b_mp_1_clear_sel_o01.

INCLUDE zaft_sela_b_mp_1_exit_101i02.



INCLUDE zaft_sela_b_mp_1_hide_xloeko02.

INCLUDE zaft_sela_b_mp_1_customize_o01.




*-----------------------------------------------------------SCREENS------------------------------------------------------------
*-100

PROCESS BEFORE OUTPUT.
  "MODULE clear_sel_fields.
  CALL SUBSCREEN sub INCLUDING sy-repid '0200'.
  MODULE hide_xloekz.
  MODULE status_0100.

PROCESS AFTER INPUT.
  MODULE exit_0100 AT EXIT-COMMAND.
  CALL SUBSCREEN sub.

  MODULE user_command_0100.

*-CONTENTS
MODULE hide_xloekz OUTPUT.
  LOOP AT SCREEN.
    IF rb_upl = 'X' AND screen-name = 'P_XLOEKZ'.
      screen-active = 0.  "Elrejti a mezőt
    ELSEIF rb_alv = 'x' AND screen-name = 'P_XLOEKZ'.
      screen-active = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.

MODULE status_0100 OUTPUT.
SET PF-STATUS 'SCR1_STATUS'.
SET TITLEBAR 'FŐ KÉPERNYŐ'.
ENDMODULE.

MODULE exit_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

MODULE user_command_0100 INPUT.
  ok_copy = ok_code.
  CLEAR ok_code.

  CASE sy-ucomm.
    WHEN 'MEHET'.
      IF rb_alv = 'X'.
        PERFORM fetch_data_zdev7ekkoekpo USING gt_outtab
                                         CHANGING it_zdev7_ekkoekpo_origin.
        CALL SCREEN 101.
      ELSEIF rb_upl = 'X'.
        PERFORM upload_zdev7ekkoekpo USING it_zdev7_ekkoekpo.
      ENDIF.

  ENDCASE.
ENDMODULE.

*-101

PROCESS BEFORE OUTPUT.
  MODULE status_0101.
  MODULE pbo_101.
    MODULE customize_alv.

PROCESS AFTER INPUT.
  MODULE user_command_0101.
  MODULE exit_101 AT EXIT-COMMAND.

*-contents

MODULE status_0101 OUTPUT.
SET PF-STATUS 'SCR2_STATUS'.
SET TITLEBAR 'SZOMASZÁD TÁBLA'.
ENDMODULE.

MODULE pbo_101 OUTPUT.

  "  " Ha az ALV már létezik, előbb töröljük a régi példányt
  "  IF NOT go_grid IS INITIAL.
  "    CLEAR go_grid.
  "  ENDIF.

  " Mezőkatalógus létrehozása
  PERFORM fieldcat_layout_init USING gt_fieldcat.

  DATA(lo_container) = NEW cl_gui_custom_container( container_name = 'CONTAINER_ALV' ).
  go_grid = NEW cl_gui_alv_grid( i_parent = lo_container ).


  " ALV beállítása és megjelenítése
  go_grid->set_table_for_first_display(
    EXPORTING
      i_structure_name              = 'ZDEV7_EKKOEKPO'
    CHANGING
      it_outtab                     = gt_outtab
      it_fieldcatalog               = gt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4
  ).

  " ALV frissítése
  go_grid->refresh_table_display( ).
  " Események regisztrálása
  go_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).

ENDMODULE.

*----------------------------------------------------------------------*
***INCLUDE ZAFT_SELA_B_MP_1_CUSTOMIZE_O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module CUSTOMIZE_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE customize_alv OUTPUT.
  DATA: xv_diff   TYPE char1,
        ls_backup TYPE zdev7_ekkoekpo.

  FIELD-SYMBOLS: <fs_outtab> LIKE ls_outtab.
  LOOP AT gt_outtab ASSIGNING <fs_outtab>.
    " Meghívjuk a funkciós modult, hogy kiderüljön, van-e eltérés
    CALL FUNCTION 'ZDEV7_013_HOME_S01E10'
      EXPORTING
        iv_ebeln      = <fs_outtab>-ebeln
      IMPORTING
        ev_difference = xv_diff.
    IF p_xloekz = 'X'.
      <fs_outtab>-xloekz = 'X'.

      IF <fs_outtab>-xloekz = 'X'.

        <fs_outtab>-lamp_icon = '@0A@'.  "Piros lámpa ha van törlés előjegyezve
      ELSEIF  xv_diff = 'X'.
        <fs_outtab>-lamp_icon = '@09@'. " sárga lámpa ha van eltérés
      ELSEIF  xv_diff = ''.
        <fs_outtab>-lamp_icon = '@08@'. " Zöld lámpa (értékek egyeznek)
      ENDIF .
      PERFORM sor_szinezes USING   <fs_outtab>-matnr
                                   <fs_outtab>-netwr
                          CHANGING <fs_outtab>-color.
      PERFORM netwr_kezdeti_allithatosag USING <fs_outtab>-xloekz
                                      ls_outtab
                                      CHANGING <fs_outtab>-celltab.
    ELSEIF p_xloekz = ''.
      <fs_outtab>-xloekz = ''.
      IF <fs_outtab>-xloekz = ''.
        <fs_outtab>-lamp_icon = '@08@'.  "zöld lámpa ha van törlés előjegyezve
      ELSEIF  xv_diff = 'X'.
        <fs_outtab>-lamp_icon = '@09@'. " sárga lámpa ha van eltérés
      ELSEIF  xv_diff = ''.
        <fs_outtab>-lamp_icon = '@08@'. " Zöld lámpa (értékek egyeznek)
      ENDIF .
      PERFORM sor_szinezes USING   <fs_outtab>-matnr
                                   <fs_outtab>-netwr
                          CHANGING <fs_outtab>-color.
      PERFORM netwr_kezdeti_allithatosag USING <fs_outtab>-xloekz
                                      ls_outtab
                                      CHANGING <fs_outtab>-celltab.
    ENDIF.
    MODIFY gt_outtab FROM <fs_outtab>.
  ENDLOOP.
ENDMODULE.

MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 100.
  ENDCASE.
ENDMODULE.


