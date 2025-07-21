*&---------------------------------------------------------------------*
*& Module Pool      ZAFT_SELA_B_MP_1
*&---------------------------------------------------------------------*
*Ez a program a ZDEV7_EKKOEKPO táblában tárolt beszerzési rendelési adatokat kezeli és jeleníti meg.
*Az adatokat az adatbázisból tölti be, és lehetőséget biztosít a felhasználónak bizonyos mezők módosítására egy ALV-ben.
*Az "XLOEKZ" (Törlés előjegyzés) oszlop egy jelölőnégyzet, amely bejelölés esetén törlésre jelöli a rendelést.
*Ha az XLOEKZ mező be van pipálva, akkor a "NETWR" (Beszerzési nettó érték) mező írásvédett lesz, és egy piros figyelmeztető ikon jelenik meg.
*Ha az XLOEKZ mező nincs bejelölve, a NETWR mező ismét szerkeszthetővé válik, és egy zöld ikon jelenik meg.
*A program tartalmaz egy "Mentés" és egy "Frissítés" gombot. Az utóbbi újratölti az adatokat, miközben megtartja a felhasználói módosításokat,
*de visszaállítja az ikonokat és a színeket, az előbbi pedig menti a módosított NETWR adatokat a fő (ZDEV7_EKKOEKPO) táblába.
*Az ALV dinamikusan frissíti a színeket és stílusokat a felhasználói bemenet és az üzleti szabályok alapján.
*&---------------------------------------------------------------------*

PROGRAM zaft_sela_b_mp_1.
*----------------------------------------------------------------ADATBÁZIS DEKLARÁCIÓK------------------------------------------------------------

TABLES: ekpo, ekko, zdev7_ekkoekpo.
DATA it_zdev7_ekkoekpo TYPE TABLE OF zdev7_ekkoekpo.
DATA it_zdev7_ekkoekpo_origin TYPE TABLE OF zdev7_ekkoekpo.
DATA gt_outtab TYPE TABLE OF zdev7_013_ekkoekpo. " ez egy munka-struktúra amely a stílus elemek miatt lett létrehozva
DATA ls_outtab TYPE zdev7_013_ekkoekpo.
*---------------------------------------------------------------TOVÁBBI DEKLARÁCIÓK-----------------------------------------------------------

INCLUDE <cl_alv_control>.

CLASS lcl_event_handler DEFINITION DEFERRED.
DATA g_repid TYPE sy-repid.
DATA gt_fieldcat TYPE lvc_t_fcat.
DATA g_user_command TYPE slis_formname VALUE 'USER-COMMAND'.
DATA layout TYPE lvc_s_layo.
DATA g_status TYPE slis_formname VALUE 'SET_STATUS'.
DATA:go_event_receiver TYPE REF TO lcl_event_handler,
     go_container      TYPE REF TO cl_gui_custom_container,
     go_grid           TYPE REF TO cl_gui_alv_grid.

DATA ok_code LIKE sy-ucomm.
DATA save_ok_code LIKE sy-ucomm.

DATA rb_alv(1) TYPE c VALUE 'X'.
DATA rb_upl(1) TYPE c.
DATA: p_xloekz TYPE c LENGTH 1.
*--------------------------------------------------------------------------------LCL_EVENT_HANDLER CLASS-------------------------------------------------------------------------------

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed.
    DATA: lt_mod_cells TYPE lvc_t_modi,
          ls_mod_cell  TYPE lvc_s_modi,
          ls_celltab   TYPE lvc_s_styl.

    LOOP AT er_data_changed->mt_good_cells INTO ls_mod_cell.
      IF ls_mod_cell-fieldname = 'XLOEKZ'.
        DATA(lv_value) = ls_mod_cell-value.
        READ TABLE gt_outtab ASSIGNING FIELD-SYMBOL(<fs_outtab>) INDEX ls_mod_cell-row_id.
        IF sy-subrc = 0.
          " Ha a checkbox be van pipálva, akkor NETWR readonly és LAMP_ICON piros
          IF lv_value = 'X'.
            <fs_outtab>-lamp_icon = '@0A@'.  " Piros ikon trafficlámpa
            LOOP AT <fs_outtab>-celltab INTO ls_celltab.
              ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
              MODIFY <fs_outtab>-celltab FROM ls_celltab TRANSPORTING style.
            ENDLOOP.
          ELSEIF lv_value = ''.
            <fs_outtab>-lamp_icon = '@08@'.  " Alapértelmezett ikon ( zöld)
            LOOP AT <fs_outtab>-celltab INTO ls_celltab.
              ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
              MODIFY <fs_outtab>-celltab FROM ls_celltab TRANSPORTING style.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ELSEIF   ls_mod_cell-fieldname = 'NETWR'.
        " Színezés a feladat alapján. Átláthatóság miatt neveztem <fs_outtab2>-nek.
        DATA(lv_netwr) = CONV netwr( ls_mod_cell-value ). "Színezéshez NETWR konvertálása számformátumba
        READ TABLE gt_outtab ASSIGNING FIELD-SYMBOL(<fs_outtab2>) INDEX ls_mod_cell-row_id.
        IF sy-subrc = 0.
          "Újraszínezés, mert másképp maradnának az előbbi szerkesztett tábla sor színezései!
          PERFORM sor_szinezes USING   <fs_outtab2>-matnr
                                     lv_netwr
                            CHANGING <fs_outtab2>-color.
        ENDIF.
      ENDIF.
    ENDLOOP.
    go_grid->refresh_table_display( i_soft_refresh = 'X' ).

    "A kurzor beállítása
    DATA: ls_row_id TYPE lvc_s_row,
          ls_col_id TYPE lvc_s_col.

    ls_row_id-index = ls_mod_cell-row_id.
    ls_col_id-fieldname = ls_mod_cell-fieldname.

    go_grid->set_current_cell_via_id(
      EXPORTING
        is_row_id = ls_row_id
        is_column_id = ls_col_id
    ).
  ENDMETHOD.

ENDCLASS.
*----------------------------------------------------------------SUBSCREEN-SZELEKCIÓS MEZŐ--------------------------------------------------

SELECTION-SCREEN: BEGIN OF SCREEN 200 AS SUBSCREEN.

  SELECT-OPTIONS: gt_ebeln FOR ekpo-ebeln DEFAULT '4500106625' TO '4500106631' ,
                  gt_ebelp FOR ekpo-ebelp,
                  gt_bukrs FOR ekko-bukrs,
                  gt_bstyp FOR ekko-bstyp,
                  gt_bsart FOR ekko-bsart.
SELECTION-SCREEN: END OF SCREEN 200.
*--------------------------------------------------------------SZUBROUTINOK------------------------------------------------------------------

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
  " elmentjük az eredeti szelekciót
  CLEAR backup_outtab.
  LOOP AT xt_outtab ASSIGNING FIELD-SYMBOL(<fs_xt>).
    "DATA(ls_backup) = backup_outtab.  " Új rekord inicializálása
    MOVE-CORRESPONDING <fs_xt> TO ls_backup.  " Csak a közös mezőket másoljuk
    APPEND ls_backup TO backup_outtab.
  ENDLOOP.
  LOOP AT xt_outtab ASSIGNING <fs_outtab>.
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
  ENDLOOP.
ENDFORM.

FORM sor_szinezes USING    xv_matnr TYPE matnr
                          xv_netwr TYPE netwr
                  CHANGING cv_color TYPE char4.
  IF xv_matnr IS NOT INITIAL AND xv_netwr = 0.
    cv_color = 'C710'. " PIROS
  ELSEIF xv_netwr = 0.
    cv_color = 'C310'. " SÁRGA
  ELSEIF xv_matnr IS NOT INITIAL.
    cv_color = 'C501'. " ZÖLD
  ENDIF.
ENDFORM.

FORM netwr_kezdeti_allithatosag USING xw_xloekz TYPE eloek
                                      xw_output LIKE ls_outtab
                                CHANGING celltab TYPE lvc_t_styl.
  DATA: xt_celltab TYPE lvc_t_styl,
        xs_celltab TYPE lvc_s_styl.
  IF xw_xloekz = 'X'.
    xs_celltab-fieldname = 'NETWR'.
    xs_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  ELSEIF xw_xloekz = ''.
    xs_celltab-fieldname = 'NETWR'.
    xs_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
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
      WHEN 'LAMP_ICON'.
        xs_fieldcat-icon = 'X'.
        xs_fieldcat-scrtext_s = 'Stát.'.
        xs_fieldcat-scrtext_m = 'Státusz ikon'.
        xs_fieldcat-scrtext_l = 'Rekord státusza'.
        MODIFY xt_fieldcat FROM xs_fieldcat INDEX sy-tabix.
      WHEN 'NETWR'.
        xs_fieldcat-style = alv_style_font_bold.
        MODIFY xt_fieldcat FROM xs_fieldcat INDEX sy-tabix.
      WHEN 'XLOEKZ'.
        xs_fieldcat-edit = 'X'.
        xs_fieldcat-checkbox = 'X'.
        xs_fieldcat-outputlen = 1.
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
  layout-cwidth_opt = 'X'.
ENDFORM.
*--------------------------------------------------------------------------------------INCLUDE-OK----------------------------------------------------------------------------------

INCLUDE zaft_sela_b_mp_1_status_010o01.

INCLUDE zaft_sela_b_mp_1_exit_0100i01.

INCLUDE zaft_sela_b_mp_1_user_commai01.

INCLUDE zaft_sela_b_mp_1_status_010o02.

INCLUDE zaft_sela_b_mp_1_pbo_101o01.

INCLUDE zaft_sela_b_mp_1_user_commai02.

INCLUDE zaft_sela_b_mp_1_pbo_load_ro01.

INCLUDE zaft_sela_b_mp_1_clear_sel_o01.

INCLUDE zaft_sela_b_mp_1_exit_101i02.

INCLUDE zaft_sela_b_mp_1_hide_xloeko02.