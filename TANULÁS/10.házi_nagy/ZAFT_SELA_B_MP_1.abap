*&---------------------------------------------------------------------*
*& Module Pool      ZAFT_SELA_B_MP_1
*&---------------------------------------------------------------------*
*This ABAP program displays and manages purchase order data stored in the ZDEV7_EKKOEKPO table.
*It retrieves data from the database and allows the user to modify specific fields in an ALV grid.
*The "Deletion Flag" (XLOEKZ) column is a checkbox that, when selected, marks the order for deletion.
*If XLOEKZ is checked, the "Net Order Value" (NETWR) field becomes read-only, and a red warning icon is displayed.
*If XLOEKZ is unchecked, the NETWR field becomes editable again, and a green icon appears.
*The program includes a refresh button, which reloads the data while keeping the user's changes but resetting the grid’s colors and icons, 
*and a "Save" button, which saves the modified NETWR data to the main (ZDEV7_EKKOEKPO) table.
*The ALV grid dynamically updates colors and styles based on the user's input and business rules.
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

TABLES: ekpo, ekko, zdev7_ekkoekpo, zdev7_ekkoekpo_o.
DATA it_zdev7_ekkoekpo TYPE TABLE OF zdev7_ekkoekpo.
DATA it_zdev7_ekkoekpo_load_from TYPE TABLE OF zdev7_ekkoekpo.
DATA it_zdev7_ekkoekpo_origin TYPE TABLE OF zdev7_ekkoekpo_o.
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
    METHODS
      update_celltab_styles.
ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed.
    DATA: lt_mod_cells TYPE lvc_t_modi,
          ls_mod_cell  TYPE lvc_s_modi,
          ls_celltab   TYPE lvc_s_styl.
    DATA(xv_diff) = abap_false.

    LOOP AT er_data_changed->mt_good_cells INTO ls_mod_cell.
      IF ls_mod_cell-fieldname = 'XLOEKZ'.
        DATA(lv_value) = ls_mod_cell-value.
        READ TABLE gt_outtab ASSIGNING FIELD-SYMBOL(<fs_outtab>) INDEX ls_mod_cell-row_id.
        IF sy-subrc = 0.
          <fs_outtab>-xloekz = lv_value. " az aktuális sor így frissül azonnal
          " Ha a checkbox be van pipálva, akkor NETWR readonly és LAMP_ICON piros
          IF lv_value = 'X'.
            <fs_outtab>-lamp_icon = '@0A@'.  " Piros ikon trafficlámpa
            LOOP AT <fs_outtab>-celltab INTO ls_celltab.
              ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
              MODIFY <fs_outtab>-celltab FROM ls_celltab TRANSPORTING style.
            ENDLOOP.
          ELSEIF lv_value = ''.
            "            DATA(xv_diff) = abap_false.
            CALL FUNCTION 'ZDEV7_013_HOME_S01E10_NETWR'
              EXPORTING
                iv_netwr      = ls_mod_cell-value
                iv_ebeln      = <fs_outtab>-ebeln
              IMPORTING
                ev_difference = xv_diff.
            IF xv_diff = abap_true.
              <fs_outtab>-lamp_icon = '@09@'.  " 🟡 Sárga ikon
            ELSE.
              <fs_outtab>-lamp_icon = '@08@'.  " 🟢 Zöld ikon
            ENDIF.
            "           <fs_outtab>-lamp_icon = '@08@'.  " Alapértelmezett ikon ( zöld)
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
          " saját funkció, hogy eltérés van-e
          "          DATA(xv_diff) = abap_false.
          CALL FUNCTION 'ZDEV7_013_HOME_S01E10_NETWR'
            EXPORTING
              iv_netwr      = ls_mod_cell-value
              iv_ebeln      = <fs_outtab2>-ebeln
            IMPORTING
              ev_difference = xv_diff.

          " Ha eltérés van, akkor sárga lámpa
          IF xv_diff = abap_true.
            <fs_outtab2>-lamp_icon = '@09@'.  " 🟡 Sárga ikon
          ELSE.
            <fs_outtab2>-lamp_icon = '@08@'.  " 🟢 Zöld ikon
          ENDIF.
          "Újraszínezés, mert másképp maradnának az előbbi szerkesztett tábla sor színezései!
          PERFORM sor_szinezes USING   <fs_outtab2>-matnr
                                     lv_netwr
                            CHANGING <fs_outtab2>-color.
        ENDIF.
      ENDIF.
    ENDLOOP.
    me->update_celltab_styles( ).
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

  METHOD update_celltab_styles. " azért fontos, mert a köv soron az xloekz pipálásakor már elfelejtette az előzően állított sor szerkeszthetőségét
    DATA: ls_celltab TYPE lvc_s_styl.
    LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<fs_row>).
      LOOP AT <fs_row>-celltab INTO ls_celltab.
        IF <fs_row>-xloekz = 'X'.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
        ELSE.
          ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
        ENDIF.
        MODIFY <fs_row>-celltab FROM ls_celltab TRANSPORTING style.
      ENDLOOP.
    ENDLOOP.
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

FORM upload_zdev7ekkoekpo_o USING xt_ekkoekpo TYPE STANDARD TABLE.
  DELETE FROM  zdev7_ekkoekpo_o.
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
  INSERT zdev7_ekkoekpo_o FROM TABLE @xt_ekkoekpo.
ENDFORM.

FORM fetch_data_zdev7ekkoekpo CHANGING  xt_outtab  TYPE STANDARD TABLE
                                        save_to_load_from TYPE STANDARD TABLE.

  FIELD-SYMBOLS: <fs_outtab> LIKE ls_outtab.
  DATA: xv_diff   TYPE char1,
        ls_backup TYPE zdev7_ekkoekpo.
  "Feltöltjük a zdev7_ekkoekpo adatbázis táblát
  DELETE FROM  zdev7_ekkoekpo.
  SELECT a~ebeln, a~bukrs, a~bstyp,
       a~bsart, a~aedat,
       b~ebelp,
       b~txz01, b~matnr, b~ematn, b~bukrs AS bukrs2,
       b~werks, b~lgort, b~matkl, b~infnr, b~idnlf,
       b~ktmng, b~menge, b~meins, b~netwr
    INTO CORRESPONDING FIELDS OF TABLE @it_zdev7_ekkoekpo
    FROM ekko AS a
    INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
    WHERE     a~ebeln IN @gt_ebeln
          AND b~ebelp IN @gt_ebelp
          AND a~bukrs IN @gt_bukrs
          AND a~bstyp IN @gt_bstyp
          AND a~bsart IN @gt_bsart.
  INSERT zdev7_ekkoekpo FROM TABLE @it_zdev7_ekkoekpo.
  "feltöltjük tulajdonképpen a gt_outtab-et
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
  " elmentjük az eredeti szelekciót a it_zdev7_ekkoekpo_load_from-ba igazából
  CLEAR save_to_load_from.
  LOOP AT xt_outtab ASSIGNING FIELD-SYMBOL(<fs_xt>).
    "DATA(ls_backup) = backup_outtab.  " Új rekord inicializálása
    MOVE-CORRESPONDING <fs_xt> TO ls_backup.  " Csak a közös mezőket másoljuk
    APPEND ls_backup TO save_to_load_from.
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
      PERFORM netwr_kezdeti_allithatosag CHANGING <fs_outtab>-xloekz
                                                  ls_outtab
                                                  <fs_outtab>-celltab.
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
      PERFORM netwr_kezdeti_allithatosag CHANGING <fs_outtab>-xloekz
                                                   ls_outtab
                                                  <fs_outtab>-celltab.
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

FORM netwr_kezdeti_allithatosag CHANGING xw_xloekz TYPE eloek
                                         xw_output LIKE ls_outtab
                                         celltab TYPE lvc_t_styl.
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

FORM get_data_for_hierseq.
  "előbb egy táblába összerakjuk az összes adatot ami kell, és csak egyszer hívjuk meg a szubrutint
  SELECT a~ebeln, a~bukrs, a~bstyp,
       a~bsart, a~aedat,
       b~ebelp,
       b~txz01, b~matnr, b~ematn, b~bukrs AS bukrs2,
       b~werks, b~lgort, b~matkl, b~infnr, b~idnlf,
       b~ktmng, b~menge, b~meins, b~netwr
    INTO CORRESPONDING FIELDS OF TABLE @it_zdev7_ekkoekpo_hierseq
    FROM ekko AS a
    INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
    WHERE     a~ebeln IN @gt_ebeln
          AND b~ebelp IN @gt_ebelp
          AND a~bukrs IN @gt_bukrs
          AND a~bstyp IN @gt_bstyp
          AND a~bsart IN @gt_bsart.

ENDFORM.


FORM display_hierseq_alv.
  DATA: lt_ekko TYPE TABLE OF zdev7_ekko,
        lt_ekpo TYPE TABLE OF zdev7_ekpo,
        ls_ekko TYPE zdev7_ekko,
        ls_ekpo TYPE zdev7_ekpo.
  DATA: lt_fieldcat_header  TYPE slis_t_fieldcat_alv,
        lt_fieldcat_item TYPE slis_t_fieldcat_alv,
        lt_fieldcat      TYPE slis_t_fieldcat_alv,
        ls_layout2       TYPE slis_layout_alv,
        ls_keyinfo       TYPE slis_keyinfo_alv.

  "betöltjük a közös táblából az adatokat külön az lt_ekpo, és ekko-ba
  LOOP AT it_zdev7_ekkoekpo_hierseq INTO DATA(ls_row).
    READ TABLE lt_ekko WITH KEY ebeln = ls_row-ebeln TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING ls_row TO ls_ekko.
      APPEND ls_ekko TO lt_ekko.
    ENDIF.
    MOVE-CORRESPONDING ls_row TO ls_ekpo.
    APPEND ls_ekpo TO lt_ekpo.
  ENDLOOP.
* --- Fejléc (EKKO) mezők field catalog ---
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDEV7_EKKO'
    CHANGING
      ct_fieldcat      = lt_fieldcat_header.

* --- Tétel (EKPO) mezők field catalog ---
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDEV7_EKPO'
    CHANGING
      ct_fieldcat      = lt_fieldcat_item.

 "egyesítsük a két mezőkatalógust
  CLEAR lt_fieldcat.
  APPEND LINES OF lt_fieldcat_header TO lt_fieldcat.
  APPEND LINES OF lt_fieldcat_item   TO lt_fieldcat.

  ls_layout2-colwidth_optimize = 'X'.
  ls_keyinfo-header01 = 'EBELN'.
  ls_keyinfo-item01   = 'EBELN' .

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      is_layout               = ls_layout2
      it_fieldcat             = lt_fieldcat_item
      i_tabname_header        = 'lt_ekko'
      i_tabname_item          = 'lt_ekpo'
      i_structure_name_header = 'ZDEV7_EKKO'
      i_structure_name_item   = 'ZDEV7_EKPO'
      is_keyinfo              = ls_keyinfo
    TABLES
      t_outtab_header         = lt_ekko
      t_outtab_item           = lt_ekpo
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
"nem használjuk!
FORM display_hierseq_alv_bonya.
  DATA: lt_ekko TYPE TABLE OF zdev7_ekko,
        lt_ekpo TYPE TABLE OF zdev7_ekpo,
        ls_ekko TYPE zdev7_ekko,
        ls_ekpo TYPE zdev7_ekpo.

  DATA: go_container_tree TYPE REF TO cl_gui_custom_container,
        go_tree           TYPE REF TO cl_gui_alv_tree.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        ls_fieldcat TYPE lvc_s_fcat.

  "betöltjük a közös táblából az adatokat külön az lt_ekpo, és ekko-ba
  LOOP AT it_zdev7_ekkoekpo_hierseq INTO DATA(ls_row).
    READ TABLE lt_ekko WITH KEY ebeln = ls_row-ebeln TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING ls_row TO ls_ekko.
      APPEND ls_ekko TO lt_ekko.
    ENDIF.
    MOVE-CORRESPONDING ls_row TO ls_ekpo.
    APPEND ls_ekpo TO lt_ekpo.
  ENDLOOP.

  IF go_tree IS INITIAL.
    IF go_container_tree IS INITIAL.
      CREATE OBJECT go_container_tree
        EXPORTING
          container_name = 'CONTAINER_TREE'.  " Ez legyen az új konténer nevét tartalmazó képernyőelem
    ENDIF.

    "hierarchia beállítása
    DATA:    ls_hierarchy TYPE treev_hhdr.
    ls_hierarchy-heading   = 'Beszerzési rendelések'. " Ez jelenik meg a hierarchia tetején
    ls_hierarchy-tooltip   = 'Beszerzési rendelés és tételei'.
    ls_hierarchy-width     = 45.
    ls_hierarchy-width_pix = ''.

    CREATE OBJECT go_tree
      EXPORTING
        parent              = go_container_tree
        node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
        i_fcat_complete     = ''
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      " Hiba esetén hibaüzenet, log, stb.
    ENDIF.
  ENDIF.

  CALL METHOD go_tree->set_table_for_first_display
    EXPORTING
      i_structure_name    = 'ZDEV7_EKPO'
      is_hierarchy_header = ls_hierarchy
    CHANGING
      it_outtab           = lt_ekpo
      it_fieldcatalog     = lt_fieldcat.

ENDFORM.
*------------------------------------------------------------------------RS_TREE_CONSTRUCT módszer------------------------------------------------------------
FORM prepare_tree_data.

  DATA: lv_id        TYPE lvc_nkey,
        lv_parent_id TYPE lvc_nkey,
        lv_counter   TYPE i VALUE 0,
        ls_data      TYPE zdev7_ekkoekpo.
  DATA lv_text TYPE char75 .
  DATA(lv_text_item) = ''.

  CLEAR gt_node.

  LOOP AT it_zdev7_ekkoekpo_hierseq INTO ls_data.

    " Új rendelés esetén új parent node

    READ TABLE it_zdev7_ekkoekpo_hierseq INTO DATA(ls_header)
         WITH KEY ebeln = ls_data-ebeln.

    lv_counter += 1.
    lv_id = lv_counter.
    lv_parent_id = lv_id. " ezt tároljuk, hogy a tételeket alá rakjuk

    CLEAR gs_node.
    gs_node-id = lv_id.
    gs_node-text = |{ ls_header-ebeln } – { ls_header-bukrs } / { ls_header-bsart } / { ls_header-aedat }|.
    gs_node-tlength = '50'.
    gs_node-color = '05'.
    gs_node-tlevel = 1.
    gs_node-type = 'S' .
    gs_node-parent = '' .

    APPEND gs_node TO gt_node.
    " Tétel mint child node
    lv_counter += 1.
    lv_id = lv_counter.

    CLEAR gs_node.
    gs_node-id = lv_id.
    gs_node-parent = lv_parent_id.
    gs_node-text = |Tétel { ls_data-ebelp } – { ls_data-matnr } – { ls_data-menge } { ls_data-meins } – { ls_data-netwr } |.
    gs_node-tlength = '50'.
    gs_node-color = '04'.
    gs_node-tlevel = 2.
    gs_node-type = 'C'. " Child node
    APPEND gs_node TO gt_node.
  ENDLOOP.
ENDFORM.

FORM build_tree_structure.

  CALL FUNCTION 'RS_TREE_CONSTRUCT'
    EXPORTING
      insert_id          = '000000'
    TABLES
      nodetab            = gt_node
    EXCEPTIONS
      tree_failure       = 1
      id_not_found       = 2
      wrong_relationship = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL FUNCTION 'RS_TREE_LIST_DISPLAY'
    EXPORTING
      callback_program = sy-repid.

ENDFORM.

"------------------------------------------------------------------------CL_SALV_TREE módszer------------------------------------------------------------
FORM salv_tree.
  DATA: it_data_tree  TYPE TABLE OF zdev7_ekkoekpo,
        it_data_tree2 TYPE TABLE OF zdev7_ekkoekpo.
  "  FIELD-SYMBOLS:     <wa_data>      TYpe zdev7_ekkoekpo.
  DATA lv_parent_key TYPE salv_de_node_key.
  DATA lv_text TYPE lvc_value.
  DATA lv_parent_text TYPE lvc_value.
  DATA lv_child_text TYPE lvc_value.

  DATA: obj_tree     TYPE REF TO cl_salv_tree,
        obj_nodes    TYPE REF TO cl_salv_nodes,
        obj_node     TYPE REF TO cl_salv_node,
        obj_settings TYPE REF TO cl_salv_tree_settings.


  TRY.
      CALL METHOD cl_salv_tree=>factory
        IMPORTING
          r_salv_tree = obj_tree
        CHANGING
          t_table     = it_data_tree.
    CATCH cx_salv_error." INTO DATA(lx_salv).
      "      MESSAGE lx_salv->get_text( ) TYPE 'E'.

  ENDTRY.

  IF obj_tree IS INITIAL.
    MESSAGE 'Nem sikerült létrehozni a SALV Tree-t' TYPE 'E'.
    RETURN.
  ENDIF.

  obj_settings = obj_tree->get_tree_settings( ).

  obj_settings->set_header( 'ALV Tree Samples' ).
  obj_settings->set_hierarchy_header( 'Megrendelése' ).
  obj_settings->set_hierarchy_tooltip( 'EKKO/EKPO' ).
  obj_settings->set_hierarchy_size( 20 ).

  obj_nodes = obj_tree->get_nodes( ).
  IF 1 = 2.
    DO 5 TIMES.
      obj_node = obj_nodes->add_node( related_node = ''
                                      text = 'Parent 1'
                                      relationship = ' ' ).

      lv_parent_key = obj_node->get_key( ).
      obj_node = obj_nodes->add_node( related_node = lv_parent_key
                                      text = 'CHILD 1'
                                      relationship = cl_gui_column_tree=>relat_first_child ).

    ENDDO.
  ENDIF.
  "feltöltjük az it_data_tree2-t
  SELECT a~ebeln, a~bukrs, a~bstyp,
       a~bsart, a~aedat,
       b~ebelp,
       b~txz01, b~matnr, b~ematn, b~bukrs AS bukrs2,
       b~werks, b~lgort, b~matkl, b~infnr, b~idnlf,
       b~ktmng, b~menge, b~meins, b~netwr
    INTO CORRESPONDING FIELDS OF TABLE @it_data_tree2
    FROM ekko AS a
    INNER JOIN ekpo AS b ON a~ebeln = b~ebeln
    WHERE     a~ebeln IN @gt_ebeln
          AND b~ebelp IN @gt_ebelp
          AND a~bukrs IN @gt_bukrs
          AND a~bstyp IN @gt_bstyp
          AND a~bsart IN @gt_bsart.

  LOOP AT it_data_tree2 INTO DATA(wa_data).
    CONCATENATE 'Rendelés:' wa_data-ebeln INTO lv_text SEPARATED BY space.

    ON CHANGE OF wa_data-ebeln.
      obj_node = obj_nodes->add_node( related_node = ''
                                        text = lv_text
                                        "data_row = wa_data
                                        relationship = '' ).
      DATA(lv_order_key) = obj_node->get_key( ).
    ENDON.

    CONCATENATE 'Tétel:' wa_data-ebelp INTO lv_text SEPARATED BY space.
    obj_node = obj_nodes->add_node( related_node = lv_order_key
                                      text = lv_text
                                      data_row = wa_data
                                      relationship = cl_gui_column_tree=>relat_first_child ).
  ENDLOOP.
  obj_tree->display( ).

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

*--------------------------------------------------------------------FUNCTIONS-----------------------------------------------------------------------------------
FUNCTION ZDEV7_013_HOME_S01E10.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_EBELN) TYPE  EKKO-EBELN
*"  EXPORTING
*"     REFERENCE(EV_DIFFERENCE) TYPE  CHAR01
*"----------------------------------------------------------------------

DATA:   "ET_MISMATCHED_ROWS TYPE ZGER_EKKOEKPO,
        lt_ekkoekpo TYPE TABLE OF Zdev7_EKKOEKPO,
        lt_ekpo      TYPE TABLE OF EKPO,
        lt_result    TYPE TABLE OF Zdev7_EKKOEKPO,
        ls_ekkoekpo  TYPE Zdev7_EKKOEKPO,
        ls_ekpo      TYPE EKPO.

  " ZEKKOEKPO tábla beolvasása
  SELECT * FROM Zdev7_EKKOEKPO INTO TABLE lt_ekkoekpo WHERE ebeln = IV_EBELN.

  " EKPO tábla beolvasása
  SELECT * FROM EKPO INTO TABLE lt_ekpo WHERE ebeln = IV_EBELN.

  " Kezdetben nincs eltérés
  CLEAR EV_DIFFERENCE.

  " Összehasonlítás ciklussal
  LOOP AT lt_ekkoekpo INTO ls_ekkoekpo.
    READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = ls_ekkoekpo-ebeln
                                                ebelp = ls_ekkoekpo-ebelp.
    IF sy-subrc <> 0 OR ls_ekkoekpo-netwr <> ls_ekpo-netwr.
      " Ha az EKPO-ban nincs meg, vagy az NETWR nem egyezik, eltérés van
      APPEND ls_ekkoekpo TO lt_result.
      EV_DIFFERENCE = 'X'.
    ELSEIF sy-subrc = 0 AND ls_ekkoekpo-netwr = ls_ekpo-netwr.
      EV_DIFFERENCE = ''.
    ENDIF.
  ENDLOOP.

  " Eltérések visszaadása
  "ET_MISMATCHED_ROWS = lt_result.
ENDFUNCTION.


FUNCTION ZDEV7_013_HOME_S01E10_NETWR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_EBELN) TYPE  EKKO-EBELN
*"     REFERENCE(IV_NETWR) TYPE  CHAR128
*"  EXPORTING
*"     REFERENCE(EV_DIFFERENCE) TYPE  CHAR01
*"----------------------------------------------------------------------
DATA:   "ET_MISMATCHED_ROWS TYPE ZGER_EKKOEKPO,
        lt_ekkoekpo TYPE TABLE OF Zdev7_EKKOEKPO,
        lt_ekpo      TYPE TABLE OF EKPO,
        lt_result    TYPE TABLE OF Zdev7_EKKOEKPO,
        ls_ekkoekpo  TYPE Zdev7_EKKOEKPO,
        ls_ekpo      TYPE EKPO.

  " ZEKKOEKPO tábla beolvasása
  SELECT * FROM Zdev7_EKKOEKPO INTO TABLE lt_ekkoekpo WHERE ebeln = IV_EBELN.

  " EKPO tábla beolvasása
  SELECT * FROM EKPO INTO TABLE lt_ekpo WHERE ebeln = IV_EBELN.

  " Kezdetben nincs eltérés
  CLEAR EV_DIFFERENCE.

  " Összehasonlítás ciklussal
  LOOP AT lt_ekkoekpo INTO ls_ekkoekpo.
    READ TABLE lt_ekpo INTO ls_ekpo WITH KEY ebeln = ls_ekkoekpo-ebeln
                                                ebelp = ls_ekkoekpo-ebelp.
    IF sy-subrc <> 0 OR iv_netwr <> ls_ekpo-netwr.
      " Ha az EKPO-ban nincs meg, vagy az NETWR nem egyezik, eltérés van
      APPEND ls_ekkoekpo TO lt_result.
      EV_DIFFERENCE = 'X'.
    ELSEIF sy-subrc = 0 AND iv_netwr = ls_ekpo-netwr.
      EV_DIFFERENCE = ''.
    ENDIF.
  ENDLOOP.

  " Eltérések visszaadása

  "ET_MISMATCHED_ROWS = lt_result.
ENDFUNCTION.
*-----------------------------------------------------------------103-screen--------------------------------------------------------------------------------------
PROCESS BEFORE OUTPUT.
 MODULE STATUS_0103.
 MODULE prep_hier_alv.

*
PROCESS AFTER INPUT.
 MODULE exit_0103 AT EXIT-COMMAND.
 MODULE USER_COMMAND_0103.

*-----------
 MODULE prep_hier_alv OUTPUT.
  PERFORM display_hierseq_alv.
ENDMODULE.
*------------
MODULE user_command_0103 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 100.
  ENDCASE.
ENDMODULE.
*------------
MODULE exit_0103 INPUT.
  LEAVE PROGRAM.
ENDMODULE.
*-----------------------------------------------------------------101-screen--------------------------------------------------------------------------------------

PROCESS BEFORE OUTPUT.
  MODULE status_0101.
  MODULE pbo_101.
 "   MODULE customize_alv.

PROCESS AFTER INPUT.
  MODULE user_command_0101.
  MODULE exit_101 AT EXIT-COMMAND.

MODULE pbo_101 OUTPUT.
  " Ha az ALV még nem létezik, csak akkor hozzuk létre!
  DATA: go_event_handler TYPE REF TO lcl_event_handler.

  IF go_grid IS INITIAL.
    " Mezőkatalógus létrehozása
    IF gt_fieldcat IS INITIAL.
      PERFORM fieldcat_layout_init USING gt_fieldcat.
    ENDIF.

    " Konténer és ALV objektum inicializálása
    IF go_container IS INITIAL.
      CREATE OBJECT go_container
        EXPORTING
          container_name = 'CONTAINER_ALV'.
    ENDIF.

    CREATE OBJECT go_grid
      EXPORTING
        i_parent = go_container.

    CREATE OBJECT go_event_handler.
    SET HANDLER go_event_handler->on_data_changed FOR go_grid.

*    CREATE OBJECT go_event_receiver.
*    SET HANDLER go_event_receiver->catch_doubleclick FOR go_grid.

    " ALV beállítása és első megjelenítés
    go_grid->set_table_for_first_display(
      EXPORTING
        i_structure_name = 'ZDEV7_EKKOEKPO'
        is_layout        = layout
      CHANGING
        it_outtab        = gt_outtab
        it_fieldcatalog  = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                         = 4
    ).
    " 🔹 ALV szerkeszthetőség beállítása
    go_grid->set_ready_for_input( 1 ).
    " Szerkeszthetőség és események regisztrálása
    go_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
  ELSE.
    " Ha az ALV már létezik, csak az adatokat frissítjük!
    go_grid->refresh_table_display( ).
  ENDIF.

ENDMODULE.

*----------------------------------------------------------------------*
***INCLUDE ZAFT_SELA_B_MP_1_USER_COMMAI02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
*  save_ok_code = ok_code.
*  CLEAR ok_code.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 100.
    WHEN '&LV'.  " Pipás targonca ikon -> mentés a ZGER_EKKOEKPO táblába
      DATA: lt_outtab  TYPE TABLE OF zdev7_ekkoekpo,  " Az ALV módosított adatai
            ls_outtab2 TYPE zdev7_ekkoekpo.
      LOOP AT lt_outtab INTO ls_outtab2.
        READ TABLE gt_outtab WITH KEY ebeln = ls_outtab-ebeln
                                      ebelp = ls_outtab-ebelp
                                      INTO DATA(ls_gt_row).
        IF sy-subrc = 0.
          ls_gt_row-netwr = ls_outtab-netwr.
          MODIFY gt_outtab FROM ls_gt_row TRANSPORTING netwr.
        ENDIF.
      ENDLOOP.
      "Adatok visszaírása az adatbázisba
      LOOP AT gt_outtab INTO ls_outtab.
        UPDATE zdev7_ekkoekpo SET netwr = @ls_outtab-netwr
        WHERE ebeln = @ls_outtab-ebeln
        AND   ebelp = @ls_outtab-ebelp.
      ENDLOOP.
      COMMIT WORK.
      MESSAGE 'Módosítások mentve.' TYPE 'I'.
    WHEN '&42'.  " Refresh ikon -> újratöltés
      DATA: lt_keep TYPE TABLE OF zdev7_013_ekkoekpo.

      " **1. Nem frissítendő mezők elmentése**
      LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<fs_keep>).
        APPEND INITIAL LINE TO lt_keep ASSIGNING FIELD-SYMBOL(<fs_backup>).
        <fs_backup>-ebeln     = <fs_keep>-ebeln.
        <fs_backup>-ebelp     = <fs_keep>-ebelp.
        <fs_backup>-lamp_icon = <fs_keep>-lamp_icon.
        <fs_backup>-color     = <fs_keep>-color.
        <fs_backup>-celltab   = <fs_keep>-celltab.
      ENDLOOP.
    CLEAR gt_outtab.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE @gt_outtab
        FROM zdev7_ekkoekpo.

      LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<fs_outtab>).
        READ TABLE it_zdev7_ekkoekpo_origin ASSIGNING FIELD-SYMBOL(<fs_origin>)
        WITH KEY ebeln = <fs_outtab>-ebeln
               ebelp = <fs_outtab>-ebelp.
        IF sy-subrc = 0.
          <fs_outtab>-netwr = <fs_origin>-netwr.  " Csak a NETWR mezőt frissítjük
        ENDIF.
        READ TABLE lt_keep ASSIGNING FIELD-SYMBOL(<fs_restore>)
          WITH KEY ebeln = <fs_outtab>-ebeln
                   ebelp = <fs_outtab>-ebelp.
        IF sy-subrc = 0.
          <fs_outtab>-lamp_icon = <fs_restore>-lamp_icon.
          <fs_outtab>-color     = <fs_restore>-color.
          <fs_outtab>-celltab   = <fs_restore>-celltab.
        ENDIF.
      ENDLOOP.
*      IF gv_refresh_done = 'X'.
*        MESSAGE 'Az adatok már vissza lettek állítva' TYPE 'I'.
*        RETURN. " Ne csináljon semmit, ha már egyszer lefutott
*      ENDIF.
*-------------------------------------------------------------------------
      " Ha az ALV objektum már létezik, frissítjük az adatokat
      PERFORM fieldcat_layout_init USING gt_fieldcat.
      IF go_grid IS NOT INITIAL.
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
        go_grid->refresh_table_display( ).
      ENDIF.
*      IF go_grid IS INITIAL.
*        " Mezőkatalógus létrehozása
*        PERFORM fieldcat_layout_init USING gt_fieldcat.
*
*        " Konténer és ALV objektum inicializálása
*        IF go_container IS INITIAL.
*          CREATE OBJECT go_container
*            EXPORTING
*              container_name = 'CONTAINER_ALV'.
*        ENDIF.
*
*        CREATE OBJECT go_grid
*          EXPORTING
*            i_parent = go_container.
*
*        " ALV beállítása és első megjelenítés
*        go_grid->set_table_for_first_display(
*          EXPORTING
*            i_structure_name = 'ZDEV7_EKKOEKPO'
*          CHANGING
*            it_outtab        = gt_outtab
*            it_fieldcatalog  = gt_fieldcat
*          EXCEPTIONS
*            invalid_parameter_combination = 1
*            program_error                 = 2
*            too_many_lines                = 3
*            OTHERS                         = 4
*        ).
*
*        " Szerkeszthetőség és események regisztrálása
*        go_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
*     ELSE.
        " Ha az ALV már létezik, csak az adatokat frissítjük!
*        go_grid->refresh_table_display( ).
*      ENDIF.
      MESSAGE 'Adatok visszaállítva az eredeti értékekre.' TYPE 'I'.
  ENDCASE.
ENDMODULE.

*-----------------------------------------------------------------100-screen----------------------------------------------------------------
PROCESS BEFORE OUTPUT.
  MODULE status_0100. "MODULE clear_sel_fields.
  CALL SUBSCREEN sub INCLUDING sy-repid '0200'.

PROCESS AFTER INPUT.
  MODULE exit_0100 AT EXIT-COMMAND.
  CALL SUBSCREEN sub.
  MODULE user_command_0100.


  MODULE user_command_0100 INPUT.
*  save_ok_code = ok_code.
*  CLEAR ok_code.

  CASE sy-ucomm.
    WHEN 'MEHET'.
        IF rb_alv = 'X'.
          PERFORM fetch_data_zdev7ekkoekpo CHANGING  gt_outtab
                                                     it_zdev7_ekkoekpo_load_from.
          CALL SCREEN 101.
  
        ELSEIF rb_upl = 'X'.
          PERFORM upload_zdev7ekkoekpo_o USING it_zdev7_ekkoekpo_origin.
          MESSAGE 'Szelekció szerint feltöltve a zdev7_ekkoekpo_o db-tábla.' TYPE 'I'.
  
  
        ELSEIF rb_hier = 'X'.
          PERFORM get_data_for_hierseq.
          PERFORM display_hierseq_alv_simple.
          "CALL SCREEN 103.
  
        ELSEIF rb_hier2 = 'X'.
          PERFORM get_data_for_hierseq.
          PERFORM prepare_tree_data.
          PERFORM build_tree_structure.
          "CALL SCREEN 104.
        ELSEIF rb_hier3 = 'X'.
          PERFORM salv_tree.
  
  ENDCASE.
ENDMODULE.