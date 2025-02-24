*&---------------------------------------------------------------------*
*& Report ZAFT_SELA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaft_sela.

INCLUDE <cl_alv_control>.

TYPE-POOLS: slis.
TABLES: ekpo, ekko, zdev7_ekkoekpo.

DATA it_zdev7_ekkoekpo TYPE TABLE OF zdev7_ekkoekpo.
DATA it_zdev7_ekkoekpo_origin TYPE TABLE OF zdev7_ekkoekpo.
DATA gt_outtab TYPE TABLE OF zdev7_013_ekkoekpo. " ez egy munka-struktúra amely a stílus elemek miatt lett létrehozva
DATA ls_outtab TYPE zdev7_013_ekkoekpo.

DATA g_repid TYPE sy-repid.
DATA gt_fieldcat TYPE lvc_t_fcat.
DATA g_user_command TYPE slis_formname VALUE 'USER-COMMAND'.
DATA layout TYPE lvc_s_layo.
DATA g_status TYPE slis_formname VALUE 'SET_STATUS'.
DATA go_grid TYPE REF TO cl_gui_alv_grid.
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
               IMPORTING er_data_changed.
ENDCLASS.
DATA go_event_receiver TYPE REF TO lcl_event_handler.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_loekz1 TYPE ekpo-loekz,
              p_loekz2 TYPE ekko-loekz.
  SELECT-OPTIONS: so_ebeln FOR ekpo-ebeln DEFAULT '4500106455' TO '4500106631' ,
                  so_ebelp FOR ekpo-ebelp,
                  so_bukrs FOR ekko-bukrs,
                  so_bstyp FOR ekko-bstyp,
                  so_bsart FOR ekko-bsart.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS: p_xloekz AS CHECKBOX USER-COMMAND a.
  PARAMETERS: rb_upl RADIOBUTTON GROUP g1 USER-COMMAND upd_screen,
              rb_alv RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF rb_upl = 'X' AND screen-name = 'P_XLOEKZ'.
      screen-active = 0.  "Elrejti a mezőt
    ELSEIF rb_alv = 'x' AND screen-name = 'P_XLOEKZ'.
      screen-active = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

INITIALIZATION.
  g_repid = sy-repid.

*------------------------------------------------START-OF-SELECTION----------------------------------------


START-OF-SELECTION.

  IF rb_upl = 'X'.
    PERFORM upload_zdev7ekkoekpo USING it_zdev7_ekkoekpo.
    IF sy-subrc = '0'.
      WRITE 'Feltöltés sikeres!'.
    ELSE.
      WRITE 'Hiba történt a feltöltés során!'.
    ENDIF.
  ELSEIF rb_alv = 'X'.
    PERFORM fetch_data_zdev7ekkoekpo USING gt_outtab
                                    CHANGING it_zdev7_ekkoekpo_origin.

    PERFORM  pbo_build_screen.
  ENDIF.

FORM set_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ALV_GUI'.
ENDFORM. "SET_STATUS

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
    WHERE     a~ebeln IN @so_ebeln
          AND b~ebelp IN @so_ebelp
          AND a~bukrs IN @so_bukrs
          AND a~bstyp IN @so_bstyp
          AND a~bsart IN @so_bsart.
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
    WHERE a~ebeln IN @so_ebeln
      AND b~ebelp IN @so_ebelp
      AND a~bukrs IN @so_bukrs
      AND a~bstyp IN @so_bstyp
      AND a~bsart IN @so_bsart.

  CLEAR backup_outtab.  " elmentjük az eredeti szelekciót
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
*      PERFORM netwr_kezdeti_allithatosag USING <fs_outtab>-xloekz
*                                      ls_outtab
*                                      CHANGING <fs_outtab>-celltab.
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
*      PERFORM netwr_kezdeti_allithatosag USING <fs_outtab>-xloekz
*                                      ls_outtab
*                                      CHANGING <fs_outtab>-celltab.
    ENDIF.
  ENDLOOP.
ENDFORM.

  CLASS lcl_event_handler IMPLEMENTATION.
    METHOD on_data_changed.
      " Végrehajtja a változások mentését az ALV-ba
      IF er_data_changed IS NOT INITIAL.
        go_grid->refresh_table_display( ).
      ENDIF.
    ENDMETHOD.
  ENDCLASS.

FORM pbo_build_screen.
  " Mezőkatalógus létrehozása
  PERFORM fieldcat_layout_init USING gt_fieldcat.

  DATA(lo_container) = NEW cl_gui_custom_container( container_name = 'CONTAINER_ALV' ).
  go_grid = NEW cl_gui_alv_grid( i_parent = lo_container ).

  go_grid->set_table_for_first_display(
    EXPORTING
      i_structure_name              =     'zdev7_ekkoekpo'
    CHANGING
      it_outtab                     =     gt_outtab
      it_fieldcatalog               =     gt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4
      ).


  " Enter és kattintás események regisztrálása
  go_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).

  " Eseménykezelő objektum létrehozása és regisztrálása
  CREATE OBJECT go_event_receiver.
  SET HANDLER go_event_receiver->on_data_changed FOR go_grid.
  CALL SCREEN 100.

  go_grid->set_ready_for_input( i_ready_for_input = 1 ).

ENDFORM.



FORM fieldcat_layout_init USING xt_fieldcat TYPE lvc_t_fcat.
  DATA: xs_fieldcat TYPE lvc_s_fcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_internal_tabname = 'GT_OUTTAB'
      i_structure_name   = 'Zdev7_013_EKKOEKPO'
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
*        xs_fieldcat-hotspot = 'X'.
        xs_fieldcat-edit = 'X'.
        xs_fieldcat-style = alv_style_font_bold.
        MODIFY xt_fieldcat FROM xs_fieldcat INDEX sy-tabix.
      WHEN 'XLOEKZ'.
        xs_fieldcat-edit = 'X'.
        xs_fieldcat-checkbox = 'X'.
        xs_fieldcat-outputlen = 1.
*        xs_fieldcat-hotspot = 'X'.
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

FORM sor_szinezes USING    xv_matnr TYPE matnr
                          xv_netwr TYPE netwr
                  CHANGING cv_color TYPE char4.
  IF xv_matnr IS NOT INITIAL AND xv_netwr = 0.
    ls_outtab-color = 'C710'. " PIROS
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

FORM pai_process_user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: ls_outtab       TYPE zdev7_013_ekkoekpo,
        lt_celltab      TYPE lvc_t_styl,
        ls_celltab      TYPE lvc_s_styl,
        alv_grid        TYPE REF TO cl_gui_alv_grid,
        x               TYPE char1 VALUE 'X',
        gv_refresh_done TYPE char1.

  "*    rs_selfield-refresh = 'X'.
  "  IF ref_grid IS INITIAL.
  "    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
  "      IMPORTING
  "        e_grid = ref_grid.
  "  ENDIF.

  "  IF NOT ref_grid IS INITIAL.
  "    CALL METHOD ref_grid->check_changed_data.
  "  ENDIF.

  CASE r_ucomm.
    WHEN 'ENTR'.
      IF rs_selfield-fieldname EQ 'XLOEKZ'.
        " Megkeressük az érintett sort
        READ TABLE gt_outtab INTO ls_outtab INDEX rs_selfield-tabindex.
        IF ls_outtab-xloekz = 'X'.
          ls_celltab-fieldname = 'NETWR'.
          ls_celltab-style = alv_style_disabled.
          APPEND ls_celltab TO lt_celltab.
        ELSEIF ls_outtab-xloekz = ''.
          ls_celltab-fieldname = 'NETWR'.
          ls_celltab-style = alv_style_enabled.
          APPEND ls_celltab TO lt_celltab.
        ENDIF.
        ls_outtab-celltab = lt_celltab.
        " Jelzőlámpa beállítása
        IF ls_outtab-xloekz = 'X'.
          ls_outtab-lamp_icon = '@0A@'.  " Piros lámpa
        ELSEIF ls_outtab-xloekz = ''.
          ls_outtab-lamp_icon = '@08@'.  " zöld lámpa
        ENDIF.
        MODIFY gt_outtab FROM ls_outtab INDEX rs_selfield-tabindex.
        rs_selfield-refresh = 'X'.
      ENDIF.

      IF rs_selfield-fieldname EQ 'NETWR'.
        " Megkeressük a módosított sort
        READ TABLE gt_outtab INTO ls_outtab INDEX rs_selfield-tabindex.
        IF sy-subrc = 0.
          " Az ALV-ban szerkesztett NETWR érték frissítése az adatbázisban
          UPDATE zdev7_ekkoekpo
            SET netwr = ls_outtab-netwr
          WHERE ebeln = ls_outtab-ebeln
            AND ebelp = ls_outtab-ebelp.

          IF sy-subrc = 0.
            COMMIT WORK.
          ELSE.
            MESSAGE 'A frissítés nem sikerült!' TYPE 'E'.
          ENDIF.

          " ALV belső tábla frissítése
          MODIFY gt_outtab FROM ls_outtab INDEX rs_selfield-tabindex.
          rs_selfield-refresh = 'X'.  " ALV újratöltés
        ENDIF.
      ENDIF.

    WHEN '&LV'.  " Pipás targonca ikon -> mentés a Zdev7_EKKOEKPO táblába
      DATA: lt_outtab  TYPE TABLE OF zdev7_ekkoekpo,  " Az ALV módosított adatai
            ls_outtab2 TYPE zdev7_ekkoekpo.
      " Frissített adatok lekérése az ALV frontendből
      "   IF alv_grid IS NOT INITIAL.
      "        CALL METHOD alv_grid->set_table_for_first_display
      "          EXPORTING
      "            i_structure_name = 'Zdev7_EKKOEKPO'
      "          CHANGING
      "           it_outtab        = gt_outtab.
      "         " ALV frissítése
      "         CALL METHOD alv_grid->check_changed_data
      "           CHANGING
      "             c_refresh = x.  " Az ALV frissítése a felhasználói pozíció megtartásával
      "ENDIF.
      " Módosítások visszaírása a belső táblába (gt_outtab)
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

      "    WHEN '&42'.  " Refresh ikon -> újratöltés
      "      "CLEAR gt_outtab.
      "      "     SELECT *
      "      "       INTO CORRESPONDING FIELDS OF TABLE @gt_outtab
      "      "       FROM zdev7_ekkoekpo.
      "      "     BREAK-POINT.
      "      LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<fs_outtab>).
      "        READ TABLE it_zdev7_ekkoekpo_origin ASSIGNING FIELD-SYMBOL(<fs_origin>)
      "        WITH KEY ebeln = <fs_outtab>-ebeln
      "               ebelp = <fs_outtab>-ebelp.
      "        IF sy-subrc = 0.
      "          <fs_outtab>-netwr = <fs_origin>-netwr.  " Csak a NETWR mezőt frissítjük
      "        ENDIF.
      "      ENDLOOP.
      "      IF gv_refresh_done = 'X'.
      "        MESSAGE 'Az adatok már vissza lettek állítva' TYPE 'I'.
      "        RETURN. " Ne csináljon semmit, ha már egyszer lefutott
      "      ENDIF.

      "      gv_refresh_done = 'X'.
      "      PERFORM display_alv.
      "      "       ALV újradeklarálása, hogy a belső tábla frissüljön
      "      "      IF alv_grid IS NOT INITIAL.
      "      "        CALL METHOD alv_grid->set_table_for_first_display
      "      "          EXPORTING
      "      "            i_structure_name = 'Zdev7_EKKOEKPO'
      "      "          CHANGING
      "      "            it_outtab        = gt_outtab.

      "      "          ALV frissítése
      "      "         CALL METHOD alv_grid->check_changed_data
      "      "           CHANGING
      "      "             c_refresh = x.  " Az ALV frissítése a felhasználói pozíció megtartásával
      "      "       ENDIF.   "Adatok frissítése az adatbázisból
      "      rs_selfield-refresh = 'X'.
      "      MESSAGE 'Adatok visszaállítva.' TYPE 'I'.

      "    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      "      LEAVE TO SCREEN 0.
  ENDCASE.
  rs_selfield-refresh = 'X'.
ENDFORM.

INCLUDE zaft_sela_status_0100o01.

INCLUDE zaft_sela_user_command_0100i01.