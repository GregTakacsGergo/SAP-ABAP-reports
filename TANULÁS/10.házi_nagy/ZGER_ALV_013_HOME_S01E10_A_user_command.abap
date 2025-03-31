FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: ls_outtab       TYPE zger_013_ekkoekpo,
        lt_celltab      TYPE lvc_t_styl,
        ls_celltab      TYPE lvc_s_styl.

  CASE r_ucomm.
    WHEN '&IC1'.
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
          UPDATE zger_ekkoekpo
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

    WHEN '&LV'.  " Pipás targonca ikon -> mentés a ZGER_EKKOEKPO táblába
      DATA: lt_outtab  TYPE TABLE OF zger_ekkoekpo,  " Az ALV módosított adatai
            ls_outtab2 TYPE zger_ekkoekpo.
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
        UPDATE zger_ekkoekpo SET netwr = @ls_outtab-netwr
        WHERE ebeln = @ls_outtab-ebeln
        AND   ebelp = @ls_outtab-ebelp.
      ENDLOOP.
      COMMIT WORK.
      MESSAGE 'Módosítások mentve.' TYPE 'I'.
      rs_selfield-refresh = 'X'.

"    WHEN '&42'.  " Refresh ikon -> újratöltés
"      "CLEAR gt_outtab.
"      "     SELECT *
"      "       INTO CORRESPONDING FIELDS OF TABLE @gt_outtab
"      "       FROM zger_ekkoekpo.
"      "     BREAK-POINT.
"      LOOP AT gt_outtab ASSIGNING FIELD-SYMBOL(<fs_outtab>).
"        READ TABLE it_zger_ekkoekpo_origin ASSIGNING FIELD-SYMBOL(<fs_origin>)
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
"      "            i_structure_name = 'ZGER_EKKOEKPO'
"      "          CHANGING
"      "            it_outtab        = gt_outtab.

"      "          ALV frissítése
"      "         CALL METHOD alv_grid->check_changed_data
"      "           CHANGING
"      "             c_refresh = x.  " Az ALV frissítése a felhasználói pozíció megtartásával
"      "       ENDIF.   "Adatok frissítése az adatbázisból
"      rs_selfield-refresh = 'X'.
"      MESSAGE 'Adatok visszaállítva.' TYPE 'I'.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  rs_selfield-refresh = 'X'.
ENDFORM.