*&---------------------------------------------------------------------*
"ZGER_ALV_008 that retrieves and displays data from the MARD table in an ALV (ABAP List Viewer) format, 
"allowing users to filter by material number, plant, and storage location. 
"It includes type declarations for storing material data, a selection screen for user input, 
"and subroutines for fetching data, building the field catalog, and displaying the ALV output. 
"The code uses standard SAP function modules to manage data retrieval and presentation in a structured and 
"user-friendly manner.
*&---------------------------------------------------------------------*
*& Report ZGER_ALV_008
*&---------------------------------------------------------------------*
*& A MARD tábla tartalmának listázása ALV-ban, szelekciós opciókkal.
*&---------------------------------------------------------------------*
REPORT zger_alv_008.

TYPE-POOLS: slis.
TABLES: mard.

" Strukturális típus a megjelenítéshez
TYPES: BEGIN OF ty_mard,
         matnr TYPE matnr,
         werks TYPE werks_d,
         lgort TYPE mard-lgort,
         labst TYPE mard-labst, " Szabad készlet példaként
       END OF ty_mard.

" Belső táblák és változók
DATA: gt_mard     TYPE TABLE OF ty_mard,    " Adatok a MARD-ból
      gs_mard     TYPE ty_mard,            " Egy rekord a belső táblában
      gt_fieldcat TYPE slis_t_fieldcat_alv, " Mezőkatalógus az ALV-hoz
      g_repid     TYPE sy-repid.            " Program azonosító

" Szelekciós opciók
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_matnr FOR mard-matnr,
                so_werks FOR mard-werks,
                so_lgort FOR mard-lgort.
SELECTION-SCREEN END OF BLOCK sel.

" Inicializáció

INITIALIZATION.
  g_repid = sy-repid.

  " Adatok kiválasztása
 
START-OF-SELECTION.
  PERFORM fetch_data.

  " ALV megjelenítés

END-OF-SELECTION.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       Adatok kiválasztása a MARD táblából
*----------------------------------------------------------------------*
FORM fetch_data.
  SELECT matnr werks lgort labst
    INTO TABLE gt_mard
    FROM mard
  WHERE matnr IN so_matnr
  AND werks IN so_werks
  AND lgort IN so_lgort.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       ALV lista megjelenítése
*----------------------------------------------------------------------*
FORM display_alv.
  " Mezőkatalógus létrehozása
  PERFORM build_fieldcat.

  " ALV megjelenítés
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = g_repid
      it_fieldcat             = gt_fieldcat[]
      i_save                  = 'A'
*      i_callback_user_command = 'USER_COMMAND' "g_user_command
    TABLES
      t_outtab                = gt_mard.
ENDFORM.
FORM user_command.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       Mezőkatalógus építése az ALV-hez
*----------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  FIELD-SYMBOLS:<lvc> TYPE slis_fieldcat_alv.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
*     I_PROGRAM_NAME   =
*     I_INTERNAL_TABNAME           =
      i_structure_name = 'ZGER_ALV'
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_INCLNAME       =
*     I_BYPASSING_BUFFER           =
*     I_BUFFER_ACTIVE  =
    CHANGING
      ct_fieldcat      = gt_fieldcat
* EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR    = 2
*     OTHERS           = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  LOOP AT gt_fieldcat ASSIGNING <lvc>.
    IF <lvc>-fieldname EQ 'MATNR'.
      <lvc>-key = 'X'.
    ENDIF.
  ENDLOOP.
*  CLEAR ls_fieldcat.
*  ls_fieldcat-fieldname = 'MATNR'.
*  ls_fieldcat-seltext_m = 'Cikkszám'.
*  ls_fieldcat-outputlen = 18.
**  ls_fieldcat-convexit = 'MATN1'.
*  APPEND ls_fieldcat TO gt_fieldcat.
*
*  CLEAR ls_fieldcat.
*  ls_fieldcat-fieldname = 'WERKS'.
*  ls_fieldcat-seltext_m = 'Gyár'.
*  ls_fieldcat-outputlen = 8.
*  APPEND ls_fieldcat TO gt_fieldcat.
*
*  CLEAR ls_fieldcat.
*  ls_fieldcat-fieldname = 'LGORT'.
*  ls_fieldcat-seltext_m = 'Raktárhely'.
*  ls_fieldcat-outputlen = 8.
*  APPEND ls_fieldcat TO gt_fieldcat.
*
*  CLEAR ls_fieldcat.
*  ls_fieldcat-fieldname = 'LABST'.
*  ls_fieldcat-seltext_m = 'Szabad készlet'.
*  ls_fieldcat-outputlen = 10.
*  APPEND ls_fieldcat TO gt_fieldcat.
ENDFORM.