*&---------------------------------------------------------------------*
*& Report ZGER_ALV_009
*&---------------------------------------------------------------------*
REPORT zger_alv_009.

TYPE-POOLS: slis.

TABLES: mara, mard, makt.

* structures for the report
TYPES:
  BEGIN OF ty_main,
    matnr       TYPE mara-matnr,
    brgew       TYPE mara-brgew,
    gewei       TYPE mara-gewei,
    maktx       TYPE makt-maktx,
    total_stock TYPE mara-meins,
  END OF ty_main.
* separate stucture for the pop-up

*    BEGIN OF ty_popup,
*    END OF ty_popup.

* internal tables and variables
DATA: gt_main     TYPE TABLE OF ty_main,
*      gt_popup TYPE TABLE OF ty_popup.

      gt_fieldcat TYPE slis_t_fieldcat_alv,
      g_repid     TYPE sy-repid VALUE sy-repid.

* selection screen fields
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS so_matnr FOR mara-matnr.
PARAMETERS: p_spras TYPE makt-spras DEFAULT 'EN' OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM build_fieldcat.

* data fetching
START-OF-SELECTION.
  PERFORM fetch_data.

END-OF-SELECTION.

  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  fetch_data
*&---------------------------------------------------------------------*
FORM fetch_data.
  DATA lt_mard TYPE TABLE OF mard.
  DATA wa_main TYPE ty_main.
  SELECT mara~matnr, mara~brgew, mara~gewei, makt~maktx,
         mard~labst + mard~insme + mard~speme AS total_stock
  INTO CORRESPONDING FIELDS OF @gt_main
  FROM mara
  LEFT JOIN mard ON mara~matnr = mard~matnr
  LEFT JOIN makt ON mara~matnr = makt~matnr
  WHERE mara~matnr IN @so_matnr AND makt~spras = @p_spras
  GROUP BY mara~matnr, mara~brgew, mara~gewei, makt~maktx.
  ENDSELECT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
FORM display_alv.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      it_fieldcat        = gt_fieldcat[]
      i_save             = 'A'
    TABLES
      t_outtab           = gt_main.
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
      i_structure_name = 'ZGER_STRUCT_KESZLET'
*     I_CLIENT_NEVER_DISPLAY       = 'X'
*     I_INCLNAME       =
*     I_BYPASSING_BUFFER           =
*     I_BUFFER_ACTIVE  =
    CHANGING
      ct_fieldcat      = gt_fieldcat.
* EXCEPTIONS
*     INCONSISTENT_INTERFACE       = 1
*     PROGRAM_ERROR    = 2
*     OTHERS           = 3

  LOOP AT gt_fieldcat ASSIGNING <lvc>.
    IF <lvc>-fieldname EQ 'MATNR'.
      <lvc>-key = 'X'.
    ENDIF.
  ENDLOOP.
ENDFORM.