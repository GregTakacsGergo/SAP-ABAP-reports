*&---------------------------------------------------------------------*
*& Report ZGER_ALV_009
*&---------------------------------------------------------------------*
REPORT zger_alv_009.

TYPE-POOLS: slis.

TABLES: mara, mard, makt.

* structures for the report
TYPES:
    BEGIN OF ty_main,
        matnr TYPE mara-matnr,
        brgew type mara-brgew,
        gewei type mara-gewei,
        maktx type makt-maktx,
        total_stock type mara-meins,
    END OF ty_main.
* separate stucture for the pop-up

    BEGIN OF ty_popup,
    END OF ty_popup.

* internal tables and variables
DATA: gt_main TYPE TABLE OF ty_main,
      gt_popup TYPE TABLE OF ty_popup.
      gs_fieldcat TYPE SLIS_T_FIELDCAT_ALV,
      g_repid     TYPE sy-repid VALUE sy-repid.

* selection screen fields 
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-Options so_matnr FOR mara-matnr.
    PARAMETERS: p_spras TYPE makt-spras DEFAULT 'EN' obligatory.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
    PERFORM prepare_fieldcat CHANGING gs_fieldcat.

* data fetching
START-OF-SELECTION.
    PERFORM fetch_data 

END-OF-SELECTION.

PERFORM display_ALV.

*&---------------------------------------------------------------------*
*&      Form  fetch_data
*&---------------------------------------------------------------------*
FORM fetch_data.
    lt_mard TYPE TABLE OF mard.
    wa_main TYPE ty_main.
    SELECT mara~matnr mara~brgew mara~gewei makt~maktx SUM(mard~labst + mard~insme + mard~speme) AS total_stock
    INTO CORRESPONDING FIELDS OF TABLE gt_main
    FROM mara
    LEFT JOIN mard ON mara~matnr = mard~matnr
    LEFT JOIN makt ON mara~matnr = makt~matnr
    WHERE mara~spras = p_spras
    GROUP BY mara~matnr, mara~brgew, mara~gewei, makt~maktx.



      

