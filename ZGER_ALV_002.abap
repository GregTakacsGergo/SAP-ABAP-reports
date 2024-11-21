*&---------------------------------------------------------------------*
*& Report ZGER_ALV_002
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGER_ALV_002.

DATA gt_mara TYPE TABLE OF MARA.
DATA LV_STRUCTURE_NAME LIKE  DD02L-TABNAME.

    SELECT * FROM MARA INTO TABLE gt_mara UP TO 10 ROWS.

    LV_STRUCTURE_NAME = 'MARA'.
    
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
            I_STRUCTURE_NAME = 'MARA'
        TABLES 
            T_OUTTAB = gt_mara.