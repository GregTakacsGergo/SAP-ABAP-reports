*&---------------------------------------------------------------------*
*& Report ZGER_ALV_001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGER_ALV_001.

DATA gt_mara TYPE TABLE OF MARA.

    SELECT * FROM MARA INTO TABLE gt_mara.

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
        EXPORTING
            I_STRUCTURE_NAME = 'MARA'
        TABLES 
            T_OUTTAB = gt_mara.