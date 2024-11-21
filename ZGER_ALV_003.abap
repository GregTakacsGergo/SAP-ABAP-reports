*&---------------------------------------------------------------------*
*& Report ZGER_ALV_003
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zger_alv_003.

DATA gt_mara TYPE TABLE OF mara.
DATA g_repid TYPE sy-repid.

G_REPID = SY-REPID.
SELECT * FROM mara INTO TABLE gt_mara UP TO 10 ROWS.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    I_CALLBACK_PROGRAM = G_REPID
    i_structure_name = 'MARA'
    I_SAVE = 'A'
  TABLES
    t_outtab         = gt_mara.