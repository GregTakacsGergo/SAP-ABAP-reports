*&---------------------------------------------------------------------*
*& Report ZGER_ALV_009_ver2
*& ZGER_ALV_009 report selection using group by seems to be not working 
*&---------------------------------------------------------------------*
REPORT zger_alv_009.

TYPE-POOLS: slis.

TABLES: mara, mard, makt.
 
TYPES: ud_struct TYPE ZGER_ALV_009_UD_STRUCT.
       ud_struct2 TYPE ZGER_ALV_009_UD_STRUCT2.

DATA: ud_struct_1 TYPE

