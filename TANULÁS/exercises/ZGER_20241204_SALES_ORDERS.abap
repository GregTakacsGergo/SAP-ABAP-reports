*&---------------------------------------------------------------------*
*& Report ZGER_20241204_SALES_ORDER
*&---------------------------------------------------------------------*
*&This report pulls sales orders combining data from vbak, and vbap tables.
*& VBAK = Sales Document Header Data , VBAP = Sales Document Item Data
*&---------------------------------------------------------------------*

REPORT zger_20241204_sales_orders.

TABLES: vbak, vbap.

SELECT-OPTIONS: s_vbeln FOR vbak-vbeln,
                s_matnr FOR vbap-matnr,
                s_erdat FOR vbak-erdat,
                s_ernam FOR vbak-ernam.



TYPES: BEGIN OF ty_order,
  vbeln type vbeln_va,
  ernam TYPE ernam,
  erdat TYPE erdat,
  posnr TYPE posnr,
  matnr TYPE matnr,
  kwmeng TYPE kwmeng,
  netwr TYPE netwr,
       END OF ty_order.

DATA: it_order TYPE TABLE OF ty_order,
      wa_order type ty_order.


  SELECT vbak~vbeln, vbak~ernam, vbak~erdat, vbap~posnr, vbap~matnr, vbap~kwmeng, vbap~netwr
      FROM vbak
      JOIN vbap  ON vbak~vbeln = vbap~vbeln
      INTO TABLE @it_order
      where vbak~vbeln in @s_vbeln
        and vbak~ernam in @s_matnr
        and vbak~erdat in @s_erdat
        and vbap~matnr in @s_ernam.

  FORMAT COLOR 2.
  WRITE:/ 'ORDER NUM' ,'CREATED BY', 'CREATED ON', 'ITEM', 'MATERIAL', 'QUANTITY', 'NET VALUE'
  COLOR OFF.


  LOOP AT it_order INTO wa_order.
    WRITE:/ wa_order-vbeln, wa_order-ernam, wa_order-erdat, wa_order-posnr, wa_order-matnr, wa_order-kwmeng, wa_order-netwr.
  ENDLOOP.