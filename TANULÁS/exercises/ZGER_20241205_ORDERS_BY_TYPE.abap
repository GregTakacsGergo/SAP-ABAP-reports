*&-------------------------------------------------------------------------
*& This report pulls sales orders combining data from vbak, and vbap tables.
*& VBAK = Sales Document Header Data , VBAP = Sales Document Item Data
*&-------------------------------------------------------------------------

Report ZGER_20241204_SALES_ORDERS.

TABLES: vbak, vbap.

SELECT-OPTIONS: s_vbeln FOR vbak-vbeln.
                s_matnr FOR vbap-matnr.
                s_erdat FOR vbak-erdat.
                s_ernam FOR vbak-ernam.

PARAMETERS: p_quote as CHECKBOX.

TYPES: BEGIN OF ty_order.
        vbeln TYPE vbeln_va,
        ernam TYPE ernam,
        erdat TYPE erdat,
        posnr TYPE posnr,
        matnr TYPE matnr,
        kwmeng TYPE kwmeng,
        netwr TYPE netwr,
      end OF ty_order.

DATA: it_order TYPE TABLE OF ty_order.      
      wa_order TYPE ty_order.
 
START-OF-SELECTION.

SELECT vbak~vbeln, vbak~ernam, vbak~erdat, vbap~posnr, vbap~matnr, vbap~kwmeng, vbap~netwr 
    INTO TABLE it_order FROM vbak AS vbak JOIN vbap AS vbap ON vbak-vbeln = vbap-vbeln.
    WHERE vbak~vbeln IN s_vbeln
      AND vbap~matnr IN s_matnr
      AND vbak~erdat IN s_erdat
      AND vbak~ernam IN s_ernam.

FORMAT COLOR 2.
    write:/ 'ORDER NUM' ,'CREATED BY', 'CREATED ON', 'ITEM', 'MATERIAL', 'QUANTITY', 'NET VALUE'
FORMAT COLOR OFF.


LOOP AT it_order INTO wa_order.
    WRITE:/ wa_order-vbeln, wa_order-ernam, wa_order-erdat, wa_order-posnr, wa_order-matnr, wa_order-kwmeng, wa_order-netwr.
endloop.
