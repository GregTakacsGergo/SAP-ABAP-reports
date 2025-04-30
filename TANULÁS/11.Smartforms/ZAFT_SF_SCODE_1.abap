*----------------------------------------------------------------------*
*       Report SF_EXAMPLE_01
*----------------------------------------------------------------------*
*       Printing of documents using Smart Forms
*----------------------------------------------------------------------*
REPORT ZAFT_SF_SCODE_1.

TABLES: vbrk, vbrp.
data: w_vbrk TYPE vbrk,
      t_vbrp TYPE STANDARD TABLE OF vbrp,
      fm_name type rs38l_fnam.

parameters:      p_vbeln TYPE vbrk-vbeln.
parameters:      p_form   type tdsfname   default 'ZAFT_SF_PR_1'.

SELECT SINGLE * FROM vbrk INTO w_vbrk WHERE vbeln EQ p_vbeln. "számla fej

SELECT * FROM vbrp INTO TABLE t_vbrp WHERE vbeln eq p_vbeln. "számla tétel

* print data
  call function 'SSF_FUNCTION_MODULE_NAME'
       exporting  formname           = p_form
*                 variant            = ' '
*                 direct_call        = ' '
       importing  fm_name            = fm_name
       exceptions no_form            = 1
                  no_function_module = 2
                  others             = 3.

  if sy-subrc <> 0.
*   error handling
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
  endif.

* now call the generated function module
  call function fm_name
       exporting
         i_vbrk = w_vbrk
         TABLES
         i_vbrp = t_vbrp
**                 archive_index        =
**                 archive_parameters   =
**                 control_parameters   =
**                 mail_appl_obj        =
**                 mail_recipient       =
**                 mail_sender          =
**                 output_options       =
**                 user_settings        = 'X'
*                  customer             = customer
*                  bookings             = bookings
*                  connections          = connections
*      importing  document_output_info =
*                 job_output_info      =
*                 job_output_options   =
       exceptions formatting_error     = 1
                  internal_error       = 2
                  send_error           = 3
                  user_canceled        = 4
                  others               = 5.

  if sy-subrc <> 0.
*   error handling
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.