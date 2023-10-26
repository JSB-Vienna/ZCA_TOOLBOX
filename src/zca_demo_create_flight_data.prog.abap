*&---------------------------------------------------------------------*
*& Report zca_demo_create_flight_data
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zca_demo_create_flight_data.


INCLUDE zca_demo_selscr_flight_model ##incl_ok.


*---------------------------------------------------------------------*
*     i n i t i a l i z a t i o n
*---------------------------------------------------------------------*
INITIALIZATION.
  DATA(flight_model) = NEW flight_model( ).


*---------------------------------------------------------------------*
*     a t   s e l e c t i o n - s c r e e n   OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  flight_model->at_sel_screen_output( ).


*---------------------------------------------------------------------*
*     a t   s e l e c t i o n - s c r e e n
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  flight_model->at_sel_screen( ).


*---------------------------------------------------------------------*
*     s t a r t - o f - s e l e c t i o n
*---------------------------------------------------------------------*
START-OF-SELECTION.
  TRY.
      flight_model->create_data( ).

    CATCH zcx_ca_error INTO DATA(lx_catched).
      MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
  ENDTRY.
