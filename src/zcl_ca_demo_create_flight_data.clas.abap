"! <p class="shorttext synchronized" lang="en">CA-TBX: (Re-)Create flight model data for demo programs</p>
CLASS zcl_ca_demo_create_flight_data DEFINITION PUBLIC
                                          FINAL
                                          CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Create flight model data</p>
      "!
      "! @parameter iv_recreate | <p class="shorttext synchronized" lang="en">X = Delete current data and create new data</p>
      create_data
        IMPORTING
          iv_recreate TYPE abap_bool DEFAULT abap_false.

ENDCLASS.



CLASS ZCL_CA_DEMO_CREATE_FLIGHT_DATA IMPLEMENTATION.


  METHOD create_data.
    IF iv_recreate EQ abap_true.
      "SUBMIT statement doesn't like constants of type pools in
      "lower versions - so replaced by literals
      SUBMIT sapbc_data_generator                        "#EC CI_SUBMIT
             WITH minimal  EQ ' '
             WITH pa_del   EQ 'X'
             WITH sbcancel EQ ' '
             WITH standard EQ ' ' AND RETURN.
    ENDIF.

    "Does any data exist
    SELECT COUNT(*) FROM spfli.                          "#EC CI_BYPASS
    IF sy-subrc NE 0.
      SUBMIT sapbc_data_generator                        "#EC CI_SUBMIT
               WITH sbcancel EQ 'X'
               WITH standard EQ 'X' AND RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
