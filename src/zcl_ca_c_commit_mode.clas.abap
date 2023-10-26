"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for commit mode</p>
CLASS zcl_ca_c_commit_mode DEFINITION PUBLIC
                                      FINAL
                                      CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">By caller / no COMMIT WORK</p>
      by_caller TYPE zca_d_commit_mode VALUE '0' ##no_text,
      "! <p class="shorttext synchronized" lang="en">COMMIT WORK (asynchronous)</p>
      asynchron TYPE zca_d_commit_mode VALUE '1' ##no_text,
      "! <p class="shorttext synchronized" lang="en">COMMIT WORK AND WAIT (synchronous)</p>
      synchron  TYPE zca_d_commit_mode VALUE '2' ##no_text.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_commit_mode.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Is it a valid commmit mode?</p>
      "!
      "! @parameter commit_mode   | <p class="shorttext synchronized" lang="en">Commit mode</p>
      "! @raising   zcx_ca_intern | <p class="shorttext synchronized" lang="en">Common exception: Internal exceptions</p>
      is_valid
        IMPORTING
          commit_mode TYPE zca_d_commit_mode
        RAISING
          zcx_ca_intern.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_commit_mode.

ENDCLASS.



CLASS ZCL_CA_C_COMMIT_MODE IMPLEMENTATION.


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_commit_mode=>singleton_instance IS NOT BOUND.
      zcl_ca_c_commit_mode=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_commit_mode=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_valid.
    "-----------------------------------------------------------------*
    "   Is it a valid commit mode?
    "-----------------------------------------------------------------*
    "Check values
    IF commit_mode CN '012'.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'COMMIT_MODE' ##no_text
          mv_msgv2 = CONV #( commit_mode ).
    ENDIF.
  ENDMETHOD.                    "is_valid
ENDCLASS.
