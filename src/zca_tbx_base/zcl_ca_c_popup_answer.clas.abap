"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for answers after popup usage</p>
CLASS zcl_ca_c_popup_answer DEFINITION PUBLIC
                                      FINAL
                                      CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Answer to popup: Yes</p>
      yes    TYPE char1             VALUE '1'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Answer to popup: No</p>
      no     TYPE char1             VALUE '2'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Answer to popup: Cancel</p>
      cancel TYPE char1             VALUE 'A'  ##no_text.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_popup_answer.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Is it a valid answer after popup call?</p>
      "!
      "! @parameter popup_answer  | <p class="shorttext synchronized" lang="en">Popup answer</p>
      "! @raising   zcx_ca_intern | <p class="shorttext synchronized" lang="en">Common exception: Internal exceptions</p>
      is_popup_answer_valid
        IMPORTING
          popup_answer TYPE char1
        RAISING
          zcx_ca_intern.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_popup_answer.

ENDCLASS.



CLASS ZCL_CA_C_POPUP_ANSWER IMPLEMENTATION.


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_popup_answer=>singleton_instance IS NOT BOUND.
      zcl_ca_c_popup_answer=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_popup_answer=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_popup_answer_valid.
    "-----------------------------------------------------------------*
    "   Is it a valid answer after popup call?
    "-----------------------------------------------------------------*
    "Check values
    IF popup_answer CN '12A' ##no_text.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_POPUP_ANSWER' ##no_text
          mv_msgv2 = CONV #( popup_answer ).
    ENDIF.
  ENDMETHOD.                    "is_popup_answer_valid
ENDCLASS.
