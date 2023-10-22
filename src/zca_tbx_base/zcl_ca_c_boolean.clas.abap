"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for boolean flags</p>
CLASS zcl_ca_c_boolean DEFINITION PUBLIC
                                  FINAL
                                  CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Boolean value: False</p>
      false TYPE abap_bool VALUE abap_false,
      "! <p class="shorttext synchronized" lang="en">Boolean value: True</p>
      true  TYPE abap_bool VALUE abap_true.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_boolean.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Is boolean value valid?</p>
      "!
      "! @parameter boolean       | <p class="shorttext synchronized" lang="en">Boolean value</p>
      "! @raising   zcx_ca_intern | <p class="shorttext synchronized" lang="en">Common exception: Internal exceptions</p>
      is_valid
        IMPORTING
          boolean TYPE abap_bool
        RAISING
          zcx_ca_intern.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_boolean.

ENDCLASS.



CLASS ZCL_CA_C_BOOLEAN IMPLEMENTATION.


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_boolean=>singleton_instance IS NOT BOUND.
      zcl_ca_c_boolean=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_boolean=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_valid.
    "-----------------------------------------------------------------*
    "   Is boolean value valid?
    "-----------------------------------------------------------------*
    "Check values
    IF boolean NE true  AND
       boolean NE false.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'BOOLEAN' ##no_text
          mv_msgv2 = CONV #( boolean ).
    ENDIF.
  ENDMETHOD.                    "is_valid
ENDCLASS.
