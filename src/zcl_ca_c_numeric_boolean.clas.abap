"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for numeric boolean values</p>
CLASS zcl_ca_c_numeric_boolean DEFINITION PUBLIC
                                          FINAL
                                          CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Numeric boolean value: False</p>
      false     TYPE dml_boolean         VALUE '0',
      "! <p class="shorttext synchronized" lang="en">Numeric boolean value: True</p>
      true      TYPE dml_boolean         VALUE '1',
      "! <p class="shorttext synchronized" lang="en">Numeric boolean value: Undefined</p>
      undefined TYPE dml_boolean         VALUE ' '.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_numeric_boolean.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Is numeric boolean value valid?</p>
      "!
      "! @parameter num_boolean | <p class="shorttext synchronized" lang="en">Numeric boolean value</p>
      is_valid
        IMPORTING
          num_boolean TYPE dml_boolean.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_numeric_boolean.

ENDCLASS.                     "zcl_ca_c_numeric_boolean  DEFINITION


CLASS zcl_ca_c_numeric_boolean IMPLEMENTATION.

  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_numeric_boolean=>singleton_instance IS NOT BOUND.
      zcl_ca_c_numeric_boolean=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_numeric_boolean=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_valid.
    "-----------------------------------------------------------------*
    "   Is numeric boolean value valid?
    "-----------------------------------------------------------------*
    "Check values
    IF num_boolean NE true  AND
       num_boolean NE false AND
       num_boolean NE undefined.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'NUM_BOOLEAN' ##no_text
          mv_msgv2 = CONV #( num_boolean ).
    ENDIF.
  ENDMETHOD.                    "is_valid

ENDCLASS.                     "zcl_ca_c_numeric_boolean  IMPLEMENTATION

