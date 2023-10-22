"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for select option tables</p>
CLASS zcl_ca_c_sel_options DEFINITION PUBLIC
                                      FINAL
                                      CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Comparison option</p>
      BEGIN OF option,
        "! <p class="shorttext synchronized" lang="en">Between</p>
        bt TYPE ddoption VALUE if_fsbp_const_range=>option_between,
        "! <p class="shorttext synchronized" lang="en">Contains pattern</p>
        cp TYPE ddoption VALUE if_fsbp_const_range=>option_contains_pattern,
        "! <p class="shorttext synchronized" lang="en">Equal</p>
        eq TYPE ddoption VALUE if_fsbp_const_range=>option_equal,
        "! <p class="shorttext synchronized" lang="en">Greater equal</p>
        ge TYPE ddoption VALUE if_fsbp_const_range=>option_greater_equal,
        "! <p class="shorttext synchronized" lang="en">Greater than</p>
        gt TYPE ddoption VALUE if_fsbp_const_range=>option_greater,
        "! <p class="shorttext synchronized" lang="en">Less equal</p>
        le TYPE ddoption VALUE if_fsbp_const_range=>option_less_equal,
        "! <p class="shorttext synchronized" lang="en">Less/lower then</p>
        lt TYPE ddoption VALUE if_fsbp_const_range=>option_less,
        "! <p class="shorttext synchronized" lang="en">Not between</p>
        nb TYPE ddoption VALUE if_fsbp_const_range=>option_not_between,
        "! <p class="shorttext synchronized" lang="en">Not equal</p>
        ne TYPE ddoption VALUE if_fsbp_const_range=>option_not_equal,
        "! <p class="shorttext synchronized" lang="en">Contains not pattern</p>
        np TYPE ddoption VALUE if_fsbp_const_range=>option_not_contains_pattern,
      END OF option,

      "! <p class="shorttext synchronized" lang="en">Including/excluding values</p>
      BEGIN OF sign,
        "! <p class="shorttext synchronized" lang="en">Excluding</p>
        excl TYPE ddsign VALUE if_fsbp_const_range=>sign_exclude,
        "! <p class="shorttext synchronized" lang="en">Including</p>
        incl TYPE ddsign VALUE if_fsbp_const_range=>sign_include,
      END OF sign.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_sel_options.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valid option for selection passed?</p>
      "!
      "! @parameter select_option | <p class="shorttext synchronized" lang="en">Option for selection</p>
      is_option_valid
        IMPORTING
          select_option TYPE ddoption,

      "! <p class="shorttext synchronized" lang="en">Valid sign for selection passed?</p>
      "!
      "! @parameter select_sign | <p class="shorttext synchronized" lang="en">Sign for selection</p>
      is_sign_valid
        IMPORTING
          select_sign TYPE ddsign.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_sel_options.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check value against fixed_values</p>
      "!
      "! @parameter value      | <p class="shorttext synchronized" lang="en">Value under test</p>
      "! @parameter param_name | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      check_against_fixed_values
        IMPORTING
          value      TYPE simple
          param_name TYPE csequence.

ENDCLASS.



CLASS ZCL_CA_C_SEL_OPTIONS IMPLEMENTATION.


  METHOD check_against_fixed_values.
    "-----------------------------------------------------------------*
    "   Check value against fixed_values
    "-----------------------------------------------------------------*
    TRY.
        NEW zcl_ca_ddic( iv_data       = value
                         iv_param_name = param_name )->check_fixed_values( iv_value       = value
                                                                           iv_raise_excep = abap_true ).

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_intern( zcx_ca_intern=>create_exception(
                                                               iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                               iv_class    = 'ZCL_CA_DDIC'
                                                               iv_method   = 'CHECK_FIXED_VALUES'
                                                               ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "check_against_fixed_values


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_sel_options=>singleton_instance IS NOT BOUND.
      zcl_ca_c_sel_options=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_sel_options=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_option_valid.
    "-----------------------------------------------------------------*
    "   Valid option for selection passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = select_option
                                param_name = 'SELECT_OPTION' ) ##no_text.
  ENDMETHOD.                    "is_location_valid


  METHOD is_sign_valid.
    "-----------------------------------------------------------------*
    "   Valid sign for selection passed?
    "-----------------------------------------------------------------*
    check_against_fixed_values( value      = select_sign
                                param_name = 'SELECT_SIGN' ) ##no_text.
  ENDMETHOD.                    "is_path_type_valid
ENDCLASS.
