"! <p class="shorttext synchronized" lang="en">CA-TBX: Data conversion</p>
CLASS zcl_ca_conv DEFINITION PUBLIC
                             FINAL
                             CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   c o n s t a n t s
    CONSTANTS:
      BEGIN OF conversion_exit_pattern,
        "! <p class="shorttext synchronized" lang="en">Pattern for dynamic execution of conversion exit for input</p>
        for_input  TYPE rs38l_fnam VALUE 'CONVERSION_EXIT_&_INPUT' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Pattern for dynamic execution of conversion exit for output</p>
        for_output TYPE rs38l_fnam VALUE 'CONVERSION_EXIT_&_OUTPUT' ##no_text,
      END   OF conversion_exit_pattern,

      BEGIN OF conv_exit_name,
        "! <p class="shorttext synchronized" lang="en">Conversion exit name: ALPHA</p>
        alpha TYPE convexit VALUE 'ALPHA' ##no_text,
      END   OF conv_exit_name,

      "! <p class="shorttext synchronized" lang="en">Decimal notation: Blank=4.567,89 / X=4,567.89 / Y=4 567,89</p>
      BEGIN OF decimal_notation,
        in_europe  TYPE char1 VALUE ' ' ##no_text,
        in_english TYPE char1 VALUE 'X' ##no_text,
        others     TYPE char1 VALUE 'Y' ##no_text,
      END   OF decimal_notation.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Convert BAPI amount into SAP internal value</p>
      "!
      "! @parameter bapi_amount     | <p class="shorttext synchronized" lang="en">Amount in BAPI format</p>
      "! @parameter currency        | <p class="shorttext synchronized" lang="en">Currency key</p>
      "! @parameter amount_internal | <p class="shorttext synchronized" lang="en">Converted amount in SAP internal format</p>
      "! @raising   zcx_ca_conv     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Conversion failed</p>
      bapi_amount_2_internal
        IMPORTING
          bapi_amount     TYPE bapicurr_d
          currency        TYPE waers OPTIONAL
        EXPORTING
          amount_internal TYPE p
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Convert 1 / 0 into X / space</p>
      "!
      "! @parameter numeric_boolean | <p class="shorttext synchronized" lang="en">Boolean value =&gt; 1 or 0</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">Flag =&gt; X or space</p>
      boolean_2_flag
        IMPORTING
          numeric_boolean TYPE dml_boolean
        RETURNING
          VALUE(result)   TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">CLASS_CONSTRUCTOR</p>
      class_constructor,

      "! <p class="shorttext synchronized" lang="en">Execute conversion exit for input or output</p>
      "!
      "! @parameter is_for_output        | <p class="shorttext synchronized" lang="en">X = Convert for output; 0 = for input</p>
      "! @parameter conversion_exit_name | <p class="shorttext synchronized" lang="en">Conversion exit name</p>
      "! @parameter element_descr        | <p class="shorttext synchronized" lang="en">Element type description of target field</p>
      "! @parameter input_value          | <p class="shorttext synchronized" lang="en">Passed value of elementary type</p>
      "! @parameter converted_value      | <p class="shorttext synchronized" lang="en">Converted value of elementary type</p>
      "! @raising   zcx_ca_conv          | <p class="shorttext synchronized" lang="en">CA-TBX exception: Conversion failed</p>
      convert_via_conversion_exit
        IMPORTING
          is_for_output          TYPE abap_bool                DEFAULT abap_true
          conversion_exit_name   TYPE convexit                 DEFAULT conv_exit_name-alpha
          element_descr          TYPE REF TO cl_abap_elemdescr OPTIONAL
          input_value            TYPE simple
        EXPORTING
          VALUE(converted_value) TYPE simple
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Konvertiere eines externen Wertes in SAP internes format</p>
      "!
      "! @parameter external_value  | <p class="shorttext synchronized" lang="en">External value in character format</p>
      "! @parameter currency        | <p class="shorttext synchronized" lang="en">Currency key</p>
      "! @parameter unit_of_measure | <p class="shorttext synchronized" lang="en">Unit of measure</p>
      "! @parameter internal_value  | <p class="shorttext synchronized" lang="en">Value in SAP internal format depending on target field</p>
      "! @raising   zcx_ca_conv     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Conversion failed</p>
      external_2_internal
        IMPORTING
          external_value  TYPE csequence
          currency        TYPE waers     OPTIONAL
          unit_of_measure TYPE meins     OPTIONAL
        EXPORTING
          internal_value  TYPE data
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Convert X / space into 1 / 0</p>
      "!
      "! @parameter flag   | <p class="shorttext synchronized" lang="en">Flag =&gt; X or space</p>
      "! @parameter result | <p class="shorttext synchronized" lang="en">Boolean value =&gt; 1 or 0</p>
      flag_2_boolean
        IMPORTING
          flag          TYPE abap_bool
        RETURNING
          VALUE(result) TYPE dml_boolean,

      "! <p class="shorttext synchronized" lang="en">Conversion of a SAP internal value into external format</p>
      "!
      "! @parameter internal_value        | <p class="shorttext synchronized" lang="en">SAP internal value of elementary type</p>
      "! @parameter date_format           | <p class="shorttext synchronized" lang="en">Date format -&gt; see domain fixed values</p>
      "! @parameter currency              | <p class="shorttext synchronized" lang="en">Currency key</p>
      "! @parameter unit_of_measure       | <p class="shorttext synchronized" lang="en">Unit of measure</p>
      "! @parameter without_seconds       | <p class="shorttext synchronized" lang="en">X = Return a time without seconds (hh:mm)</p>
      "! @parameter result_is_for_idoc    | <p class="shorttext synchronized" lang="en">X = Receiving field is an IDoc field (has own conv. rules)</p>
      "! @parameter return_in_bapi_format | <p class="shorttext synchronized" lang="en">X = Return amount as BAPI amount (has diff nbr of decimals)</p>
      "! @parameter external_value        | <p class="shorttext synchronized" lang="en">Converted value in external format, e. g. for BSP,BDC,IDoc</p>
      "! @raising   zcx_ca_conv           | <p class="shorttext synchronized" lang="en">CA-TBX exception: Conversion failed</p>
      internal_2_external
        IMPORTING
          internal_value        TYPE data
          date_format           TYPE xudatfm    OPTIONAL
          currency              TYPE waers      OPTIONAL
          unit_of_measure       TYPE meins      OPTIONAL
          without_seconds       TYPE abap_bool  DEFAULT abap_false
          result_is_for_idoc    TYPE abap_bool  DEFAULT abap_false
          return_in_bapi_format TYPE abap_bool  DEFAULT abap_false
        EXPORTING
          external_value        TYPE csequence
        RAISING
          zcx_ca_conv,

      "! <p class="shorttext synchronized" lang="en">Convert SAP internal amount into external value</p>
      "!
      "! @parameter internal_amount | <p class="shorttext synchronized" lang="en">SAP internal amount</p>
      "! @parameter currency        | <p class="shorttext synchronized" lang="en">Currency key</p>
      "! @parameter external_amount | <p class="shorttext synchronized" lang="en">Converted amount in external format</p>
      "! @raising   zcx_ca_conv     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Conversion failed</p>
      internal_amount_2_external
        IMPORTING
          internal_amount TYPE data
          currency        TYPE waers OPTIONAL
        EXPORTING
          external_amount TYPE csequence
        RAISING
          zcx_ca_conv.


* P R I V A T E   S E C T I O N
protected section.
  PRIVATE SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for boolean flags</p>
      boolean               TYPE REF TO zcl_ca_c_boolean,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for numeric boolean values</p>
      num_boolean           TYPE REF TO zcl_ca_c_numeric_boolean,

      "! <p class="shorttext synchronized" lang="en">Default settings of user</p>
      user_default_settings TYPE usdefaults.

ENDCLASS.



CLASS ZCL_CA_CONV IMPLEMENTATION.


  METHOD bapi_amount_2_internal.
    "-----------------------------------------------------------------*
    "   Conversion of a BAPI amount value into an internal (packed) value
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      returned_message     TYPE bapireturn.

    "Check if the passed value is an elementary type
    DATA(element_descr) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_data( amount_internal ) ).

    "Convert into an internal value AND ...
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
      EXPORTING
        currency             = currency
        amount_external      = bapi_amount
        max_number_of_digits = element_descr->output_length
      IMPORTING
        amount_internal      = amount_internal
        return               = returned_message.
    DATA(exception) = CAST zcx_ca_conv( zcx_ca_error=>create_exception(
                                                iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                iv_function = 'BAPI_CURRENCY_CONV_TO_INTERNAL'  ##no_text
                                                is_return   = VALUE #( type       = returned_message-type
                                                                       id         = returned_message-code(2)
                                                                       number     = returned_message-code+2(3)
                                                                       message_v1 = returned_message-message_v1
                                                                       message_v2 = returned_message-message_v2
                                                                       message_v3 = returned_message-message_v3
                                                                       message_v4 = returned_message-message_v4 ) ) ).
    IF exception IS BOUND.
      RAISE EXCEPTION exception.
    ENDIF.
  ENDMETHOD.                    "bapi_amount_2_internal


  METHOD boolean_2_flag.
    "-----------------------------------------------------------------*
    "   Conversion of a boolean value (0/1) into a flag (blank/X)
    "-----------------------------------------------------------------*
    result = abap_false.
    IF numeric_boolean EQ num_boolean->true.
      result = abap_true.
    ENDIF.
  ENDMETHOD.                    "boolean_2_flag


  METHOD class_constructor.
    "-----------------------------------------------------------------*
    "   Class constructor
    "-----------------------------------------------------------------*
    boolean     = zcl_ca_c_boolean=>get_instance( ).
    num_boolean = zcl_ca_c_numeric_boolean=>get_instance( ).

    "Get current user and its defaults of user master
    DATA(_active_sap_user_id) = cl_abap_syst=>get_user_name( ).
    CALL FUNCTION 'SUSR_USER_DEFAULTS_GET'
      EXPORTING
        user_name     = _active_sap_user_id
      IMPORTING
        user_defaults = user_default_settings.
  ENDMETHOD.                    "class_constructor


  METHOD convert_via_conversion_exit.
    "-----------------------------------------------------------------*
    "   Dynamic execution of standard conversion exits
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _exception               TYPE REF TO cx_root,
      _conversion_exception    TYPE REF TO zcx_ca_conv,
      _element_descr           TYPE REF TO cl_abap_elemdescr,
      _fm_name_conversion_exit TYPE rs38l_fnam,
      _input_value             TYPE c LENGTH 4000,
      _output_length           TYPE i.

    "Get description of output field
    TRY.
        IF element_descr IS BOUND.
          _element_descr = element_descr.
        ELSE.
          _element_descr ?= cl_abap_typedescr=>describe_by_data( converted_value ).
        ENDIF.

      CATCH cx_root INTO _exception.
        _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                                        iv_class    = 'ZCL_CA_CONV'
                                                                        iv_method   = 'CONVERT_VIA_CONVERSION_EXIT'
                                                                        ix_error    = _exception ) ) ##no_text.
        IF _conversion_exception IS BOUND.
          RAISE EXCEPTION _conversion_exception.
        ENDIF.
    ENDTRY.

    "Is it for output or input
    CASE is_for_output.
      WHEN abap_false.
        _fm_name_conversion_exit = conversion_exit_pattern-for_input.
      WHEN OTHERS.
        _fm_name_conversion_exit = conversion_exit_pattern-for_output.
    ENDCASE.

    "Complete conversion exit name
    _fm_name_conversion_exit = replace( val  = _fm_name_conversion_exit
                                        sub  = '&' ##no_text
                                        with = conversion_exit_name
                                        occ  = 1 ).

    "In case of strings the length is zero. Use then length of input value.
    IF _element_descr->output_length EQ 0.
      _output_length = strlen( input_value ).
    ELSE.
      _output_length = _element_descr->output_length.
    ENDIF.

    "The ALPHA conversion exit raises a dump if the input value is LONGER than
    "output field. To avoid this the value is here shortened. Since the ALPHA
    "exit expect only character values (no strings!) the input length is
    "equal to output length (in opposite to a packed field). So we can compare
    "with the output length.
    IF conversion_exit_name  EQ conv_exit_name-alpha AND
       strlen( input_value ) GT _output_length.
      _input_value = input_value(_output_length).
    ELSE.
      _input_value = input_value.
    ENDIF.

    TRY.
        CALL FUNCTION _fm_name_conversion_exit
          EXPORTING
            input         = _input_value
          IMPORTING
            output        = converted_value
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        IF sy-subrc NE 0.
          _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                                          iv_function = _fm_name_conversion_exit
                                                                          iv_subrc    = sy-subrc ) ) ##no_text.
          IF _conversion_exception IS BOUND.
            RAISE EXCEPTION _conversion_exception.
          ENDIF.
        ENDIF.

      CATCH cx_sy_dyn_call_illegal_func INTO _exception.
        "Function module does not exist
        _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                                        iv_function = _fm_name_conversion_exit
                                                                        ix_error    = _exception ) ) ##no_text.
        IF _conversion_exception IS BOUND.
          RAISE EXCEPTION _conversion_exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "convert_via_conversion_exit


  METHOD external_2_internal.
    "-----------------------------------------------------------------*
    "   Conversion of an external value into an internal value
    "-----------------------------------------------------------------*
    "Local constant
    CONSTANTS:
      _prog_n_var_name_rsdynss0_gl TYPE char50 VALUE '(RSDYNSS0)GL'.

    "Local data definitions
    DATA:
      _exception                  TYPE REF TO cx_root,
      _conversion_exception       TYPE REF TO zcx_ca_conv,
      _type_descr                 TYPE REF TO cl_abap_typedescr,
      _external_value_ref         TYPE REF TO data,
      _conversion_error           TYPE rsconverr,
      _field_descr_for_conversion TYPE rsconvert,
      _date                       TYPE sydatum,
      _return_code                TYPE sysubrc,
      _number_of_decimals         TYPE i,
*     Only to set SYST-MSGx variables for exception call
      _message_text               TYPE bapi_msg.

    FIELD-SYMBOLS:
      <rsdynss0_gl>          TYPE data,
      <field_in_rsdynss0_gl> TYPE data,
      <external_value>       TYPE clike,
      <decimal_notation>     TYPE char1.

* for completion of this method use class CL_ABAP_DECFLOAT for type f values

    "If no external value is transferred clear result and leave
    CLEAR internal_value.
    IF external_value IS INITIAL.
      RETURN.
    ENDIF.

    "Check if the receiving field is an elementary type
    _type_descr = cl_abap_typedescr=>describe_by_data( internal_value ).
    IF _type_descr->kind NE cl_abap_typedescr=>kind_elem.
      "Parameter &1 is not of an elementary type
      RAISE EXCEPTION TYPE zcx_ca_conv
        EXPORTING
          textid   = zcx_ca_conv=>param_is_not_elem
          mv_msgty = c_msgty_e
          mv_msgv1 = 'INTERNAL_VALUE' ##no_text.
    ENDIF.

    "Cast description to elementary type to get more detailed informations
    DATA(_element_descr_int_value) = CAST cl_abap_elemdescr( _type_descr ).

    "Check if the passed inbound field is an elementary type
    _type_descr = cl_abap_typedescr=>describe_by_data( external_value ).
    IF _type_descr->kind NE _type_descr->kind_elem.
      "Parameter &1 is not of an elementary type
      RAISE EXCEPTION TYPE zcx_ca_conv
        EXPORTING
          textid   = zcx_ca_conv=>param_is_not_elem
          mv_msgty = c_msgty_e
          mv_msgv1 = 'EXTERNAL_VALUE' ##no_text.
    ENDIF.

    "If the inbound value is of type string create a character field in the
    "used length. Otherwise later used DESCRIBE statement dumps.
    IF _type_descr->type_kind NE _type_descr->typekind_string.
      ASSIGN external_value TO <external_value>.

    ELSE.
      TRY.
          DATA(_length_external_value) = strlen( external_value ).
          CREATE DATA _external_value_ref TYPE c LENGTH _length_external_value.
          ASSIGN _external_value_ref->* TO <external_value>.
          <external_value> = external_value.

        CATCH cx_sy_create_data_error INTO _exception.
          _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                                          iv_class    = 'ZCL_CA_CONV'
                                                                          iv_method   = 'EXTERNAL_2_INTERNAL' ##no_text
                                                                          ix_error    = _exception ) ).
          IF _conversion_exception IS BOUND.
            RAISE EXCEPTION _conversion_exception.
          ENDIF.
      ENDTRY.
    ENDIF.

    IF _element_descr_int_value->is_ddic_type( ) EQ _element_descr_int_value->true.
      "Call method not functional to be able to catch the exceptions
      CALL METHOD _element_descr_int_value->get_ddic_field
        RECEIVING
          p_flddescr   = DATA(_ddic_info_int_value)
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3.
      IF sy-subrc NE 0.
        _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                                        iv_class    = 'CL_ABAP_ELEMDESCR'
                                                                        iv_method   = 'GET_DDIC_FIELD' ##no_text
                                                                        iv_subrc    = sy-subrc ) ).
        IF _conversion_exception IS BOUND.
          RAISE EXCEPTION _conversion_exception.
        ENDIF.
      ENDIF.

      _field_descr_for_conversion-decimals   = _ddic_info_int_value-decimals.
      _field_descr_for_conversion-dddecimals = _ddic_info_int_value-decimals.
      _field_descr_for_conversion-length     = _ddic_info_int_value-intlen.
      _field_descr_for_conversion-olength    = _ddic_info_int_value-outputlen.
      _field_descr_for_conversion-type       = _ddic_info_int_value-inttype.
      _field_descr_for_conversion-convexit   = _ddic_info_int_value-convexit.
      _field_descr_for_conversion-lower      = _ddic_info_int_value-lowercase.
      "Set "with sign" otherwise negative values will not be accepted
      _field_descr_for_conversion-sign       = _ddic_info_int_value-sign.

    ELSE.
      "Set values from element description
      _field_descr_for_conversion-type       = _element_descr_int_value->type_kind.
      _field_descr_for_conversion-length     = _element_descr_int_value->length.
      _field_descr_for_conversion-olength    = _element_descr_int_value->output_length.
      _field_descr_for_conversion-decimals   = _element_descr_int_value->decimals.
      "Set 'with sign' otherwise negative values will not be accepted
      _field_descr_for_conversion-sign       = abap_true.
    ENDIF.

    "Initialize target field
    CLEAR internal_value.

    "Convert some types in another way
    TRY.
        CASE _field_descr_for_conversion-type.
          WHEN _element_descr_int_value->typekind_string.
            "Internal type is   s t r i n g
            internal_value = <external_value>.
            IF _element_descr_int_value->is_ddic_type( ) EQ _element_descr_int_value->true AND
               _field_descr_for_conversion-lower         EQ abap_false.
              TRANSLATE internal_value TO UPPER CASE.
            ENDIF.
            "Leave method after successful conversion
            RETURN.

          WHEN _element_descr_int_value->typekind_date.
            "Internal type is   d a t e
            "Check at first, if it is already a valid date
            _date = <external_value>.
            CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
              EXPORTING
                date                      = _date
              EXCEPTIONS
                plausibility_check_failed = 1
                OTHERS                    = 2.
            "If yes, return external value
            IF sy-subrc EQ 0.
              internal_value = <external_value>.
              "Otherwise convert it
            ELSE.
              cl_abap_datfm=>conv_date_ext_to_int(
                                             EXPORTING
                                               im_datext = <external_value>
                                             IMPORTING
                                               ex_datint = internal_value ).
            ENDIF.
            "Leave method after successful conversion
            RETURN.

          WHEN _element_descr_int_value->typekind_time.
            "Internal type is   t i m e
            cl_abap_timefm=>conv_time_ext_to_int(
                                            EXPORTING
                                              time_ext = <external_value>
                                            IMPORTING
                                              time_int = internal_value ).
            "Leave method after successful conversion
            RETURN.
        ENDCASE.

      CATCH cx_abap_datfm_no_date
            cx_abap_datfm_invalid_date
            cx_abap_datfm_format_unknown
            cx_abap_datfm_ambiguous
            cx_abap_timefm_invalid       INTO _exception.
        _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                                        iv_class    = 'ZCL_CA_CONV'
                                                                        iv_method   = 'EXTERNAL_2_INTERNAL' ##no_text
                                                                        ix_error    = _exception ) ).
        IF _conversion_exception IS BOUND.
          RAISE EXCEPTION _conversion_exception.
        ENDIF.
    ENDTRY.

    "Get decimals of currency
    IF currency IS NOT INITIAL.
      SELECT SINGLE currdec INTO  _number_of_decimals
                            FROM  tcurx
                            WHERE currkey EQ currency.
      IF sy-subrc EQ 0.
        _field_descr_for_conversion-decimals = _number_of_decimals.
      ELSE.
        _field_descr_for_conversion-decimals = 2.
      ENDIF.
    ENDIF.

    "Set decimals
    _field_descr_for_conversion-quan_unit = unit_of_measure.

    IF _field_descr_for_conversion-clength IS INITIAL.
      PERFORM ileng_2_cleng IN PROGRAM rsdynss0
                            USING    _field_descr_for_conversion-type
                                     _field_descr_for_conversion-length
                            CHANGING _field_descr_for_conversion-clength.
    ENDIF.

    "If conversion exit is defined, execute it by ourself,
    "because the ALPHA exit has special need.
    IF _field_descr_for_conversion-convexit IS NOT INITIAL.
      convert_via_conversion_exit(
                            EXPORTING
                              is_for_output        = abap_false
                              conversion_exit_name = _field_descr_for_conversion-convexit
                              element_descr        = _element_descr_int_value
                              input_value          = <external_value>
                            IMPORTING
                              converted_value      = internal_value ).

    ELSE.
      "Call standard subroutine for conversion
      PERFORM convert_ex_2_in IN PROGRAM rsdynss0
                                   USING _field_descr_for_conversion
                                         <external_value>
                                CHANGING _return_code
                                         _conversion_error
                                         internal_value.
      IF _return_code   NE 0       AND
         internal_value IS INITIAL.
        CASE _field_descr_for_conversion-type.
          WHEN cl_abap_typedescr=>typekind_packed.
            "P a c k e d   v a l u e s
            "SPACE = 1.234.567,89 (= e. g. DE);
            "X     = 1,234,567.89 (= e. g. US);
            "Y     = 1 234 567,89 (no country defined for it in T005X)
            "These three decimal notations are possible, but only the values SPACE and X are maintained in
            "T005X (see description of SET COUNTRY). So Y is skipped during this processing.
            DO 3 TIMES.
              DATA(offset) = sy-index - 1.
              ASSIGN decimal_notation+offset(1) TO <decimal_notation>.
              IF <decimal_notation> EQ user_default_settings-dcpfm.
                "This case was already tried before and leads to an error
                CONTINUE.
              ENDIF.
              "Set another country to force the use of another decimal notation and ...
              CASE <decimal_notation>.
                WHEN decimal_notation-in_europe.
                  SET COUNTRY 'DE' ##no_text.
                WHEN decimal_notation-in_english.
                  SET COUNTRY 'US' ##no_text.
                WHEN decimal_notation-others.
                  CONTINUE.                " no country known
              ENDCASE.
              "... and try again.
              "Therefore a value in the conversion program must be initialized.
              "Otherwise the masks of the first try will be used again.
              ASSIGN (_prog_n_var_name_rsdynss0_gl) TO <rsdynss0_gl>.
              IF sy-subrc EQ 0.
                DO 4 TIMES.
                  ASSIGN COMPONENT sy-index OF STRUCTURE <rsdynss0_gl>
                                                      TO <field_in_rsdynss0_gl>.
                  CLEAR <field_in_rsdynss0_gl>.
                ENDDO.
              ENDIF.
              CLEAR: _conversion_error,
                     _return_code.
              PERFORM convert_ex_2_in IN PROGRAM rsdynss0
                                           USING _field_descr_for_conversion
                                                 <external_value>
                                        CHANGING _return_code
                                                 _conversion_error
                                                 internal_value.
              IF _return_code EQ 0.
                EXIT.                      " leave if the conversion was successful
              ENDIF.
            ENDDO.
            "Reset formatting as defined in user master
            SET COUNTRY space.
        ENDCASE.
      ENDIF.
    ENDIF.

    IF _return_code EQ 0.
      IF _element_descr_int_value->is_ddic_type( ) EQ _element_descr_int_value->true          AND
         _field_descr_for_conversion-type          EQ _element_descr_int_value->typekind_char AND
         _field_descr_for_conversion-lower         EQ abap_false.
        TRANSLATE internal_value TO UPPER CASE.
      ENDIF.

      IF currency                               IS NOT INITIAL                               AND
         _field_descr_for_conversion-type       EQ _element_descr_int_value->typekind_packed AND
         _field_descr_for_conversion-dddecimals NE 0                                         AND
         _field_descr_for_conversion-decimals   EQ 0.
        internal_value = internal_value / ( 10 ** _field_descr_for_conversion-dddecimals ).
      ENDIF.
    ENDIF.

    IF _return_code NE 0.
      CASE _return_code.
        WHEN 1.
          "Entry is not numeric
          MESSAGE s738(db) WITH _conversion_error-ill_token INTO _message_text.
        WHEN 2.
          "Too many decimal places (maximum &)
          MESSAGE s739(db) WITH _field_descr_for_conversion-decimals
                                _conversion_error-decimals INTO _message_text.
        WHEN 3.
          "Specify the sign either at the beginning or at the end
          MESSAGE s740(db) INTO _message_text.
        WHEN 4.
          "Correct the distance (&1) between "&2" and "&2" or "&2" and "&3"
          MESSAGE s741(db) WITH _conversion_error-dist_1000
                                _conversion_error-tdelimiter
                                _conversion_error-ddelimiter INTO _message_text.
        WHEN 5.
          "Entry is too long: Only & digits are allowed in the whole number part
          MESSAGE s744(db) WITH _conversion_error-digits_max
                                _conversion_error-digits_inp INTO _message_text.
        WHEN 6.
          "Signs are not allowed here
          MESSAGE s745(db) INTO _message_text.
        WHEN 7.
          "Entered value is too large (maximum &2)
          MESSAGE s746(db) WITH _conversion_error-input
                                _conversion_error-max INTO _message_text.
        WHEN 8.
          "Entered value is too small (maximum &2)
          MESSAGE s747(db) WITH _conversion_error-input
                                _conversion_error-min INTO _message_text.
        WHEN 9.
          "Invalid date format. Please enter date in the format &1
          MESSAGE s748(db) WITH _conversion_error-date_mask
                                _conversion_error-date_delim INTO _message_text.
        WHEN 10.
          "Invalid date: Please enter date in the format &1
          MESSAGE s749(db) WITH _conversion_error-date_mask
                                _conversion_error-date_delim INTO _message_text.
        WHEN 11 ##number_ok.
          "Invalid time format. Please enter time in the format &1
          MESSAGE s751(db) WITH 'HH:MM:SS' ':' INTO _message_text.
        WHEN 12 ##number_ok.
          "Invalid time: Please enter time in the format &1
          MESSAGE s753(db) WITH 'HH:MM:SS' ':' INTO _message_text.
        WHEN 13 ##number_ok.
          "Invalid character "&" in hexadecimal field
          MESSAGE s754(db) WITH _conversion_error-ill_token INTO _message_text.
        WHEN 14 ##number_ok.
          "You cannot begin the entry with the exponent
          MESSAGE s841(db) WITH _conversion_error-tdelimiter
                                _conversion_error-ddelimiter INTO _message_text.
        WHEN 15 ##number_ok.
          "Exponent empty
          MESSAGE s842(db) WITH _conversion_error-tdelimiter
                                _conversion_error-ddelimiter INTO _message_text.
        WHEN 16 ##number_ok.
          "'&1', but no decimal places
          MESSAGE s843(db) WITH _conversion_error-ddelimiter INTO _message_text.
        WHEN 17 ##number_ok.
          "Invalid exponent
          MESSAGE s844(db) WITH _conversion_error-tdelimiter
                                _conversion_error-ddelimiter INTO _message_text.
        WHEN 18 ##number_ok.
          "'&2' after 'E'
          MESSAGE s845(db) WITH _conversion_error-tdelimiter
                                _conversion_error-ddelimiter INTO _message_text.
        WHEN 19 ##number_ok.
          "Exponent too large
          MESSAGE s846(db) WITH _conversion_error-expomax
                                _conversion_error-ddelimiter INTO _message_text.
        WHEN 20 ##number_ok.
          "Date is not valid -> send message of function module
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO _message_text.
        WHEN OTHERS.
          "Conversion error
          MESSAGE s755(db) INTO _message_text.
      ENDCASE.

      _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls   = zcx_ca_conv=>c_zcx_ca_conv
                                                                      iv_function   = 'RSDYNSS0'
                                                                      iv_subroutine = 'CONVERT_EX_2_IN' ##no_text
                                                                      iv_subrc      = _return_code ) ).
      IF _conversion_exception IS BOUND.
        RAISE EXCEPTION _conversion_exception.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "external_2_internal


  METHOD flag_2_boolean.
    "-----------------------------------------------------------------*
    "   Conversion of a flag (blank/X) into a boolean value (0/1).
    "   Input for TRUE can be: X, x, Y, J, j. Anything else is
    "   returned as FALSE.
    "-----------------------------------------------------------------*
    IF flag CA 'XxYJj' ##no_text.
      result = num_boolean->true.
    ELSE.
      result = num_boolean->false.
    ENDIF.
  ENDMETHOD.                    "flag_2_boolean


  METHOD internal_2_external.
    "-----------------------------------------------------------------*
    "   Conversion of an internal value into an external appearance
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _exception            TYPE REF TO cx_root,
      _conversion_exception TYPE REF TO zcx_ca_conv,
      _date_format          TYPE xudatfm,
      _offset_for_sign      TYPE syfdpos,
      _amount               TYPE wertv9,
      _return_code_of_write TYPE sysubrc,
      _external_value       TYPE c LENGTH 255.

* for completion of this method use class CL_ABAP_DECFLOAT for type f values

    "Check if the passed value is an elementary type
    DATA(_type_descr) = cl_abap_typedescr=>describe_by_data( internal_value ).
    IF _type_descr->kind NE cl_abap_typedescr=>kind_elem.
      "Parameter &1 has not an elementary type
      RAISE EXCEPTION TYPE zcx_ca_conv
        EXPORTING
          textid   = zcx_ca_conv=>param_is_not_elem
          mv_msgty = c_msgty_e
          mv_msgv1 = 'INTERNAL_VALUE' ##no_text.
    ENDIF.

    "Cast description to elemntary type to get more detailed informations
    DATA(_element_descr_int_value) = CAST cl_abap_elemdescr( _type_descr ).
    IF _element_descr_int_value->is_ddic_type( ) EQ _element_descr_int_value->true.
      "Get DDIC description
      DATA(_ddic_info_int_value) = _element_descr_int_value->get_ddic_field( ).
    ENDIF.

    "Initialize target field
    CLEAR external_value.

    "Use conversion exit if exist
    IF _ddic_info_int_value-convexit IS NOT INITIAL.
      convert_via_conversion_exit(
                            EXPORTING
                              conversion_exit_name = _ddic_info_int_value-convexit
                              input_value          = internal_value
                            IMPORTING
                              converted_value      = external_value ).

      "Use  e d i t   m a s k   if exist
    ELSEIF _element_descr_int_value->edit_mask IS NOT INITIAL                             AND
           _element_descr_int_value->type_kind NE _element_descr_int_value->typekind_date AND
           _element_descr_int_value->type_kind NE _element_descr_int_value->typekind_time.
      WRITE internal_value USING EDIT MASK _element_descr_int_value->edit_mask
                                        TO external_value.
      _return_code_of_write = sy-subrc.

    ELSE.
      "Prepare external value depending on the internal data type
      CASE _element_descr_int_value->type_kind.
        WHEN _element_descr_int_value->typekind_date.
          "internal type -  D a t e
          "IDocs use the SAP internal format as standard format - no need to convert
          IF result_is_for_idoc EQ abap_true.
            external_value = internal_value.

          ELSE.
            "Use user setting for date conversion if no format was passed
            IF date_format IS INITIAL.
              _date_format = user_default_settings-datfm.
            ELSE.
              _date_format = date_format.
            ENDIF.

            TRY.
                cl_abap_datfm=>conv_date_int_to_ext(
                                              EXPORTING
                                                im_datint   = internal_value
                                                im_datfmdes = _date_format
                                              IMPORTING
                                                ex_datext   = external_value ).
                "There is a problem, didn't know what but the method before
                "returns no value. So we do this workaround.
                IF external_value IS INITIAL.
                  IF _date_format CA '23'.                  "#EC DATFM
                    WRITE internal_value MM/DD/YYYY TO external_value. "#EC DATFM
                  ELSE.
                    WRITE internal_value DD/MM/YYYY TO external_value. "#EC DATFM
                  ENDIF.
                ENDIF.
                _return_code_of_write = sy-subrc.

              CATCH cx_abap_datfm_format_unknown INTO _exception.
                _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                                                iv_class    = 'CL_ABAP_DATFM'
                                                                                iv_method   = 'CONV_DATE_INT_TO_EXT'
                                                                                ix_error    = _exception ) ) ##no_text.
                IF _conversion_exception IS BOUND.
                  RAISE EXCEPTION _conversion_exception.
                ENDIF.
            ENDTRY.
          ENDIF.

          "internal type -  T i m e
        WHEN _element_descr_int_value->typekind_time.
          IF result_is_for_idoc EQ abap_true.
            external_value = internal_value.

          ELSE.
            TRY.
                "Value ENVIRONMENT use the WRITE-statement with additon
                "environment TIME FORMAT.
                "format_according_to = cl_abap_timefm=>user  is defined, but not allowed
                cl_abap_timefm=>conv_time_int_to_ext(
                                                EXPORTING
                                                  time_int            = internal_value
                                                  without_seconds     = without_seconds
                                                  format_according_to = cl_abap_timefm=>environment
                                                IMPORTING
                                                  time_ext            = DATA(_converted_time) ).
                external_value = _converted_time.

              CATCH cx_parameter_invalid_range INTO _exception.
                _conversion_exception = CAST #( zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_conv=>c_zcx_ca_conv
                                                                                iv_class    = 'CL_ABAP_TIMEFM'
                                                                                iv_method   = 'CONV_TIME_INT_TO_EXT'
                                                                                ix_error    = _exception ) ) ##no_text.
                IF _conversion_exception IS BOUND.
                  RAISE EXCEPTION _conversion_exception.
                ENDIF.
            ENDTRY.
          ENDIF.

        WHEN _element_descr_int_value->typekind_num.
          "internal type -  n u m e r i c    c h a r a c t e r
          IF result_is_for_idoc EQ abap_true.
            external_value = internal_value.
          ELSE.
            convert_via_conversion_exit(
                                    EXPORTING
                                      input_value     = internal_value
                                    IMPORTING
                                      converted_value = external_value ).
          ENDIF.

        WHEN _element_descr_int_value->typekind_packed.
          "internal type -  p a c k e d   v a l u e
          CASE _ddic_info_int_value-datatype.
            WHEN 'CURR' ##no_text.
              "C u r r e n c y
              internal_amount_2_external(
                                    EXPORTING
                                      internal_amount = internal_value
                                      currency        = currency
                                    IMPORTING
                                      external_amount = _external_value ).

            WHEN 'QUAN' ##no_text.
              "Q u a n t i t y
              IF unit_of_measure IS INITIAL.
                WRITE internal_value TO _external_value.    "#EC *

              ELSE.
                "Check existence of the unit
                SELECT COUNT( * ) FROM  t006             "#EC CI_BYPASS
                                  WHERE msehi EQ unit_of_measure.
                IF sy-dbcnt NE 1.
                  "Parameter '&1' has invalid value '&2'
                  RAISE EXCEPTION TYPE zcx_ca_conv
                    EXPORTING
                      textid   = zcx_ca_conv=>param_invalid
                      mv_msgty = c_msgty_e
                      mv_msgv1 = 'UNIT_OF_MEASURE' ##no_text
                      mv_msgv2 = CONV #( unit_of_measure ).

                ELSE.
                  WRITE internal_value UNIT unit_of_measure
                                       TO _external_value.
                ENDIF.
              ENDIF.
              _return_code_of_write = sy-subrc.

            WHEN OTHERS.
              CASE return_in_bapi_format.
                WHEN abap_false.
                  IF _element_descr_int_value->decimals IS NOT INITIAL.
                    WRITE internal_value DECIMALS _element_descr_int_value->decimals
                                               TO _external_value.
                  ELSE.
                    WRITE internal_value TO _external_value. "#EC *
                  ENDIF.
                  _return_code_of_write = sy-subrc.

                WHEN abap_true.
                  "A BAPI amount value (has 4 decimals) is also character value
                  "Convert BAPI amount into an internal value AND ...
                  bapi_amount_2_internal(
                                    EXPORTING
                                      bapi_amount     = internal_value
                                      currency        = currency
                                    IMPORTING
                                      amount_internal = _amount ).

                  "... then reconvert into an "normal" external value
                  internal_amount_2_external(
                                        EXPORTING
                                          internal_amount = _amount
                                          currency        = currency
                                        IMPORTING
                                          external_amount = _external_value ).
              ENDCASE.
          ENDCASE.

          SHIFT _external_value LEFT DELETING LEADING space.
          external_value = _external_value.

        WHEN _element_descr_int_value->typekind_float OR   " Floating point
             _element_descr_int_value->typekind_int1  OR
             _element_descr_int_value->typekind_int2  OR
             _element_descr_int_value->typekind_int.
          "internal type -  f l o a t   a n d   i n t e g e r
          "WRITE these data types to string fields is not allowed, so
          "take the way via char-field
          WRITE internal_value TO _external_value.          "#EC *
          _return_code_of_write = sy-subrc.
          SHIFT _external_value LEFT DELETING LEADING space.
          external_value = _external_value.

          "Not allowed types
        WHEN _element_descr_int_value->typekind_hex      OR   " hexadecimal
             _element_descr_int_value->typekind_xstring.      " hex string
          "Parameter '&1' has invalid value '&2'
          RAISE EXCEPTION TYPE zcx_ca_conv
            EXPORTING
              textid   = zcx_ca_conv=>param_invalid
              mv_msgty = c_msgty_e
              mv_msgv1 = 'INTERNAL_VALUE' ##no_text
              mv_msgv2 = 'Types HEX and XSTRING are not allowed'(u01).

          "Other internal types - mostly character
        WHEN OTHERS.
          external_value = internal_value.
      ENDCASE.

      "Reconvert editing for IDoc values
      IF   result_is_for_idoc                  EQ abap_true                                 AND
         ( _element_descr_int_value->type_kind EQ _element_descr_int_value->typekind_packed  OR
           _element_descr_int_value->type_kind EQ _element_descr_int_value->typekind_int1    OR
           _element_descr_int_value->type_kind EQ _element_descr_int_value->typekind_int2    OR
           _element_descr_int_value->type_kind EQ _element_descr_int_value->typekind_int ).
        "The IDoc format for packed values is only a dot as decimal delimiter
        "and the negtive sign at the end
        CASE user_default_settings-dcpfm.               " see fixed values of domain
          WHEN space.            " = 1.234.567,89
            TRANSLATE external_value USING '. '.    " Convert dots into spaces
            CONDENSE external_value NO-GAPS.        " Delete spaces
            TRANSLATE external_value USING ',.'.    " Convert comma into dot

          WHEN 'X' ##no_text.    " = 1,234,567.89
            TRANSLATE external_value USING ', '.    " Convert commas into spaces
            CONDENSE external_value NO-GAPS.        " Delete spaces

          WHEN 'Y' ##no_text.    " = 1 234 567,89
            CONDENSE external_value NO-GAPS.        " Delete spaces
            TRANSLATE external_value USING ',.'.    " Convert comma into dot
        ENDCASE.

        "Set sign at the end, if it doesn't already
        IF internal_value LT 0.
          TRANSLATE external_value USING '- '.      " Delete existing sign in any case
          SHIFT external_value LEFT DELETING LEADING space.   " Set value far left
          _offset_for_sign = strlen( external_value ).
          external_value+_offset_for_sign = '-'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF _return_code_of_write NE 0.
      "Conversion via WRITE statement failed
      RAISE EXCEPTION TYPE zcx_ca_conv
        EXPORTING
          textid   = zcx_ca_conv=>write_failed
          mv_msgty = c_msgty_e.
    ENDIF.

    "Convert to upper case if required
    IF _element_descr_int_value->is_ddic_type( ) EQ _element_descr_int_value->true AND
       _ddic_info_int_value-lowercase            IS INITIAL.
      TRANSLATE external_value TO UPPER CASE.
    ENDIF.

    "Delete leading spaces
    SHIFT external_value LEFT DELETING LEADING space.
  ENDMETHOD.                    "internal_2_external


  METHOD internal_amount_2_external.
    "-----------------------------------------------------------------*
    "   Conversion of an internal amount into an external appearance
    "-----------------------------------------------------------------*
    CLEAR external_amount.
    IF currency IS INITIAL.
      WRITE internal_amount TO external_amount.             "#EC *

    ELSE.
      "Check existence of the currency
      SELECT COUNT( * ) FROM  tcurc                      "#EC CI_BYPASS
                        WHERE waers EQ currency.
      IF sy-dbcnt NE 1.
        "Parameter '&1' has invalid value '&2'
        RAISE EXCEPTION TYPE zcx_ca_conv
          EXPORTING
            textid   = zcx_ca_conv=>param_invalid
            mv_msgty = c_msgty_e
            mv_msgv1 = 'CURRENCY' ##no_text
            mv_msgv2 = CONV #( currency ).

      ELSE.
        WRITE internal_amount CURRENCY currency TO external_amount.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "internal_amount_2_external
ENDCLASS.
