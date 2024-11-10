"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Abstract SUPER exception + helper methods</p>
CLASS zcx_ca_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  ABSTRACT
  CREATE PUBLIC .

* P U B L I C   S E C T I O N
  PUBLIC SECTION.

*   i n t e r f a c e s
    INTERFACES if_abap_behv_message .
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

*   a l i a s e s
*     Message variables
    ALIASES ms_default_textid
      FOR if_t100_message~default_textid .
    ALIASES ms_t100key
      FOR if_t100_message~t100key .
    ALIASES mv_msgty
      FOR if_t100_dyn_msg~msgty .
    ALIASES mv_msgv1
      FOR if_t100_dyn_msg~msgv1 .
    ALIASES mv_msgv2
      FOR if_t100_dyn_msg~msgv2 .
    ALIASES mv_msgv3
      FOR if_t100_dyn_msg~msgv3 .
    ALIASES mv_msgv4
      FOR if_t100_dyn_msg~msgv4 .
    ALIASES mv_severity
      FOR if_abap_behv_message~m_severity .
    ALIASES t_severity
      FOR if_abap_behv_message~t_severity .

    CONSTANTS:
      BEGIN OF any_other_msg,
        msgid TYPE symsgid VALUE 'S1',
        msgno TYPE symsgno VALUE '897',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
      END OF any_other_msg .
    CONSTANTS:
      BEGIN OF error_func_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_func_call .
    CONSTANTS:
      BEGIN OF error_meth_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_meth_call .
    CONSTANTS:
      BEGIN OF error_subr_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_subr_call .
    CONSTANTS:
      BEGIN OF error_unknown_comp,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_unknown_comp .
    CONSTANTS:
      BEGIN OF zcx_ca_error,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_error .
*   c o n s t a n t s
*     Message types
    CONSTANTS c_msgty_e TYPE if_abap_behv_message=>t_char01 VALUE if_abap_behv_message=>severity-error ##NO_TEXT.
    CONSTANTS c_msgty_w TYPE if_abap_behv_message=>t_char01 VALUE if_abap_behv_message=>severity-warning ##NO_TEXT.
    CONSTANTS c_msgty_i TYPE if_abap_behv_message=>t_char01 VALUE if_abap_behv_message=>severity-information ##NO_TEXT.
    CONSTANTS c_msgty_s TYPE if_abap_behv_message=>t_char01 VALUE if_abap_behv_message=>severity-success ##NO_TEXT.
    "! <p class="shorttext synchronized" lang="en">My own name</p>
    CONSTANTS c_zcx_ca_error TYPE seoclsname VALUE 'ZCX_CA_ERROR' ##NO_TEXT.
    "! <p class="shorttext synchronized" lang="en">Message error type for comparison</p>
    CONSTANTS c_msgty_eax TYPE char3 VALUE 'EAX' ##NO_TEXT.
*   i n s t a n c e   a t t r i b u t e s
    "! <p class="shorttext synchronized" lang="en">All messages passed from BAPI call</p>
    DATA mt_return TYPE bapiret2_t READ-ONLY .
*     s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">Return code of catched classical exception</p>
    DATA mv_subrc TYPE syst_subrc READ-ONLY .

*   s t a t i c   m e t h o d s
    "! <p class="shorttext synchronized" lang="en">Convert former BAPI messages into current format</p>
    "!
    "! @parameter it_return   | <p class="shorttext synchronized" lang="en">BAPI messages in former format</p>
    "! @parameter rt_bapiret2 | <p class="shorttext synchronized" lang="en">Converted messages in current format</p>
    CLASS-METHODS conv_bapireturn_2_bapiret2
      IMPORTING
        !it_return         TYPE icl_t_bapireturn
      RETURNING
        VALUE(rt_bapiret2) TYPE bapiret2_t .
    "! <p class="shorttext synchronized" lang="en">Create exception instance</p>
    "!
    "! @parameter iv_excp_cls   | <p class="shorttext synchronized" lang="en">Type of new exception</p>
    "! @parameter iv_function   | <p class="shorttext synchronized" lang="en">Name of function module or program (addition to subroutine)</p>
    "! @parameter iv_class      | <p class="shorttext synchronized" lang="en">Name of executed class (addition to IV_METHOD)</p>
    "! @parameter iv_method     | <p class="shorttext synchronized" lang="en">Name of executed method (addition to IV_CLASS)</p>
    "! @parameter iv_subroutine | <p class="shorttext synchronized" lang="en">Name of subroutine</p>
    "! @parameter iv_msgty      | <p class="shorttext synchronized" lang="en">Message type</p>
    "! @parameter is_msg        | <p class="shorttext synchronized" lang="en">Message details (pass ONLY if not from SYST!!)</p>
    "! @parameter is_return     | <p class="shorttext synchronized" lang="en">Message details from BAPI call</p>
    "! @parameter it_return     | <p class="shorttext synchronized" lang="en">All messages from BAPI call</p>
    "! @parameter iv_subrc      | <p class="shorttext synchronized" lang="en">Return code</p>
    "! @parameter ix_error      | <p class="shorttext synchronized" lang="en">Catched exception</p>
    "! @parameter rx_excep      | <p class="shorttext synchronized" lang="en">Created exception instance</p>
    CLASS-METHODS create_exception
      IMPORTING
        !iv_excp_cls    TYPE seoclsname DEFAULT c_zcx_ca_error
        !iv_function    TYPE rs38l_fnam OPTIONAL
        !iv_class       TYPE seoclsname OPTIONAL
        !iv_method      TYPE seocpdname OPTIONAL
        !iv_subroutine  TYPE formname OPTIONAL
        !iv_msgty       TYPE syst_msgty DEFAULT c_msgty_e
        !is_msg         TYPE dcmessage OPTIONAL
        !is_return      TYPE bapiret2 OPTIONAL
        !it_return      TYPE bapiret2_t OPTIONAL
        VALUE(iv_subrc) TYPE syst_subrc OPTIONAL
        !ix_error       TYPE REF TO cx_root OPTIONAL
      RETURNING
        VALUE(rx_excep) TYPE REF TO zcx_ca_error .
    "! <p class="shorttext synchronized" lang="en">Extracting error message (NO result if type is NOT EAX!!)</p>
    "!
    "! @parameter iv_msgty  | <p class="shorttext synchronized" lang="en">Message type</p>
    "! @parameter iv_subrc  | <p class="shorttext synchronized" lang="en">Return code</p>
    "! @parameter is_msg    | <p class="shorttext synchronized" lang="en">Message details</p>
    "! @parameter is_return | <p class="shorttext synchronized" lang="en">Message details from BAPI call</p>
    "! @parameter it_return | <p class="shorttext synchronized" lang="en">All messages from BAPI call</p>
    "! @parameter ix_error  | <p class="shorttext synchronized" lang="en">Catched exception</p>
    "! @parameter rs_return | <p class="shorttext synchronized" lang="en">Extracted message</p>
    CLASS-METHODS extract_message
      IMPORTING
        !iv_msgty        TYPE syst_msgty DEFAULT c_msgty_e
        VALUE(iv_subrc)  TYPE syst_subrc OPTIONAL
        !is_msg          TYPE dcmessage OPTIONAL
        !is_return       TYPE bapiret2 OPTIONAL
        !it_return       TYPE bapiret2_t OPTIONAL
        !ix_error        TYPE REF TO cx_root OPTIONAL
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    "! <p class="shorttext synchronized" lang="en">Determine (readable) exception position in source code</p>
    "!
    "! @parameter ix_error   | <p class="shorttext synchronized" lang="en">Catched exception</p>
    "! @parameter rs_src_pos | <p class="shorttext synchronized" lang="en">(Readable) exception position in source code</p>
    CLASS-METHODS get_exception_position
      IMPORTING
        !ix_error         TYPE REF TO cx_root
      RETURNING
        VALUE(rs_src_pos) TYPE zca_s_excep_srcpos .
    "! <p class="shorttext synchronized" lang="en">Determine message variables + other details from exception</p>
    "!
    "! @parameter iv_msgty  | <p class="shorttext synchronized" lang="en">Message type</p>
    "! @parameter ix_error  | <p class="shorttext synchronized" lang="en">Catched exception</p>
    "! @parameter rs_return | <p class="shorttext synchronized" lang="en">Extracted message</p>
    CLASS-METHODS get_msg_details_from_excep
      IMPORTING
        !iv_msgty        TYPE syst_msgty DEFAULT c_msgty_e
        !ix_error        TYPE REF TO cx_root
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
*   i n s t a n c e   m e t h o d s
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid      LIKE if_t100_message=>t100key OPTIONAL
        !previous    LIKE previous OPTIONAL
        !mt_return   TYPE bapiret2_t OPTIONAL
        !mv_subrc    TYPE syst_subrc OPTIONAL
        !mv_msgty    TYPE symsgty
        !mv_msgv1    TYPE symsgv OPTIONAL
        !mv_msgv2    TYPE symsgv OPTIONAL
        !mv_msgv3    TYPE symsgv OPTIONAL
        !mv_msgv4    TYPE symsgv OPTIONAL
        !mv_severity TYPE t_severity OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Set catched exception instance later</p>
    "!
    "! @parameter ix_error | <p class="shorttext synchronized" lang="en">Catched exception</p>
    METHODS set_previous_late
      IMPORTING
        !ix_error TYPE REF TO cx_root .
* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.



* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
      "! <p class="shorttext synchronized" lang="en">Structure for saving original message</p>
      ms_other_msg TYPE scx_t100key.
ENDCLASS.



CLASS zcx_ca_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    me->mt_return = mt_return .
    me->mv_subrc = mv_subrc .
    me->mv_msgty = mv_msgty .
    me->mv_msgv1 = mv_msgv1 .
    me->mv_msgv2 = mv_msgv2 .
    me->mv_msgv3 = mv_msgv3 .
    me->mv_msgv4 = mv_msgv4 .
    me->mv_severity = mv_severity .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_ca_error .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD conv_bapireturn_2_bapiret2.
    "-----------------------------------------------------------------*
    "   Convert former BAPI messages into current format
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_return2            TYPE bapiret2.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      CALL FUNCTION 'BALW_RETURN_TO_RET2'
        EXPORTING
          return_in = <ls_return>
        IMPORTING
          return_ou = ls_return2.

      APPEND ls_return2 TO rt_bapiret2.
    ENDLOOP.
  ENDMETHOD.                    "conv_bapireturn_2_bapiret2


  METHOD create_exception.
    "-----------------------------------------------------------------*
    "   Create and raise exception instance of an inheriting exception
    "   class and tries to use the original message. Otherwise a common
    "   message will be used.
    "-----------------------------------------------------------------*
    "Extract message from input
    DATA(ls_return) = extract_message( iv_msgty  = iv_msgty
                                       iv_subrc  = iv_subrc
                                       is_msg    = is_msg
                                       is_return = is_return
                                       it_return = it_return
                                       ix_error  = ix_error ).

    "If no error message was found, e. g. in BAPI messages which returns
    "no returncode, leave
    IF ls_return IS INITIAL AND
       iv_subrc  EQ 0.
      RETURN.
    ENDIF.

    "Message is incomplete -> provide a common message
    IF ls_return-id     IS INITIAL OR
       ls_return-number IS INITIAL.
      IF iv_function IS NOT INITIAL.
        "Exception triggered in function module &1 (RC = &2)
        ls_return-id         = error_func_call-msgid.
        ls_return-number     = error_func_call-msgno.
        ls_return-message_v1 = iv_function.

      ELSEIF iv_method IS NOT INITIAL.
        "Exception triggered in class->method &1 (RC = &2)
        ls_return-id         = error_meth_call-msgid.
        ls_return-number     = error_meth_call-msgno.
        ls_return-message_v1 = iv_class && '=>' && iv_method.

      ELSEIF iv_subroutine IS NOT INITIAL.
        "Exception triggered in subroutine &1 with return code &2
        ls_return-id         = error_subr_call-msgid.
        ls_return-number     = error_subr_call-msgno.
        ls_return-message_v1 = iv_subroutine && '(' && iv_function && ')'.
      ELSE.
        "Exception triggered in unknown component (&1) (RC = &2)
        ls_return-id         = error_unknown_comp-msgid.
        ls_return-number     = error_unknown_comp-msgno.
        ls_return-message_v1 = 'not passed'(e01).
      ENDIF.

      IF ix_error IS BOUND.
        DATA(lo_type_desc) = cl_abap_typedescr=>describe_by_object_ref(
          p_object_ref = ix_error ).
        ls_return-message_v2 = lo_type_desc->get_relative_name( ).
      ELSE.
        ls_return-message_v2 = condense( CONV symsgv( iv_subrc ) ).
      ENDIF.
    ENDIF.

    "Set values into a common structure
    CLEAR ms_other_msg.
    ms_other_msg-msgid = ls_return-id.
    ms_other_msg-msgno = ls_return-number.
    IF ls_return-message_v1 IS NOT INITIAL.
      ms_other_msg-attr1 = 'MV_MSGV1' ##no_text.
    ENDIF.
    IF ls_return-message_v2 IS NOT INITIAL.
      ms_other_msg-attr2 = 'MV_MSGV2' ##no_text.
    ENDIF.
    IF ls_return-message_v3 IS NOT INITIAL.
      ms_other_msg-attr3 = 'MV_MSGV3' ##no_text.
    ENDIF.
    IF ls_return-message_v4 IS NOT INITIAL.
      ms_other_msg-attr4 = 'MV_MSGV4' ##no_text.
    ENDIF.

    "Create exception with available message
    CREATE OBJECT rx_excep TYPE (iv_excp_cls)
      EXPORTING
        textid    = ms_other_msg
        previous  = ix_error
        mt_return = it_return
        mv_subrc  = iv_subrc
        mv_msgty  = ls_return-type
        mv_msgv1  = ls_return-message_v1
        mv_msgv2  = ls_return-message_v2
        mv_msgv3  = ls_return-message_v3
        mv_msgv4  = ls_return-message_v4.

    rx_excep->if_abap_behv_message~m_severity = COND #( WHEN ls_return-type CA 'AX'
                                                          THEN if_abap_behv_message=>severity-error
                                                          ELSE CONV #( ls_return-type ) ) ##no_text.
  ENDMETHOD.                    "create_exception


  METHOD extract_message.
    "-----------------------------------------------------------------*
    "   Extract message from passed values, but no result if type is not EAX.
    "-----------------------------------------------------------------*
    "Multiple BAPI messages
    IF it_return IS NOT INITIAL.
      "Search for first occuring error message after BAPI call
      LOOP AT it_return INTO  rs_return
                        WHERE type CA c_msgty_eax.
        IF rs_return-id     IS NOT INITIAL AND
           rs_return-number IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.

    ELSEIF is_return IS NOT INITIAL.
      "Single BAPI message
      rs_return = is_return.

    ELSEIF is_msg IS NOT INITIAL.
      "Message from SYST
      rs_return-type       = is_msg-msgty.
      rs_return-id         = is_msg-msgid.
      rs_return-number     = is_msg-msgno.
      rs_return-message_v1 = is_msg-msgv1.
      rs_return-message_v2 = is_msg-msgv2.
      rs_return-message_v3 = is_msg-msgv3.
      rs_return-message_v4 = is_msg-msgv4.

    ELSEIF ix_error IS BOUND.
      "This method supplies the SYST-MSG*-variables
      rs_return = get_msg_details_from_excep( ix_error ).

    ELSEIF iv_subrc NE 0.
      rs_return-type       = sy-msgty.
      rs_return-id         = sy-msgid.
      rs_return-number     = sy-msgno.
      rs_return-message_v1 = sy-msgv1.
      rs_return-message_v2 = sy-msgv2.
      rs_return-message_v3 = sy-msgv3.
      rs_return-message_v4 = sy-msgv4.
    ENDIF.

    IF rs_return IS INITIAL.             "Can happen if IT_RETURN contains no error message
      RETURN.
    ENDIF.

    "The message type should only be set if external callers supplies it. Internal calls DON'T provide this
    "value to be able to distinguish if a BAPI returned an error or not.
    IF rs_return-type IS INITIAL AND
       iv_msgty       IS SUPPLIED.
      rs_return-type = iv_msgty.
    ENDIF.

    IF rs_return-type NA c_msgty_eax.
      CLEAR rs_return.                   "Create no exception instance
      RETURN.
    ENDIF.

    IF rs_return-id     IS NOT INITIAL AND
       rs_return-number IS NOT INITIAL.
      MESSAGE ID  rs_return-id
           TYPE   rs_return-type
           NUMBER rs_return-number
           WITH   rs_return-message_v1 rs_return-message_v2
                  rs_return-message_v3 rs_return-message_v4 INTO rs_return-message.
    ENDIF.
  ENDMETHOD.                    "extract_message


  METHOD get_exception_position.
    "-----------------------------------------------------------------*
    "   Convert internal exception position into readable source position
    "-----------------------------------------------------------------*
    "Get raw source position from exception instance
    ix_error->get_source_position(
      IMPORTING
        program_name = rs_src_pos-prog
        include_name = rs_src_pos-incl
        source_line  = rs_src_pos-line ).

    "Resolve technical object name into readable names
    cl_oo_classname_service=>get_method_by_include(
      EXPORTING
        incname             = rs_src_pos-incl
      RECEIVING
        mtdkey              = DATA(ls_meth_key)
      EXCEPTIONS
        class_not_existing  = 1
        method_not_existing = 2
        OTHERS              = 3 ).
    CASE sy-subrc.
      WHEN 0.
        rs_src_pos-class = ls_meth_key-clsname.
        rs_src_pos-meth  = ls_meth_key-cpdname.

      WHEN 1.
        rs_src_pos-class = rs_src_pos-prog.
        rs_src_pos-meth  = rs_src_pos-incl.

      WHEN OTHERS.
        "As long as e. g. the name of local class method can not be
        "resolved we provide only the technical names
        rs_src_pos-class =
          cl_oo_classname_service=>get_clsname_by_include(
          rs_src_pos-incl ).
        rs_src_pos-meth  = rs_src_pos-incl.
    ENDCASE.
  ENDMETHOD.                    "get_exception_position


  METHOD get_msg_details_from_excep.
    "-----------------------------------------------------------------*
    "   Get message details from catched exception
    "-----------------------------------------------------------------*
    "Initialize flag
    DATA(lv_is_otr_msg) = abap_false.

    "Get source position where exception occurred
    DATA(ls_src_pos)    = get_exception_position( ix_error ).
    rs_return-parameter = ls_src_pos-class.
    rs_return-field     = ls_src_pos-meth.
    rs_return-row       = ls_src_pos-line.

    TRY.
        "If this cast works, it is a new exception with T100 message
        DATA(lo_msg_t100) = CAST if_t100_message( ix_error ).
        "***  Method supplies the SY-MSGxx-fields ***
        cl_message_helper=>set_msg_vars_for_if_t100_msg( lo_msg_t100 ).

      CATCH cx_sy_message_illegal_text
            cx_sy_move_cast_error.
        "It is a OTR message
        lv_is_otr_msg = abap_true.
    ENDTRY.

    TRY.
        "Only an own exception can carry the message type
        DATA(lx_error) = CAST zcx_ca_error( ix_error ).
        IF lx_error->mv_msgty IS INITIAL.
          lx_error->mv_msgty = c_msgty_e.
        ENDIF.
        rs_return-type = lx_error->mv_msgty.

      CATCH cx_sy_move_cast_error.
        TRY.
            DATA(lx_intern) = CAST zcx_ca_intern( ix_error ).
            IF lx_intern->mv_msgty IS INITIAL.
              lx_intern->mv_msgty = c_msgty_e.
            ENDIF.
            rs_return-type = lx_intern->mv_msgty.

          CATCH cx_sy_move_cast_error.
        ENDTRY.
    ENDTRY.

    "If the message type wasn't delivered by the exception use default value
    IF rs_return-type IS INITIAL.
      rs_return-type = iv_msgty.
    ENDIF.

    CASE lv_is_otr_msg.
      WHEN abap_true.
        "It is a OTR message that has no message number. To be
        "able to send it via message set a common message number
        rs_return-id         = 'S1' ##no_text.
        rs_return-number     = '897'.                 "&1&2&3&4
        "Get OTR message to provide it via the message variables
        rs_return-message    = ix_error->get_text( ).
        rs_return-message_v1 = sy-msgv1 = rs_return-message(50).
        rs_return-message_v2 = sy-msgv2 = rs_return-message+50(50).
        rs_return-message_v3 = sy-msgv3 = rs_return-message+100(50).
        rs_return-message_v4 = sy-msgv4 = rs_return-message+150(50).

      WHEN abap_false.
        rs_return-id         = sy-msgid.
        rs_return-number     = sy-msgno.
        rs_return-message_v1 = sy-msgv1.
        rs_return-message_v2 = sy-msgv2.
        rs_return-message_v3 = sy-msgv3.
        rs_return-message_v4 = sy-msgv4.
    ENDCASE.
  ENDMETHOD.                    "get_msg_details_from_excep


  METHOD set_previous_late.
    "-----------------------------------------------------------------*
    "   Set catched exception after creation of the exception
    "-----------------------------------------------------------------*
    previous = ix_error.
  ENDMETHOD.                    "set_previous_late
ENDCLASS.
