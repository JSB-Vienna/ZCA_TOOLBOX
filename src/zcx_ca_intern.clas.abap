"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Internal exceptions</p>
class ZCX_CA_INTERN definition
  public
  inheriting from CX_NO_CHECK
  create public .

* P U B L I C   S E C T I O N
public section.

*   i n t e r f a c e s
  interfaces IF_ABAP_BEHV_MESSAGE .
  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

*   a l i a s e s
*     Message variables
  aliases MS_DEFAULT_TEXTID
    for IF_T100_MESSAGE~DEFAULT_TEXTID .
  aliases MS_T100KEY
    for IF_T100_MESSAGE~T100KEY .
  aliases MV_MSGTY
    for IF_T100_DYN_MSG~MSGTY .
  aliases MV_MSGV1
    for IF_T100_DYN_MSG~MSGV1 .
  aliases MV_MSGV2
    for IF_T100_DYN_MSG~MSGV2 .
  aliases MV_MSGV3
    for IF_T100_DYN_MSG~MSGV3 .
  aliases MV_MSGV4
    for IF_T100_DYN_MSG~MSGV4 .
  aliases MV_SEVERITY
    for IF_ABAP_BEHV_MESSAGE~M_SEVERITY .
  aliases T_SEVERITY
    for IF_ABAP_BEHV_MESSAGE~T_SEVERITY .

  constants:
    BEGIN OF any_other_msg,
        msgid TYPE symsgid VALUE 'S1',
        msgno TYPE symsgno VALUE '897',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
      END OF any_other_msg .
  constants:
    BEGIN OF assign_comp_failed,
        msgid TYPE symsgid VALUE 'CRM_SCOUT',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF assign_comp_failed .
  constants:
    BEGIN OF assign_val_failed,
        msgid TYPE symsgid VALUE 'CRM_SCOUT',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF assign_val_failed .
  constants:
    BEGIN OF comm_failure,
        msgid TYPE symsgid VALUE 'CMS',
        msgno TYPE symsgno VALUE '212',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
      END OF comm_failure .
  constants:
    BEGIN OF error_func_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_func_call .
  constants:
    BEGIN OF error_meth_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_meth_call .
  constants:
    BEGIN OF error_subr_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_subr_call .
  constants:
    BEGIN OF error_unknown_comp,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_unknown_comp .
  constants:
    BEGIN OF has_no_fixed_vals,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '036',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF has_no_fixed_vals .
  constants:
    BEGIN OF no_struct_no_table,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '034',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_struct_no_table .
  constants:
    BEGIN OF obj_is_not_elementary,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '031',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF obj_is_not_elementary .
  constants:
    BEGIN OF obj_not_ddic_type,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '035',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF obj_not_ddic_type .
  constants:
    BEGIN OF param_invalid,
        msgid TYPE symsgid VALUE 'SRT_WSP2',
        msgno TYPE symsgno VALUE '304',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_invalid .
  constants:
    BEGIN OF param_not_supplied,
        msgid TYPE symsgid VALUE 'SRT_WSP2',
        msgno TYPE symsgno VALUE '306',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF param_not_supplied .
  constants:
    BEGIN OF syst_failure,
        msgid TYPE symsgid VALUE 'CMS',
        msgno TYPE symsgno VALUE '213',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
      END OF syst_failure .
  constants:
    BEGIN OF zcx_ca_intern,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_intern .
  constants:
    BEGIN OF data_creation_failed,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '033',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF data_creation_failed .
  constants:
    BEGIN OF at_least_one,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
      END OF at_least_one .
*   c o n s t a n t s
*     Message types
  constants C_MSGTY_E type IF_ABAP_BEHV_MESSAGE=>T_CHAR01 value IF_ABAP_BEHV_MESSAGE=>SEVERITY-ERROR ##NO_TEXT.
  constants C_MSGTY_W type IF_ABAP_BEHV_MESSAGE=>T_CHAR01 value IF_ABAP_BEHV_MESSAGE=>SEVERITY-WARNING ##NO_TEXT.
  constants C_MSGTY_I type IF_ABAP_BEHV_MESSAGE=>T_CHAR01 value IF_ABAP_BEHV_MESSAGE=>SEVERITY-INFORMATION ##NO_TEXT.
  constants C_MSGTY_S type IF_ABAP_BEHV_MESSAGE=>T_CHAR01 value IF_ABAP_BEHV_MESSAGE=>SEVERITY-SUCCESS ##NO_TEXT.
      "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_INTERN type SEOCLSNAME value 'ZCX_CA_INTERN' ##NO_TEXT.
*   i n s t a n c e   a t t r i b u t e s
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">All messages passed from BAPI call</p>
  data MT_RETURN type BAPIRET2_T read-only .
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Return code</p>
  data MV_SUBRC type SYST_SUBRC read-only .

*      "! <p class="shorttext synchronized" lang="en">Message type</p>
*      mv_msgty  TYPE syst_msgty,
*      "! <p class="shorttext synchronized" lang="en">Message variable 1</p>
*      mv_msgv1  TYPE syst_msgv,
*      "! <p class="shorttext synchronized" lang="en">Message variable 2</p>
*      mv_msgv2  TYPE syst_msgv,
*      "! <p class="shorttext synchronized" lang="en">Message variable 3</p>
*      mv_msgv3  TYPE syst_msgv,
*      "! <p class="shorttext synchronized" lang="en">Message variable 4</p>
*      mv_msgv4  TYPE syst_msgv.
*   s t a t i c   m e t h o d s
      "! <p class="shorttext synchronized" lang="en">Message type</p>
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
  class-methods CREATE_EXCEPTION
    importing
      !IV_EXCP_CLS type SEOCLSNAME default C_ZCX_CA_INTERN
      !IV_FUNCTION type RS38L_FNAM optional
      !IV_CLASS type SEOCLSNAME optional
      !IV_METHOD type SEOCMPNAME optional
      !IV_SUBROUTINE type FORMNAME optional
      !IV_MSGTY type SYST_MSGTY default C_MSGTY_E
      !IS_MSG type DCMESSAGE optional
      !IS_RETURN type BAPIRET2 optional
      !IT_RETURN type BAPIRET2_T optional
      value(IV_SUBRC) type SYSUBRC optional
      !IX_ERROR type ref to CX_ROOT optional
    returning
      value(RX_EXCEP) type ref to ZCX_CA_INTERN .
*   i n s t a n c e   m e t h o d s
      "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYMSGTY optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional
      !MV_SEVERITY type T_SEVERITY optional .
      "! <p class="shorttext synchronized" lang="en">Set catched exception instance later</p>
      "!
      "! @parameter ix_error | <p class="shorttext synchronized" lang="en">Catched exception</p>
* P R O T E C T E D   S E C T I O N
  methods SET_PREVIOUS_LATE
    importing
      !IX_ERROR type ref to CX_ROOT .
  PROTECTED SECTION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
      "! <p class="shorttext synchronized" lang="en">Structure for saving original message</p>
      ms_other_msg TYPE scx_t100key.
ENDCLASS.



CLASS ZCX_CA_INTERN IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MT_RETURN = MT_RETURN .
me->MV_SUBRC = MV_SUBRC .
me->MV_MSGTY = MV_MSGTY .
me->MV_MSGV1 = MV_MSGV1 .
me->MV_MSGV2 = MV_MSGV2 .
me->MV_MSGV3 = MV_MSGV3 .
me->MV_MSGV4 = MV_MSGV4 .
me->MV_SEVERITY = MV_SEVERITY .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_INTERN .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD create_exception.
    "-----------------------------------------------------------------*
    "   Create exception instance of an inheriting exception class
    "   and tries to use the original message. Otherwise a common
    "   message will be used.
    "-----------------------------------------------------------------*
    "Extract message from input
    DATA(ls_return) = zcx_ca_error=>extract_message( iv_msgty  = iv_msgty
                                                     iv_subrc  = iv_subrc
                                                     is_msg    = is_msg
                                                     is_return = is_return
                                                     it_return = it_return
                                                     ix_error  = ix_error ).

    "If no error message was found, e. g. in BAPI messages, leave
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

      ls_return-message_v2 = condense( CONV symsgv( iv_subrc ) ).
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
        mv_msgty  = iv_msgty
        mv_msgv1  = ls_return-message_v1
        mv_msgv2  = ls_return-message_v2
        mv_msgv3  = ls_return-message_v3
        mv_msgv4  = ls_return-message_v4.

    rx_excep->if_abap_behv_message~m_severity = CONV #( ls_return-type ).
  ENDMETHOD.                    "create_exception


  METHOD set_previous_late.
    "-----------------------------------------------------------------*
    "   Set catched exception after creation of the exception
    "-----------------------------------------------------------------*
    previous = ix_error.
  ENDMETHOD.                    "set_previous_late
ENDCLASS.
