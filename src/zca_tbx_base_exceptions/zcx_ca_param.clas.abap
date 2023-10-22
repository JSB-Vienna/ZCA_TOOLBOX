"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
class ZCX_CA_PARAM definition
  public
  inheriting from ZCX_CA_ERROR
  create public .

public section.

  constants:
    begin of AT_LEAST_ONE,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '007',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value 'MV_MSGV4',
    end of AT_LEAST_ONE .
  constants:
    begin of COMM_FAILURE,
      msgid type symsgid value 'CMS',
      msgno type symsgno value '212',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value 'MV_MSGV4',
    end of COMM_FAILURE .
  constants:
    begin of PARAM_INVALID,
      msgid type symsgid value 'SRT_WSP2',
      msgno type symsgno value '304',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PARAM_INVALID .
  constants:
    begin of PARAM_NOT_SUPPLIED,
      msgid type symsgid value 'SRT_WSP2',
      msgno type symsgno value '306',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PARAM_NOT_SUPPLIED .
  constants:
    begin of SYST_FAILURE,
      msgid type symsgid value 'CMS',
      msgno type symsgno value '213',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value 'MV_MSGV4',
    end of SYST_FAILURE .
  constants:
    begin of ZCX_CA_PARAM,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_PARAM .
  constants:
    begin of NO_AUTHORITY,
      msgid type symsgid value '06',
      msgno type symsgno value '199',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value 'MV_MSGV4',
    end of NO_AUTHORITY .
  "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_PARAM type SEOCLSNAME value 'ZCX_CA_PARAM' ##NO_TEXT.

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
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_PARAM IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
MV_SEVERITY = MV_SEVERITY
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_PARAM .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
