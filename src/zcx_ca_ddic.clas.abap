"! <p class="shorttext synchronized" lang="en">CA-TBX exception: DDIC determination failed</p>
class ZCX_CA_DDIC definition
  public
  inheriting from ZCX_CA_PARAM
  create public .

public section.

  constants:
    begin of ZCX_CA_DDIC,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '110',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_DDIC .
  constants:
    begin of METHOD_NOT_ALLOWED_FOR_KIND,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '111',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of METHOD_NOT_ALLOWED_FOR_KIND .
  constants:
    begin of NAME_NEEDED_FOR_STRUCT_TABLE,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '112',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NAME_NEEDED_FOR_STRUCT_TABLE .
  constants:
    begin of OBJECT_NOT_FOUND,
      msgid type symsgid value 'E0',
      msgno type symsgno value '504',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OBJECT_NOT_FOUND .
    "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_DDIC type SEOCLSNAME value 'ZCX_CA_DDIC' ##NO_TEXT.

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
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_CA_DDIC IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous    = previous
        mt_return   = mt_return
        mv_subrc    = mv_subrc
        mv_msgty    = mv_msgty
        mv_msgv1    = mv_msgv1
        mv_msgv2    = mv_msgv2
        mv_msgv3    = mv_msgv3
        mv_msgv4    = mv_msgv4
        mv_severity = mv_severity.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = zcx_ca_ddic .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
