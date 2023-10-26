"! <p class="shorttext synchronized" lang="en">CA-TBX exception: UI interaction messages</p>
class ZCX_CA_UI definition
  public
  inheriting from ZCX_CA_ERROR
  create public .

public section.

  constants:
    begin of ZCX_CA_UI,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_UI .
  constants:
    begin of CANC_BY_USER,
      msgid type symsgid value 'SY',
      msgno type symsgno value '556',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CANC_BY_USER .
  constants:
    begin of ACTION_CANCELLED,
      msgid type symsgid value 'SWL',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ACTION_CANCELLED .
  constants:
    begin of SEL_ONLY_ONE_ROW,
      msgid type symsgid value '0K',
      msgno type symsgno value '035',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEL_ONLY_ONE_ROW .
  constants:
    begin of SEL_ONLY_ONE_COL,
      msgid type symsgid value 'MF',
      msgno type symsgno value '701',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEL_ONLY_ONE_COL .
  constants:
    begin of SEL_AL_ONE_ROW,
      msgid type symsgid value '0K',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEL_AL_ONE_ROW .
  constants:
    begin of SEL_AL_ONE_COL,
      msgid type symsgid value '0K',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEL_AL_ONE_COL .
  constants:
    begin of SEL_ROW_NOT_FOUND,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEL_ROW_NOT_FOUND .
  constants:
    begin of FUNC_NOT_SUPPORTED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FUNC_NOT_SUPPORTED .
  constants:
    begin of NO_VALUES_FOUND,
      msgid type symsgid value 'SAPBC_GLOBAL',
      msgno type symsgno value '038',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_VALUES_FOUND .
  constants:
    begin of SEL_COLS_NOT_SUPPORTED,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '084',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SEL_COLS_NOT_SUPPORTED .
  constants:
    begin of PLEASE_SAVE_BEFORE,
      msgid type symsgid value 'SA',
      msgno type symsgno value '706',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of PLEASE_SAVE_BEFORE .
  constants:
    begin of CELL_IS_EMPTY,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '100',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CELL_IS_EMPTY .
  constants:
    begin of DATA_ERRONEOUS_NO_SAVE,
      msgid type symsgid value 'FVD_LOCAC',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DATA_ERRONEOUS_NO_SAVE .
  constants:
    begin of DATA_ERRONEOUS_NO_EXEC,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '101',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DATA_ERRONEOUS_NO_EXEC .
  constants:
    begin of FILL_ALL_REQ_FLDS,
      msgid type symsgid value '00',
      msgno type symsgno value '055',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FILL_ALL_REQ_FLDS .
  constants:
    begin of DATA_SAVED,
      msgid type symsgid value '/BOFU/COMMON',
      msgno type symsgno value '026',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DATA_SAVED .
  "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_UI type SEOCLSNAME value 'ZCX_CA_UI' ##NO_TEXT.

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



CLASS ZCX_CA_UI IMPLEMENTATION.


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
  IF_T100_MESSAGE~T100KEY = ZCX_CA_UI .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
