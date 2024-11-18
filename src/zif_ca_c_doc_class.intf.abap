"! <p class="shorttext synchronized" lang="en">CA-TBX: OBSOLETE!! Use ZCL_CA_C_TEXT_PREPARATION instead</p>
INTERFACE zif_ca_c_doc_class PUBLIC.
* c o n s t a n t s
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Text format: HTML</p>
    c_docclass_htm TYPE zca_d_doc_class        VALUE 'HTM'  ##no_text,
    "! <p class="shorttext synchronized" lang="en">Text format: Text</p>
    c_docclass_raw TYPE zca_d_doc_class        VALUE 'RAW'  ##no_text.
ENDINTERFACE.
