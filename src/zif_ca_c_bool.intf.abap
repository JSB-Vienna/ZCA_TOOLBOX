"! <p class="shorttext synchronized" lang="en">CA-TBX: OBOSLETE! Use ZCL_CA_C_NUMERIC_BOOLEAN!</p>
INTERFACE zif_ca_c_bool PUBLIC.
*   c o n s t a n t s
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Boolean value: True</p>
    c_true      TYPE dml_boolean VALUE '1' ##no_text,
    "! <p class="shorttext synchronized" lang="en">Boolean value: False</p>
    c_false     TYPE dml_boolean VALUE '0' ##no_text,
    "! <p class="shorttext synchronized" lang="en">Boolean value: Undefined</p>
    c_undefined TYPE dml_boolean VALUE ' ' ##no_text.
ENDINTERFACE.
