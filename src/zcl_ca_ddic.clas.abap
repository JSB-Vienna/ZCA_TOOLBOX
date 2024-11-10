"! <p class="shorttext synchronized" lang="en">CA-TBX: Check of / informations to data objects</p>
CLASS zcl_ca_ddic DEFINITION
  PUBLIC
  CREATE PUBLIC .

* P U B L I C   S E C T I O N
  PUBLIC SECTION.

*   i n t e r f a c e s
    INTERFACES if_xo_const_message .

*   i n s t a n c e   a t t r i b u t e s
*     o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">RTTI type description</p>
    DATA mo_type_desc TYPE REF TO cl_abap_typedescr READ-ONLY .

*   i n s t a n c e   m e t h o d s
    "! <p class="shorttext synchronized" lang="en">Constructor for RTTI type description instance</p>
    "!
    "! @parameter iv_name       | <p class="shorttext synchronized" lang="en">Object name (DDIC or class / interface)</p>
    "! @parameter iv_data       | <p class="shorttext synchronized" lang="en">Data field / value</p>
    "! @parameter ir_data       | <p class="shorttext synchronized" lang="en">Reference of a data object / value</p>
    "! @parameter io_object     | <p class="shorttext synchronized" lang="en">Reference of an object (class / interface)</p>
    "! @parameter iv_param_name | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
    "! @parameter iv_langu      | <p class="shorttext synchronized" lang="en">Language key for DDIC text, e.g. for field labels</p>
    "! @raising   zcx_ca_param  | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
    METHODS constructor
      IMPORTING
        !iv_name       TYPE csequence OPTIONAL
        !iv_data       TYPE data OPTIONAL
        !ir_data       TYPE REF TO data OPTIONAL
        !io_object     TYPE REF TO object OPTIONAL
        !iv_param_name TYPE csequence OPTIONAL
        !iv_langu      TYPE sylangu DEFAULT sy-langu
      RAISING
        zcx_ca_param .
    "! <p class="shorttext synchronized" lang="en">Validate value against fixed values of a domain</p>
    "!
    "! @parameter iv_value       | <p class="shorttext synchronized" lang="en">Value under test</p>
    "! @parameter iv_raise_excep | <p class="shorttext synchronized" lang="en">X = Raise exception when invalid</p>
    "! @parameter result         | <p class="shorttext synchronized" lang="en">X = Value is valid</p>
    "! @raising   zcx_ca_param   | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
    METHODS check_fixed_values
      IMPORTING
        !iv_value       TYPE simple
        !iv_raise_excep TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result)   TYPE abap_bool
      RAISING
        zcx_ca_param .
    "! <p class="shorttext synchronized" lang="en">Return a list with type descriptions for a structure/table</p>
    "!
    "! @parameter iv_level     | <p class="shorttext synchronized" lang="en">Level of resolution of the struc. (9 = deepest resolution)</p>
    "! @parameter result       | <p class="shorttext synchronized" lang="en">Component list to requested structure or table</p>
    "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
    METHODS get_component_list
      IMPORTING
        !iv_level     TYPE i DEFAULT 9
      RETURNING
        VALUE(result) TYPE abap_component_view_tab
      RAISING
        zcx_ca_param .
    "! <p class="shorttext synchronized" lang="en">Determine field label from DDIC</p>
    "!
    "! @parameter iv_name      | <p class="shorttext synchronized" lang="en">Object name (DDIC or class / interface)</p>
    "! @parameter result       | <p class="shorttext synchronized" lang="en">Short description and labels of data element</p>
    "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
    METHODS get_field_label_from_ddic
      IMPORTING
        !iv_name      TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE dd04tv
      RAISING
        zcx_ca_param .
    "! <p class="shorttext synchronized" lang="en">Return field list for a Structure/(internal) table (DFIES)</p>
    "!
    "! @parameter result       | <p class="shorttext synchronized" lang="en">Field list (DFIES)</p>
    "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
    METHODS get_field_list
      RETURNING
        VALUE(result) TYPE ddfields
      RAISING
        zcx_ca_param .
    "! <p class="shorttext synchronized" lang="en">Determination of fixed values to a table field/data element</p>
    "!
    "! @parameter result       | <p class="shorttext synchronized" lang="en">Fixed values</p>
    "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
    METHODS get_fixed_values
      RETURNING
        VALUE(result) TYPE ddfixvalues
      RAISING
        zcx_ca_param .
    METHODS get_type_descr
      RETURNING
        VALUE(rr_type_descr) TYPE REF TO cl_abap_typedescr .


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Language for texts of DDIC objects</p>
      mv_langu      TYPE syst_langu,
      "! <p class="shorttext synchronized" lang="en">Name of checked parameter for error messages</p>
      mv_param_name TYPE syst_msgv.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get RTTI description by name</p>
      "!
      "! @parameter iv_name      | <p class="shorttext synchronized" lang="en">Object name (DDIC or class / interface)</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_by_name
        IMPORTING
          iv_name TYPE csequence
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Get RTTI description by data object</p>
      "!
      "! @parameter iv_data      | <p class="shorttext synchronized" lang="en">Data field / value</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_by_data
        IMPORTING
          iv_data TYPE data
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Get RTTI description by data reference</p>
      "!
      "! @parameter ir_data      | <p class="shorttext synchronized" lang="en">Reference of a data object / value</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_by_data_reference
        IMPORTING
          ir_data TYPE REF TO data
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Get RTTI description by an object reference (class/interf.)</p>
      "!
      "! @parameter io_object    | <p class="shorttext synchronized" lang="en">Reference of an object (class/interface)</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_by_object
        IMPORTING
          io_object TYPE REF TO object
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Get structured RTTI type description</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">RTTI structure description</p>
      get_descr_of_struct_type
        RETURNING
          VALUE(result) TYPE REF TO cl_abap_structdescr,

      "! <p class="shorttext synchronized" lang="en">Set technical values of elementary non-DDIC type</p>
      "!
      "! @parameter struc_descr  | <p class="shorttext synchronized" lang="en">RTTI type description a structured type</p>
      "! @parameter struc_comp   | <p class="shorttext synchronized" lang="en">Single component of structure to be analyzed</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Technical values of current component</p>
      set_tech_vals_of_elem_non_ddic
        IMPORTING
          struc_descr   TYPE REF TO cl_abap_structdescr
          struc_comp    TYPE REF TO abap_simple_componentdescr
        RETURNING
          VALUE(result) TYPE dfies,

      "! <p class="shorttext synchronized" lang="en">Set technical values of complex types</p>
      "!
      "! @parameter struc_descr  | <p class="shorttext synchronized" lang="en">RTTI type description a structured type</p>
      "! @parameter struc_comp   | <p class="shorttext synchronized" lang="en">Single component of structure to be analyzed</p>
      "! @parameter field_pos    | <p class="shorttext synchronized" lang="en">Field position in the structure</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Technical values of current component</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      set_tech_vals_of_complex_types
        IMPORTING
          struc_descr   TYPE REF TO cl_abap_structdescr
          struc_comp    TYPE REF TO abap_simple_componentdescr
        CHANGING
          field_pos     TYPE tabfdpos
        RETURNING
          VALUE(result) TYPE dfies
        RAISING
          zcx_ca_param.

ENDCLASS.



CLASS zcl_ca_ddic IMPLEMENTATION.

  METHOD check_fixed_values.
    "-----------------------------------------------------------------*
    "   Check a value against defined fixed values of a domain
    "-----------------------------------------------------------------*
    DATA(lo_sel_options) = zcl_ca_c_sel_options=>get_instance( ).
    DATA(fixed_values) = get_fixed_values( ).

    "Check if value fit into fixed values of domain
    result = abap_false.
    LOOP AT fixed_values REFERENCE INTO DATA(fixed_value).
      CASE fixed_value->option.
        WHEN lo_sel_options->option-eq.
          IF iv_value EQ fixed_value->low.
            result = abap_true.
          ENDIF.

        WHEN lo_sel_options->option-bt.
          IF iv_value BETWEEN fixed_value->low AND fixed_value->high.
            result = abap_true.
          ENDIF.
      ENDCASE.

      IF result EQ abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    "Raise exception if is requested
    IF iv_raise_excep EQ abap_true   AND
       result         EQ abap_false.
      WRITE iv_value LEFT-JUSTIFIED TO sy-msgv2.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = mv_param_name
          mv_msgv2 = sy-msgv2.
    ENDIF.
  ENDMETHOD.                    "check_fixed_values


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor for RTTI type description instance
    "-----------------------------------------------------------------*
    IF iv_name IS NOT INITIAL.
      get_by_name( iv_name ).

    ELSEIF iv_data IS SUPPLIED.
      get_by_data( iv_data ).

    ELSEIF ir_data IS BOUND.
      get_by_data_reference( ir_data ).


    ELSEIF io_object IS BOUND.
      get_by_object( io_object ).

    ELSE.
      "At least one of the following parameters must be set: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_NAME'
          mv_msgv2 = 'IV_DATA'
          mv_msgv3 = 'IR_DATA'
          mv_msgv4 = 'IR_OBJECT' ##no_text.
    ENDIF.

    "Set parameter name if it is not set by caller
    IF iv_param_name IS SUPPLIED AND
       iv_param_name IS NOT INITIAL.
      mv_param_name = iv_param_name.
    ELSEIF iv_name IS SUPPLIED.
      mv_param_name = 'IV_NAME' ##no_text.
    ELSEIF iv_data IS SUPPLIED.
      mv_param_name = 'IV_DATA' ##no_text.
    ELSEIF ir_data IS SUPPLIED.
      mv_param_name = 'IR_DATA' ##no_text.
    ELSEIF ir_data IS SUPPLIED.
      mv_param_name = 'IO_OBJECT' ##no_text.
    ENDIF.

    mv_langu = iv_langu.
  ENDMETHOD.                    "constructor


  METHOD get_by_name.
    "-----------------------------------------------------------------*
    "   Get RTTI description by name
    "-----------------------------------------------------------------*
    cl_abap_typedescr=>describe_by_name(
                                    EXPORTING
                                      p_name         = iv_name
                                    RECEIVING
                                      p_descr_ref    = mo_type_desc
                                    EXCEPTIONS
                                      type_not_found = 1
                                      OTHERS         = 2 ).
    CASE sy-subrc.
      WHEN 0.
        "Everything is fine

      WHEN 1.
        "Parameter '&1' has invalid value '&2'
        RAISE EXCEPTION TYPE zcx_ca_param
          EXPORTING
            textid   = zcx_ca_param=>param_invalid
            mv_msgty = c_msgty_e
            mv_msgv1 = 'IV_NAME' ##no_text
            mv_msgv2 = CONV #( iv_name ).

      WHEN OTHERS.
        DATA(lx_error) = zcx_ca_intern=>create_exception(
                                      iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                      iv_class    = 'CL_ABAP_TYPEDESCR'
                                      iv_method   = 'DESCRIBE_BY_NAME'
                                      iv_subrc    = sy-subrc ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "get_by_name


  METHOD get_by_data_reference.
    "-----------------------------------------------------------------*
    "   Get RTTI description by data reference
    "-----------------------------------------------------------------*
    cl_abap_typedescr=>describe_by_data_ref(
                                        EXPORTING
                                          p_data_ref           = ir_data
                                        RECEIVING
                                          p_descr_ref          = mo_type_desc
                                        EXCEPTIONS
                                          reference_is_initial = 1
                                          OTHERS               = 2 ).
    CASE sy-subrc.
      WHEN 0.
        "Everything is fine

      WHEN 1.
        "Parameter '&1' has invalid value '&2'
        RAISE EXCEPTION TYPE zcx_ca_param
          EXPORTING
            textid   = zcx_ca_param=>param_invalid
            mv_msgty = c_msgty_e
            mv_msgv1 = 'IR_DATA' ##no_text
            mv_msgv2 = 'Data reference'(drf).

      WHEN OTHERS.
        DATA(lx_error) = zcx_ca_intern=>create_exception(
                                        iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                        iv_class    = 'CL_ABAP_TYPEDESCR'
                                        iv_method   = 'DESCRIBE_BY_DATA_REF'
                                        iv_subrc    = sy-subrc ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "get_by_data_reference


  METHOD get_by_object.
    "-----------------------------------------------------------------*
    "   Get RTTI description by an object reference (class/interface)
    "-----------------------------------------------------------------*
    cl_abap_typedescr=>describe_by_object_ref(
                                        EXPORTING
                                          p_object_ref         = io_object
                                        RECEIVING
                                          p_descr_ref          = mo_type_desc
                                        EXCEPTIONS
                                          reference_is_initial = 1
                                          OTHERS               = 2 ).
    CASE sy-subrc.
      WHEN 0.
        "Everything is fine

      WHEN 1.
        "Parameter '&1' has invalid value '&2'
        RAISE EXCEPTION TYPE zcx_ca_param
          EXPORTING
            textid   = zcx_ca_param=>param_invalid
            mv_msgty = c_msgty_e
            mv_msgv1 = 'IO_OBJECT' ##no_text
            mv_msgv2 = 'Object reference'(orf).

      WHEN OTHERS.
        DATA(lx_error) = zcx_ca_intern=>create_exception(
                                       iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                       iv_class    = 'CL_ABAP_TYPEDESCR'
                                       iv_method   = 'DESCRIBE_BY_OBJECT_REF'
                                       iv_subrc    = sy-subrc ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "get_by_object


  METHOD get_by_data.
    "-----------------------------------------------------------------*
    "   Get RTTI description by data object
    "-----------------------------------------------------------------*
    mo_type_desc = cl_abap_typedescr=>describe_by_data( iv_data ).
  ENDMETHOD.                    "get_by_data


  METHOD get_component_list.
    "-----------------------------------------------------------------*
    "   Get components of a structured object, also tables
    "-----------------------------------------------------------------*
    "Get components of structure resolving includes down to given level
    result = get_descr_of_struct_type( )->get_included_view( iv_level ).
  ENDMETHOD.                    "get_component_list


  METHOD get_descr_of_struct_type.
    "-----------------------------------------------------------------*
    "   Get structured RTTI type description
    "-----------------------------------------------------------------*
    "If object is a table determine structure definition
    CASE mo_type_desc->type_kind.
      WHEN mo_type_desc->typekind_table.
        "It is a table
        DATA(table_descr) = CAST cl_abap_tabledescr( mo_type_desc ).
        result ?= table_descr->get_table_line_type( ).

      WHEN mo_type_desc->typekind_struct1 OR
           mo_type_desc->typekind_struct2.
        "It is a flat or complex structure
        result ?= mo_type_desc.

      WHEN OTHERS.
        "Object (in parameter) &1 is no structure and no table
        RAISE EXCEPTION TYPE zcx_ca_intern
          EXPORTING
            textid   = zcx_ca_intern=>no_struct_no_table
            mv_msgty = c_msgty_e
            mv_msgv1 = mv_param_name.
    ENDCASE.
  ENDMETHOD.                    "get_descr_of_struct_type


  METHOD get_field_list.
    "-----------------------------------------------------------------*
    "   Get field list of a structured object, also tables
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      techn_fld_descr TYPE dfies,
      field_pos       TYPE tabfdpos.

    DATA(struc_descr) = get_descr_of_struct_type( ).

    CASE struc_descr->is_ddic_type( ).
      WHEN abap_true.
        struc_descr->get_ddic_field_list(
                                      EXPORTING
                                        p_langu      = mv_langu
                                      RECEIVING
                                        p_field_list = result
                                      EXCEPTIONS
                                        not_found    = 1
                                        no_ddic_type = 2
                                        OTHERS       = 3 ).
        IF sy-subrc NE 0.
          DATA(lx_error) = zcx_ca_intern=>create_exception(
                                              iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                              iv_class    = 'CL_ABAP_STRUCTDESCR'
                                              iv_method   = 'GET_DDIC_FIELD_LIST'
                                              iv_subrc    = sy-subrc ) ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
        ENDIF.

      WHEN abap_false.
        "It is NOT a DDIC type - prepare DFIES list from single components
        "Resolve structure with this method to get also components of includes
        DATA(struc_comps) = struc_descr->get_included_view( 9 ).
        LOOP AT struc_comps REFERENCE INTO DATA(struc_comp).
          CLEAR techn_fld_descr.
          "Set position
          ADD 1 TO field_pos.
          techn_fld_descr-position = field_pos.

          "Elementary types
          IF struc_comp->type->kind EQ struc_comp->type->kind_elem.
            CASE struc_comp->type->is_ddic_type( ).
              WHEN abap_true.
                "D D I C   t y p e
                "Cast data description into elementary description
                DATA(lo_elem_desc) = CAST cl_abap_elemdescr( struc_comp->type ).
                "This method return the data element name in TABNAME ...
                techn_fld_descr = lo_elem_desc->get_ddic_field( ).
                "... so it will be corrected here
                techn_fld_descr-tabname   = struc_descr->get_relative_name( ).
                techn_fld_descr-fieldname = struc_comp->name.
                techn_fld_descr-comptype  = 'E' ##no_text.     " overwrite value of method

              WHEN abap_false.
                "L o c a l   t y p e  - set only available values
                techn_fld_descr = set_tech_vals_of_elem_non_ddic( struc_descr = struc_descr
                                                                      struc_comp  = struc_comp ).
            ENDCASE.

          ELSE.
            techn_fld_descr = set_tech_vals_of_complex_types(
                                                           EXPORTING
                                                             struc_descr = struc_descr
                                                             struc_comp  = struc_comp
                                                           CHANGING
                                                             field_pos   = field_pos ).
          ENDIF.

          "Attach component to fields list
          APPEND techn_fld_descr TO result.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.                    "get_field_list


  METHOD get_fixed_values.
    "-----------------------------------------------------------------*
    "   Get fixed values including descriptions of a domain
    "-----------------------------------------------------------------*
    "Is of kind elementary?
    IF mo_type_desc->kind NE mo_type_desc->kind_elem.
      "Parameter &1 is not typed as elementary type
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>obj_is_not_elementary
          mv_msgty = c_msgty_e
          mv_msgv1 = mv_param_name.
    ENDIF.

    "Is DDIC type?
    IF mo_type_desc->is_ddic_type( ) EQ abap_false.
      "Object (in parameter) &1 is not defined in DDIC
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>obj_not_ddic_type
          mv_msgty = c_msgty_e
          mv_msgv1 = mv_param_name.
    ENDIF.

    "Get fixed values
    DATA(element_descr) = CAST cl_abap_elemdescr( mo_type_desc ).
    result = element_descr->get_ddic_fixed_values( ).
    IF result IS INITIAL.
      "Object (in parameter) &1 has no fixed values - has no or is no domain
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>has_no_fixed_vals
          mv_msgty = c_msgty_e
          mv_msgv1 = mv_param_name.
    ENDIF.
  ENDMETHOD.                    "get_fixed_values


  METHOD get_field_label_from_ddic.
    "-----------------------------------------------------------------*
    "   Determine field label from DDIC
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_element           TYPE REF TO cl_abap_elemdescr.

    IF mo_type_desc->kind NE mo_type_desc->kind_elem AND
       iv_name            IS INITIAL.
      "Name must be supplied for non-elementary objects
      RAISE EXCEPTION TYPE zcx_ca_ddic
        EXPORTING
          textid   = zcx_ca_ddic=>name_needed_for_struct_table
          mv_msgty = zcx_ca_ddic=>c_msgty_e.
    ENDIF.

    CASE mo_type_desc->kind.
      WHEN mo_type_desc->kind_elem.
        lo_element ?= mo_type_desc.

      WHEN mo_type_desc->kind_struct OR
           mo_type_desc->kind_table.
        DATA(lt_components) = get_component_list( ).
        lo_element ?= VALUE #( lt_components[ name = iv_name ]-type OPTIONAL ).

      WHEN OTHERS.
        "This method is only allowed for elementary, structured or table objects
        RAISE EXCEPTION TYPE zcx_ca_ddic
          EXPORTING
            textid   = zcx_ca_ddic=>method_not_allowed_for_kind
            mv_msgty = zcx_ca_ddic=>c_msgty_e.
    ENDCASE.

    IF lo_element IS NOT BOUND.
      "Object & not found
      RAISE EXCEPTION TYPE zcx_ca_ddic
        EXPORTING
          textid   = zcx_ca_ddic=>object_not_found
          mv_msgty = zcx_ca_ddic=>c_msgty_e
          mv_msgv1 = iv_name.
    ENDIF.

    lo_element->get_ddic_field(
                            EXPORTING
                              p_langu      = sy-langu
                            RECEIVING
                              p_flddescr   = DATA(ls_ddic_description)
                            EXCEPTIONS
                              not_found    = 1
                              no_ddic_type = 2
                              OTHERS       = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_ddic( zcx_ca_error=>create_exception(
                                                         iv_excp_cls = zcx_ca_ddic=>c_zcx_ca_ddic
                                                         iv_class    = 'CL_ABAP_ELEMDESCR'
                                                         iv_method   = 'GET_DDIC_FIELD'
                                                         iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    result-ddlanguage = ls_ddic_description-langu.
    result-ddtext     = ls_ddic_description-fieldtext.
    result-reptext    = ls_ddic_description-reptext.
    result-rollname   = ls_ddic_description-rollname.
    result-scrtext_l  = ls_ddic_description-scrtext_l.
    result-scrtext_m  = ls_ddic_description-scrtext_m.
    result-scrtext_s  = ls_ddic_description-scrtext_s.
  ENDMETHOD.                    "get_field_label_from_ddic


  METHOD set_tech_vals_of_complex_types.
    "-----------------------------------------------------------------*
    "   Set technical values of complex types
    "-----------------------------------------------------------------*
    result-tabname   = struc_descr->get_relative_name( ).
    result-fieldname = struc_comp->name.
    result-langu     = mv_langu.
    result-inttype   = struc_comp->type->type_kind.
    result-intlen    = struc_comp->type->length.

    "Set component type and set ROLLNAME
    CASE struc_comp->type->kind.
      WHEN struc_comp->type->kind_ref.
        result-comptype = 'R' ##no_text.
        result-genkey   = abap_true.

      WHEN struc_comp->type->kind_table.
        DATA(table_descr_of_column) = CAST cl_abap_tabledescr( struc_comp->type ).
        result-comptype = 'L' ##no_text.
        result-rollname = table_descr_of_column->get_relative_name( ).

      WHEN struc_comp->type->kind_struct.
        DATA(struc_descr_of_column) = CAST cl_abap_tabledescr( struc_comp->type ).
        result-comptype   = 'S' ##no_text.
        result-rollname   = struc_descr_of_column->get_relative_name( ).
        DATA(struc_comps) = NEW zcl_ca_ddic( iv_name = result-rollname )->get_field_list( ).
        field_pos         = field_pos + lines( struc_comps ).
    ENDCASE.
  ENDMETHOD.                    "set_tech_vals_of_complex_types


  METHOD set_tech_vals_of_elem_non_ddic.
    "-----------------------------------------------------------------*
    "   Set technical values of elementary non-DDIC type
    "-----------------------------------------------------------------*
    "Cast data description into elementary description
    DATA(elementary_descr) = CAST cl_abap_elemdescr( struc_comp->type ).

    result-tabname   = struc_descr->get_relative_name( ).
    result-fieldname = struc_comp->name.
    result-langu     = mv_langu.
    result-leng      = elementary_descr->length / cl_abap_char_utilities=>charsize.
    result-intlen    = elementary_descr->length.
    result-outputlen = elementary_descr->output_length.
    result-decimals  = elementary_descr->decimals.
*    result-datatype  = lo_elem_desc->?
    result-inttype   = elementary_descr->type_kind.
    result-mask      = elementary_descr->edit_mask.
    result-masklen   = strlen( elementary_descr->edit_mask ).
    result-dynpfld   = abap_true.

    "The first three types are new and not available in target systems
    IF "elementary_descr->type_kind EQ elementary_descr->typekind_decfloat   OR
       "elementary_descr->type_kind EQ elementary_descr->typekind_decfloat16 OR
       "elementary_descr->type_kind EQ elementary_descr->typekind_decfloat34 OR
       elementary_descr->type_kind EQ elementary_descr->typekind_float      OR
       elementary_descr->type_kind EQ elementary_descr->typekind_packed     OR
       elementary_descr->type_kind EQ elementary_descr->typekind_int        OR " INT4.
       elementary_descr->type_kind EQ elementary_descr->typekind_int2       OR
       elementary_descr->type_kind EQ elementary_descr->typekind_int1.
      result-sign = abap_true.
    ENDIF.
  ENDMETHOD.                    "set_tech_vals_of_elem_non_ddic


  METHOD get_type_descr.

    IF mo_type_desc IS BOUND.
      rr_type_descr = mo_type_desc.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
