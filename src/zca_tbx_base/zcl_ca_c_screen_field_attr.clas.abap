"! <p class="shorttext synchronized" lang="en">CA-TBX: Screen field attributes (usage with table SCREEN)</p>
CLASS zcl_ca_c_screen_field_attr DEFINITION PUBLIC
                                            CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Line type to range type for screen field name</p>
      BEGIN OF ty_s_requested_scr_field_name,
        sign   TYPE ddsign,
        option TYPE ddoption,
        low    TYPE dynfnam,
        high   TYPE dynfnam,
      END   OF ty_s_requested_scr_field_name,
      "! <p class="shorttext synchronized" lang="en">Range type for screen field names</p>
      ty_requested_scr_field_names TYPE RANGE OF dynfnam,

      "! <p class="shorttext synchronized" lang="en">Line type to range type for screen field name</p>
      BEGIN OF ty_s_requested_scr_modif_group,
        sign   TYPE ddsign,
        option TYPE ddoption,
        low    TYPE char3,
        high   TYPE char3,
      END   OF ty_s_requested_scr_modif_group,
      "! <p class="shorttext synchronized" lang="en">Range type for screen modification groups</p>
      ty_requested_scr_modif_groups TYPE RANGE OF char3.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Screen input control</p>
      BEGIN OF input_control,
        "! <p class="shorttext synchronized" lang="en">Field input control: Input is possible</p>
        possible    TYPE scrffobl VALUE ' ' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Field input control: Input not allowed (display)</p>
        no_input    TYPE scrffobl VALUE 'N' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Field input control: Input is required / obligatory</p>
        obligatory  TYPE scrffobl VALUE 'O' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Field input control: Input is recommended</p>
        recommended TYPE scrffobl VALUE 'R' ##no_text,
      END OF input_control,

      "! <p class="shorttext synchronized" lang="en">Screen attribute switches for table SCREEN</p>
      BEGIN OF switch,
        "! <p class="shorttext synchronized" lang="en">Screen attribute switch: Off</p>
        off               TYPE num1 VALUE '0' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute switch: On</p>
        on                TYPE num1 VALUE '1' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute: Display input field as recommended</p>
        is_recommended    TYPE num1 VALUE '2' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute: Display F4 button always</p>
        display_f4_always TYPE num1 VALUE '2' ##no_text,
      END OF switch,

      "! <p class="shorttext synchronized" lang="en">Column names (= attributes) of table SCREEN</p>
      BEGIN OF screen_attr,
        "! <p class="shorttext synchronized" lang="en">Screen attribute name: Active</p>
        active      TYPE fieldname VALUE 'ACTIVE' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute name: Display 3d</p>
        display_3d  TYPE fieldname VALUE 'DISPLAY_3D' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute name: Input</p>
        input       TYPE fieldname VALUE 'INPUT' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute name: Intensified</p>
        intensified TYPE fieldname VALUE 'INTENSIFIED' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute name: Invisible</p>
        invisible   TYPE fieldname VALUE 'INVISIBLE' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute name: Output</p>
        output      TYPE fieldname VALUE 'OUTPUT' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute name: Request</p>
        request     TYPE fieldname VALUE 'REQUEST' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Screen attribute name: Required</p>
        required    TYPE fieldname VALUE 'REQUIRED' ##no_text,
      END OF screen_attr,

      "! <p class="shorttext synchronized" lang="en">Boolean operator for IF statement</p>
      BEGIN OF boolean_operator,
        "! <p class="shorttext synchronized" lang="en">Boolean operator: OR</p>
        or  TYPE num1 VALUE '1' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Boolean operator: AND</p>
        and TYPE num1 VALUE '2' ##no_text,
      END OF boolean_operator.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for select options / range tables</p>
      sel_options          TYPE REF TO zcl_ca_c_sel_options READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_screen_field_attr.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Activate screen field</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      activate
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Create entry for screen field name for adding to RANGE table</p>
      "!
      "! @parameter incl_or_excl      | <p class="shorttext synchronized" lang="en">Including or excluding passed value</p>
      "! @parameter screen_field_name | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Entry with screen field name for RANGE table</p>
      create_range_entry_for_fname
        IMPORTING
          incl_or_excl      TYPE ddsign DEFAULT zcl_ca_c_sel_options=>sign-incl
          screen_field_name TYPE dynfnam OPTIONAL
        RETURNING
          VALUE(result)     TYPE ty_s_requested_scr_field_name,

      "! <p class="shorttext synchronized" lang="en">Create entry for scr. modif group for adding to RANGE table</p>
      "!
      "! @parameter incl_or_excl       | <p class="shorttext synchronized" lang="en">Including or excluding passed value</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Screen modification group (generic value is possible)</p>
      "! @parameter result             | <p class="shorttext synchronized" lang="en">Entry with screen modification group for RANGE table</p>
      create_range_entry_for_mod_grp
        IMPORTING
          incl_or_excl       TYPE ddsign DEFAULT zcl_ca_c_sel_options=>sign-incl
          screen_modif_group TYPE char3 OPTIONAL
        RETURNING
          VALUE(result)      TYPE ty_s_requested_scr_modif_group,

      "! <p class="shorttext synchronized" lang="en">Deactivate screen field</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      deactivate
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Get actual program Id and dynpro number from system program</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Program name and dynpro number</p>
      get_actual_screen
        RETURNING
          VALUE(result) TYPE /aif/extdynpro,

      "! <p class="shorttext synchronized" lang="en">Hide screen field</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      hide
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Display screen field</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      make_visible
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Modify screen field attributes</p>
      "!
      "! @parameter req_screen_field_names  | <p class="shorttext synchronized" lang="en">Range table for requested screen field (generic) names</p>
      "! @parameter req_screen_modif_groups | <p class="shorttext synchronized" lang="en">Range table for (generic) screen modification group values</p>
      "! @parameter boolean_operator        | <p class="shorttext synchronized" lang="en">Boolean operator OR or AND to combine field name and group</p>
      "! @parameter attribute_name          | <p class="shorttext synchronized" lang="en">Name of a column in structure SCREEN -&gt; use SCREEN_ATTR-*</p>
      "! @parameter attribute_value         | <p class="shorttext synchronized" lang="en">Switch value -&gt; use SWITCH-*</p>
      modify_attributes
        IMPORTING
          req_screen_field_names  TYPE ty_requested_scr_field_names OPTIONAL
          req_screen_modif_groups TYPE ty_requested_scr_modif_groups OPTIONAL
          boolean_operator        TYPE num1 DEFAULT boolean_operator-or
          attribute_name          TYPE fieldname
          attribute_value         TYPE simple,

      "! <p class="shorttext synchronized" lang="en">Set dropdown values for specific selection parameter</p>
      "!
      "! @parameter sel_param_name | <p class="shorttext synchronized" lang="en">Name of selection parameter</p>
      "! @parameter dropdown_list  | <p class="shorttext synchronized" lang="en">Value list for dropdown</p>
      "! @raising   zcx_ca_param   | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      set_dd_list_for_field
        IMPORTING
          sel_param_name TYPE dynfnam
          dropdown_list  TYPE vrm_values
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Set number of modification group for next call of this class</p>
      "!
      "! @parameter screen_modif_group_id | <p class="shorttext synchronized" lang="en">Id of screen modification group that should be changed</p>
      set_modification_group_id
        IMPORTING
          screen_modif_group_id TYPE numc1 DEFAULT '1',

      "! <p class="shorttext synchronized" lang="en">Set screen field as obligatory input</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      set_as_obligatory
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Set screen field for possible input (= not obligatory)</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      set_as_possible
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Set screen field as required input</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      set_as_recommended
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Open screen field for input</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      set_for_input
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Open screen field for output</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      set_for_output
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Set screen field to display only</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      set_to_display_only
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Set screen field to no output</p>
      "!
      "! @parameter screen_field_name  | <p class="shorttext synchronized" lang="en">Name of screen field (generic value is possible)</p>
      "! @parameter screen_modif_group | <p class="shorttext synchronized" lang="en">Value of screen modification group (gen. value is possible)</p>
      set_to_no_output
        IMPORTING
          screen_field_name  TYPE dynfnam OPTIONAL
          screen_modif_group TYPE char3   OPTIONAL.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Backup SCREEN settings for deactivation/reactivation</p>
      BEGIN OF ty_s_screen_backuup.
        INCLUDE TYPE /aif/extdynpro AS s_screen_id.
    TYPES:
        t_screen TYPE /scf/dynpro_screen_tab,
      END   OF ty_s_screen_backuup,
      "! <p class="shorttext synchronized" lang="en">Backups SCREEN settings for deactivation/reactivation</p>
      ty_t_screen_backups TYPE SORTED TABLE OF ty_s_screen_backuup
                                            WITH UNIQUE KEY s_screen_id.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Check variants when looping at table SCREEN</p>
      BEGIN OF check_variant,
        scr_field_name  TYPE int1 VALUE 1,
        scr_modif_group TYPE int1 VALUE 2,
        both            TYPE int1 VALUE 3,
      END OF check_variant.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Backup of table SCREEN to be able to deactivate/activate</p>
      screen_backups        TYPE ty_t_screen_backups,

*     d a t a   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Backup of currently active screen</p>
      screen_backup         TYPE REF TO ty_s_screen_backuup,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Workarea for table SCREEN in actual LOOP at it</p>
      screen_wa             TYPE screen,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = Screen field should be ACTIVATED</p>
      is_activate           TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">Number of screen field modification group</p>
      screen_modif_group_id TYPE numc1   VALUE '1'.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_screen_field_attr.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Backup of table SCREEN to be able to deactivate/activate</p>
      backup_n_load_screen_attr,

      "! <p class="shorttext synchronized" lang="en">Set attribute changes into table SCREEN</p>
      modify_table_screen.

ENDCLASS.



CLASS zcl_ca_c_screen_field_attr IMPLEMENTATION.


  METHOD set_as_recommended.
    "-----------------------------------------------------------------*
    "   Set screen field for possible input
    "-----------------------------------------------------------------*
    "May the field is currently not adjusted for Input
    set_for_input( screen_field_name  = screen_field_name
                   screen_modif_group = screen_modif_group ).

    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-required
        attribute_value         = switch-is_recommended ) ##no_text.
  ENDMETHOD.                    "set_as_recommended


  METHOD set_for_input.
    "-----------------------------------------------------------------*
    "   Set screen field for input
    "-----------------------------------------------------------------*
    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-input
        attribute_value         = switch-on ) ##no_text.
  ENDMETHOD.                    "set_for_input


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_screen_field_attr=>singleton_instance IS NOT BOUND.
      zcl_ca_c_screen_field_attr=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_screen_field_attr=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD backup_n_load_screen_attr.
    "-----------------------------------------------------------------*
    "   Backup of table SCREEN to be able to deactivate/activate.
    "   The consumer is responsible to create the instance of this
    "   class at the right time.
    "-----------------------------------------------------------------*
    TRY.
        DATA(_screen_id) = get_actual_screen( ).
        IF _screen_id IS INITIAL.
          RETURN.
        ENDIF.

        screen_backup = REF #( screen_backups[ progname = _screen_id-progname
                                               dynpro   = _screen_id-dynpro ] ).

      CATCH cx_sy_itab_line_not_found.
        INSERT VALUE #( progname = _screen_id-progname
                        dynpro   = _screen_id-dynpro   ) INTO TABLE screen_backups
                                                         REFERENCE INTO screen_backup.
        LOOP AT SCREEN INTO DATA(_screen).
          APPEND _screen TO screen_backup->t_screen.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.                    "backup_n_load_screen_attr


  METHOD set_as_obligatory.
    "-----------------------------------------------------------------*
    "   Set screen field for obligatory input
    "-----------------------------------------------------------------*
    "May the field is currently not adjusted for Input
    set_for_input( screen_field_name  = screen_field_name
                   screen_modif_group = screen_modif_group ).

    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-required
        attribute_value         = switch-on ) ##no_text.
  ENDMETHOD.                    "set_as_obligatory


  METHOD set_as_possible.
    "-----------------------------------------------------------------*
    "   Set screen field for possible input (= not obligatory)
    "-----------------------------------------------------------------*
    "May the field is currently not adjusted for Input
    set_for_input( screen_field_name  = screen_field_name
                   screen_modif_group = screen_modif_group ).

    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-required
        attribute_value         = switch-off ) ##no_text.
  ENDMETHOD.                    "set_as_possible


  METHOD activate.
    "-----------------------------------------------------------------*
    "   Activate screen field
    "-----------------------------------------------------------------*
    "Since the deactivation sets the flag ACTIVE, VISIBLE and OUTPUT, but the setting the flag ACTIVE is
    "not able to reconstruct this, the following solution was created:

    "With creating an instance of this class a backup of table SCREEN was made. To reactivate the fields
    "the corresponding entry of the backup table will be taken into account.
    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-active
        attribute_value         = switch-on ) ##no_text.
  ENDMETHOD.                    "activate


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    zcl_ca_c_sel_options=>get_instance( ).
  ENDMETHOD.                    "constructor


  METHOD create_range_entry_for_fname.
    "-----------------------------------------------------------------*
    "   Create entry for screen field name for adding to RANGE table
    "-----------------------------------------------------------------*
    IF screen_field_name IS INITIAL.
      RETURN.
    ENDIF.

    result = VALUE #( sign   = incl_or_excl
                      option = COND #( WHEN screen_field_name CA '*+'
                                         THEN zcl_ca_c_sel_options=>option-cp
                                         ELSE zcl_ca_c_sel_options=>option-eq )
                      low    = screen_field_name ).
  ENDMETHOD.                    "create_range_entry_for_fname


  METHOD create_range_entry_for_mod_grp.
    "-----------------------------------------------------------------*
    "   Create entry for scr. modif group for adding to RANGE table
    "-----------------------------------------------------------------*
    IF screen_modif_group IS INITIAL.
      RETURN.
    ENDIF.

    result = VALUE #( sign   = incl_or_excl
                      option = COND #( WHEN screen_modif_group CA '*+'
                                         THEN zcl_ca_c_sel_options=>option-cp
                                         ELSE zcl_ca_c_sel_options=>option-eq )
                      low    = screen_modif_group ).
  ENDMETHOD.                    "create_range_entry_for_mod_grp


  METHOD deactivate.
    "-----------------------------------------------------------------*
    "   Deactivate screen field
    "-----------------------------------------------------------------*
    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-active
        attribute_value         = switch-off ) ##no_text.
  ENDMETHOD.                    "deactivate


  METHOD get_actual_screen.
    "-----------------------------------------------------------------*
    "   Get actual program + dynpro - SY-REPID + SY-CPROG is sometimes wrong
    "-----------------------------------------------------------------*
    "Result = e. g. :ZFIIV_TEST_TASKS(1000)
    cl_dynp_ral_api=>get_current_path(
                                IMPORTING
                                  es_path = DATA(_screen_id_as_string) ).

    SHIFT _screen_id_as_string LEFT BY 1 PLACES.
    TRANSLATE _screen_id_as_string USING '($)$'.   "Convert all delimiter in $
    SPLIT _screen_id_as_string AT '$' INTO result-progname  result-dynpro
                               IN CHARACTER MODE.
  ENDMETHOD.                    "get_actual_dynpro


  METHOD hide.
    "-----------------------------------------------------------------*
    "   Hide screen field = OUTPUT OFF and INVISIBLE ON
    "-----------------------------------------------------------------*
    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-invisible
        attribute_value         = switch-on ) ##no_text.
  ENDMETHOD.                    "hide


  METHOD make_visible.
    "-----------------------------------------------------------------*
    "   Display screen field
    "-----------------------------------------------------------------*
    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-invisible
        attribute_value         = switch-off ) ##no_text.
  ENDMETHOD.                    "make_visible


  METHOD modify_attributes.
    "-----------------------------------------------------------------*
    "   Modify screen field attributes
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _check_variant        TYPE int1.

    FIELD-SYMBOLS:
      <attribute_value> TYPE simple,
      <scr_modif_group> TYPE char3.

    IF sy-dynnr IS INITIAL.
      RETURN.
    ENDIF.

    "If screen fields were deactivated (set ACTIVE to zero) the standard changes several attributes too, but
    "is not able to recover these settings when the fields were reactivated. This is why the original screen
    "definition is saved locally and is used to recover the original settings when reactivating screen fields.
    backup_n_load_screen_attr( ).

    "Delete initial lines -> was easier than to maintain the calling methods
    DATA(_requested_scr_field_names)  = req_screen_field_names.
    DELETE _requested_scr_field_names WHERE sign IS INITIAL.
    DATA(_requested_scr_modif_groups) = req_screen_modif_groups.
    DELETE _requested_scr_modif_groups WHERE sign IS INITIAL.

    "Assign field of structure SCREEN for usage and determine the how to check the values
    IF _requested_scr_field_names IS NOT INITIAL.
      _check_variant += check_variant-scr_field_name.
    ENDIF.

    IF _requested_scr_modif_groups IS NOT INITIAL.
      _check_variant += check_variant-scr_modif_group.
      "Only the groups 1 - 4 is possible - just in case you don't know that ;).
      DATA(_column_name_scr_modif_group) = CONV fieldname( |GROUP{ screen_modif_group_id }| ).
      ASSIGN COMPONENT _column_name_scr_modif_group OF STRUCTURE screen_wa TO <scr_modif_group>.
      ASSERT sy-subrc EQ 0.
    ENDIF.

    "Set the boolean operator. The consumer value is overruled if only one of the passed ranges is filled.
    "The operator AND is always right if one of the ranges is empty.
    DATA(_boolean_operator) = boolean_operator.
    IF _check_variant NE check_variant-both.
      _boolean_operator = me->boolean_operator-and.
    ENDIF.

    "The attribute is used in any case
    ASSIGN COMPONENT attribute_name OF STRUCTURE screen_wa TO <attribute_value>.
    ASSERT sy-subrc EQ 0.

    "See hint in method ACTIVATE why this is necessary
    is_activate = xsdbool( attribute_name  EQ screen_attr-active AND
                           attribute_value EQ switch-on ).

    "Manipulate screen field attributes
    LOOP AT SCREEN INTO screen_wa.
      CASE _check_variant.
        WHEN check_variant-scr_field_name.
          IF screen_wa-name IN _requested_scr_field_names.
            <attribute_value> = attribute_value.
            modify_table_screen( ).
          ENDIF.

        WHEN check_variant-scr_modif_group.
          IF <scr_modif_group> IN _requested_scr_modif_groups.
            <attribute_value> = attribute_value.
            modify_table_screen( ).
          ENDIF.

        WHEN check_variant-both.
          CASE _boolean_operator.
            WHEN me->boolean_operator-or.
              IF screen_wa-name    IN _requested_scr_field_names  OR
                 <scr_modif_group> IN _requested_scr_modif_groups.
                <attribute_value> = attribute_value.
                modify_table_screen( ).
              ENDIF.

            WHEN me->boolean_operator-and.
              IF screen_wa-name    IN _requested_scr_field_names  AND
                 <scr_modif_group> IN _requested_scr_modif_groups.
                <attribute_value> = attribute_value.
                modify_table_screen( ).
              ENDIF.
          ENDCASE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.                    "modify_attributes


  METHOD modify_table_screen.
    "-----------------------------------------------------------------*
    "   Set attribute changes into table SCREEN
    "-----------------------------------------------------------------*
    "Overwrite passed attribute value in case it is to ACTIVATE the screen field
    IF is_activate EQ abap_true.
      screen_wa = screen_backup->t_screen[ name = screen_wa-name ].
    ENDIF.

    MODIFY SCREEN FROM screen_wa.
  ENDMETHOD.                    "modify_table_screen


  METHOD set_dd_list_for_field.
    "-----------------------------------------------------------------*
    "   Set dropdown values for specific selection parameter
    "-----------------------------------------------------------------*
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = CONV vrm_id( condense( translate( val  = sel_param_name
                                                            from = '*'  to = ' ' ) ) )
        values          = dropdown_list
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ca_param USING MESSAGE.
    ENDIF.
  ENDMETHOD.                    "set_dd_list_for_field


  METHOD set_for_output.
    "-----------------------------------------------------------------*
    "   Set screen field for output
    "-----------------------------------------------------------------*
    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-output
        attribute_value         = switch-on ) ##no_text.
  ENDMETHOD.                    "set_for_output


  METHOD set_modification_group_id.
    "-----------------------------------------------------------------*
    "   Set number of modification group for next call of this class
    "-----------------------------------------------------------------*
    me->screen_modif_group_id = screen_modif_group_id.
  ENDMETHOD.                    "set_modification_group_id


  METHOD set_to_display_only.
    "-----------------------------------------------------------------*
    "   Set screen field to display only
    "-----------------------------------------------------------------*
    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-input
        attribute_value         = switch-off ) ##no_text.
  ENDMETHOD.                    "set_to_display_only


  METHOD set_to_no_output.
    "-----------------------------------------------------------------*
    "   Set screen field to no output
    "-----------------------------------------------------------------*
    modify_attributes(
        req_screen_field_names  = VALUE #( ( create_range_entry_for_fname( screen_field_name = screen_field_name ) ) )
        req_screen_modif_groups = VALUE #( ( create_range_entry_for_mod_grp( screen_modif_group = screen_modif_group ) ) )
        attribute_name          = screen_attr-output
        attribute_value         = switch-off ) ##no_text.
  ENDMETHOD.                    "set_to_no_output
ENDCLASS.
