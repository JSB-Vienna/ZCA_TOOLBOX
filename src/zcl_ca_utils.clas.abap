"! <p class="shorttext synchronized" lang="en">CA-TBX: Wrapped functions for easier use</p>
CLASS zcl_ca_utils DEFINITION PUBLIC
                              FINAL
                              CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="de">Details to a date</p>
      BEGIN OF ty_s_date_details,
        weekday_id         TYPE fhc_weekday,
        weekday_name_short TYPE kurzt,
        weekday_name_long  TYPE langt,
        is_a_holiday       TYPE abap_boolean,
        holiday_name_short TYPE textk,
        holiday_name_long  TYPE textl_d,
      END   OF ty_s_date_details.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants for commit mode</p>
      mo_commit_modes  TYPE REF TO zcl_ca_c_commit_mode READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Constants for answers after popup usage</p>
      mo_popup_answers TYPE REF TO zcl_ca_c_popup_answer READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Type conform default values for method parameters</p>
      BEGIN OF ms_default_value READ-ONLY,
        button_text_1 TYPE text12,
        button_text_2 TYPE text12,
      END   OF ms_default_value.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Class constructor</p>
      class_constructor,

      "! <p class="shorttext synchronized" lang="en">Compose name / description and technical id for output</p>
      "!
      "! <p>Concatenates the name or description together with its technical value like in the following example:
      "! "Siemens (34539)" = Name of the vendor (vendor id alpha converted). If the technical Id is initial it
      "! will be ignored and method returns only the name / description. In this example "Siemens".</p>
      "!
      "! @parameter iv_descr    | <p class="shorttext synchronized" lang="en">Name / description</p>
      "! @parameter iv_techn_id | <p class="shorttext synchronized" lang="en">Technical Id prepared for output (except ALPHA conversion)</p>
      compose_name_n_techn_id
        IMPORTING
          iv_descr      TYPE clike
          iv_techn_id   TYPE clike
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Execute COMMIT WORK</p>
      "!
      "! @parameter iv_commit_mode | <p class="shorttext synchronized" lang="en">Type of commit (0=by caller; 1=asynch; 2=synch/wait)</p>
      "! @parameter iv_for_bapi    | <p class="shorttext synchronized" lang="en">X = Use BAPI function module (includes buffer refresh)</p>
      do_commit
        IMPORTING
          iv_commit_mode TYPE zca_d_commit_mode DEFAULT zcl_ca_c_commit_mode=>asynchron
          iv_for_bapi    TYPE abap_bool         DEFAULT abap_false,

      "! <p class="shorttext synchronized" lang="en">Determine base transaction code of a transaction variant</p>
      "!
      "! @parameter iv_ta_variant     | <p class="shorttext synchronized" lang="en">Current transaction variant</p>
      "! @parameter rv_actual_ta_code | <p class="shorttext synchronized" lang="en">Actual base transaction code</p>
      get_called_ta_2_ta_variant
        IMPORTING
          iv_ta_variant            TYPE syst_tcode
        RETURNING
          VALUE(rv_actual_ta_code) TYPE syst_tcode,

      "! <p class="shorttext synchronized" lang="en">Get details to a date (weekday + holiday name in logon lang)</p>
      "!
      "! @parameter iv_calendar_id | <p class="shorttext synchronized" lang="en">(Legacy) Id of the factory calendar</p>
      "! @parameter iv_date        | <p class="shorttext synchronized" lang="en">Date to analyze</p>
      "! @parameter result         | <p class="shorttext synchronized" lang="en">Details to the date</p>
      "! @raising   zcx_ca_param   | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      get_details_2_date
        IMPORTING
          iv_calendar_id TYPE wfcid DEFAULT 'AT'
          iv_date        TYPE fhc_date
        RETURNING
          VALUE(result)  TYPE ty_s_date_details
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Create icon (by corresponding FM)</p>
      "!
      "! @parameter iv_icon      | <p class="shorttext synchronized" lang="en">Icon name</p>
      "! @parameter iv_text      | <p class="shorttext synchronized" lang="en">Text (behind icon)</p>
      "! @parameter iv_quickinfo | <p class="shorttext synchronized" lang="en">Quickinfo</p>
      "! @parameter iv_add_qinfo | <p class="shorttext synchronized" lang="en">X = Attach standard quickinfo; 0 = Suppress any quickinfo</p>
      "! @parameter rv_icon      | <p class="shorttext synchronized" lang="en">Carrier field for icons</p>
      icon_create
        IMPORTING
          iv_icon        TYPE csequence
          iv_text        TYPE csequence OPTIONAL
          iv_quickinfo   TYPE csequence OPTIONAL
          iv_add_qinfo   TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(rv_icon) TYPE icon_text,

      "! <p class="shorttext synchronized" lang="en">Checks whether the GUI is currently available</p>
      "!
      "! @parameter rv_is_gui_available | <p class="shorttext synchronized" lang="en">X = GUI is available</p>
      is_gui_available
        RETURNING
          VALUE(rv_is_gui_available) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Check if a  HTML client (Fiori/Business client) is running</p>
      "!
      "! @parameter rv_html_client_is_running | <p class="shorttext synchronized" lang="en">X = HTML / Business client / Fiori client</p>
      is_html_client_running
        RETURNING
          VALUE(rv_html_client_is_running) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Is save phase of a RAP application active?</p>
      "!
      "! @parameter rv_is_active | <p class="shorttext synchronized" lang="en">X = Save phase is active</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      is_save_phase_of_rap_active
        RETURNING
          VALUE(rv_is_active) TYPE abap_boolean
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Check whether the update task is active</p>
      "!
      "! @parameter rv_is_in_update_task | <p class="shorttext synchronized" lang="en">X = Update task is active</p>
      is_update_task_active
        RETURNING
          VALUE(rv_is_in_update_task) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Standard popup to confirm loss of data</p>
      "!
      "! <p>Since only the cancel button can be hidden, the second button is used instead. It is up to the
      "! consumer to provide others and handle the result respectively if necessary.</p>
      "!
      "! @parameter iv_request       | <p class="shorttext synchronized" lang="en">Request / theme / what is it about (default = see in method)</p>
      "! @parameter iv_question      | <p class="shorttext synchronized" lang="en">Question text (default = see in method)</p>
      "! @parameter iv_button_icon_1 | <p class="shorttext synchronized" lang="en">Icon for button 1 (default = ICON_SYSTEM_SAVE)</p>
      "! @parameter iv_button_text_1 | <p class="shorttext synchronized" lang="en">Text for button 1 (default = 'Save')</p>
      "! @parameter iv_button_icon_2 | <p class="shorttext synchronized" lang="en">Icon for button 2 (default = ICON_CANCEL -> see comment)</p>
      "! @parameter iv_button_text_2 | <p class="shorttext synchronized" lang="en">Text for button 2 (default = 'Cancel')</p>
      "! @parameter iv_defult_button | <p class="shorttext synchronized" lang="en">Cursor is positioned on the first (=1) / second (=2) button</p>
      "! @parameter iv_use_cancel    | <p class="shorttext synchronized" lang="en">X = Display cancel button</p>
      "! @parameter rv_answer        | <p class="shorttext synchronized" lang="en">1 = Yes, save; 2 = No, ignore changs; A = Cancel action</p>
      popup_to_confirm_loss_of_data
        IMPORTING
          iv_request       TYPE text60 OPTIONAL
          iv_question      TYPE flt_text  OPTIONAL
          iv_button_icon_1 TYPE iconname  DEFAULT 'ICON_SYSTEM_SAVE'
          iv_button_text_1 TYPE text12    DEFAULT ms_default_value-button_text_1
          iv_button_icon_2 TYPE iconname  DEFAULT 'ICON_CANCEL'
          iv_button_text_2 TYPE text12    DEFAULT ms_default_value-button_text_2
          iv_defult_button TYPE num1 DEFAULT '1'
          iv_use_cancel    TYPE abap_bool DEFAULT abap_false
        RETURNING
          VALUE(rv_answer) TYPE char1 ##no_text,

      "! <p class="shorttext synchronized" lang="en">Execute ROLLBACK WORK</p>
      "!
      "! @parameter iv_for_bapi | <p class="shorttext synchronized" lang="en">X = Use BAPI function module (includes buffer refresh)</p>
      rollback
        IMPORTING
          iv_for_bapi TYPE abap_bool DEFAULT abap_false.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants for boolean flags</p>
      mo_boolean          TYPE REF TO zcl_ca_c_boolean,
      "! <p class="shorttext synchronized" lang="en">Factory calendar</p>
      mo_factory_calendar TYPE REF TO if_fhc_fcal_runtime,  "cl_scal_factorycalendar,
      "! <p class="shorttext synchronized" lang="en">Holiday calendar assigned to factory calendar</p>
      mo_holiday_calendar TYPE REF TO if_fhc_hcal_runtime,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Weekday names</p>
      mt_weekday_names    TYPE cl_fhc_weekday_utilities=>tt_fhc_weekdaynames.

ENDCLASS.



CLASS zcl_ca_utils IMPLEMENTATION.

  METHOD class_constructor.
    "-----------------------------------------------------------------*
    "   Class constructor
    "-----------------------------------------------------------------*
    mo_commit_modes  = zcl_ca_c_commit_mode=>get_instance( ).
    mo_popup_answers = zcl_ca_c_popup_answer=>get_instance( ).
    mo_boolean       = zcl_ca_c_boolean=>get_instance( ).

    ms_default_value-button_text_1 = 'Save'(pu3).
    ms_default_value-button_text_2 = 'Cancel'(pu4).

    TRY.
        cl_fhc_weekday_utilities=>get_weekday_names(
                                                EXPORTING
                                                  iv_language = cl_abap_syst=>get_logon_language( )
                                                IMPORTING
                                                  et_weekdays = mt_weekday_names ).

      CATCH cx_fhc_runtime INTO DATA(lx_caught).
        MESSAGE lx_caught TYPE zcx_ca_error=>c_msgty_e.
    ENDTRY.
  ENDMETHOD.                    "class_constructor


  METHOD compose_name_n_techn_id.
    "-----------------------------------------------------------------*
    "   Compose name / description and technical id for output
    "-----------------------------------------------------------------*
    IF iv_techn_id IS INITIAL.
      result = iv_descr.

    ELSEIF iv_descr IS INITIAL.
      result = |{ iv_techn_id ALPHA = OUT }|.

    ELSE.
      result = |{ iv_descr } | &
               |({ condense( COND string( WHEN iv_techn_id CO '0123456789 '
                                            THEN |{ iv_techn_id ALPHA = OUT }|
                                            ELSE iv_techn_id ) ) })| ##no_text.
    ENDIF.
  ENDMETHOD.                    "compose_name_n_techn_id


  METHOD do_commit.
    "-----------------------------------------------------------------*
    "   Execute COMMIT WORK
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_return            TYPE bapiret2.

    mo_commit_modes->is_valid( iv_commit_mode ).
    mo_boolean->is_valid( iv_for_bapi ).

    IF iv_commit_mode EQ mo_commit_modes->by_caller OR     "Is in responsibility of the consumer OR
       is_update_task_active( ).                           "is not allowed at this point in time
      RETURN.
    ENDIF.

    CASE iv_for_bapi.
      WHEN abap_false.
        CASE iv_commit_mode.
          WHEN mo_commit_modes->asynchron.
            COMMIT WORK.

          WHEN mo_commit_modes->synchron.
            COMMIT WORK AND WAIT.
        ENDCASE.

      WHEN abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait   = CONV bapiwait( xsdbool( iv_commit_mode EQ mo_commit_modes->synchron ) )
          IMPORTING
            return = ls_return.

        DATA(lx_error) = CAST zcx_ca_intern( zcx_ca_intern=>create_exception(
                                                                 iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                                 iv_function = 'BAPI_TRANSACTION_COMMIT'
                                                                 is_return   = ls_return ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "do_commit


  METHOD get_called_ta_2_ta_variant.
    "-----------------------------------------------------------------*
    "   Create icon (by corresponding FM)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_ta_param   TYPE tcdparam,
      lv_ta_variant TYPE tcvariant ##needed.

    "HINT: If needed: Parameter transactions, like F-41 or F-43 have to be selected in the same table
    "with PARAM LIKE '/N%'. The actual transaction follows on these 2 digits followed by a space.

    "Get parameters to transaction - Parameters of transaction variants begin with a double @-sign.
    SELECT SINGLE param INTO  lv_ta_param
                        FROM  tstcp
                        WHERE tcode EQ iv_ta_variant
                          AND param LIKE '@@%'.
    "No entry found, then base transaction has no variant
    IF sy-subrc NE 0.
      rv_actual_ta_code = iv_ta_variant.
      RETURN.
    ENDIF.

    "Delete leading sign @ and split rest in called transaction and its variant
    SHIFT lv_ta_param LEFT DELETING LEADING '@'.
    SPLIT lv_ta_param AT space INTO rv_actual_ta_code
                                    lv_ta_variant.
  ENDMETHOD.                    "get_called_ta_2_ta_variant


  METHOD get_details_2_date.
    "-----------------------------------------------------------------*
    "   Get details to a date (weekday + holiday name in logon lang)
    "-----------------------------------------------------------------*
    TRY.
        IF mo_factory_calendar IS NOT BOUND OR
           mo_factory_calendar->get_id( ) NE iv_calendar_id.
          mo_factory_calendar = cl_scal_factorycalendar=>create_instance(
                                             iv_factorycalendar_id = iv_calendar_id
                                             iv_language           = cl_abap_syst=>get_logon_language( ) ).
          mo_holiday_calendar = mo_factory_calendar->get_hcal_assignment( ).
        ENDIF.

        result = CORRESPONDING #( mt_weekday_names[
                        weekday = cl_fhc_weekday_utilities=>get_weekday_from_date( iv_date ) ]
                                                    MAPPING weekday_id         = weekday
                                                            weekday_name_short = weekday_name_short
                                                            weekday_name_long  = weekday_name ).

        result-is_a_holiday = abap_false.
        IF mo_holiday_calendar->is_holiday( iv_date ).
          result-is_a_holiday = abap_true.
          DATA(ls_holiday_names) = mo_holiday_calendar->get_holiday( iv_date )->get_text( ).
          result-holiday_name_short = ls_holiday_names-title.
          result-holiday_name_long  = ls_holiday_names-description.
        ENDIF.

      CATCH cx_fhc_runtime INTO DATA(lx_caught).
        DATA(lx_error) = CAST zcx_ca_param( zcx_ca_error=>create_exception(
                                                           iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                           ix_error    = lx_caught ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_details_2_date


  METHOD icon_create.
    "-----------------------------------------------------------------*
    "   Create icon (by corresponding FM)
    "-----------------------------------------------------------------*
    "Create icon with requested components
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = iv_icon
        text                  = iv_text
        info                  = iv_quickinfo
        add_stdinf            = CONV icon_int( iv_add_qinfo )
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2   "Should not happen because of returnfield length
        OTHERS                = 3.
    IF sy-subrc NE 0.
      DATA(lx_error) = zcx_ca_intern=>create_exception(
                                           iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                           iv_function = 'ICON_CREATE'
                                           iv_subrc    = sy-subrc ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "icon_create


  METHOD is_gui_available.
    "-----------------------------------------------------------------*
    "   Checks whether the GUI is currently available
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_in_update_task   TYPE syst_subrc,
      lv_gui_is_available TYPE abap_boolean.

    rv_is_gui_available = abap_false.

    "sy-batch to check for background process
    "sy-binpt to check for batch processing
    "sy-oncom = 'P' to check for commit operation going on
    "sy-oncom = 'X' to check for RFC call
    IF sy-batch EQ abap_true OR
       sy-binpt NE space     OR
       sy-oncom CA 'PX' ##no_text.
      RETURN.
    ENDIF.

    "Check if Call is in update task
    CALL FUNCTION 'TH_IN_UPDATE_TASK'
      IMPORTING
        in_update_task = lv_in_update_task.

    IF lv_in_update_task IS NOT INITIAL.
      RETURN.
    ENDIF.

    "Check if GUI is avaiable
    CALL FUNCTION 'GUI_IS_AVAILABLE'
      IMPORTING
        return = rv_is_gui_available.
  ENDMETHOD.                    "is_gui_available


  METHOD is_html_client_running.
    "-----------------------------------------------------------------*
    "   Check whether a  HTML client (Fiori or Business client) is running
    "-----------------------------------------------------------------*
    rv_html_client_is_running = cl_gui_object=>www_active.
  ENDMETHOD.                    "is_html_client_running


  METHOD is_save_phase_of_rap_active.
    "-----------------------------------------------------------------*
    "   Is save phase of a RAP application active?
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_parameters TYPE abap_parmbind_tab,
      lr_phase      TYPE REF TO data.

    TRY.
        "It is programmed this way as the class couldn't exist in an older release
        DATA(lo_class_descr) = CAST cl_abap_objectdescr(
                                      NEW zcl_ca_ddic( iv_name = 'CL_ABAP_BEHV_AUX' )->mo_type_desc ) ##no_text.
        DATA(lr_method) = REF #( lo_class_descr->methods[ name = 'GET_CURRENT_PHASE' ] ) ##no_text. "doesn't exist in 7.57 SP05 / HANA 2022/05

        CREATE DATA lr_phase TYPE ('CL_ABAP_BEHV_AUX=>T_PHASE') ##no_text.
        INSERT VALUE #( name  = 'PHASE'
                        kind  = cl_abap_objectdescr=>receiving
                        value = lr_phase ) INTO TABLE lt_parameters.
        CALL METHOD ('CL_ABAP_BEHV_AUX')=>('GET_CURRENT_PHASE')
          PARAMETER-TABLE lt_parameters ##no_text.

        ASSIGN ('CL_ABAP_BEHV_AUX=>INTERACTION') TO FIELD-SYMBOL(<lv_interaction>) ##no_text.
        ASSERT sy-subrc EQ 0.
        ASSIGN ('CL_ABAP_BEHV_AUX=>EARLY_SAVE') TO FIELD-SYMBOL(<lv_early_save>) ##no_text.
        ASSERT sy-subrc EQ 0.
        ASSIGN ('CL_ABAP_BEHV_AUX=>LATE_SAVE') TO FIELD-SYMBOL(<lv_late_save>) ##no_text.
        ASSERT sy-subrc EQ 0.
        lr_phase = lt_parameters[ name = 'PHASE' ]-value ##no_text.

        rv_is_active = xsdbool( lr_phase->* EQ <lv_interaction>  OR
                                lr_phase->* EQ <lv_early_save>   OR
                                lr_phase->* EQ <lv_late_save> ).

      CATCH cx_sy_dyn_call_error
            cx_sy_ref_is_initial INTO DATA(lx_dyn_call_error).
        DATA(lx_error) = CAST zcx_ca_param( zcx_ca_error=>create_exception(
                                                             iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                             ix_error    = lx_dyn_call_error ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.

      CATCH zcx_ca_error
            cx_sy_itab_line_not_found INTO DATA(lx_catched).   "This is wrong, but currently no better solution available
        rv_is_active = abap_false.
    ENDTRY.
  ENDMETHOD.                    "is_save_phase_of_rap_active


  METHOD is_update_task_active.
    "-----------------------------------------------------------------*
    "   Check whether the execution is already in the update task
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_is_in_update_task TYPE syst_subrc.

    CALL FUNCTION 'TH_IN_UPDATE_TASK'
      IMPORTING
        in_update_task = lv_is_in_update_task.

    rv_is_in_update_task = xsdbool( lv_is_in_update_task EQ 1 ).
  ENDMETHOD.                    "is_update_task_active


  METHOD popup_to_confirm_loss_of_data.
    "-----------------------------------------------------------------*
    "   Standard popup to confirm loss of data
    "-----------------------------------------------------------------*
    DATA(lv_request) = iv_request.
    IF lv_request IS INITIAL.
      lv_request = TEXT-pu1.      "There are unsaved changes!
    ENDIF.

    DATA(lv_question) = iv_question.
    IF lv_question IS INITIAL.
      lv_question = TEXT-pu2.      "Do you like to save these changes?
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = lv_request
        text_question         = lv_question
        icon_button_1         = iv_button_icon_1
        text_button_1         = iv_button_text_1
        icon_button_2         = iv_button_icon_2
        text_button_2         = iv_button_text_2
        default_button        = iv_defult_button
        display_cancel_button = iv_use_cancel
        start_column          = 25
        start_row             = 6
      IMPORTING
        answer                = rv_answer ##no_text.
  ENDMETHOD.                    "popup_to_confirm_loss_of_data


  METHOD rollback.
    "-----------------------------------------------------------------*
    "   Execute COMMIT WORK
    "-----------------------------------------------------------------*
    mo_boolean->is_valid( iv_for_bapi ).
    CASE iv_for_bapi.
      WHEN abap_false.
        ROLLBACK WORK.                                 "#EC CI_ROLLBACK

      WHEN abap_true.
        "Parameter RETURN will always be empty - so no error handling necessary
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDCASE.
  ENDMETHOD.                    "do_commit

ENDCLASS.
