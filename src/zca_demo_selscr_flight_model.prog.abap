
* t a b l e s   /   s t r u c t u r e s   for selection field definition
TABLES:
  sscrfields.      "Fields and function codes on selection screens


* s e l e c t i o n   f i e l d s
*- Options generating flight model data ------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bfm WITH FRAME TITLE txbl_bfm.
  PARAMETERS:
*   Keep current data?
    p_rb_kep RADIOBUTTON GROUP del DEFAULT 'X'
                                   USER-COMMAND rb_data_generator,
*   Delete and create new data?
    p_rb_del RADIOBUTTON GROUP del.

  SELECTION-SCREEN BEGIN OF BLOCK fma WITH FRAME.
    "All text values (= TXCM_+++) are set in method INITIALIZATION
    SELECTION-SCREEN COMMENT /1(75) txcm_fms MODIF ID fmc.
    SELECTION-SCREEN COMMENT /1(75) txcm_fmt MODIF ID fmc.
    SELECTION-SCREEN COMMENT /1(75) txcm_fmu MODIF ID fmc.
  SELECTION-SCREEN END   OF BLOCK fma.

  SELECTION-SCREEN BEGIN OF BLOCK fmb WITH FRAME.
    SELECTION-SCREEN COMMENT /1(75) txcm_fmv MODIF ID fmc.
    SELECTION-SCREEN COMMENT /1(75) txcm_fmw MODIF ID fmc.
  SELECTION-SCREEN END   OF BLOCK fmb.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
*     Minimum
      p_rb_min RADIOBUTTON GROUP size MODIF ID fmc.
    SELECTION-SCREEN COMMENT 04(20) FOR FIELD p_rb_min MODIF ID fmc.
    SELECTION-SCREEN COMMENT 26(30) txcm_min MODIF ID fmc.      "...   ~ 95 records
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
*     Standard
      p_rb_std RADIOBUTTON GROUP size
                           MODIF ID fmc
                           DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 04(20) FOR FIELD p_rb_std MODIF ID fmc.
    SELECTION-SCREEN COMMENT 26(30) txcm_std MODIF ID fmc.      "...  ~ 350 records
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
*     Maximum
      p_rb_max RADIOBUTTON GROUP size MODIF ID fmc.
    SELECTION-SCREEN COMMENT 04(20) FOR FIELD p_rb_max MODIF ID fmc.
    SELECTION-SCREEN COMMENT 26(30) txcm_max MODIF ID fmc.      "... ~ 1300 records
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS:
*     Monster
      p_rb_mst RADIOBUTTON GROUP size MODIF ID fmc.
    SELECTION-SCREEN COMMENT 04(20) FOR FIELD p_rb_mst MODIF ID fmc.
    SELECTION-SCREEN COMMENT 26(30) txcm_mst MODIF ID fmc.      "... ~ 4900 records
  SELECTION-SCREEN END   OF LINE.

  SELECTION-SCREEN BEGIN OF BLOCK fmd WITH FRAME.
    PARAMETERS:
*   Create canc. entries in SBOOK?
      p_cb_csb AS CHECKBOX DEFAULT abap_false MODIF ID fmc.
  SELECTION-SCREEN END   OF BLOCK fmd.
SELECTION-SCREEN END   OF BLOCK bfm.


SELECTION-SCREEN FUNCTION KEY 1.




"! <p class="shorttext synchronized" lang="en">(Re-)Create flight model data for demo programs</p>
CLASS flight_model DEFINITION FINAL
                              CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Construtcor</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Control / adjust selection screen fields</p>
      at_sel_screen_output,

      "! <p class="shorttext synchronized" lang="en">Check selection values and handle user commands</p>
      at_sel_screen,

      "! <p class="shorttext synchronized" lang="en">Create flight model data</p>
      create_data
        RAISING
          zcx_ca_param.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Job name</p>
      c_job_name       TYPE btcjob            VALUE 'ZCA_DEMO_CRE_FLIGHT_MODEL_DATA'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Job name</p>
      c_data_generator TYPE syrepid           VALUE 'SAPBC_DATA_GENERATOR'  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Execution mode for print parameters</p>
      c_mode_batch     TYPE syst_callr        VALUE 'BATCH' ##no_text.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Screen field attributes (usage with table SCREEN)</p>
      scr_fld_attr           TYPE REF TO zcl_ca_c_screen_field_attr,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = Many records to delete -> reason for background execution</p>
      large_size_to_delete   TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">To set default values and adjust the selection screen</p>
      number_sflight_entries TYPE i.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Execute data creation online / dialog</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Description</p>
      execute_online
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Execute data creation in background</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Description</p>
      execute_in_background
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Determine print parameters of the current user</p>
      "!
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Print parameters of the user</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Description</p>
      get_print_parameters
        RETURNING
          VALUE(result) TYPE pri_params
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Create a job number</p>
      "!
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Job number</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Description</p>
      create_job
        RETURNING
          VALUE(result) TYPE btcjobcnt
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Evaluate returncode of SUBMIT statement</p>
      "!
      "! @parameter iv_submit_return_code | <p class="shorttext synchronized" lang="en">Description</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Description</p>
      evaluate_submit_error
        IMPORTING
          VALUE(iv_submit_return_code) TYPE syst_subrc
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Close job that starts processing</p>
      "!
      "! @parameter iv_job_number   | <p class="shorttext synchronized" lang="en">Job number</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Description</p>
      close_job
        IMPORTING
          iv_job_number TYPE btcjobcnt
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Open documentation of standard program for data generation</p>
      open_docu_data_generator.

ENDCLASS.



CLASS flight_model IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Construtcor
    "-----------------------------------------------------------------*
    scr_fld_attr = zcl_ca_c_screen_field_attr=>get_instance( ).

    p_rb_kep = abap_true.
    p_rb_del = abap_false.

    SELECT COUNT(*) INTO @number_sflight_entries        "#EC CI_NOWHERE
                    FROM sflight.

    p_rb_std             = abap_false.
    large_size_to_delete = abap_false.

    IF number_sflight_entries EQ 0.
      p_rb_std = abap_true.

    ELSEIF number_sflight_entries BETWEEN 1 AND 300.
      p_rb_min = abap_true.

    ELSEIF number_sflight_entries BETWEEN 301 AND 1200.
      p_rb_std = abap_true.

    ELSEIF number_sflight_entries BETWEEN 1201 AND 4500.
      p_rb_max             = abap_true.
      large_size_to_delete = abap_true.

    ELSEIF number_sflight_entries GE 4501.
      p_rb_mst             = abap_true.
      large_size_to_delete = abap_true.
    ENDIF.

    "Set values for predefined pushbutton
    sscrfields-functxt_01 =
           VALUE smp_dyntxt( icon_id   = icon_system_help
                             icon_text = 'Data generator'(hdg)
                             quickinfo = 'Help of SAP flight model data generator'(qdg) ).

    "Set frame title and comments of selection screen block BFM
    txbl_bfm = 'Options for generation of the flight model data'(bfm).

    txcm_fms = 'Choose the data creation size of records in table SFLIGHT. The last'(fms).
    txcm_fmt = 'two option are only possible in background processing. A job will be'(fmt).
    txcm_fmu = 'created that can be monitored via transaction SMX.'(fmu).

    txcm_fmv = 'If the preselection is not "Standard" it reflects the current DB table'(fmv).
    txcm_fmw = 'size available.'(fmw).

    txcm_min = '... ~ 95 records'(min).
    txcm_std = '... ~ 350 records'(std).
    txcm_max = '... ~ 1300 records'(max).
    txcm_mst = '... ~ 4900 records'(mst).
  ENDMETHOD.                    "initialization


  METHOD at_sel_screen_output.
    "-----------------------------------------------------------------*
    "   Control / adjust selection screen fields
    "-----------------------------------------------------------------*
    LOOP AT SCREEN INTO DATA(field).
      CASE abap_true.
        WHEN p_rb_del.
          "Display all
          field-active    = scr_fld_attr->switch-on.
          field-invisible = scr_fld_attr->switch-off.

        WHEN p_rb_kep.
          IF field-group1 EQ 'FMC'         OR
             field-name   CP '%BFM*BLOCK*' ##no_text.
            "Hide all fields about size and additional options for it
            field-active    = scr_fld_attr->switch-off.
            field-invisible = scr_fld_attr->switch-on.
          ENDIF.
      ENDCASE.

      MODIFY SCREEN FROM field.
    ENDLOOP.
  ENDMETHOD.                    "at_sel_screen_output


  METHOD at_sel_screen.
    "-----------------------------------------------------------------*
    "   Check selection values and handle user commands
    "-----------------------------------------------------------------*
    CASE sscrfields-ucomm.
      WHEN 'FC01' ##no_text.
        "Open documentation of standard program
        open_docu_data_generator( ).
    ENDCASE.
  ENDMETHOD.                    "at_sel_screen


  METHOD create_data.
    "-----------------------------------------------------------------*
    "   Create flight model data
    "-----------------------------------------------------------------*
    IF p_rb_kep EQ abap_true.
      "Data should be kept as they are
      RETURN.
    ENDIF.

    CASE abap_true.
      WHEN p_rb_min OR  p_rb_std.
        execute_online( ).

      WHEN large_size_to_delete OR  p_rb_max OR  p_rb_mst.
        execute_in_background( ).

        RAISE EXCEPTION TYPE zcx_ca_param
          MESSAGE ID '38' TYPE 'E' NUMBER 001 WITH
          'Data creation is started in background due to the'(ib1)
          'requested record size, either for deletion or'(ib2)
          'creation. Please monitor using transaction SMX.'(ib3).
    ENDCASE.
  ENDMETHOD.                    "create_data


  METHOD execute_online.
    "-----------------------------------------------------------------*
    "   Execute data creation online / dialog
    "-----------------------------------------------------------------*
    IF p_rb_del               EQ abap_true AND
       number_sflight_entries GT 0.
      "Data deletion
      SUBMIT sapbc_data_generator                        "#EC CI_SUBMIT
                   WITH minimal  EQ abap_false
                   WITH standard EQ abap_false
                   WITH maximal  EQ abap_false
                   WITH monster  EQ abap_false
                   WITH sbcancel EQ abap_false
                   WITH pa_book  EQ abap_false
                   WITH pa_del   EQ abap_true
                   WITH pa_dark  EQ abap_true AND RETURN.
      evaluate_submit_error( sy-subrc ).
    ENDIF.

    "Data creation
    SUBMIT sapbc_data_generator                          "#EC CI_SUBMIT
                   WITH minimal  EQ p_rb_min
                   WITH standard EQ p_rb_std
                   WITH maximal  EQ p_rb_max
                   WITH monster  EQ p_rb_mst
                   WITH sbcancel EQ p_cb_csb
                   WITH pa_book  EQ abap_true
                   WITH pa_del   EQ abap_false
                   WITH pa_dark  EQ abap_true AND RETURN.
    evaluate_submit_error( sy-subrc ).
  ENDMETHOD.                    "execute_online


  METHOD execute_in_background.
    "-----------------------------------------------------------------*
    "   Execute data creation in background
    "-----------------------------------------------------------------*
    DATA(print_parameters) = get_print_parameters( ).
    DATA(job_number)       = create_job( ).

    IF p_rb_del               EQ abap_true AND
       number_sflight_entries GT 0.
      "Data deletion
      SUBMIT sapbc_data_generator                        "#EC CI_SUBMIT
             TO SAP-SPOOL
             WITHOUT SPOOL DYNPRO
             SPOOL PARAMETERS print_parameters
             VIA JOB c_job_name NUMBER job_number WITH minimal  EQ abap_false
                                                  WITH standard EQ abap_false
                                                  WITH maximal  EQ abap_false
                                                  WITH monster  EQ abap_false
                                                  WITH sbcancel EQ abap_false
                                                  WITH pa_book  EQ abap_false
                                                  WITH pa_del   EQ abap_true
                                                  WITH pa_dark  EQ abap_true AND RETURN.
      evaluate_submit_error( sy-subrc ).
    ENDIF.

    "Data creation
    SUBMIT sapbc_data_generator                          "#EC CI_SUBMIT
           TO SAP-SPOOL
           WITHOUT SPOOL DYNPRO
           SPOOL PARAMETERS print_parameters
           VIA JOB c_job_name NUMBER job_number WITH minimal  EQ p_rb_min
                                                WITH standard EQ p_rb_std
                                                WITH maximal  EQ p_rb_max
                                                WITH monster  EQ p_rb_mst
                                                WITH sbcancel EQ p_cb_csb
                                                WITH pa_book  EQ abap_true
                                                WITH pa_del   EQ abap_false
                                                WITH pa_dark  EQ abap_true AND RETURN.
    evaluate_submit_error( sy-subrc ).
    close_job( job_number ).
  ENDMETHOD.                    "execute_in_background


  METHOD get_print_parameters.
    "-----------------------------------------------------------------*
    "   Determine print parameters of the current user
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_valid           TYPE abap_bool  VALUE abap_false,
      lv_valid_for_spool TYPE abap_bool  VALUE abap_true.

    CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
        mode                     = c_mode_batch
        no_dialog                = abap_true
      IMPORTING
        valid                    = lv_valid
        valid_for_spool_creation = lv_valid_for_spool
        out_parameters           = result
      EXCEPTIONS
        OTHERS                   = 1.
    DATA(lx_error) =
         CAST zcx_ca_param(
                zcx_ca_error=>create_exception(
                         iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                         iv_function = 'GET_PRINT_PARAMETERS'
                         iv_subrc    = sy-subrc ) )  ##no_text.
    IF lx_error IS BOUND.
      RAISE EXCEPTION lx_error.
    ENDIF.

    IF lv_valid EQ abap_false.
      RAISE EXCEPTION TYPE zcx_ca_param. "See description of FM above for further hints
    ENDIF.

    IF lv_valid_for_spool EQ abap_false.
      RAISE EXCEPTION TYPE zcx_ca_param.
    ENDIF.
  ENDMETHOD.                    "get_print_parameters


  METHOD create_job.
    "-----------------------------------------------------------------*
    "   Create a job number
    "-----------------------------------------------------------------*
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = c_job_name
        jobclass         = 'C'
      IMPORTING
        jobcount         = result
      EXCEPTIONS
        cant_create_job  = 1           "Job cannot be created, see system log
        invalid_job_data = 2           "Job Contains Invalid Job Data, See SYSLOG
        jobname_missing  = 3           "Job Name Not Specified
        error_message    = 4
        OTHERS           = 5 ##no_text.
    DATA(lx_error) =
         CAST zcx_ca_param(
                zcx_ca_error=>create_exception(
                         iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                         iv_function = 'JOB_OPEN'
                         iv_subrc    = sy-subrc ) )  ##no_text.
    IF lx_error IS BOUND.
      RAISE EXCEPTION lx_error.
    ENDIF.
  ENDMETHOD.                    "create_job


  METHOD evaluate_submit_error.
    "-----------------------------------------------------------------*
    "   Evaluate returncode of SUBMIT statement
    "-----------------------------------------------------------------*
    CASE iv_submit_return_code.
      WHEN 4.
        RAISE EXCEPTION TYPE zcx_ca_param
          MESSAGE ID '38' TYPE 'E' NUMBER 001 WITH
          'Scheduling was terminated by the user on the'(es1)
          'selection screen'(es2).

      WHEN 8.
        RAISE EXCEPTION TYPE zcx_ca_param
          MESSAGE ID '38' TYPE 'E' NUMBER 001 WITH
          'Error during the scheduling, that is during the'(es3)
          'internal call of JOB_SUBMIT'(es4).

      WHEN 12.
        RAISE EXCEPTION TYPE zcx_ca_param
          MESSAGE ID '38' TYPE 'E' NUMBER 001 WITH
          'Error during internal number assignment'(es5).
    ENDCASE.
  ENDMETHOD.                    "evaluate_submit_error


  METHOD close_job.
    "-----------------------------------------------------------------*
    "   Create a job number
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_is_released          TYPE btcchar1.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobname              = c_job_name
        jobcount             = iv_job_number
        strtimmed            = abap_true
      IMPORTING
        job_was_released     = lv_is_released
      EXCEPTIONS
        cant_start_immediate = 1        "Cannot Start Immediately
        invalid_startdate    = 2        "Start Condition is Invalid
        jobname_missing      = 3        "Job Name Missing (Wildcards Allowed)
        job_close_failed     = 4        "Error During JOB_CLOSE, See SYSLOG
        job_nosteps          = 5        "Job Specified Does Not Contain Any Steps
        job_notex            = 6        "Specified Job Does Not Exist
        lock_failed          = 7        "Lock Attempt Failed
        invalid_target       = 8        "Target Server or Group is Invalid
        invalid_time_zone    = 9        "Time Zone Invalid
        error_message        = 10
        OTHERS               = 11 ##no_text.
    DATA(lx_error) =
         CAST zcx_ca_param(
                zcx_ca_error=>create_exception(
                         iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                         iv_function = 'JOB_CLOSE'
                         iv_subrc    = sy-subrc ) )  ##no_text.
    IF lx_error IS BOUND.
      RAISE EXCEPTION lx_error.
    ENDIF.
  ENDMETHOD.                    "close_job


  METHOD open_docu_data_generator.
    "-----------------------------------------------------------------*
    "   Open documentation of standard program for data generation
    "-----------------------------------------------------------------*
    CALL FUNCTION 'DSYS_SHOW_FOR_F1HELP'
      EXPORTING
        dokclass         = 'RE'
        dokname          = c_data_generator
        short_text       = 'X'
      EXCEPTIONS
        class_unknown    = 1
        object_not_found = 2
        error_message    = 3
        OTHERS           = 4 ##no_text.

    DATA(lx_error) =
         CAST zcx_ca_param(
                zcx_ca_error=>create_exception(
                         iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                         iv_function = 'DSYS_SHOW_FOR_F1HELP'
                         iv_subrc    = sy-subrc ) )  ##no_text.
    IF lx_error IS BOUND.
      MESSAGE lx_error TYPE lx_error->c_msgty_e.
    ENDIF.
  ENDMETHOD.                    "open_docu_data_generator

ENDCLASS.
