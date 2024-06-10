"! <p class="shorttext synchronized" lang="en">CA-TBX: Helper methods for the control framework</p>
CLASS zcl_ca_cfw_util DEFINITION PUBLIC
                                 FINAL
                                 CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">ALV GRID - Refresh list display</p>
      "!
      "! @parameter io_alv_grid     | <p class="shorttext synchronized" lang="en">ALV List Viewer control</p>
      "! @parameter is_stable       | <p class="shorttext synchronized" lang="en">ALV-Control: Refresh: Row / column stability</p>
      "! @parameter iv_soft_refresh | <p class="shorttext synchronized" lang="en">X = Excluding sort, filter, etc.</p>
      alv_refresh_table_display
        IMPORTING
          io_alv_grid     TYPE REF TO cl_gui_alv_grid
          is_stable       TYPE lvc_s_stbl OPTIONAL
          iv_soft_refresh TYPE abap_bool  DEFAULT abap_false,

      "! <p class="shorttext synchronized" lang="en">ALV GRID - Register edit event</p>
      "!
      "! Use event id MC_EVT_MODIFIED to make the ALV editable in general and use event id MC_EVT_ENTER
      "! to activate raising the events DATA_CHANGED and DATA_CHANGED_FINISHED at using key ENTER.
      "!
      "! @parameter io_alv_grid | <p class="shorttext synchronized" lang="en">ALV List Viewer control</p>
      "! @parameter iv_event_id | <p class="shorttext synchronized" lang="en">Event id (use CL_GUI_ALV_GRID=>MC_EVT_MOD* or -ENTER)</p>
      alv_register_edit_event
        IMPORTING
          io_alv_grid TYPE REF TO cl_gui_alv_grid
          iv_event_id TYPE i DEFAULT cl_gui_alv_grid=>mc_evt_modified,

      "! <p class="shorttext synchronized" lang="en">Create ALV grid table control</p>
      "!
      "! @parameter io_parent        | <p class="shorttext synchronized" lang="en">CL_GUI_CONTAINER=&gt;SCREENx/DESKTOP or own container</p>
      "! @parameter io_applog_parent | <p class="shorttext synchronized" lang="en">Container for application log</p>
      "! @parameter iv_cnt_name      | <p class="shorttext synchronized" lang="en">Element name of custom container in your dynpro</p>
      "! @parameter ro_alv_grid      | <p class="shorttext synchronized" lang="en">Created ALV grid table control</p>
      create_alv_grid_control
        IMPORTING
          io_parent          TYPE REF TO cl_gui_container
          io_applog_parent   TYPE REF TO cl_gui_container OPTIONAL
          iv_cnt_name        TYPE csequence OPTIONAL
        RETURNING
          VALUE(ro_alv_grid) TYPE REF TO cl_gui_alv_grid,

      "! <p class="shorttext synchronized" lang="en">Create docking container</p>
      "!
      "! @parameter io_parent   | <p class="shorttext synchronized" lang="en">CL_GUI_CONTAINER=&gt;SCREENx/DESKTOP / nothing for own dynpro</p>
      "! @parameter iv_cnt_name | <p class="shorttext synchronized" lang="en">Element name of custom container in your dynpro</p>
      "! @parameter iv_repid    | <p class="shorttext synchronized" lang="en">Name of program the screen is assigned to</p>
      "! @parameter iv_dynnr    | <p class="shorttext synchronized" lang="en">Dynpro / screen number</p>
      "! @parameter ro_custcont | <p class="shorttext synchronized" lang="en">Created custom container</p>
      create_custom_container
        IMPORTING
          io_parent          TYPE REF TO cl_gui_container OPTIONAL
          iv_cnt_name        TYPE csequence
          VALUE(iv_repid)    TYPE syrepid    OPTIONAL
          VALUE(iv_dynnr)    TYPE syst_dynnr OPTIONAL
        RETURNING
          VALUE(ro_custcont) TYPE REF TO cl_gui_custom_container,

      "! <p class="shorttext synchronized" lang="en">Create dialog box container</p>
      "!
      "! @parameter io_parent         | <p class="shorttext synchronized" lang="en">CL_GUI_CONTAINER=&gt;SCREENx/DESKTOP or own container</p>
      "! @parameter iv_repid          | <p class="shorttext synchronized" lang="en">Name of program the screen is assigned to</p>
      "! @parameter iv_dynnr          | <p class="shorttext synchronized" lang="en">Dynpro / screen number the container should assigned to</p>
      "! @parameter iv_width          | <p class="shorttext synchronized" lang="en">Width of the dialog box</p>
      "! @parameter iv_height         | <p class="shorttext synchronized" lang="en">Height of the dialog box</p>
      "! @parameter iv_top            | <p class="shorttext synchronized" lang="en">Position of the dialog box: Top edge</p>
      "! @parameter iv_left           | <p class="shorttext synchronized" lang="en">Position of the dialog box: Left edge</p>
      "! @parameter iv_metric         | <p class="shorttext synchronized" lang="en">Metric unit f. extension (use const CL_GUI_CONTROL=&gt;METRIC*)</p>
      "! @parameter iv_caption        | <p class="shorttext synchronized" lang="en">Header text / information</p>
      "! @parameter iv_cnt_name       | <p class="shorttext synchronized" lang="en">Name for control</p>
      "! @parameter iv_style          | <p class="shorttext synchronized" lang="en">Summarize all const CL_GUI_CONTROL=&gt;WS_* to be used</p>
      "! @parameter iv_lifetime       | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=&gt;LIFETIME_*)</p>
      "! @parameter ro_diaboxcont     | <p class="shorttext synchronized" lang="en">Created docking container</p>
      create_dialogbox_container
        IMPORTING
          io_parent            TYPE REF TO cl_gui_container OPTIONAL
          VALUE(iv_repid)      TYPE syrepid    OPTIONAL
          VALUE(iv_dynnr)      TYPE syst_dynnr OPTIONAL
          iv_width             TYPE i DEFAULT 30
          iv_height            TYPE i DEFAULT 30
          iv_top               TYPE i DEFAULT 0
          iv_left              TYPE i DEFAULT 0
          iv_metric            TYPE i DEFAULT cl_gui_control=>metric_default
          iv_caption           TYPE csequence OPTIONAL
          iv_cnt_name          TYPE csequence OPTIONAL
          iv_style             TYPE i OPTIONAL
          iv_lifetime          TYPE i DEFAULT cl_gui_control=>lifetime_default
        RETURNING
          VALUE(ro_diaboxcont) TYPE REF TO cl_gui_dialogbox_container,

      "! <p class="shorttext synchronized" lang="en">Create docking container</p>
      "!
      "! @parameter io_parent    | <p class="shorttext synchronized" lang="en">CL_GUI_CONTAINER=&gt;SCREENx/DESKTOP / not for own dynpro</p>
      "! @parameter iv_repid     | <p class="shorttext synchronized" lang="en">Name of program the screen is assigned to</p>
      "! @parameter iv_dynnr     | <p class="shorttext synchronized" lang="en">Dynpro / screen number the container should assigned to</p>
      "! @parameter iv_side      | <p class="shorttext synchronized" lang="en">Assign to side x (use const CL_GUI_DOCKING_CONTAINER=>DOCK*)</p>
      "! @parameter iv_ratio     | <p class="shorttext synchronized" lang="en">Size in percent of the dynpro/screen; wins over IV_EXTENSION</p>
      "! @parameter iv_extension | <p class="shorttext synchronized" lang="en">Extension size in defined metric unit</p>
      "! @parameter iv_metric    | <p class="shorttext synchronized" lang="en">Metric unit for extension (use const CL_GUI_DOCK.=>METRIC*)</p>
      "! @parameter iv_caption   | <p class="shorttext synchronized" lang="en">Header text / information</p>
      "! @parameter iv_cnt_name  | <p class="shorttext synchronized" lang="en">Element name of custom container in your dynpro</p>
      "! @parameter iv_style     | <p class="shorttext synchronized" lang="en">Summarize all const CL_GUI_CONTROL=&gt;WS_* to be used</p>
      "! @parameter iv_lifetime  | <p class="shorttext synchronized" lang="en">Lifetime of control (use const CL_GUI_CONTROL=&gt;LIFETIME_*)</p>
      "! @parameter ro_dockcont  | <p class="shorttext synchronized" lang="en">Created docking container</p>
      create_docking_container
        IMPORTING
          io_parent          TYPE REF TO cl_gui_container OPTIONAL
          VALUE(iv_repid)    TYPE syrepid    OPTIONAL
          VALUE(iv_dynnr)    TYPE syst_dynnr OPTIONAL
          iv_side            TYPE i          DEFAULT cl_gui_docking_container=>dock_at_left
          iv_ratio           TYPE i          OPTIONAL
          iv_extension       TYPE i          OPTIONAL
          iv_metric          TYPE i          DEFAULT cl_gui_docking_container=>metric_default
          iv_caption         TYPE csequence  OPTIONAL
          iv_cnt_name        TYPE csequence  OPTIONAL
          iv_style           TYPE i          OPTIONAL
          iv_lifetime        TYPE i          DEFAULT cl_gui_control=>lifetime_default
        RETURNING
          VALUE(ro_dockcont) TYPE REF TO cl_gui_docking_container,

      "! <p class="shorttext synchronized" lang="en">Create splitter container</p>
      "!
      "! @parameter io_parent            | <p class="shorttext synchronized" lang="en">CL_GUI_CONTAINER=&gt;SCREENx/DESKTOP / nothing for own dynpro</p>
      "! @parameter iv_rows              | <p class="shorttext synchronized" lang="en">Number of rows to be displayed</p>
      "! @parameter iv_columns           | <p class="shorttext synchronized" lang="en">Number of columns / digits to be displayed</p>
      "! @parameter iv_cnt_name          | <p class="shorttext synchronized" lang="en">Element name of splitter container in your dynpro</p>
      "! @parameter iv_set_mode_relative | <p class="shorttext synchronized" lang="en">X = Set relative mode for row and cols (= Percent)</p>
      "! @parameter ro_splitter          | <p class="shorttext synchronized" lang="en">Created splitter container</p>
      create_splitter_container
        IMPORTING
          io_parent            TYPE REF TO cl_gui_container
          iv_rows              TYPE i
          iv_columns           TYPE i
          iv_cnt_name          TYPE csequence OPTIONAL
          iv_set_mode_relative TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(ro_splitter)   TYPE REF TO cl_gui_splitter_container,

      "! <p class="shorttext synchronized" lang="en">Create text edit control</p>
      "!
      "! @parameter io_parent                     | <p class="shorttext synchronized" lang="en">CL_GUI_CONTAINER=&gt;SCREENx/DESKTOP / nothing for own dynpro</p>
      "! @parameter iv_display_only               | <p class="shorttext synchronized" lang="en">X = Activate mode 'display-only'</p>
      "! @parameter iv_hide_statusbar             | <p class="shorttext synchronized" lang="en">X = Hide status bar</p>
      "! @parameter iv_hide_toolbar               | <p class="shorttext synchronized" lang="en">X = Hide tool bar</p>
      "! @parameter iv_max_chars                  | <p class="shorttext synchronized" lang="en">Max. number of digits</p>
      "! @parameter iv_style                      | <p class="shorttext synchronized" lang="en">Control style</p>
      "! @parameter iv_wordwrap_mode              | <p class="shorttext synchronized" lang="en">0=OFF; 1=wrap at window border; 2=wrap at fix pos.</p>
      "! @parameter iv_wordwrap_position          | <p class="shorttext synchronized" lang="en">Position of wrap; ignored if IV_WORDWRAP_MODE<>2</p>
      "! @parameter iv_wordwrap_to_linebreak_mode | <p class="shorttext synchronized" lang="en">X = Word wrap at line number; 0 = Keep word wrap as is</p>
      "! @parameter iv_filedrop_mode              | <p class="shorttext synchronized" lang="en">Event mode to handle drop of files on control</p>
      "! @parameter iv_lifetime                   | <p class="shorttext synchronized" lang="en">For lifetime management</p>
      "! @parameter iv_cnt_name                   | <p class="shorttext synchronized" lang="en">Name for splitter container</p>
      "! @parameter ro_text_edit                  | <p class="shorttext synchronized" lang="en">Created text edit control</p>
      create_text_edit_control
        IMPORTING
          io_parent                     TYPE REF TO cl_gui_container
          iv_display_only               TYPE abap_bool DEFAULT abap_true
          iv_hide_statusbar             TYPE abap_bool DEFAULT abap_true
          iv_hide_toolbar               TYPE abap_bool DEFAULT abap_true
          iv_max_chars                  TYPE i OPTIONAL
          iv_style                      TYPE i DEFAULT 0
          iv_wordwrap_mode              TYPE i DEFAULT cl_gui_textedit=>wordwrap_at_windowborder
          iv_wordwrap_position          TYPE i DEFAULT -1
          iv_wordwrap_to_linebreak_mode TYPE i DEFAULT cl_gui_textedit=>false
          iv_filedrop_mode              TYPE i DEFAULT cl_gui_textedit=>dropfile_event_off
          iv_lifetime                   TYPE i OPTIONAL
          iv_cnt_name                   TYPE csequence OPTIONAL
        RETURNING
          VALUE(ro_text_edit)           TYPE REF TO cl_gui_textedit,

      "! <p class="shorttext synchronized" lang="en">CFW: Forwarding of a GUI event to proxy object</p>
      dispatch,

      "! <p class="shorttext synchronized" lang="en">CFW: Flush - Raise data exchange with GUI</p>
      flush,

      "! <p class="shorttext synchronized" lang="en">Containter + inheritors: Get inner height of container</p>
      "!
      "! @parameter io_container    | <p class="shorttext synchronized" lang="en">Instance of control / container</p>
      "! @parameter rv_inner_height | <p class="shorttext synchronized" lang="en">Inner height of control / container</p>
      get_inner_height
        IMPORTING
          io_container    TYPE REF TO cl_gui_container
        CHANGING
          rv_inner_height TYPE i,

      "! <p class="shorttext synchronized" lang="en">Splitter: Determine actual height of a row</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_id       | <p class="shorttext synchronized" lang="en">Row Id</p>
      "! @parameter rv_height   | <p class="shorttext synchronized" lang="en">Current row height</p>
      get_row_height
        IMPORTING
          io_splitter      TYPE REF TO cl_gui_splitter_container
          iv_id            TYPE i
        RETURNING
          VALUE(rv_height) TYPE i,

      "! <p class="shorttext synchronized" lang="en">Containter + inheritors: Get inner height of container</p>
      "!
      "! @parameter io_control | <p class="shorttext synchronized" lang="en">Instance of control / container</p>
      "! @parameter rv_visible | <p class="shorttext synchronized" lang="en">X = Control / container is visible</p>
      get_visible
        IMPORTING
          io_control        TYPE REF TO cl_gui_control
        RETURNING
          VALUE(rv_visible) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Containter: Link control to another container</p>
      "!
      "! @parameter io_from_container | <p class="shorttext synchronized" lang="en">Instance of control / container</p>
      "! @parameter iv_prog           | <p class="shorttext synchronized" lang="en">Program name to dynpro</p>
      "! @parameter iv_dynnr          | <p class="shorttext synchronized" lang="en">Dynpro number</p>
      "! @parameter iv_to_container   | <p class="shorttext synchronized" lang="en">Name of target container (in scr painter or set by SET_NAME)</p>
      link
        IMPORTING
          io_from_container TYPE REF TO cl_gui_container
          VALUE(iv_prog)    TYPE syrepid   OPTIONAL
          VALUE(iv_dynnr)   TYPE sydynnr   OPTIONAL
          iv_to_container   TYPE csequence OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Control: (Un-)Register event for size control</p>
      "!
      "! @parameter io_control  | <p class="shorttext synchronized" lang="en">Instance of control / container</p>
      "! @parameter iv_register | <p class="shorttext synchronized" lang="en">1 = Register event / 0 = Unregister event</p>
      register_event_size_control
        IMPORTING
          io_control  TYPE REF TO cl_gui_control
          iv_register TYPE i DEFAULT 1,

      "! <p class="shorttext synchronized" lang="en">Control: (Un-)Register event for right click at control</p>
      "!
      "! @parameter io_control  | <p class="shorttext synchronized" lang="en">Instance of control / container</p>
      "! @parameter iv_register | <p class="shorttext synchronized" lang="en">1 = Register event / 0 = Unregister event</p>
      register_event_right_click
        IMPORTING
          io_control  TYPE REF TO cl_gui_control
          iv_register TYPE i DEFAULT 1,

      "! <p class="shorttext synchronized" lang="en">Splitter: Set column mode</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_mode     | <p class="shorttext synchronized" lang="en">Mode (use const MODE_* of splitter control)</p>
      set_column_mode
        IMPORTING
          io_splitter TYPE REF TO cl_gui_splitter_container
          iv_mode     TYPE i,

      "! <p class="shorttext synchronized" lang="en">Splitter: Spaltentrenner fixieren</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_id       | <p class="shorttext synchronized" lang="en">Number of sash (count left to right)</p>
      "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">1=fixation (use const TRUE/FALSE of splitter)</p>
      set_column_sash_fix
        IMPORTING
          io_splitter TYPE REF TO cl_gui_splitter_container
          iv_id       TYPE i
          iv_value    TYPE i DEFAULT cl_gui_splitter_container=>true,

      "! <p class="shorttext synchronized" lang="en">Splitter: Set visibility of column sash</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_id       | <p class="shorttext synchronized" lang="en">Number of sash (count left to right)</p>
      "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">1=visible (use const TRUE/FALSE of splitter)</p>
      set_column_sash_visible
        IMPORTING
          io_splitter TYPE REF TO cl_gui_splitter_container
          iv_id       TYPE i
          iv_value    TYPE i DEFAULT cl_gui_splitter_container=>true,

      "! <p class="shorttext synchronized" lang="en">Splitter: Set column width</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_id       | <p class="shorttext synchronized" lang="en">Column Id</p>
      "! @parameter iv_width    | <p class="shorttext synchronized" lang="en">New column width</p>
      set_column_width
        IMPORTING
          io_splitter TYPE REF TO cl_gui_splitter_container
          iv_id       TYPE i
          iv_width    TYPE i,

      "! <p class="shorttext synchronized" lang="en">Control: Set focus on passed control</p>
      "!
      "! @parameter io_control | <p class="shorttext synchronized" lang="en">Instance of control / container to be focused on</p>
      set_focus
        IMPORTING
          io_control TYPE REF TO cl_gui_control,

      "! <p class="shorttext synchronized" lang="en">Control: Set name for control / container</p>
      "!
      "! @parameter io_control  | <p class="shorttext synchronized" lang="en">Instance of control / container</p>
      "! @parameter iv_cnt_name | <p class="shorttext synchronized" lang="en">Name for control</p>
      set_name
        IMPORTING
          io_control  TYPE REF TO cl_gui_control
          iv_cnt_name TYPE csequence,

      "! <p class="shorttext synchronized" lang="en">CFW: Set a new function code for PAI event</p>
      "!
      "! @parameter iv_new_fcode | <p class="shorttext synchronized" lang="en">Function code</p>
      set_new_ok_code
        IMPORTING
          iv_new_fcode TYPE syucomm DEFAULT space,

      "! <p class="shorttext synchronized" lang="en">Control: Set application events for control</p>
      "!
      "! @parameter io_control     | <p class="shorttext synchronized" lang="en">Instance of control / container</p>
      "! @parameter it_appl_events | <p class="shorttext synchronized" lang="en">Application events to be registered</p>
      set_registered_events
        IMPORTING
          io_control     TYPE REF TO cl_gui_control
          it_appl_events TYPE cntl_simple_events,

      "! <p class="shorttext synchronized" lang="en">Splitter: Set row height</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_id       | <p class="shorttext synchronized" lang="en">Row Id</p>
      "! @parameter iv_height   | <p class="shorttext synchronized" lang="en">New row height</p>
      set_row_height
        IMPORTING
          io_splitter TYPE REF TO cl_gui_splitter_container
          iv_id       TYPE i
          iv_height   TYPE i,

      "! <p class="shorttext synchronized" lang="en">Splitter: Set row mode</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_mode     | <p class="shorttext synchronized" lang="en">Mode (use const MODE_* of splitter control)</p>
      set_row_mode
        IMPORTING
          io_splitter TYPE REF TO cl_gui_splitter_container
          iv_mode     TYPE i,

      "! <p class="shorttext synchronized" lang="en">Splitter: Fixation of row sash</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_id       | <p class="shorttext synchronized" lang="en">Number of sash (count top down)</p>
      "! @parameter iv_value    | <p class="shorttext synchronized" lang="en">1=fixation (use const TRUE/FALSE of splitter)</p>
      set_row_sash_fix
        IMPORTING
          io_splitter TYPE REF TO cl_gui_splitter_container
          iv_id       TYPE i
          iv_value    TYPE i DEFAULT cl_gui_splitter_container=>true,

      "! <p class="shorttext synchronized" lang="en">Splitter: Set visibility of row sash</p>
      "!
      "! @parameter io_splitter | <p class="shorttext synchronized" lang="en">Instance of splitter control / container</p>
      "! @parameter iv_id       | <p class="shorttext synchronized" lang="en">Number of sash (count top down)</p>
      "! @parameter IV_VALUE    | <p class="shorttext synchronized" lang="en">1=visible (use const TRUE/FALSE of splitter)</p>
      set_row_sash_visible
        IMPORTING
          io_splitter TYPE REF TO cl_gui_splitter_container
          iv_id       TYPE i
          iv_value    TYPE i DEFAULT cl_gui_splitter_container=>true,

      "! <p class="shorttext synchronized" lang="en">Control: Hide or show control / container</p>
      "!
      "! @parameter io_control | <p class="shorttext synchronized" lang="en">Instance of control / container</p>
      "! @parameter iv_visible | <p class="shorttext synchronized" lang="en">X = Display / 0 = Hide</p>
      set_visible
        IMPORTING
          io_control TYPE REF TO cl_gui_control
          iv_visible TYPE abap_bool DEFAULT abap_true.

* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
      "! <p class="shorttext synchronized" lang="en">Height of a control / container</p>
      mv_height       TYPE i,
      "! <p class="shorttext synchronized" lang="en">Inner height of a control / container</p>
      mv_inner_height TYPE i,
      "! <p class="shorttext synchronized" lang="en">Visibility of a control / container</p>
      mv_is_visible   TYPE abap_bool.

ENDCLASS.



CLASS zcl_ca_cfw_util IMPLEMENTATION.

  METHOD alv_refresh_table_display.
    "-----------------------------------------------------------------*
    "   ALV GRID: Refresh table display
    "-----------------------------------------------------------------*
    io_alv_grid->refresh_table_display(
                                  EXPORTING
                                    is_stable      = is_stable
                                    i_soft_refresh = iv_soft_refresh
                                  EXCEPTIONS
                                    finished       = 1
                                    OTHERS         = 2 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_ALV_GRID'
                                                         iv_method   = 'REFRESH_TABLE_DISPLAY'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "alv_refresh_table_display


  METHOD alv_register_edit_event.
    "-----------------------------------------------------------------*
    "   ALV GRID: Register edit event
    "-----------------------------------------------------------------*
    io_alv_grid->register_edit_event(
                                  EXPORTING
                                    i_event_id = iv_event_id
                                  EXCEPTIONS
                                    error      = 1
                                    OTHERS     = 2 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_ALV_GRID'
                                                         iv_method   = 'REGISTER_EDIT_EVENT'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "alv_register_edit_event


  METHOD create_alv_grid_control.
    "-----------------------------------------------------------------*
    "   Create ALV grid table control
    "-----------------------------------------------------------------*
    IF ro_alv_grid IS NOT SUPPLIED.
      "Parameter '&1' ist nicht angegeben
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_not_supplied
          mv_msgty = c_msgty_e
          mv_msgv1 = 'RO_ALV_GRID' ##no_text.
    ENDIF.

    "Create custom container control
    CREATE OBJECT ro_alv_grid
      EXPORTING
        i_parent          = io_parent
        i_applogparent    = io_applog_parent
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_ALV_GRID'
                                                         iv_method   = 'CONSTRUCTOR'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    "Set name explicitly because super constructors don't do it.
    IF iv_cnt_name IS NOT INITIAL.
      set_name( io_control  = ro_alv_grid
                iv_cnt_name = iv_cnt_name ).
    ENDIF.
  ENDMETHOD.                    "create_alv_grid_control


  METHOD create_custom_container.
    "-----------------------------------------------------------------*
    "   Create custom container
    "-----------------------------------------------------------------*
    IF ro_custcont IS NOT SUPPLIED.
      "Parameter '&1' is not specified
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_not_supplied
          mv_msgty = c_msgty_e
          mv_msgv1 = 'RO_CUSTCONT' ##no_text.
    ENDIF.

    "Create custom container control
    CREATE OBJECT ro_custcont
      EXPORTING
        parent                      = io_parent
        repid                       = iv_repid
        dynnr                       = iv_dynnr
        container_name              = CONV char30( iv_cnt_name )
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CUSTOM_CONTAINER'
                                                         iv_method   = 'CONSTRUCTOR'
                                                         iv_subrc    = sy-subrc )  ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "create_custom_container


  METHOD create_dialogbox_container.
    "-----------------------------------------------------------------*
    "   Create dialog box container
    "   This container needs a handler for event CLOSE of the
    "   container class
    "-----------------------------------------------------------------*
    IF ro_diaboxcont IS NOT SUPPLIED.
      "Parameter '&1' is not specified
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_not_supplied
          mv_msgty = c_msgty_e
          mv_msgv1 = 'RO_DIABOXCONT' ##no_text.
    ENDIF.

    "Create docking container control
    CREATE OBJECT ro_diaboxcont
      EXPORTING
        parent                      = io_parent
        repid                       = iv_repid
        dynnr                       = iv_dynnr
        width                       = iv_width
        height                      = iv_height
        top                         = iv_top
        left                        = iv_left
        metric                      = iv_metric
        no_autodef_progid_dynnr     = xsdbool( iv_repid IS NOT INITIAL AND
                                               iv_dynnr IS NOT INITIAL )
        caption                     = CONV text120( iv_caption )
        lifetime                    = iv_lifetime
        style                       = iv_style
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_DIALOG_BOX_CONTAINER'
                                                         iv_method   = 'CONSTRUCTOR'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    "Set name explicitly because super constructors don't do it.
    IF iv_cnt_name IS NOT INITIAL.
      set_name( io_control  = ro_diaboxcont
                iv_cnt_name = iv_cnt_name ).
    ENDIF.
  ENDMETHOD.                    "create_dialogbox_container


  METHOD create_docking_container.
    "-----------------------------------------------------------------*
    "   Create docking container
    "-----------------------------------------------------------------*
    IF ro_dockcont IS NOT SUPPLIED.
      "Parameter '&1' is not specified
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_not_supplied
          mv_msgty = c_msgty_e
          mv_msgv1 = 'RO_DOCKCONT' ##no_text.
    ENDIF.

    "Create custom container control
    CREATE OBJECT ro_dockcont
      EXPORTING
        parent                      = io_parent
        repid                       = iv_repid
        dynnr                       = iv_dynnr
        side                        = iv_side
        extension                   = iv_extension
        metric                      = iv_metric
        ratio                       = iv_ratio
        no_autodef_progid_dynnr     = xsdbool( iv_repid IS NOT INITIAL AND
                                               iv_dynnr IS NOT INITIAL )
        caption                     = CONV text80( iv_caption )
        lifetime                    = iv_lifetime
        style                       = iv_style
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_DOCKING_CONTAINER'
                                                         iv_method   = 'CONSTRUCTOR'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    "Set name explicitly because super constructors don't do it.
    IF iv_cnt_name IS NOT INITIAL.
      set_name( io_control  = ro_dockcont
                iv_cnt_name = iv_cnt_name ).
    ENDIF.
  ENDMETHOD.                    "create_docking_container


  METHOD create_splitter_container.
    "-----------------------------------------------------------------*
    "   Create splitter container
    "-----------------------------------------------------------------*
    IF ro_splitter IS NOT SUPPLIED.
      "Parameter '&1' is not specified
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_not_supplied
          mv_msgty = c_msgty_e
          mv_msgv1 = 'RO_SPLITTER' ##no_text.
    ENDIF.

    "Is parent supplied or already created?
    IF io_parent IS NOT BOUND.
      "Value &1&2 of parameter &3 is invalid
      RAISE EXCEPTION TYPE zcx_ca_intern
        EXPORTING
          textid   = zcx_ca_intern=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = 'NOT BOUND'
          mv_msgv3 = 'IO_PARENT' ##no_text.
    ENDIF.

    "Create splitter control
    CREATE OBJECT ro_splitter
      EXPORTING
        parent            = io_parent
        rows              = iv_rows
        columns           = iv_columns
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'CONSTRUCTOR'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    "Set name explicitly because super constructors don't do it.
    IF iv_cnt_name IS NOT INITIAL.
      set_name( io_control  = ro_splitter
                iv_cnt_name = iv_cnt_name ).
    ENDIF.

    "If is requested, set mode of columns and rows to relative, to be able
    "to define height and width in percent
    IF iv_set_mode_relative EQ abap_true.
      "Set row mode to relative (percentage)
      set_row_mode( io_splitter = ro_splitter
                    iv_mode     = ro_splitter->mode_relative ).

      "Set column mode to relative (percentage)
      set_column_mode( io_splitter = ro_splitter
                       iv_mode     = ro_splitter->mode_relative ).
    ENDIF.
  ENDMETHOD.                    "create_splitter_control


  METHOD create_text_edit_control.
    "-----------------------------------------------------------------*
    "   Create textedit control
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_intern TYPE REF TO zcx_ca_intern,
      lv_mode   TYPE i.

    "Create text edit control
    CREATE OBJECT ro_text_edit
      EXPORTING
        max_number_chars           = iv_max_chars
        style                      = iv_style
        wordwrap_mode              = iv_wordwrap_mode
        wordwrap_position          = iv_wordwrap_position
        wordwrap_to_linebreak_mode = iv_wordwrap_to_linebreak_mode
        filedrop_mode              = iv_filedrop_mode
        parent                     = io_parent
        lifetime                   = iv_lifetime
      EXCEPTIONS
        error_cntl_create          = 1
        error_cntl_init            = 2
        error_cntl_link            = 3
        error_dp_create            = 4
        gui_type_not_supported     = 5
        OTHERS                     = 6.
    IF sy-subrc NE 0.
      lx_intern ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                    iv_class    = 'CL_GUI_TEXTEDIT'
                                                    iv_method   = 'CONSTRUCTOR'
                                                    iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    "Set name explicitly because super constructors don't do it.
    IF iv_cnt_name IS NOT INITIAL.
      set_name( io_control  = ro_text_edit
                iv_cnt_name = iv_cnt_name ).
    ENDIF.

    "Set to display only
    CASE iv_display_only.
      WHEN abap_true.
        lv_mode = ro_text_edit->true.
      WHEN OTHERS.
        lv_mode = ro_text_edit->false.
    ENDCASE.
    ro_text_edit->set_readonly_mode(
                                EXPORTING
                                  readonly_mode          = lv_mode
                                EXCEPTIONS
                                  error_cntl_call_method = 1
                                  invalid_parameter      = 2
                                  OTHERS                 = 3 ).
    IF sy-subrc NE 0.
      lx_intern ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                    iv_class    = 'CL_GUI_TEXTEDIT'
                                                    iv_method   = 'SET_READONLY_MODE'
                                                    iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    "Hide status bar
    CASE iv_hide_statusbar.
      WHEN abap_true.
        lv_mode = ro_text_edit->false.
      WHEN OTHERS.
        lv_mode = ro_text_edit->true.
    ENDCASE.
    ro_text_edit->set_statusbar_mode(
                              EXPORTING
                                statusbar_mode         = lv_mode
                              EXCEPTIONS
                                error_cntl_call_method = 1
                                invalid_parameter      = 2
                                OTHERS                 = 3 ).
    IF sy-subrc NE 0.
      lx_intern ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                    iv_class    = 'CL_GUI_TEXTEDIT'
                                                    iv_method   = 'SET_STATUSBAR_MODE'
                                                    iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    "Hide tool bar
    CASE iv_hide_toolbar.
      WHEN abap_true.
        lv_mode = ro_text_edit->false.
      WHEN OTHERS.
        lv_mode = ro_text_edit->true.
    ENDCASE.
    ro_text_edit->set_toolbar_mode(
                              EXPORTING
                                toolbar_mode           = lv_mode
                              EXCEPTIONS
                                error_cntl_call_method = 1
                                invalid_parameter      = 2
                                OTHERS                 = 3 ).
    IF sy-subrc NE 0.
      lx_intern ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                    iv_class    = 'CL_GUI_TEXTEDIT'
                                                    iv_method   = 'SET_TOOLBAR_MODE'
                                                    iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "create_text_edit_control


  METHOD dispatch.
    "-----------------------------------------------------------------*
    "   CFW: Forwarding a GUI events to a proxy object
    "-----------------------------------------------------------------*
    cl_gui_cfw=>dispatch(
                    IMPORTING
                      return_code = DATA(lv_rc) ).
    IF lv_rc NE cl_gui_cfw=>rc_found.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CFW'
                                                         iv_method   = 'DISPATCH'
                                                         iv_subrc    = lv_rc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "dispatch


  METHOD flush.
    "-----------------------------------------------------------------*
    "   CFW: Flush - triggers exchange with GUI
    "-----------------------------------------------------------------*
    cl_gui_cfw=>flush(
                  EXCEPTIONS
                    cntl_system_error = 1
                    cntl_error        = 2
                    OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CFW'
                                                         iv_method   = 'FLUSH'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "flush


  METHOD get_inner_height.
    "-----------------------------------------------------------------*
    "   Container + inheritors: Get inner height
    "-----------------------------------------------------------------*
    "Using a RETURNING parameter is only possible, if the result is
    "stored in a class attribute, since the CFW does not allow to write
    "such a result into a local variable.
    CLEAR mv_inner_height.
    io_container->get_inner_height(
                              IMPORTING
                                inner_height      = mv_inner_height
                              EXCEPTIONS
                                cntl_error        = 1
                                OTHERS            = 2 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTAINER'
                                                         iv_method   = 'GET_INNER_HEIGHT'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    rv_inner_height = mv_inner_height.
  ENDMETHOD.                    "get_inner_height


  METHOD get_row_height.
    "-----------------------------------------------------------------*
    "   Splitter: Get height of a row
    "-----------------------------------------------------------------*
    "Using a RETURNING parameter is only possible, if the result is
    "stored in a class attribute, since the CFW does not allow to write
    "such a result into a local variable.
    CLEAR mv_height.
    io_splitter->get_row_height(
                            EXPORTING
                              id                = iv_id
                            IMPORTING
                              result            = mv_height
                            EXCEPTIONS
                              cntl_error        = 1
                              cntl_system_error = 2
                              OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'GET_ROW_HEIGHT'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    rv_height = mv_height.
  ENDMETHOD.                    "get_row_height


  METHOD get_visible.
    "-----------------------------------------------------------------*
    "   Container + inheritors: Get visibility
    "-----------------------------------------------------------------*
    "Using a RETURNING parameter is only possible, if the result is
    "stored in a class attribute, since the CFW does not allow to write
    "such a result into a local variable.
    CLEAR mv_is_visible.
    io_control->get_visible(
                        IMPORTING
                          visible    = mv_is_visible
                        EXCEPTIONS
                          cntl_error = 1
                          OTHERS     = 2 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTROL'
                                                         iv_method   = 'GET_VISIBLE'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    rv_visible = abap_false.
    IF mv_is_visible EQ abap_true.
      rv_visible = abap_true.
    ENDIF.
  ENDMETHOD.                    "get_visible


  METHOD link.
    "-----------------------------------------------------------------*
    "   Container: Link control to another container
    "-----------------------------------------------------------------*
    io_from_container->link(
                        EXPORTING
                          repid                       = iv_prog
                          dynnr                       = iv_dynnr
                          container                   = CONV char30( iv_to_container )
                        EXCEPTIONS
                          cntl_error                  = 1
                          cntl_system_error           = 2
                          lifetime_dynpro_dynpro_link = 3
                          OTHERS                      = 4 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTAINER'
                                                         iv_method   = 'LINK'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "link


  METHOD register_event_right_click.
    "-----------------------------------------------------------------*
    "   Control: (Un-)Register event for right click at control
    "-----------------------------------------------------------------*
    io_control->reg_event_right_click(
                                  EXPORTING
                                    register                 = iv_register
                                  EXCEPTIONS
                                    error_regist_event       = 1
                                    error_unregist_event     = 2
                                    cntl_error               = 3
                                    event_already_registered = 4
                                    event_not_registered     = 5
                                    OTHERS                   = 6 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTROL'
                                                         iv_method   = 'REG_EVENT_SIZE_CONTROL'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "register_event_right_click


  METHOD register_event_size_control.
    "-----------------------------------------------------------------*
    "   Control: (Un-)Register event for size control
    "-----------------------------------------------------------------*
    io_control->reg_event_size_control(
                                  EXPORTING
                                    register                 = iv_register
                                  EXCEPTIONS
                                    error_regist_event       = 1
                                    error_unregist_event     = 2
                                    cntl_error               = 3
                                    event_already_registered = 4
                                    event_not_registered     = 5
                                    OTHERS                   = 6 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTROL'
                                                         iv_method   = 'REG_EVENT_SIZE_CONTROL'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "register_event_size_control


  METHOD set_column_mode.
    "-----------------------------------------------------------------*
    "   Splitter: Set mode for the columns
    "-----------------------------------------------------------------*
    io_splitter->set_column_mode(
                          EXPORTING
                            mode              = iv_mode
                          EXCEPTIONS
                            cntl_error        = 1
                            cntl_system_error = 2
                            OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'SET_COLUMN_MODE'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_column_mode


  METHOD set_column_sash_fix.
    "-----------------------------------------------------------------*
    "   Splitter: Freeze a column sash
    "-----------------------------------------------------------------*
    io_splitter->set_column_sash(
                            EXPORTING
                              id                = iv_id
                              type              = io_splitter->type_movable
                              value             = iv_value
                            EXCEPTIONS
                              cntl_error        = 1
                              cntl_system_error = 2
                              OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'SET_COLUMN_SASH'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_column_sash_fix


  METHOD set_column_sash_visible.
    "-----------------------------------------------------------------*
    "   Splitter: Set visibility of a column sash
    "-----------------------------------------------------------------*
    io_splitter->set_column_sash(
                            EXPORTING
                              id                = iv_id
                              type              = io_splitter->type_sashvisible
                              value             = iv_value
                            EXCEPTIONS
                              cntl_error        = 1
                              cntl_system_error = 2
                              OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'SET_COLUMN_SASH'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_column_sash_visible


  METHOD set_column_width.
    "-----------------------------------------------------------------*
    "   Splitter: Set column width
    "-----------------------------------------------------------------*
    io_splitter->set_column_width(
                            EXPORTING
                              id                = iv_id
                              width             = iv_width
                            EXCEPTIONS
                              cntl_error        = 1
                              cntl_system_error = 2
                              OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'SET_COLUMN_WIDTH'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_column_width


  METHOD set_focus.
    "-----------------------------------------------------------------*
    "   Control: Set focus at passed control
    "-----------------------------------------------------------------*
    cl_gui_control=>set_focus(
                          EXPORTING
                            control           = io_control
                          EXCEPTIONS
                            cntl_error        = 1
                            cntl_system_error = 2
                            OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTROL'
                                                         iv_method   = 'SET_FOCUS'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_focus


  METHOD set_name.
    "-----------------------------------------------------------------*
    "   Control: Set name for control/container
    "-----------------------------------------------------------------*
    IF iv_cnt_name IS INITIAL.
      RETURN.
    ENDIF.

    io_control->set_name(
                    EXPORTING
                      name           = CONV string( iv_cnt_name )
                    EXCEPTIONS
                      cntl_error     = 1
                      parent_no_name = 2
                      illegal_name   = 3
                      OTHERS         = 4 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTROL'
                                                         iv_method   = 'SET_NAME'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_name


  METHOD set_new_ok_code.
    "-----------------------------------------------------------------*
    "   CFW: Set a new FCode in Eventhandler for PAI
    "-----------------------------------------------------------------*
    cl_gui_cfw=>set_new_ok_code(
                           EXPORTING
                             new_code = iv_new_fcode
                           IMPORTING
                             rc       = DATA(lv_rc) ).
    IF lv_rc NE cl_gui_cfw=>rc_posted.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CFW'
                                                         iv_method   = 'SET_NEW_OK_CODE'
                                                         iv_subrc    = lv_rc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_new_ok_code


  METHOD set_registered_events.
    "-----------------------------------------------------------------*
    "   Control: Set application events for Control
    "-----------------------------------------------------------------*
    io_control->set_registered_events(
                                EXPORTING
                                  events                    = it_appl_events
                                EXCEPTIONS
                                  cntl_error                = 1
                                  cntl_system_error         = 2
                                  illegal_event_combination = 3
                                  OTHERS                    = 4 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTROL'
                                                         iv_method   = 'SET_REGISTERED_EVENTS'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_registered_events


  METHOD set_row_height.
    "-----------------------------------------------------------------*
    "   Splitter: Set height of a row
    "-----------------------------------------------------------------*
    io_splitter->set_row_height(
                            EXPORTING
                              id                = iv_id
                              height            = iv_height
                            EXCEPTIONS
                              cntl_error        = 1
                              cntl_system_error = 2
                              OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'SET_ROW_HEIGHT'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_row_height


  METHOD set_row_mode.
    "-----------------------------------------------------------------*
    "   Splitter: Set mode for the rows
    "-----------------------------------------------------------------*
    io_splitter->set_row_mode(
                          EXPORTING
                            mode              = iv_mode
                          EXCEPTIONS
                            cntl_error        = 1
                            cntl_system_error = 2
                            OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'SET_ROW_MODE'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_row_mode


  METHOD set_row_sash_fix.
    "-----------------------------------------------------------------*
    "   Splitter: Freeze a row sash
    "-----------------------------------------------------------------*
    io_splitter->set_row_sash(
                          EXPORTING
                            id                = iv_id
                            type              = io_splitter->type_movable
                            value             = iv_value
                          EXCEPTIONS
                            cntl_error        = 1
                            cntl_system_error = 2
                            OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'SET_ROW_SASH'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_row_sash_fix


  METHOD set_row_sash_visible.
    "-----------------------------------------------------------------*
    "   Splitter: Set visibility of a row sash
    "-----------------------------------------------------------------*
    io_splitter->set_row_sash(
                          EXPORTING
                            id                = iv_id
                            type              = io_splitter->type_sashvisible
                            value             = iv_value
                          EXCEPTIONS
                            cntl_error        = 1
                            cntl_system_error = 2
                            OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_SPLITTER_CONTAINER'
                                                         iv_method   = 'SET_ROW_SASH'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_row_sash_visible


  METHOD set_visible.
    "-----------------------------------------------------------------*
    "   Control: Hide or display a control/container
    "-----------------------------------------------------------------*
    io_control->set_visible(
                        EXPORTING
                          visible           = iv_visible
                        EXCEPTIONS
                          cntl_error        = 1
                          cntl_system_error = 2
                          OTHERS            = 3 ).
    IF sy-subrc NE 0.
      DATA(lx_intern) = zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                         iv_class    = 'CL_GUI_CONTROL'
                                                         iv_method   = 'SET_VISIBLE'
                                                         iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_visible

ENDCLASS.
