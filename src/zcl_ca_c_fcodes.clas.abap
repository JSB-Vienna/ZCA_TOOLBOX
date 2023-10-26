"! <p class="shorttext synchronized" lang="en">CA-TBX: Function code constants (with suggested icons)</p>
CLASS zcl_ca_c_fcodes DEFINITION PUBLIC
                                 CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Fct. code: Collapse area (ICON_DATA_AREA_COLLAPSE)</p>
      area_collapse     TYPE syucomm VALUE 'AREA_COLLAPSE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Fct. code: Expand area (ICON_DATA_AREA_EXPAND)</p>
      area_expand       TYPE syucomm VALUE 'AREA_EXPAND' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Back</p>
      back              TYPE syucomm VALUE 'BACK' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Cancel (ICON_CANCEL)</p>
      cancel            TYPE syucomm VALUE 'CANCEL' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Change (ICON_CHANGE)</p>
      change            TYPE syucomm VALUE 'CHANGE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Check / proof (ICON_CHECK)</p>
      check             TYPE syucomm VALUE 'CHECK' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Complete (ICON_COMPLETE)</p>
      complete          TYPE syucomm VALUE 'COMPLETE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Copy ( ICON_COPY_OBJECT)</p>
      copy              TYPE syucomm VALUE 'COPY' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Create (ICON_CREATE)</p>
      create            TYPE syucomm VALUE 'CREATE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Delete (ICON_DELETE)</p>
      delete            TYPE syucomm VALUE 'DELETE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Select all (ICON_DESELECT_ALL)</p>
      deselect_all      TYPE syucomm VALUE 'DESELECT_ALL' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Pickup / show detail (ICON_SELECT_DETAIL)</p>
      detail            TYPE syucomm VALUE 'DETAIL' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Display (ICON_DISPLAY)</p>
      display           TYPE syucomm VALUE 'DISPLAY' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Continue / enter (ICON_OKAY)</p>
      enter             TYPE syucomm VALUE 'ENTER' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Exit</p>
      exit              TYPE syucomm VALUE 'EXIT' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: First object (ICON_TOTAL_LEFT)</p>
      obj_first         TYPE syucomm VALUE 'FIRST_OBJ' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Last object (ICON_TOTAL_RIGHT)</p>
      obj_last          TYPE syucomm VALUE 'LAST_OBJ' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Next object (ICON_COLUMN_RIGHT)</p>
      obj_next          TYPE syucomm VALUE 'NEXT_OBJ' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Previous object (ICON_COLUMN_LEFT)</p>
      obj_prev          TYPE syucomm VALUE 'PREV_OBJ' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: First page (ICON_FIRST_PAGE)</p>
      page_first        TYPE syucomm VALUE 'FIRST_PAGE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Last page (ICON_LAST_PAGE)</p>
      page_last         TYPE syucomm VALUE 'LAST_PAGE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Next page (ICON_NEXT_PAGE)</p>
      page_next         TYPE syucomm VALUE 'NEXT_PAGE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Previous page (ICON_PREVIOUS_PAGE)</p>
      page_prev         TYPE syucomm VALUE 'PREV_PAGE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Personnel settings (ICON_PERSONAL_SETTINGS)</p>
      personal_settings TYPE syucomm VALUE 'PERS_SETTINGS' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Print (ICON_PRINT)</p>
      print             TYPE syucomm VALUE 'PRINT' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Redo / Repeat (ICON_SYSTEM_REDO)</p>
      redo              TYPE syucomm VALUE 'REDO' ##NO_TEXT,
      "! <p class="shorttext synchronized" lang="en">Function code: Refresh view (ICON_REFRESH)</p>
      refresh           TYPE syucomm VALUE 'REFRESH' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Add row (ICON_CREATE)</p>
      row_add           TYPE syucomm VALUE 'ADD_ROW' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Delete row (ICON_DELETE_ROW)</p>
      row_del           TYPE syucomm VALUE 'DEL_ROW' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Insert row (ICON_INSERT_ROW)</p>
      row_ins           TYPE syucomm VALUE 'INS_ROW' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Save / change data (ICON_SYSTEM_SAVE)</p>
      save              TYPE syucomm VALUE 'SAVE' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Search / find (ICON_SEARCH)</p>
      search            TYPE syucomm VALUE 'SEARCH' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Search / find next (ICON_SEARCH_NEXT)</p>
      search_next       TYPE syucomm VALUE 'SEARCH_NEXT' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Select all (ICON_SELECT_ALL)</p>
      select_all        TYPE syucomm VALUE 'SELECT_ALL' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Switch mode / function (ICON_TOGGLE_FUNCTION)</p>
      toggle_func       TYPE syucomm VALUE 'TOGGLE_FUNC' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Switch between views (ICON_TOGGLE_DISPLAY)</p>
      toggle_disp       TYPE syucomm VALUE 'TOGGLE_DISP' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Function code: Switch between disp+chng mode (ICON_TOGGLE_*)</p>
      toggle_disp_chg   TYPE syucomm VALUE 'TOGGLE_DISP_CHG' ##NO_TEXT,
      "! <p class="shorttext synchronized" lang="en">Function code: Undo (ICON_SYSTEM_UNDO)</p>
      undo              TYPE syucomm VALUE 'UNDO' ##no_text.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_fcodes.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_fcodes.

ENDCLASS.



CLASS ZCL_CA_C_FCODES IMPLEMENTATION.


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_fcodes=>singleton_instance IS NOT BOUND.
      zcl_ca_c_fcodes=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_fcodes=>singleton_instance.
  ENDMETHOD.                    "get_instance
ENDCLASS.
