"! <p class="shorttext synchronized" lang="en">CA-TBX: Mapping between Business Object keys + their GUIDs</p>
CLASS zcl_ca_map_bo_key_2_guid DEFINITION PUBLIC
                                          CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">My BO key</p>
      ms_bo_key TYPE sibflporb READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">My GUID</p>
      mv_guid   TYPE sysuuid_c32 READ-ONLY.


*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Determine GUID by Business Object key</p>
      "!
      "! <p>Providing an existing Business Object key is in responsibility of the consumer of
      "! this class. This method will always create a GUID if the BO key doesn't already exist.</p>
      "!
      "! @parameter is_bo_key | <p class="shorttext synchronized" lang="en">Business Object key</p>
      "! @parameter result    | <p class="shorttext synchronized" lang="en">GUID</p>
      get_guid_by_bo_key
        IMPORTING
          is_bo_key     TYPE sibflporb
        RETURNING
          VALUE(result) TYPE sysuuid_c32,

      "! <p class="shorttext synchronized" lang="en">Determine Business Object key by GUID</p>
      "!
      "! <p>This method will raise an exception in case the passed GUID doesn't exist. This is
      "! because the mapping bases on existing Business objects, which means that the first use
      "! of this class has always to be executed using a BO key.</p>
      "!
      "! @parameter iv_guid      | <p class="shorttext synchronized" lang="en">GUID</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Business Object key</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_bo_key_by_guid
        IMPORTING
          iv_guid       TYPE sysuuid_c32
        RETURNING
          VALUE(result) TYPE sibflporb
        RAISING
          zcx_ca_dbacc.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get GUID by Business Object key from DB</p>
      get_guid,

      "! <p class="shorttext synchronized" lang="en">Get Business Object key by GUID from DB</p>
      "!
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_bo_key
        RAISING
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Create GUID for Business Object and write into DB</p>
      create_guid.

ENDCLASS.



CLASS ZCL_CA_MAP_BO_KEY_2_GUID IMPLEMENTATION.


  METHOD get_bo_key.
    "-----------------------------------------------------------------*
    "   Get Business Object key by GUID from DB
    "-----------------------------------------------------------------*
    SELECT SINGLE instid,  typeid,  catid
                           INTO  CORRESPONDING FIELDS OF @ms_bo_key
                           FROM  zca_map_bo_guid
                           WHERE guid EQ @mv_guid.
    IF sy-subrc NE 0.
      "No entry exists for & in Table &
      RAISE EXCEPTION TYPE zcx_ca_dbacc
        EXPORTING
          textid   = zcx_ca_dbacc=>no_entry
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( mv_guid )
          mv_msgv2 = 'ZCA_MAP_BO_GUID' ##no_text.
    ENDIF.
  ENDMETHOD.                    "get_bo_key


  METHOD get_bo_key_by_guid.
    "-----------------------------------------------------------------*
    "   Determine Business Object key by GUID
    "-----------------------------------------------------------------*
    mv_guid = iv_guid.

    IF ms_bo_key IS INITIAL.
      get_bo_key( ).
    ENDIF.

    result = ms_bo_key.
  ENDMETHOD.                    "get_bo_key_by_guid


  METHOD create_guid.
    "-----------------------------------------------------------------*
    "   Get GUID from DB
    "-----------------------------------------------------------------*
    TRY.
        mv_guid = cl_system_uuid=>create_uuid_c32_static( ).

        INSERT zca_map_bo_guid FROM @( VALUE #( s_bo_key = ms_bo_key
                                                guid     = mv_guid ) ).
        IF sy-subrc NE 0.
          "Inserting entry into table &1 with key &2&3 failed
          RAISE EXCEPTION TYPE zcx_ca_intern
            MESSAGE ID 'ZCA_TOOLBOX' TYPE c_msgty_e NUMBER '010'
            WITH 'ZCA_MAP_BO_GUID'
                |{ ms_bo_key-catid } { ms_bo_key-typeid }|
                ms_bo_key-instid ##no_text.
        ENDIF.

      CATCH cx_uuid_error INTO DATA(lx_catched).
        DATA(lx_error) =
             CAST zcx_ca_intern(
                    zcx_ca_intern=>create_exception(
                             iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                             iv_class    = 'CL_SYSTEM_UUID'
                             iv_method   = 'CREATE_UUID_C32_STATIC'
                             ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "create_guid


  METHOD get_guid.
    "-----------------------------------------------------------------*
    "   Get GUID by Business Object key from DB
    "-----------------------------------------------------------------*
    SELECT SINGLE guid INTO  @mv_guid
                       FROM  zca_map_bo_guid
                       WHERE instid EQ @ms_bo_key-instid
                         AND typeid EQ @ms_bo_key-typeid
                         AND catid  EQ @ms_bo_key-catid.
    IF sy-subrc NE 0.
      create_guid( ).
    ENDIF.
  ENDMETHOD.                    "get_guid


  METHOD get_guid_by_bo_key.
    "-----------------------------------------------------------------*
    "   Determine GUID by Business Object key
    "-----------------------------------------------------------------*
    ms_bo_key = is_bo_key.
    ms_bo_key-typeid = to_upper( ms_bo_key-typeid ).

    IF mv_guid IS INITIAL.
      get_guid( ).
    ENDIF.

    result = mv_guid.
  ENDMETHOD.                    "get_guid_by_bo_key
ENDCLASS.
