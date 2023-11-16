"! <p class="shorttext synchronized" lang="en">DO NOT USE! Tech. purpose! APACK manifest for package</p>
CLASS zcl_ca_base_toolbox_apm DEFINITION PUBLIC
                                         FINAL
                                         CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_apack_manifest.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor.
ENDCLASS.



CLASS ZCL_CA_BASE_TOOLBOX_APM IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    zif_apack_manifest~descriptor =
         VALUE #(
            group_id        = 'itinere.at'
            artifact_id     = 'zca_tbx_base'
            version         = '0.1.0'
            repository_type = 'abapGit'
            git_url         = 'https://github.com/JSB-Vienna/zca_tbx_base.git'
            dependencies    =
                  VALUE #(
                        group_id       = 'itinere.at'
                      ( artifact_id    = 'zca_toolbox'
                        git_url        = 'https://github.com/JSB-Vienna/zca_toolbox.git'
                        version        = '1.0.0'
                        target_package = 'zca_toolbox' )
                      ( artifact_id    = 'zca_tbx_base_exceptions'
                        git_url        = 'https://github.com/JSB-Vienna/zca_tbx_base_exceptions.git'
                        version        = '0.1.0'
                        target_package = 'zca_tbx_base_exceptions' ) ) ) ##no_text.
  ENDMETHOD.                    "constructor
ENDCLASS.
