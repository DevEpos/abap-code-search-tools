"! <p class="shorttext synchronized">Resource to handle user specific settings of Code Search</p>
CLASS zcl_adcoset_adt_res_cs_config DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get REDEFINITION.
    METHODS put REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS validate_settings
      IMPORTING
        settings TYPE zif_adcoset_ty_adt_types=>ty_code_search_settings
      RAISING
        cx_adt_rest.

    METHODS complete_settings
      CHANGING
        settings TYPE zif_adcoset_ty_adt_types=>ty_code_search_settings.
ENDCLASS.


CLASS zcl_adcoset_adt_res_cs_config IMPLEMENTATION.
  METHOD get.
    response->set_body_data( content_handler = zcl_adcoset_adt_ch_factory=>create_cs_settings_ch( )
                             data            = zcl_adcoset_search_settings=>get_settings( ) ).
  ENDMETHOD.

  METHOD put.
    DATA settings TYPE zif_adcoset_ty_adt_types=>ty_code_search_settings.

    request->get_body_data( EXPORTING content_handler = zcl_adcoset_adt_ch_factory=>create_cs_settings_ch( )
                            IMPORTING data            = settings ).

    validate_settings( settings ).
    complete_settings( CHANGING settings = settings ).

    MODIFY zadcoset_csset FROM settings.
  ENDMETHOD.

  METHOD validate_settings.
    IF settings-parallel_enabled = abap_false.
      RETURN.
    ENDIF.

    IF settings-parallel_server_group IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM rzllitab
        WHERE classname = @settings-parallel_server_group
          AND grouptype = @zif_adcoset_c_global=>c_group_type_server_group
        INTO @DATA(group_exists).

      IF group_exists = abap_false.
        RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
          EXPORTING text = |The server group '{ settings-parallel_server_group }' does not exist|.
      ENDIF.
    ENDIF.

    IF    settings-parallel_proc_pack_size < zif_adcoset_c_global=>c_parl_proc_min_pack_size
       OR settings-parallel_proc_pack_size > zif_adcoset_c_global=>c_parl_proc_max_pack_size.
      RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
        EXPORTING text = |Package size must be between {
                           zif_adcoset_c_global=>c_parl_proc_min_pack_size NUMBER = USER } | &&
                         |and { zif_adcoset_c_global=>c_parl_proc_max_pack_size NUMBER = USER }|.
    ENDIF.
  ENDMETHOD.

  METHOD complete_settings.
    settings-uname = sy-uname.
    IF settings-parallel_proc_pack_size IS INITIAL.
      settings-parallel_proc_pack_size = zif_adcoset_c_global=>c_parl_proc_min_pack_size.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
