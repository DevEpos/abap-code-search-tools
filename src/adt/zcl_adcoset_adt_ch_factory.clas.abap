"! <p class="shorttext synchronized">ADT content handler factory</p>
CLASS zcl_adcoset_adt_ch_factory DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Creates content handler for feature list</p>
    CLASS-METHODS create_feature_list_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Creates content handler for code search settings</p>
    CLASS-METHODS create_cs_settings_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Creates content handler for code search result</p>
    CLASS-METHODS create_search_result_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Creates content handler for Code Search Scope</p>
    CLASS-METHODS create_search_scope_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.

    "! <p class="shorttext synchronized">Creates content handler for Code Search Scope Parameters</p>
    CLASS-METHODS create_search_scope_params_ch
      RETURNING
        VALUE(result) TYPE REF TO if_adt_rest_content_handler.
ENDCLASS.


CLASS zcl_adcoset_adt_ch_factory IMPLEMENTATION.
  METHOD create_feature_list_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZADCOSET_ADT_PLUGIN_FEATURES'
                                                      root_name = 'PLUGIN_FEATURES' ).
  ENDMETHOD.

  METHOD create_cs_settings_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZADCOSET_SEARCH_SETTINGS'
                                                      root_name = 'SETTINGS' ).
  ENDMETHOD.

  METHOD create_search_result_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZADCOSET_SEARCH_RESULT'
                                                      root_name = 'ROOT' ).
  ENDMETHOD.

  METHOD create_search_scope_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZADCOSET_SEARCH_SCOPE'
                                                      root_name = 'SCOPE' ).
  ENDMETHOD.

  METHOD create_search_scope_params_ch.
    result = cl_adt_rest_st_handler=>create_instance( st_name   = 'ZADCOSET_SEARCH_SCOPE_PARAMS'
                                                      root_name = 'SCOPE_PARAMS' ).
  ENDMETHOD.
ENDCLASS.
