"! <p class="shorttext synchronized" lang="en">ADT content handler factory</p>
CLASS zcl_adcoset_adt_ch_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates content handler for feature list</p>
      create_feature_list_ch
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      "! <p class="shorttext synchronized" lang="en">Creates content handler for code search settings</p>
      create_cs_settings_ch
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler,
      "! <p class="shorttext synchronized" lang="en">Creates content handler for code search result</p>
      create_search_result_ch
        RETURNING
          VALUE(result) TYPE REF TO if_adt_rest_content_handler.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_adt_ch_factory IMPLEMENTATION.


  METHOD create_feature_list_ch.
    result = cl_adt_rest_st_handler=>create_instance(
      st_name   = 'ZADCOSET_ADT_PLUGIN_FEATURES'
      root_name = 'PLUGIN_FEATURES' ).
  ENDMETHOD.


  METHOD create_cs_settings_ch.
    result = cl_adt_rest_st_handler=>create_instance(
      st_name   = 'ZADCOSET_SEARCH_SETTINGS'
      root_name = 'SETTINGS' ).
  ENDMETHOD.


  METHOD create_search_result_ch.
    result = cl_adt_rest_st_handler=>create_instance(
      st_name   = 'ZADCOSET_SEARCH_RESULT'
      root_name = 'ROOT' ).
  ENDMETHOD.

ENDCLASS.
