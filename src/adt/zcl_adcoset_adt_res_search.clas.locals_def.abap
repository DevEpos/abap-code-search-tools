*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF ty_param_flags,
    negation             TYPE abap_bool,
    patterns             TYPE abap_bool,
    auto_prefix_matching TYPE abap_bool,
  END OF ty_param_flags.

"! Code Search query
CLASS lcl_search_query DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      "! Creates new search query
      constructor
        IMPORTING
          request TYPE REF TO if_adt_rest_request,
      "! Runs the query
      run
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_adt_types=>ty_code_search_result
        RAISING
          cx_adt_rest.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      request TYPE REF TO if_adt_rest_request,
      BEGIN OF matcher_config,
        use_regex   TYPE abap_bool,
        enable_pcre TYPE abap_bool,
      END OF matcher_config,
      read_packages TYPE abap_bool,
      settings      TYPE zif_adcoset_ty_global=>ty_search_settings_external.

    METHODS:
      get_matcher_type
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_matcher_type,
      parse_parameters
        IMPORTING
          request TYPE REF TO if_adt_rest_request
        RAISING
          cx_adt_rest,
      get_patterns
        RAISING
          cx_adt_rest,
      get_persisted_settings,
      get_class_scope
        RAISING
          cx_adt_rest,
      complete_settings.
ENDCLASS.


CLASS lcl_search_result DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          raw_result             TYPE zif_adcoset_ty_global=>ty_search_result
          read_package_hierarchy TYPE abap_bool OPTIONAL,
      convert_to_adt_result
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_adt_types=>ty_code_search_result.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      raw_result             TYPE zif_adcoset_ty_global=>ty_search_result,
      read_package_hierarchy TYPE abap_bool,
      adt_obj_factory        TYPE REF TO zif_adcoset_adt_obj_factory,
      adt_result             TYPE zif_adcoset_ty_adt_types=>ty_code_search_result.

    METHODS:
      set_durations,
      create_adt_links,
      add_main_object_ref
        IMPORTING
          search_result_object TYPE REF TO zif_adcoset_ty_adt_types=>ty_code_search_object
          object_info          TYPE zif_adcoset_ty_global=>ty_tadir_object
          raw_match            TYPE zif_adcoset_ty_global=>ty_search_match,
      create_match_objects
        IMPORTING
          search_result_object TYPE REF TO zif_adcoset_ty_adt_types=>ty_code_search_object
          object_info          TYPE zif_adcoset_ty_global=>ty_tadir_object
          raw_matches          TYPE zif_adcoset_ty_global=>ty_search_matches
        RAISING
          zcx_adcoset_static_error,
      create_match_object_ref
        IMPORTING
          object_info   TYPE zif_adcoset_ty_global=>ty_tadir_object
          match         TYPE zif_adcoset_ty_global=>ty_search_match
        RETURNING
          VALUE(result) TYPE sadt_object_reference
        RAISING
          zcx_adcoset_static_error,
      determine_package_hierarchy,
      create_incl_match_objects
        IMPORTING
          parent_search_result_object TYPE zif_adcoset_ty_adt_types=>ty_code_search_object
          object_info                 TYPE zif_adcoset_ty_global=>ty_tadir_object
          raw_matches                 TYPE zif_adcoset_ty_global=>ty_search_matches
        RETURNING
          VALUE(result)               TYPE zif_adcoset_ty_adt_types=>ty_code_search_objects,
      create_std_match_objects
        IMPORTING
          search_result_object TYPE REF TO zif_adcoset_ty_adt_types=>ty_code_search_object
          object_info          TYPE zif_adcoset_ty_global=>ty_tadir_object
          raw_matches          TYPE zif_adcoset_ty_global=>ty_search_matches
        RAISING
          zcx_adcoset_static_error.
ENDCLASS.
