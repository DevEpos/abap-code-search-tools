*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES:
  BEGIN OF ty_param_flags,
    negation TYPE abap_bool,
    patterns TYPE abap_bool,
  END OF ty_param_flags.

"! Code Search query
CLASS lcl_search_query DEFINITION
  FINAL
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
      request  TYPE REF TO if_adt_rest_request,
      settings TYPE zif_adcoset_ty_global=>ty_search_settings_external.

    METHODS:
      get_matcher_type
        IMPORTING
          use_regex     TYPE abap_bool
          use_pcre      TYPE abap_bool
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_matcher_type,
      parse_parameters
        IMPORTING
          request TYPE REF TO if_adt_rest_request
        RAISING
          cx_adt_rest,
      get_owners
        RAISING
          cx_adt_rest,
      get_packages,
      get_appl_comps,
      get_object_types,
      get_object_names,
      get_created_dates,
      get_patterns,
      fill_range
        IMPORTING
          input TYPE string_table
          flags TYPE ty_param_flags OPTIONAL
        EXPORTING
          range TYPE ANY TABLE.
ENDCLASS.
