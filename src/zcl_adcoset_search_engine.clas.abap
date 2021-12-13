"! <p class="shorttext synchronized" lang="en">Code Search Engine</p>
CLASS zcl_adcoset_search_engine DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieves instance of search engine</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_adcoset_search_engine,
      "! <p class="shorttext synchronized" lang="en">Code search for a given package</p>
      "! <strong>NOTE:</strong> <br>
      "! This method should only be called from a parallel task
      search_code_in_package
        IMPORTING
          input  TYPE zif_adcoset_ty_global=>ty_search_package
        EXPORTING
          output TYPE zif_adcoset_ty_global=>ty_search_matches.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Search source code</p>
      search_code
        IMPORTING
          search_config TYPE zif_adcoset_ty_global=>ty_search_settings_external
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_matches
        RAISING
          zcx_adcoset_static_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      instance TYPE REF TO zcl_adcoset_search_engine.

    METHODS:
      validate_matchers
        IMPORTING
          matcher_type TYPE zif_adcoset_ty_global=>ty_matcher_type
          patterns     TYPE zif_adcoset_ty_global=>ty_search_settings_external-pattern_range
        RAISING
          zcx_adcoset_static_error.
ENDCLASS.



CLASS zcl_adcoset_search_engine IMPLEMENTATION.


  METHOD get_instance.
    IF instance IS INITIAL.
      instance = NEW #( ).
    ENDIF.

    result = instance.
  ENDMETHOD.


  METHOD search_code_in_package.
    TRY.
        DATA(query) = zcl_adcoset_search_query_fac=>create_query(
          scope    = zcl_adcoset_search_scope_fac=>create_final_scope( objects = input-objects )
          settings = input-settings ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.

    query->run( ).

    output = query->get_results( ).
  ENDMETHOD.


  METHOD search_code.
    validate_matchers(
      matcher_type = search_config-matcher_type
      patterns     = search_config-pattern_range ).

    DATA(query) = zcl_adcoset_search_query_fac=>create_query(
      parallel_processing = search_config-parallel_processing
      scope               = zcl_adcoset_search_scope_fac=>create_scope(
        search_scope  = search_config-search_scope
        parallel_mode = search_config-parallel_processing-enabled )
      settings            = search_config-internal_settings ).

    query->run( ).

    result = query->get_results( ).
  ENDMETHOD.


  METHOD validate_matchers.
    CHECK matcher_type <> zif_adcoset_c_global=>c_matcher_type-substring.

    TRY.
        LOOP AT patterns ASSIGNING FIELD-SYMBOL(<pattern_range>).
          zcl_adcoset_matcher_factory=>create_matcher(
            type    = matcher_type
            pattern = <pattern_range>-low ).
        ENDLOOP.
      CATCH cx_sy_regex INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_adcoset_static_error
          EXPORTING
            text = error->get_text( ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
