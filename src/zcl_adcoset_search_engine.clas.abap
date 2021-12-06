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
          VALUE(result) TYPE REF TO zcl_adcoset_search_engine.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Search source code</p>
      search_code
        IMPORTING
          search_config TYPE zif_adcoset_ty_global=>ty_search_settings_extended
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
          patterns     TYPE zif_adcoset_ty_global=>ty_search_settings_extended-pattern_range
        RAISING
          zcx_adcoset_static_error,
      create_matchers
        IMPORTING
          matcher_type  TYPE zif_adcoset_ty_global=>ty_matcher_type
          ignore_case   TYPE abap_bool
          patterns      TYPE zif_adcoset_ty_global=>ty_search_settings_extended-pattern_range
        RETURNING
          VALUE(result) TYPE zif_adcoset_pattern_matcher=>ty_ref_tab
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


  METHOD search_code.
    " 1) create matchers / validate patterns (if regex is active)
    validate_matchers(
      matcher_type = search_config-matcher_type
      patterns     = search_config-pattern_range ).

    " 2) create scope
    DATA(scope) = CAST zif_adcoset_search_scope( NEW zcl_adcoset_search_scope( search_config-search_scope ) ).

    " 3) create processing packages (if parallel processing)

    " 4) process objects in scope

    " 5) collect / enrich results
  ENDMETHOD.


  METHOD create_matchers.

    LOOP AT patterns ASSIGNING FIELD-SYMBOL(<pattern_range>).
      TRY.
          result = VALUE #( BASE result
            ( zcl_adcoset_matcher_factory=>create_matcher(
                type        = matcher_type
                pattern     = <pattern_range>-low
                ignore_case = ignore_case ) ) ).
        CATCH zcx_adcoset_no_matcher
              cx_sy_regex INTO DATA(matcher_error).
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              previous = matcher_error.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD validate_matchers.
    CHECK matcher_type <> zif_adcoset_c_global=>c_matcher_type-substring.

    LOOP AT patterns ASSIGNING FIELD-SYMBOL(<pattern_range>).
      zcl_adcoset_matcher_factory=>create_matcher(
        type    = matcher_type
        pattern = <pattern_range>-low ).
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
