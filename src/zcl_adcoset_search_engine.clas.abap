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
      "! <p class="shorttext synchronized" lang="en">Runs code search (for ARFC call)</p>
      "! <strong>NOTE:</strong> <br>
      "! This method should only be called from a parallel task, see {@link zif_adcoset_parl_task_runner }
      run_code_search_arfc
        IMPORTING
          input  TYPE zif_adcoset_ty_global=>ty_search_package
        EXPORTING
          output TYPE zif_adcoset_ty_global=>ty_search_package_result.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Search source code</p>
      search_code
        IMPORTING
          search_config TYPE zif_adcoset_ty_global=>ty_search_settings_external
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_result
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
          patterns     TYPE zif_adcoset_ty_global=>ty_patterns
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


  METHOD run_code_search_arfc.
    zcl_adcoset_parl_proc_utils=>assert_async_rfc_call( ).

    TRY.
        DATA(query) = zcl_adcoset_search_query_fac=>create_query(
          scope    = zcl_adcoset_search_scope_fac=>create_final_scope( objects = input-objects )
          settings = input-settings ).
      CATCH zcx_adcoset_static_error.
    ENDTRY.

    query->run( ).

    output = VALUE #(
      result_objects         = query->get_results( )
      searched_objects_count = zcl_adcoset_search_protocol=>get_searched_object_count( )
      searched_sources_count = zcl_adcoset_search_protocol=>get_searched_sources_count( )
      loc                    = zcl_adcoset_search_protocol=>get_loc( )
      messages               = zcl_adcoset_log=>get_messages( ) ).
  ENDMETHOD.


  METHOD search_code.
    zcl_adcoset_log=>clear( ).
    zcl_adcoset_search_protocol=>reset( ).

    validate_matchers(
      matcher_type = search_config-matcher_type
      patterns     = search_config-patterns ).

    DATA(query) = zcl_adcoset_search_query_fac=>create_query(
      parallel_processing = search_config-parallel_processing
      scope               = zcl_adcoset_search_scope_fac=>create_scope(
        search_scope  = search_config-search_scope )
      settings            = search_config-internal_settings ).

    DATA(timer) = cl_abap_runtime=>create_hr_timer( ).
    timer->get_runtime( ).
    query->run( ).
    DATA(duration) = timer->get_runtime( ).

    result = VALUE #(
      results                = query->get_results( )
      messages               = zcl_adcoset_log=>get_messages( )
      duration_in_ms         = duration / 1000
      searched_objects_count = zcl_adcoset_search_protocol=>get_searched_object_count( )
      searched_sources_count = zcl_adcoset_search_protocol=>get_searched_sources_count( )
      loc                    = zcl_adcoset_search_protocol=>get_loc( ) ).
  ENDMETHOD.


  METHOD validate_matchers.
    CHECK matcher_type <> zif_adcoset_c_global=>c_matcher_type-substring.

    TRY.
        LOOP AT patterns ASSIGNING FIELD-SYMBOL(<pattern>).
          zcl_adcoset_matcher_factory=>create_matcher(
            type    = matcher_type
            pattern = <pattern> ).
        ENDLOOP.
      CATCH cx_sy_regex INTO DATA(error).
        RAISE EXCEPTION TYPE zcx_adcoset_static_error
          EXPORTING
            text = error->get_text( ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
