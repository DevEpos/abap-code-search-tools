"! <p class="shorttext synchronized" lang="en">Search query for code search</p>
CLASS zcl_adcoset_search_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_search_query.

    METHODS:
      constructor
        IMPORTING
          scope           TYPE REF TO zif_adcoset_search_scope
          settings        TYPE zif_adcoset_ty_global=>ty_search_settings
          custom_settings TYPE zif_adcoset_ty_global=>ty_custom_search_settings
          matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab
          monitor         TYPE REF TO zif_adcoset_search_progmon.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      scope           TYPE REF TO zif_adcoset_search_scope,
      monitor         TYPE REF TO zif_adcoset_search_progmon,
      settings        TYPE zif_adcoset_ty_global=>ty_search_settings,
      custom_settings TYPE zif_adcoset_ty_global=>ty_custom_search_settings,
      matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab,
      search_results  TYPE zif_adcoset_ty_global=>ty_search_result_objects.
    METHODS handle_matches
      IMPORTING
        object  TYPE zif_adcoset_ty_global=>ty_tadir_object
        matches TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDCLASS.



CLASS zcl_adcoset_search_query IMPLEMENTATION.

  METHOD constructor.
    ASSERT:
      scope IS BOUND,
      matchers IS NOT INITIAL.

    me->scope = scope.
    me->monitor = monitor.
    me->settings = settings.
    me->custom_settings = custom_settings.
    me->matchers = matchers.
  ENDMETHOD.

  METHOD zif_adcoset_search_query~run.

    WHILE scope->has_next_package( ).

      LOOP AT scope->next_package( ) ASSIGNING FIELD-SYMBOL(<object>).
        IF monitor->is_done( ).
          RETURN.
        ENDIF.

        TRY.
            DATA(source_code_provider) = zcl_adcoset_csp_factory=>get_search_provider(
              type            = <object>-type
              search_settings = settings
              custom_settings = custom_settings
              matchers        = matchers ).

            DATA(source_code_reader) = zcl_adcoset_scr_factory=>get_reader(
              type         = <object>-type
              is_multiline = settings-multiline_search
              line_feed    = settings-line_feed ).

            DATA(matches) = source_code_provider->search(
              object          = <object>
              src_code_reader = source_code_reader ).

            IF matches IS NOT INITIAL.
              handle_matches(
                object  = <object>
                matches = matches ).
            ENDIF.
          CATCH zcx_adcoset_static_error.
        ENDTRY.
      ENDLOOP.

    ENDWHILE.

  ENDMETHOD.


  METHOD zif_adcoset_search_query~get_results.
    result = search_results.
  ENDMETHOD.


  METHOD handle_matches.

    DATA(match_count) = lines( matches ).
    monitor->add_match_count( match_count ).
    DATA(search_result) = VALUE zif_adcoset_ty_global=>ty_search_result_object(
      object       = object
      text_matches = matches
      match_count  = match_count ).
    INSERT search_result INTO TABLE search_results.

  ENDMETHOD.

ENDCLASS.
