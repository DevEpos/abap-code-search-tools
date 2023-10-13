"! <p class="shorttext synchronized">Search query for code search</p>
CLASS zcl_adcoset_search_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_search_query.

    METHODS constructor
      IMPORTING
        !scope          TYPE REF TO zif_adcoset_search_scope
        settings        TYPE zif_adcoset_ty_global=>ty_search_settings
        custom_settings TYPE zif_adcoset_ty_global=>ty_custom_search_settings
        matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.

  PRIVATE SECTION.
    DATA scope TYPE REF TO zif_adcoset_search_scope.
    DATA settings TYPE zif_adcoset_ty_global=>ty_search_settings.
    DATA custom_settings TYPE zif_adcoset_ty_global=>ty_custom_search_settings.
    DATA src_code_searcher TYPE REF TO zif_adcoset_src_code_searcher.
    DATA search_results TYPE zif_adcoset_ty_global=>ty_search_result_objects.
ENDCLASS.


CLASS zcl_adcoset_search_query IMPLEMENTATION.
  METHOD constructor.
    ASSERT:
      scope IS BOUND,
      matchers IS NOT INITIAL.

    me->scope           = scope.
    me->settings        = settings.
    me->custom_settings = custom_settings.
    src_code_searcher = zcl_adcoset_scs_factory=>create( matchers = matchers
                                                         settings = settings ).
  ENDMETHOD.

  METHOD zif_adcoset_search_query~run.
    WHILE scope->has_next_package( ).

      LOOP AT scope->next_package( ) ASSIGNING FIELD-SYMBOL(<object>).
        TRY.
            DATA(source_code_provider) = zcl_adcoset_csp_factory=>get_search_provider(
                                             type            = <object>-type
                                             custom_settings = custom_settings ).

            DATA(source_code_reader) = zcl_adcoset_scr_factory=>get_reader( type         = <object>-type
                                                                            is_multiline = settings-multiline_search
                                                                            line_feed    = settings-line_feed ).

            DATA(matches) = source_code_provider->search( object            = <object>
                                                          src_code_searcher = src_code_searcher
                                                          src_code_reader   = source_code_reader ).

            IF matches IS NOT INITIAL.
              INSERT VALUE #( object       = <object>
                              text_matches = matches
                              match_count  = lines( matches ) ) INTO TABLE search_results.
            ENDIF.
          CATCH zcx_adcoset_static_error.
        ENDTRY.

        zcl_adcoset_search_protocol=>increment_searched_objs_count( ).
      ENDLOOP.

    ENDWHILE.
  ENDMETHOD.

  METHOD zif_adcoset_search_query~get_results.
    result = search_results.
  ENDMETHOD.
ENDCLASS.
