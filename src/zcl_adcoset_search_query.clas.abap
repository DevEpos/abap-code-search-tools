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
          matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      scope             TYPE REF TO zif_adcoset_search_scope,
      settings          TYPE zif_adcoset_ty_global=>ty_search_settings,
      custom_settings   TYPE zif_adcoset_ty_global=>ty_custom_search_settings,
      src_code_searcher TYPE REF TO zif_adcoset_src_code_searcher,
      search_results    TYPE zif_adcoset_ty_global=>ty_search_result_objects.
    METHODS search_dependent_includes
      IMPORTING
        source_code_provider TYPE REF TO zif_adcoset_code_search_prov
        source_code_reader   TYPE REF TO zif_adcoset_src_code_reader
        object               TYPE zif_adcoset_ty_global=>ty_tadir_object.
ENDCLASS.



CLASS zcl_adcoset_search_query IMPLEMENTATION.

  METHOD constructor.
    ASSERT:
      scope IS BOUND,
      matchers IS NOT INITIAL.

    me->scope = scope.
    me->settings = settings.
    me->custom_settings = custom_settings.
    src_code_searcher = zcl_adcoset_scs_factory=>create(
      matchers = matchers
      settings = settings ).
  ENDMETHOD.

  METHOD zif_adcoset_search_query~run.

    WHILE scope->has_next_package( ).

      LOOP AT scope->next_package( ) ASSIGNING FIELD-SYMBOL(<object>).
        TRY.
            DATA(source_code_provider) = zcl_adcoset_csp_factory=>get_search_provider(
              type            = <object>-type
              custom_settings = custom_settings ).

            DATA(source_code_reader) = zcl_adcoset_scr_factory=>get_reader(
              type         = <object>-type
              is_multiline = settings-multiline_search
              line_feed    = settings-line_feed ).

            DATA(matches) = source_code_provider->search(
              object            = <object>
              src_code_searcher = src_code_searcher
              src_code_reader   = source_code_reader ).

            IF matches IS NOT INITIAL.
              INSERT VALUE #(
                object       = <object>
                text_matches = matches
                match_count  = lines( matches ) ) INTO TABLE search_results.
            ENDIF.

            IF <object>-type = zif_adcoset_c_global=>c_source_code_type-program.
              search_dependent_includes(
                source_code_provider = source_code_provider
                source_code_reader   = source_code_reader
                object               = <object> ).
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


  METHOD search_dependent_includes.
    DATA: includes      TYPE STANDARD TABLE OF progname,
          include_infos TYPE zif_adcoset_ty_global=>ty_tadir_objects.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program    = object-name
      TABLES
        includetab = includes
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc = 0 AND includes IS NOT INITIAL.
      " select additional information about includes
      SELECT obj_name AS name,
             object AS type,
             devclass AS package_name,
             author AS owner
        FROM tadir
        FOR ALL ENTRIES IN @includes
        WHERE obj_name = @includes-table_line
          AND object = @zif_adcoset_c_global=>c_source_code_type-program
        INTO CORRESPONDING FIELDS OF TABLE @include_infos.

      LOOP AT include_infos ASSIGNING FIELD-SYMBOL(<include_info>).
        " only search include if not already searched
        IF line_exists( search_results[ object-name = <include_info>-name
                                        object-type = <include_info>-type ] ).
          CONTINUE.
        ENDIF.

        DATA(matches) = source_code_provider->search(
          object            = <include_info>
          src_code_searcher = src_code_searcher
          src_code_reader   = source_code_reader ).

        IF matches IS NOT INITIAL.
          INSERT VALUE #(
            object       = <include_info>
            text_matches = matches
            match_count  = lines( matches ) ) INTO TABLE search_results.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.

ENDCLASS.
