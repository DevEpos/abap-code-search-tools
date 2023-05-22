*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_search_query IMPLEMENTATION.


  METHOD constructor.
    me->request = request.
  ENDMETHOD.


  METHOD run.
    parse_parameters( request ).
    complete_settings( ).

    TRY.
        DATA(search_result) = zcl_adcoset_search_engine=>get_instance( )->search_code( search_config = settings ).
        result = NEW lcl_result_converter(
          raw_result             = search_result
          read_package_hierarchy = read_packages )->convert( ).
        result-messages = search_result-messages.
      CATCH zcx_adcoset_static_error INTO DATA(search_error).
        RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
          EXPORTING
            previous = search_error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_matcher_type.

    IF matcher_config-use_regex = abap_true.
      IF zcl_adcoset_pcre_util=>is_pcre_supported( ).
        result = zif_adcoset_c_global=>c_matcher_type-pcre.
      ELSE.
        result = zif_adcoset_c_global=>c_matcher_type-posix_regex.
      ENDIF.
    ELSE.
      result = zif_adcoset_c_global=>c_matcher_type-substring.
    ENDIF.

  ENDMETHOD.


  METHOD parse_parameters.
    settings-ignore_case = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-ignore_case
      request    = request ).

    settings-ignore_comment_lines = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-ignore_comment_lines
      request    = request ).

    settings-multiline_search = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-multi_line
      request    = request ).

    settings-match_all_patterns = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-match_all_patterns
      request    = request ).

    settings-sequential_matching = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-sequential_matching
      request    = request ).

    matcher_config-use_regex = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-use_regex
      request    = request ).

    settings-search_scope = VALUE #(
      max_objects    = zcl_adcoset_adt_request_util=>get_integer_query_parameter(
        param_name = zif_adcoset_c_global=>c_search_params-max_objects
        request    = request )
      scope_id       = zcl_adcoset_adt_request_util=>get_uuid_uri_query_parameter(
        param_name = zif_adcoset_c_global=>c_search_params-scope_id
        mandatory  = abap_true
        request    = request )
      current_offset = zcl_adcoset_adt_request_util=>get_integer_query_parameter(
        param_name = zif_adcoset_c_global=>c_search_params-scope_offset
        mandatory  = abap_true
        request    = request ) ).

    get_patterns( ).
    get_class_scope( ).
    get_fugr_scope( ).

    settings-custom_settings-prog-expand_includes = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-expand_prog_includes
      request    = request ).

  ENDMETHOD.


  METHOD complete_settings.
    get_persisted_settings( ).
    settings-matcher_type = get_matcher_type( ).
    settings-line_feed = |\n|.
    settings-is_adt = abap_true.
  ENDMETHOD.


  METHOD get_patterns.
    settings-patterns = VALUE #(
      FOR pattern IN  zcl_adcoset_adt_request_util=>get_query_parameter_values(
        param_name = zif_adcoset_c_global=>c_search_params-search_pattern
        mandatory  = abap_true
        request    = request )
      ( content = replace( val = pattern sub = |\r\n| with = |\n| occ = 0 ) ) ).

    IF settings-sequential_matching = abap_true.
      TRY.
          settings-patterns = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns = settings-patterns ).
        CATCH zcx_adcoset_static_error INTO DATA(parse_error).
          " Safety precausion. If called from ADT the sequence is already validated
          RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
            EXPORTING
              previous = parse_error.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD get_persisted_settings.
    DATA(server_settings) = zcl_adcoset_search_settings=>get_settings( ).
    settings-pcre_settings = VALUE #(
      extended_mode_disabled   = server_settings-pcre_ext_mode_disabled
      single_line_mode_enabled = server_settings-pcre_single_line_enabled ).
    settings-parallel_processing = VALUE #(
      enabled      = server_settings-parallel_enabled
      server_group = server_settings-parallel_server_group ).
  ENDMETHOD.


  METHOD get_class_scope.
    DATA: scopes TYPE string_table.

    DATA(scope_list) = zcl_adcoset_adt_request_util=>get_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-class_includes
      request    = request ).

    IF scope_list IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT scope_list AT ',' INTO TABLE scopes.

    ASSIGN settings-custom_settings-class-include_flags TO FIELD-SYMBOL(<class_incl_flags>).

    LOOP AT scopes INTO DATA(scope).
      CASE scope.

        WHEN zif_adcoset_c_global=>c_class_include_id-all.
          <class_incl_flags> = VALUE #(
            public_section    = abap_true
            protected_section = abap_true
            private_section   = abap_true
            methods           = abap_true
            main              = abap_true
            test              = abap_true
            macro             = abap_true
            local_def         = abap_true
            local_impl        = abap_true ).
          EXIT.

        WHEN zif_adcoset_c_global=>c_class_include_id-public_section.
          <class_incl_flags>-public_section = abap_true.

        WHEN zif_adcoset_c_global=>c_class_include_id-protected_section.
          <class_incl_flags>-protected_section = abap_true.

        WHEN zif_adcoset_c_global=>c_class_include_id-private_section.
          <class_incl_flags>-private_section = abap_true.

        WHEN zif_adcoset_c_global=>c_class_include_id-methods.
          <class_incl_flags>-methods = abap_true.

        WHEN zif_adcoset_c_global=>c_class_include_id-local_definitions.
          <class_incl_flags>-local_def = abap_true.

        WHEN zif_adcoset_c_global=>c_class_include_id-local_implementation.
          <class_incl_flags>-local_impl = abap_true.

        WHEN zif_adcoset_c_global=>c_class_include_id-macros.
          <class_incl_flags>-macro = abap_true.

        WHEN zif_adcoset_c_global=>c_class_include_id-tests.
          <class_incl_flags>-test = abap_true.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_fugr_scope.
    DATA: scopes TYPE string_table.

    DATA(scope_list) = zcl_adcoset_adt_request_util=>get_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-fugr_includes
      request    = request ).

    IF scope_list IS INITIAL.
      RETURN.
    ENDIF.

    SPLIT scope_list AT ',' INTO TABLE scopes.

    ASSIGN settings-custom_settings-fugr-include_flags TO FIELD-SYMBOL(<fugr_incl_flags>).

    LOOP AT scopes INTO DATA(scope).
      CASE scope.

        WHEN zif_adcoset_c_global=>c_fugr_include_id-all.
          <fugr_incl_flags> = VALUE #(
            function     = abap_true
            non_function = abap_true ).
          EXIT.

        WHEN zif_adcoset_c_global=>c_fugr_include_id-function.
          <fugr_incl_flags>-function = abap_true.

        WHEN zif_adcoset_c_global=>c_fugr_include_id-non_function.
          <fugr_incl_flags>-non_function = abap_true.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_result_converter IMPLEMENTATION.

  METHOD constructor.
    me->raw_result = raw_result.
    adt_obj_factory = zcl_adcoset_adt_obj_factory=>get_instance( ).
    cds_name_mapper = NEW #( ).
  ENDMETHOD.


  METHOD convert.
    init_result( ).

    determine_package_hierarchy( ).
    add_packages_to_adt_result( ).

    convert_matches_to_adt_result( ).
    adjust_cds_display_names( ).

    result = adt_result.
  ENDMETHOD.


  METHOD init_result.
    adt_result-query_time_in_ms = raw_result-duration_in_ms.
    adt_result-number_of_searched_objects = raw_result-searched_objects_count.
    adt_result-number_of_searched_sources = raw_result-searched_sources_count.
    adt_result-loc = raw_result-loc.
  ENDMETHOD.


  METHOD convert_matches_to_adt_result.

    LOOP AT raw_result-results ASSIGNING FIELD-SYMBOL(<raw_result>).

      TRY.
          DATA(search_result_object) = VALUE zif_adcoset_ty_adt_types=>ty_code_search_object(
            parent_uri      = get_package_uri( <raw_result>-object-package_name )
            adt_main_object = VALUE #(
              name        = <raw_result>-object-name
              responsible = <raw_result>-object-owner ) ).

          DATA(adt_ref) = adt_obj_factory->get_object_ref_for_trobj(
            type = <raw_result>-object-type
            name = <raw_result>-object-name ).

          search_result_object-uri = adt_ref-uri.
          search_result_object-adt_main_object-type = adt_ref-type.

          create_match_objects(
            search_result_object = REF #( search_result_object )
            object_info          = <raw_result>-object
            raw_matches          = <raw_result>-text_matches ).

        CATCH zcx_adcoset_static_error ##needed.
          " ignore the result in case of a mapping error
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_match_objects.
    DATA(l_raw_matches) = raw_matches.

    IF object_info-type = zif_adcoset_c_global=>c_source_code_type-program.
      " handle direct program matches
      DATA(direct_program_matches) = VALUE zif_adcoset_ty_global=>ty_search_matches(
        FOR <prog_raw_match> IN l_raw_matches
        WHERE ( include = object_info-name )
        ( <prog_raw_match> ) ).
      DELETE l_raw_matches WHERE include = object_info-name.
      create_std_match_objects(
        search_result_object = search_result_object
        object_info          = object_info
        raw_matches          = direct_program_matches ).
    ENDIF.

    IF object_info-type = zif_adcoset_c_global=>c_source_code_type-class OR
        object_info-type = zif_adcoset_c_global=>c_source_code_type-function_group OR
        object_info-type = zif_adcoset_c_global=>c_source_code_type-program.
      DATA(incl_match_objects) = create_incl_match_objects(
        parent_search_result_object = search_result_object->*
        object_info                 = object_info
        raw_matches                 = l_raw_matches ).

      IF incl_match_objects IS NOT INITIAL.
        adt_result-code_search_objects = VALUE #( BASE adt_result-code_search_objects
         ( search_result_object->* )
         ( LINES OF incl_match_objects ) ).
      ELSEIF object_info-type = zif_adcoset_c_global=>c_source_code_type-program AND
          search_result_object->matches IS NOT INITIAL.
        adt_result-code_search_objects = VALUE #( BASE adt_result-code_search_objects
         ( search_result_object->* ) ).
      ENDIF.
    ELSE.
      create_std_match_objects(
        search_result_object = search_result_object
        object_info          = object_info
        raw_matches          = l_raw_matches ).

      IF search_result_object->matches IS NOT INITIAL.
        APPEND search_result_object->* TO adt_result-code_search_objects REFERENCE INTO DATA(added_result_obj).

        IF cds_name_mapper->collect_entry( name = CONV #( search_result_object->adt_main_object-name )
                                           type = CONV #( search_result_object->adt_main_object-type(4) ) ).
          main_objs_for_name_mapping = VALUE #( BASE main_objs_for_name_mapping ( REF #( added_result_obj->adt_main_object ) ) ).
        ENDIF.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD determine_package_hierarchy.
    DATA: packages_to_read TYPE STANDARD TABLE OF ty_package,
          read_packages    TYPE STANDARD TABLE OF ty_package.

    packages_to_read = VALUE #(
      FOR <res_obj> IN raw_result-results ( package_name = <res_obj>-object-package_name ) ).
    SORT packages_to_read.
    DELETE ADJACENT DUPLICATES FROM packages_to_read.

    WHILE packages_to_read IS NOT INITIAL.
      SELECT devclass AS package_name,
             parentcl AS parent_package_name
        FROM tdevc
        FOR ALL ENTRIES IN @packages_to_read
        WHERE devclass = @packages_to_read-package_name
        INTO CORRESPONDING FIELDS OF TABLE @read_packages.

      CLEAR packages_to_read.

      LOOP AT read_packages ASSIGNING FIELD-SYMBOL(<read_package>).
        CHECK NOT line_exists( packages[ package_name = <read_package>-package_name ] ).

        IF <read_package>-parent_package_name IS NOT INITIAL.
          packages_to_read = VALUE #( BASE packages_to_read ( package_name = <read_package>-parent_package_name ) ).
        ENDIF.

        TRY.
            <read_package>-uri = adt_obj_factory->get_object_ref_for_trobj(
              type = zif_adcoset_c_global=>c_tadir_type-package
              name = CONV #( <read_package>-package_name ) )-uri.
          CATCH zcx_adcoset_static_error.
        ENDTRY.

        packages = VALUE #( BASE packages ( <read_package> ) ).
      ENDLOOP.

    ENDWHILE.

  ENDMETHOD.


  METHOD add_packages_to_adt_result.

    LOOP AT packages ASSIGNING FIELD-SYMBOL(<package>).

      APPEND VALUE #(
          uri             = <package>-uri
          adt_main_object = VALUE #(
            type = c_adt_wb_object_type-package
            name = <package>-package_name )
        ) TO adt_result-code_search_objects ASSIGNING FIELD-SYMBOL(<package_adt_result>).

      IF <package>-parent_package_name IS NOT INITIAL.
        <package_adt_result>-parent_uri = get_package_uri( <package>-parent_package_name ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_incl_match_objects.
    FIELD-SYMBOLS: <raw_match> TYPE zif_adcoset_ty_global=>ty_search_match.


    " Skip matches of program include in result
    LOOP AT raw_matches ASSIGNING FIELD-SYMBOL(<raw_match_group>) WHERE include <> object_info-name
      GROUP BY <raw_match_group>-include.

      TRY.
          DATA(incl_object_ref) = adt_obj_factory->get_object_ref_for_include(
            main_program = object_info-name
            include      = <raw_match_group>-include ).
        CATCH zcx_adcoset_static_error.
          CONTINUE.
      ENDTRY.

      DATA(incl_search_result_object) = VALUE zif_adcoset_ty_adt_types=>ty_code_search_object(
        uri             = incl_object_ref-uri
        parent_uri      = parent_search_result_object-uri
        adt_main_object = VALUE #(
          name = <raw_match_group>-display_name
          type = COND #(
            WHEN <raw_match_group>-adt_include_type IS NOT INITIAL
              THEN <raw_match_group>-adt_include_type
            ELSE incl_object_ref-type ) ) ).

      LOOP AT GROUP <raw_match_group> ASSIGNING <raw_match>.
        add_main_object_ref(
          search_result_object = REF #( incl_search_result_object )
          object_info          = object_info
          raw_match            = <raw_match> ).
      ENDLOOP.

      IF incl_search_result_object-matches IS NOT INITIAL.
        result = VALUE #( BASE result
          ( incl_search_result_object ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD create_std_match_objects.

    LOOP AT raw_matches ASSIGNING FIELD-SYMBOL(<raw_match>).
      add_main_object_ref(
        search_result_object = search_result_object
        object_info          = object_info
        raw_match            = <raw_match> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_main_object_ref.

    TRY.
        DATA(match_object_ref) = create_match_object_ref(
          object_info = object_info
          match       = raw_match ).
      CATCH zcx_adcoset_static_error.
        RETURN.
    ENDTRY.

    search_result_object->matches = VALUE #( BASE search_result_object->matches
      ( uri          = match_object_ref-uri
        snippet      = raw_match-snippet
        long_snippet = raw_match-long_snippet ) ).

    ADD 1 TO adt_result-number_of_results.

  ENDMETHOD.


  METHOD create_match_object_ref.

    CASE object_info-type.

      WHEN zif_adcoset_c_global=>c_source_code_type-class OR
           zif_adcoset_c_global=>c_source_code_type-function_group OR
           zif_adcoset_c_global=>c_source_code_type-program.

        result = adt_obj_factory->get_object_ref_for_include(
          main_program      = object_info-name
          include           = match-include
          start_line        = match-start_line
          start_line_offset = match-start_column
          end_line          = match-end_line
          end_line_offset   = match-end_column ).

      WHEN zif_adcoset_c_global=>c_source_code_type-interface OR
           zif_adcoset_c_global=>c_source_code_type-access_control OR
           zif_adcoset_c_global=>c_source_code_type-behavior_definition OR
           zif_adcoset_c_global=>c_source_code_type-data_definition OR
           zif_adcoset_c_global=>c_source_code_type-type_group OR
           zif_adcoset_c_global=>c_source_code_type-metadata_extension OR
           zif_adcoset_c_global=>c_source_code_type-simple_transformation OR
           zif_adcoset_c_global=>c_source_code_type-program.

        result = adt_obj_factory->get_object_ref_for_trobj(
          type                   = object_info-type
          name                   = object_info-name
          append_source_uri_path = abap_true ).


        adjust_adt_obj( CHANGING adt_obj_ref = result ).

        adt_obj_factory->add_position_fragment(
          EXPORTING
            start_line   = match-start_line
            start_column = match-start_column
            end_line     = match-end_line
            end_column   = match-end_column
          CHANGING
            link         = result-uri ).
    ENDCASE.

  ENDMETHOD.


  METHOD adjust_adt_obj.

    " for some reason the URI for the programs of a Business Object Type are generated incorrectly,
    "  so they have to be adjusted
    IF adt_obj_ref-type = 'SOBJ/P'.
      DATA(source_main_offset) = find( val = adt_obj_ref-uri sub = '/source/main' ).
      IF source_main_offset > 0.
        adt_obj_ref-uri = adt_obj_ref-uri(source_main_offset).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_package_uri.
    CHECK packages IS NOT INITIAL.

    result = VALUE #( packages[ package_name = package_name ]-uri OPTIONAL ).
  ENDMETHOD.


  METHOD adjust_cds_display_names.
    IF cds_name_mapper->map_entries( ) = abap_false.
      RETURN.
    ENDIF.

    LOOP AT main_objs_for_name_mapping INTO DATA(main_obj).
      DATA(entity_name) = cds_name_mapper->get_display_name( name = CONV #( main_obj->name )
                                                             type = CONV #( main_obj->type(4) ) ).
      IF entity_name IS NOT INITIAL.
        main_obj->name = entity_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
