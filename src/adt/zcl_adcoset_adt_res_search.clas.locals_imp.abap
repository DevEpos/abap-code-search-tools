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
        result = NEW lcl_search_result(
          raw_result             = search_result
          read_package_hierarchy = read_packages )->convert_to_adt_result( ).
      CATCH zcx_adcoset_static_error INTO DATA(search_error).
        RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
          EXPORTING
            previous = search_error.
    ENDTRY.

  ENDMETHOD.


  METHOD get_matcher_type.

    IF matcher_config-enable_pcre = abap_true.
      " TODO: some notify use that PCRE is not supported in the system??
      IF zcl_adcoset_matcher_factory=>is_pcre_supported( ).
        result = zif_adcoset_c_global=>c_matcher_type-pcre.
      ELSE.
        result = zif_adcoset_c_global=>c_matcher_type-posix_regex.
      ENDIF.
    ELSEIF matcher_config-use_regex = abap_true.
      result = zif_adcoset_c_global=>c_matcher_type-posix_regex.
    ELSE.
      result = zif_adcoset_c_global=>c_matcher_type-substring.
    ENDIF.

  ENDMETHOD.


  METHOD parse_parameters.
    " hard code the line feed to
    settings-all_results = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-all_results
      request    = request ).

    settings-ignore_case = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-ignore_case
      request    = request ).

    settings-ignore_comment_lines = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-ignore_comment_lines
      request    = request ).

    settings-multiline_search = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-multi_line
      request    = request ).

    settings-match_all_patterns = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-match_all_patterns
      request    = request ).

    read_packages = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-read_package_hierarchy
      request    = request ).

    matcher_config-use_regex = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-use_regex
      request    = request ).

    IF settings-all_results = abap_false.
      settings-max_results = zcl_adcoset_adt_request_util=>get_integer_param_value(
        param_name    = zif_adcoset_c_global=>c_search_params-max_results
        default_value = 100
        request       = request ).
    ENDIF.

    settings-search_scope-max_objects = zcl_adcoset_adt_request_util=>get_integer_param_value(
      param_name = zif_adcoset_c_global=>c_search_params-max_objects
      request    = request ).

    get_owners( ).
    get_packages( ).
    get_appl_comps( ).
    get_object_types( ).
    get_object_names( ).
    get_created_dates( ).
    get_patterns( ).
    get_class_scope( ).
  ENDMETHOD.


  METHOD complete_settings.
    get_persisted_settings( ).
    settings-matcher_type = get_matcher_type( ).
    settings-line_feed = |\r\n|.
  ENDMETHOD.


  METHOD get_owners.
    DATA(owners) = zcl_adcoset_adt_request_util=>get_request_param_value(
      param_name = zif_adcoset_c_global=>c_search_params-owner
      upper_case = abap_true
      request    = request ).

    split_into_range(
      EXPORTING
        filter_name = zif_adcoset_c_global=>c_search_params-owner
        input       = owners
        flags       = VALUE #( negation = abap_true patterns = abap_true )
      IMPORTING
        range_table = settings-search_scope-owner_range ).
  ENDMETHOD.


  METHOD get_packages.
    DATA(packages) = zcl_adcoset_adt_request_util=>get_request_param_value(
      param_name = zif_adcoset_c_global=>c_search_params-package
      upper_case = abap_true
      request    = request ).

    split_into_range(
      EXPORTING
        filter_name = zif_adcoset_c_global=>c_search_params-package
        input       = packages
        flags       = VALUE #( negation = abap_true patterns = abap_true )
      IMPORTING
        range_table = settings-search_scope-package_range ).
  ENDMETHOD.


  METHOD get_appl_comps.
    DATA(packages) = zcl_adcoset_adt_request_util=>get_request_param_value(
      param_name = zif_adcoset_c_global=>c_search_params-appl_comp
      upper_case = abap_true
      request    = request ).

    split_into_range(
      EXPORTING
        filter_name = zif_adcoset_c_global=>c_search_params-appl_comp
        input       = packages
        flags       = VALUE #( negation = abap_true )
      IMPORTING
        range_table = settings-search_scope-appl_comp_range ).
  ENDMETHOD.


  METHOD get_object_types.
    DATA(types) = zcl_adcoset_adt_request_util=>get_request_param_value(
      param_name = zif_adcoset_c_global=>c_search_params-object_type
      upper_case = abap_true
      request    = request ).

    split_into_range(
      EXPORTING
        filter_name = zif_adcoset_c_global=>c_search_params-object_type
        input       = types
        flags       = VALUE #( negation = abap_true )
      IMPORTING
        range_table = settings-search_scope-object_type_range ).
  ENDMETHOD.


  METHOD get_object_names.
    DATA(object_names) = zcl_adcoset_adt_request_util=>get_request_param_value(
      param_name = zif_adcoset_c_global=>c_search_params-object_name
      upper_case = abap_true
      request    = request ).

    split_into_range(
      EXPORTING
        filter_name = zif_adcoset_c_global=>c_search_params-object_name
        input       = object_names
        separator   = ` `
        flags       = VALUE #( negation = abap_true patterns = abap_true auto_prefix_matching = abap_true )
      IMPORTING
        range_table = settings-search_scope-object_name_range ).
  ENDMETHOD.


  METHOD get_created_dates.
    DATA: dates TYPE string_table.

    DATA(dates_list) = zcl_adcoset_adt_request_util=>get_request_param_value(
      param_name = zif_adcoset_c_global=>c_search_params-created_date
      upper_case = abap_true
      request    = request ).

    SPLIT dates_list AT ',' INTO TABLE dates.

    LOOP AT dates ASSIGNING FIELD-SYMBOL(<date>).
      APPEND INITIAL LINE TO settings-search_scope-created_on_range ASSIGNING FIELD-SYMBOL(<created_range>).

      <created_range> = <date>.
    ENDLOOP.
  ENDMETHOD.



  METHOD get_patterns.
    settings-pattern_range = VALUE #(
      FOR pattern IN  zcl_adcoset_adt_request_util=>get_request_param_values(
        param_name = zif_adcoset_c_global=>c_search_params-search_pattern
        mandatory  = abap_true
        request    = request )
      ( sign = 'I' option = 'CP' low = pattern ) ).
  ENDMETHOD.


  METHOD get_persisted_settings.

    DATA(server_settings) = zcl_adcoset_search_settings=>get_settings( ).
    settings-parallel_processing = VALUE #(
      enabled      = server_settings-parallel_enabled
      server_group = server_settings-parallel_server_group ).

    matcher_config-enable_pcre = server_settings-pcre_enabled.
  ENDMETHOD.


  METHOD get_class_scope.
    DATA: scopes TYPE string_table.

    DATA(scope_list) = zcl_adcoset_adt_request_util=>get_request_param_value(
      param_name = zif_adcoset_c_global=>c_search_params-class_search_scope
      request    = request ).

    SPLIT scope_list AT ',' INTO TABLE scopes.

    LOOP AT scopes INTO DATA(scope).
      CASE scope.

        WHEN zif_adcoset_c_global=>c_cls_search_scope-all.
          settings-custom_settings-class = VALUE #(
            search_main_incl       = abap_true
            search_local_def_incl  = abap_true
            search_local_impl_incl = abap_true
            search_macro_incl      = abap_true
            search_test_incl       = abap_true ).
          EXIT.

        WHEN zif_adcoset_c_global=>c_cls_search_scope-global.
          settings-custom_settings-class-search_main_incl = abap_true.

        WHEN zif_adcoset_c_global=>c_cls_search_scope-local_definitions.
          settings-custom_settings-class-search_local_def_incl = abap_true.

        WHEN zif_adcoset_c_global=>c_cls_search_scope-local_implementation.
          settings-custom_settings-class-search_local_impl_incl = abap_true.

        WHEN zif_adcoset_c_global=>c_cls_search_scope-macros.
          settings-custom_settings-class-search_macro_incl = abap_true.

        WHEN zif_adcoset_c_global=>c_cls_search_scope-tests.
          settings-custom_settings-class-search_test_incl = abap_true.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD split_into_range.
    DATA: tokens    TYPE string_table,
          new_range TYPE REF TO data.

    CHECK input IS NOT INITIAL.

    SPLIT input AT separator INTO TABLE tokens.

    LOOP AT tokens INTO DATA(token).

      CHECK token IS NOT INITIAL.

      CREATE DATA new_range LIKE LINE OF range_table.
      ASSIGN new_range->* TO FIELD-SYMBOL(<new_range>).

      ASSIGN COMPONENT 'SIGN' OF STRUCTURE <new_range> TO FIELD-SYMBOL(<sign>).
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <new_range> TO FIELD-SYMBOL(<option>).
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <new_range> TO FIELD-SYMBOL(<low>).

      ASSERT: <sign> IS ASSIGNED,
              <option> IS ASSIGNED,
              <low> IS ASSIGNED.

      IF token CA '*?'.
        IF flags-patterns = abap_false.
          RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
            EXPORTING
              text = |Parameter '{ filter_name }' does not support patterns|.
        ENDIF.
        token = replace( val = token sub = '?' occ = 0  with = '+' ).
        <option> = 'CP'.
      ELSE.
        <option> = 'EQ'.
      ENDIF.

      DATA(length) = strlen( token ).
      DATA(last_char_offset) = length - 1.

      IF token(1) = '!'.
        IF flags-negation = abap_false.
          RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
            EXPORTING
              text = |Parameter '{ filter_name }' does not support negation!|.
        ENDIF.
        IF length = 1.
          RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
            EXPORTING
              text = |No value provided after negation character '!' for Parameter '{ filter_name }'!|.
        ENDIF.
        <sign> = 'E'.
        token = token+1.
        length = length - 1.
        last_char_offset = last_char_offset - 1.
      ELSE.
        <sign> = 'I'.
      ENDIF.

      IF flags-auto_prefix_matching = abap_true.
        <option> = 'CP'.
        IF token+last_char_offset(1) = '<'.
          token = token(last_char_offset).
          IF token NA '+*'.
            <option> = 'EQ'.
          ENDIF.
        ELSEIF token+last_char_offset(1) <> '*'.
          token = |{ token }*|.
        ENDIF.
      ENDIF.

      <low> = token.

      " verify that token truly has a value
      IF <low> IS NOT INITIAL.
        APPEND <new_range> TO range_table.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.


CLASS lcl_search_result IMPLEMENTATION.

  METHOD constructor.
    me->raw_result = raw_result.
    adt_obj_factory = zcl_adcoset_adt_obj_factory=>get_instance( ).
  ENDMETHOD.


  METHOD convert_to_adt_result.
    set_durations( ).
    IF read_package_hierarchy = abap_true.
      determine_package_hierarchy( ).
    ENDIF.
    create_adt_links( ).

    result = adt_result.
  ENDMETHOD.


  METHOD set_durations.
    adt_result-query_time_in_ms = raw_result-duration_in_ms.
  ENDMETHOD.


  METHOD create_adt_links.

    LOOP AT raw_result-results ASSIGNING FIELD-SYMBOL(<raw_result>).

      TRY.
          DATA(search_result_object) = VALUE zif_adcoset_ty_adt_types=>ty_code_search_object(
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

    IF object_info-type = zif_adcoset_c_global=>c_source_code_type-class OR
        object_info-type = zif_adcoset_c_global=>c_source_code_type-function_group.
      adt_result-code_search_objects = VALUE #( BASE adt_result-code_search_objects
       ( search_result_object->* ) ).
      create_incl_match_objects(
        parent_search_result_object = search_result_object->*
        object_info                 = object_info
        raw_matches                 = raw_matches ).
    ELSE.
      create_std_match_objects(
        search_result_object = search_result_object
        object_info          = object_info
        raw_matches          = raw_matches ).

      adt_result-code_search_objects = VALUE #( BASE adt_result-code_search_objects
       ( search_result_object->* ) ).
    ENDIF.

  ENDMETHOD.


  METHOD determine_package_hierarchy.

  ENDMETHOD.


  METHOD create_incl_match_objects.

    LOOP AT raw_matches ASSIGNING FIELD-SYMBOL(<raw_match_group>)
      GROUP BY <raw_match_group>-include.

      DATA(incl_object_ref) = adt_obj_factory->get_object_ref_for_include(
        main_program = object_info-name
        include      = <raw_match_group>-include ).

      DATA(incl_search_result_object) = VALUE zif_adcoset_ty_adt_types=>ty_code_search_object(
        uri             = incl_object_ref-uri
        parent_uri      = parent_search_result_object-uri
        adt_main_object = VALUE #(
          name = <raw_match_group>-display_name
          type = COND #(
            WHEN <raw_match_group>-adt_include_type IS NOT INITIAL
              THEN <raw_match_group>-adt_include_type
            ELSE incl_object_ref-type ) ) ).

      LOOP AT GROUP <raw_match_group> ASSIGNING FIELD-SYMBOL(<raw_match>).
        add_main_object_ref(
          search_result_object = REF #( incl_search_result_object )
          object_info          = object_info
          raw_match            = <raw_match> ).
      ENDLOOP.

      adt_result-code_search_objects = VALUE #( BASE adt_result-code_search_objects
        ( incl_search_result_object ) ).

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

    DATA(match_object_ref) = create_match_object_ref(
      object_info = object_info
      match       = raw_match ).

    search_result_object->matches = VALUE #( BASE search_result_object->matches
      ( uri     = match_object_ref-uri
        snippet = raw_match-snippet ) ).

    ADD 1 TO adt_result-number_of_results.

  ENDMETHOD.


  METHOD create_match_object_ref.

    CASE object_info-type.

      WHEN zif_adcoset_c_global=>c_source_code_type-class OR
           zif_adcoset_c_global=>c_source_code_type-function_group.

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
ENDCLASS.
