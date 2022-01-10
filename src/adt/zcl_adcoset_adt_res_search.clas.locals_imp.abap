*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_search_query IMPLEMENTATION.


  METHOD constructor.
    me->request = request.
  ENDMETHOD.


  METHOD run.
    result = VALUE zif_adcoset_ty_adt_types=>ty_code_search_result(
      number_of_results   = 1
      code_search_objects = VALUE #(
        ( adt_main_object = VALUE #(
            name = 'DEMO_CDS_AGGREGATE'
            type = 'DDLS/DF' )
          uri             = '/sap/bc/adt/ddic/ddl/sources/demo_cds_aggregate'
          matches = VALUE #(
            ( snippet = 'sum(fltime) as sum_fltime'
              uri     = '/sap/bc/adt/ddic/ddl/sources/demo_cds_aggregate/source/main#start=9,13;end=9,15' ) ) )
        ( adt_main_object = VALUE #(
            name = 'CL_OO_CLASSNAME_SERVICE'
            type = 'CLAS/OC' )
          uri             = '/sap/bc/adt/oo/classes/cl_oo_classname_service' )
        ( adt_main_object = VALUE #(
            name = 'Public Section'
            type = 'CLAS/I' )
          uri             = '/sap/bc/adt/oo/classes/cl_oo_classname_service/source/main#start=6,0;end=6,14'
          parent_uri      = '/sap/bc/adt/oo/classes/cl_oo_classname_service'
          matches = VALUE #(
            ( snippet = 'Public Section'
              uri     = '/sap/bc/adt/oo/classes/cl_oo_classname_service/source/main#start=18,0;end=18,31' ) ) )
        ( adt_main_object = VALUE #(
            name = 'GET_MATCHER_TYPE'
            type = 'CLAS/OM' )
          uri             = '/sap/bc/adt/oo/classes/cl_oo_classname_service/source/main#start=177,7;end=177,29'
          parent_uri      = '/sap/bc/adt/oo/classes/cl_oo_classname_service'
          matches = VALUE #(
            ( snippet = '  insert wa_result into table result.'
              uri     = '/sap/bc/adt/oo/classes/cl_oo_classname_service/source/main#start=238,19;end=238,36' ) ) ) ) ).
  ENDMETHOD.


  METHOD get_matcher_type.

    IF use_pcre = abap_true.
      " TODO: some notify use that PCRE is not supported in the system??
      IF zcl_adcoset_matcher_factory=>is_pcre_supported( ).
        result = zif_adcoset_c_global=>c_matcher_type-pcre.
      ELSE.
        result = zif_adcoset_c_global=>c_matcher_type-posix_regex.
      ENDIF.
    ELSEIF use_regex = abap_true.
      result = zif_adcoset_c_global=>c_matcher_type-posix_regex.
    ELSE.
      result = zif_adcoset_c_global=>c_matcher_type-substring.
    ENDIF.

  ENDMETHOD.


  METHOD parse_parameters.
    settings-all_results = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-all_results
      request    = request ).

    settings-ignore_case = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-ignore_case
      request    = request ).

    settings-match_all_patterns = zcl_adcoset_adt_request_util=>get_boolean_req_param(
      param_name = zif_adcoset_c_global=>c_search_params-match_all_patterns
      request    = request ).

    get_owners( ).
    get_packages( ).
    get_appl_comps( ).
    get_object_types( ).
    get_object_names( ).
    get_created_dates( ).
    get_patterns( ).

  ENDMETHOD.


  METHOD get_owners.
    DATA(owners) = zcl_adcoset_adt_request_util=>get_request_param_values(
      param_name = zif_adcoset_c_global=>c_search_params-owner
      upper_case = abap_true
      request    = request ).

    fill_range(
      EXPORTING
        input = owners
        flags = VALUE #( negation = abap_true patterns = abap_true )
      IMPORTING
        range = settings-search_scope-owner_range ).
  ENDMETHOD.


  METHOD get_packages.

  ENDMETHOD.


  METHOD get_appl_comps.

  ENDMETHOD.


  METHOD get_object_types.

  ENDMETHOD.


  METHOD get_object_names.

  ENDMETHOD.


  METHOD get_created_dates.

  ENDMETHOD.


  METHOD get_patterns.

  ENDMETHOD.


  METHOD fill_range.

    LOOP AT input INTO DATA(input_line).
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
