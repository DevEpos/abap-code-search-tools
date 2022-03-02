"! <p class="shorttext synchronized" lang="en">Resource for Code Search Pattern Validation</p>
CLASS zcl_adcoset_adt_res_pattrnval DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM cl_adt_rest_resource
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      post REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: is_sequential_matching TYPE abap_bool,
          is_regex               TYPE abap_bool,
          pattern_configs        TYPE zif_adcoset_ty_global=>ty_patterns.
    METHODS:
      collect_patterns
        IMPORTING
          patterns TYPE string,
      get_uri_params
        IMPORTING
          request TYPE REF TO if_adt_rest_request,
      validate
        RAISING
          cx_adt_rest,
      validate_regex_patterns
        RAISING
          cx_adt_rest,
      validate_sequential_patterns
        RAISING
          cx_adt_rest.
ENDCLASS.



CLASS zcl_adcoset_adt_res_pattrnval IMPLEMENTATION.

  METHOD post.
    DATA: patterns          TYPE string,
          separate_patterns TYPE string_table.

    request->get_body_data(
      EXPORTING
        content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
      IMPORTING
        data            = patterns ).

    IF patterns IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
        EXPORTING
          text = |No pattern supplied|.
    ENDIF.

    collect_patterns( patterns ).
    get_uri_params( request ).
    validate( ).

  ENDMETHOD.


  METHOD collect_patterns.
    DATA separate_patterns TYPE string_table.

    SPLIT patterns AT |\r\n| INTO TABLE separate_patterns.
    pattern_configs = VALUE zif_adcoset_ty_global=>ty_patterns(
      FOR <pattern> IN separate_patterns
      ( content = <pattern> ) ).
  ENDMETHOD.


  METHOD get_uri_params.
    is_sequential_matching = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-sequential_matching
      request    = request ).

    is_regex = zcl_adcoset_adt_request_util=>get_boolean_query_parameter(
      param_name = zif_adcoset_c_global=>c_search_params-use_regex
      request    = request ).
  ENDMETHOD.


  METHOD validate.
    IF is_sequential_matching = abap_true.
      validate_sequential_patterns( ).
    ENDIF.

    IF is_regex = abap_true.
      validate_regex_patterns( ).
    ENDIF.
  ENDMETHOD.


  METHOD validate_regex_patterns.
    DATA(matcher_type) = COND #(
      WHEN zcl_adcoset_pcre_util=>is_pcre_supported( ) THEN
        zif_adcoset_c_global=>c_matcher_type-pcre
      ELSE
        zif_adcoset_c_global=>c_matcher_type-posix_regex ).

    LOOP AT pattern_configs ASSIGNING FIELD-SYMBOL(<pattern>).
      TRY.
          zcl_adcoset_matcher_factory=>create_matcher(
            type    = matcher_type
            pattern = <pattern> ).
        CATCH cx_sy_regex INTO DATA(regex_error).
          RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
            EXPORTING
              text = |{ regex_error->get_text( ) }: "{ <pattern>-content }"|.
        CATCH zcx_adcoset_no_matcher ##no_handler.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD validate_sequential_patterns.
    TRY.
        pattern_configs = zcl_adcoset_pattern_util=>parse_pattern_sequence( patterns = pattern_configs ).
      CATCH zcx_adcoset_static_error INTO DATA(parsing_error).
        RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
          EXPORTING
            previous = parsing_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
