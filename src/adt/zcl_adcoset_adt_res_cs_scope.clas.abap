"! <p class="shorttext synchronized">Resource for Search Scope Definition</p>
CLASS zcl_adcoset_adt_res_cs_scope DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS post REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS c_value_separator TYPE string VALUE ',' ##NO_TEXT.

    TYPES:
      BEGIN OF ty_param_flags,
        negation             TYPE abap_bool,
        patterns             TYPE abap_bool,
        auto_prefix_matching TYPE abap_bool,
      END OF ty_param_flags.

    "! External Scope
    DATA scope_ext TYPE zif_adcoset_ty_adt_types=>ty_search_scope.
    DATA expiration TYPE i.
    "! Data for scope determination
    DATA scope_ranges TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.

    METHODS parse_parameters
      IMPORTING
        scope_params TYPE zif_adcoset_ty_adt_types=>ty_search_scope_params
      RAISING
        cx_adt_rest.

    METHODS extract_owners
      IMPORTING
        param_value TYPE string
      RAISING
        cx_adt_rest.

    METHODS extract_packages
      IMPORTING
        param_value TYPE string
      RAISING
        cx_adt_rest.

    METHODS extract_appl_comps
      IMPORTING
        param_value TYPE string
      RAISING
        cx_adt_rest.

    METHODS extract_object_types
      IMPORTING
        param_value TYPE string
      RAISING
        cx_adt_rest.

    METHODS extract_object_names
      IMPORTING
        param_value TYPE string
      RAISING
        cx_adt_rest.

    METHODS extract_created_dates
      IMPORTING
        param_value TYPE string
      RAISING
        cx_adt_rest.

    METHODS extract_tag_ids
      IMPORTING
        param_value TYPE string.

    METHODS extract_tr_requests
      IMPORTING
        param_value TYPE string
      RAISING
        cx_adt_rest.

    METHODS split_into_range
      IMPORTING
        filter_name TYPE string
        separator   TYPE string         DEFAULT c_value_separator
        !input      TYPE string
        !flags      TYPE ty_param_flags OPTIONAL
      EXPORTING
        range_table TYPE STANDARD TABLE
      RAISING
        zcx_adcoset_adt_rest.

    METHODS persist_scope
      RAISING
        zcx_adcoset_adt_rest.

    METHODS delete_expired_scopes.
    METHODS determine_scope.
ENDCLASS.


CLASS zcl_adcoset_adt_res_cs_scope IMPLEMENTATION.
  METHOD delete_expired_scopes.
    DATA current_time TYPE timestampl.

    GET TIME STAMP FIELD current_time.

    DELETE FROM zadcoset_csscope WHERE created_by          = sy-uname
                                   AND expiration_datetime < current_time.
  ENDMETHOD.

  METHOD determine_scope.
    DATA(scope) = zcl_adcoset_search_scope_fac=>create_scope( CORRESPONDING #( scope_ranges ) ).

    " update the scope ranges from the created scope as maybe some packages had to be
    " resolved first (i.e. determine subpackages)
    scope_ranges = scope->get_scope_ranges( ).

    scope_ext-object_count = scope->count( ).
  ENDMETHOD.

  METHOD extract_appl_comps.
    DATA(appl_comps) = to_upper( param_value ).

    split_into_range( EXPORTING filter_name = zif_adcoset_c_global=>c_search_params-appl_comp
                                input       = appl_comps
                                flags       = VALUE #( negation = abap_true auto_prefix_matching = abap_true )
                      IMPORTING range_table = scope_ranges-appl_comp_range ).
  ENDMETHOD.

  METHOD extract_created_dates.
    DATA dates TYPE string_table.

    DATA(dates_list) = param_value.

    SPLIT dates_list AT c_value_separator INTO TABLE dates.

    LOOP AT dates ASSIGNING FIELD-SYMBOL(<date>).
      APPEND INITIAL LINE TO scope_ranges-created_on_range ASSIGNING FIELD-SYMBOL(<created_range>).

      <created_range> = <date>.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_object_names.
    DATA(object_names) = to_upper( param_value ).

    split_into_range(
      EXPORTING filter_name = zif_adcoset_c_global=>c_search_params-object_name
                input       = object_names
                separator   = ` `
                flags       = VALUE #( negation = abap_true patterns = abap_true auto_prefix_matching = abap_true )
      IMPORTING range_table = scope_ranges-object_name_range ).
  ENDMETHOD.

  METHOD extract_object_types.
    DATA(types) = to_upper( param_value ).

    split_into_range( EXPORTING filter_name = zif_adcoset_c_global=>c_search_params-object_type
                                input       = types
                                flags       = VALUE #( negation = abap_true )
                      IMPORTING range_table = scope_ranges-object_type_range ).
  ENDMETHOD.

  METHOD extract_owners.
    DATA(owners) = to_upper( param_value ).

    split_into_range( EXPORTING filter_name = zif_adcoset_c_global=>c_search_params-owner
                                input       = owners
                                flags       = VALUE #( negation = abap_true patterns = abap_true )
                      IMPORTING range_table = scope_ranges-owner_range ).

    LOOP AT scope_ranges-owner_range ASSIGNING FIELD-SYMBOL(<owner_range>) WHERE low = 'ME'.
      <owner_range>-low = sy-uname.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_packages.
    DATA(packages) = to_upper( param_value ).

    split_into_range( EXPORTING filter_name = zif_adcoset_c_global=>c_search_params-package
                                input       = packages
                                flags       = VALUE #( negation = abap_true patterns = abap_true )
                      IMPORTING range_table = scope_ranges-package_range ).
  ENDMETHOD.

  METHOD extract_tag_ids.
    DATA ext_uuids TYPE string_table.
    DATA int_uuid TYPE sysuuid_x16.

    " safety check, in case API was not called from ADT Code Search Plugin
    CHECK zcl_adcoset_extensions_util=>is_abap_tags_available( ).

    DATA(ext_uuid_csv) = param_value.

    SPLIT ext_uuid_csv AT c_value_separator INTO TABLE ext_uuids.

    LOOP AT ext_uuids INTO DATA(ext_uuid).
      CLEAR int_uuid.
      REPLACE ALL OCCURRENCES OF '-' IN ext_uuid WITH space.
      TRY.
          cl_system_uuid=>convert_uuid_c32_static( EXPORTING uuid     = to_upper( ext_uuid )
                                                   IMPORTING uuid_x16 = int_uuid ).
          scope_ranges-tag_id_range = VALUE #( BASE scope_ranges-tag_id_range
                                               ( sign = 'I' option = 'EQ' low = int_uuid ) ).
        CATCH cx_uuid_error.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD extract_tr_requests.
    DATA(tr_request) = to_upper( param_value ).

    split_into_range( EXPORTING filter_name = zif_adcoset_c_global=>c_search_params-tr_request
                                input       = tr_request
                                flags       = VALUE #( negation = abap_true )
                      IMPORTING range_table = scope_ranges-tr_request_range ).
  ENDMETHOD.

  METHOD parse_parameters.
    LOOP AT scope_params ASSIGNING FIELD-SYMBOL(<param>).

      CASE <param>-name.

        WHEN zif_adcoset_c_global=>c_search_params-owner.
          extract_owners( <param>-value ).

        WHEN zif_adcoset_c_global=>c_search_params-package.
          extract_packages( <param>-value ).

        WHEN zif_adcoset_c_global=>c_search_params-appl_comp.
          extract_appl_comps( <param>-value ).

        WHEN zif_adcoset_c_global=>c_search_params-object_type.
          extract_object_types( <param>-value ).

        WHEN zif_adcoset_c_global=>c_search_params-object_name.
          extract_object_names( <param>-value ).

        WHEN zif_adcoset_c_global=>c_search_params-created_date.
          extract_created_dates( <param>-value ).

        WHEN zif_adcoset_c_global=>c_search_params-tag_id.
          extract_tag_ids( <param>-value ).

        WHEN zif_adcoset_c_global=>c_search_params-tr_request.
          extract_tr_requests( <param>-value ).

      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD persist_scope.
    DATA(scope_db) = VALUE zadcoset_csscope( created_by = sy-uname ).

    scope_db-scope_type = COND #( WHEN scope_ranges-tr_request_range IS NOT INITIAL
                                  THEN zif_adcoset_c_global=>c_scope_type-transport_request
                                  ELSE zif_adcoset_c_global=>c_scope_type-universal_scope ).

    CALL TRANSFORMATION id
         SOURCE data = scope_ranges
         RESULT XML scope_db-ranges_data.

    TRY.
        scope_ext-id = cl_system_uuid=>create_uuid_x16_static( ).
        scope_db-id =
          scope_ext-id.
      CATCH cx_uuid_error INTO DATA(uuid_error).
        RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
          EXPORTING previous = uuid_error.
    ENDTRY.

    GET TIME STAMP FIELD scope_db-expiration_datetime.
    scope_db-expiration_datetime = cl_abap_tstmp=>add( tstmp = scope_db-expiration_datetime
                                                       secs  = zif_adcoset_c_global=>c_default_scope_expiration ).

    INSERT zadcoset_csscope FROM scope_db.
  ENDMETHOD.

  METHOD post.
    DATA scope_params TYPE zif_adcoset_ty_adt_types=>ty_search_scope_params.

    delete_expired_scopes( ).

    request->get_body_data( EXPORTING content_handler = zcl_adcoset_adt_ch_factory=>create_search_scope_params_ch( )
                            IMPORTING data            = scope_params ).

    parse_parameters( scope_params ).
    determine_scope( ).

    IF scope_ext-object_count > 0.
      persist_scope( ).

      response->set_body_data( content_handler = zcl_adcoset_adt_ch_factory=>create_search_scope_ch( )
                               data            = scope_ext ).
    ELSE.
      response->set_status( cl_rest_status_code=>gc_success_no_content ).
    ENDIF.
  ENDMETHOD.

  METHOD split_into_range.
    DATA tokens TYPE string_table.
    DATA new_range TYPE REF TO data.

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
            EXPORTING text = |Parameter '{ filter_name }' does not support patterns|.
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
            EXPORTING text = |Parameter '{ filter_name }' does not support negation!|.
        ENDIF.
        IF length = 1.
          RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
            EXPORTING text = |No value provided after negation character '!' for Parameter '{ filter_name }'!|.
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
