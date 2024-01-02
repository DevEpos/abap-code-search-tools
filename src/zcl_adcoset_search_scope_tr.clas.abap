"! <p class="shorttext synchronized">Search scope implementation</p>
CLASS zcl_adcoset_search_scope_tr DEFINITION
  PUBLIC
  INHERITING FROM zcl_adcoset_search_scope_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

    METHODS zif_adcoset_search_scope~next_package REDEFINITION.

  PROTECTED SECTION.
    METHODS determine_count REDEFINITION.

  PRIVATE SECTION.
    DATA native_scope_query TYPE REF TO zcl_adcoset_nsql_sscope_query.

    "! Enhance the type filter with corresponding LIMU types
    METHODS add_subobj_type_to_filter.
    METHODS resolve_tr_request.
    METHODS init_native_scope_query.

    "! Read Source Code Objects from Transport Requests
    METHODS get_tr_objects
      IMPORTING
        max_rows      TYPE i
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tr_request_objects.

    "! Creates native query for reading scope objects and count
    METHODS create_native_query
      RETURNING
        VALUE(result) TYPE REF TO zcl_adcoset_nsql_sscope_query.
ENDCLASS.


CLASS zcl_adcoset_search_scope_tr IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    search_ranges = search_scope-ranges.
    init( search_scope ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~next_package.
    DATA(max_rows) = get_max_rows( ).

    DATA(tr_objects) = get_tr_objects( max_rows ).
    result = VALUE #( count   = lines( tr_objects )
                      objects = NEW zcl_adcoset_tr_obj_processor( tr_objects    = tr_objects
                                                                  search_ranges = search_ranges )->run( ) ).

    current_offset = current_offset + result-count.

    IF result-count < max_rows.
      all_packages_read = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD determine_count.
    IF     search_ranges-object_type_range IS INITIAL
       AND search_ranges-object_name_range IS INITIAL
       AND search_ranges-tr_request_range  IS INITIAL.
      RETURN.
    ENDIF.

    add_subobj_type_to_filter( ).
    resolve_tr_request( ).
    resolve_packages( ).

    DATA(count_query) = create_native_query( ).
    count_query->set_select( value    = `programid pgmid, ` &&
                                        `objecttype obj_type, ` &&
                                        `objectname obj_name`
                             distinct = abap_true ).

    object_count = count_query->execute_cte_count( ).

    IF max_objects > 0 AND object_count > max_objects.
      object_count = max_objects.
      more_objects_in_scope = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_tr_objects.
    DATA tr_objects TYPE zif_adcoset_ty_global=>ty_std_tr_request_objects.

    IF     search_ranges-object_type_range IS INITIAL
       AND search_ranges-object_name_range IS INITIAL
       AND search_ranges-tr_request_range  IS INITIAL.
      RETURN.
    ENDIF.

    init_native_scope_query( ).

    native_scope_query->set_offset( current_offset ).
    native_scope_query->set_limit( max_rows ).

    IF native_scope_query->execute_query( REF #( tr_objects ) ).
      result = tr_objects.
    ENDIF.
  ENDMETHOD.

  METHOD add_subobj_type_to_filter.
    " some R3TR object types are associated with LIMU types which can be included
    " in transport requests. These types are added to the filter criteria
    " special case: REPS LIMU type is associate with PROG as well as FUGR
    DATA subobject_type_ranges TYPE RANGE OF trobjtype.

    LOOP AT search_ranges-object_type_range REFERENCE INTO DATA(object_type_range).
      IF object_type_range->low = zif_adcoset_c_global=>c_source_code_type-class.
        subobject_type_ranges = VALUE #(
            BASE subobject_type_ranges
            sign   = object_type_range->sign
            option = 'EQ'
            ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_definition )
            ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_include )
            ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_private_section )
            ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_protected_section )
            ( low =  zif_adcoset_c_global=>c_source_code_limu_type-class_public_section )
            ( low =  zif_adcoset_c_global=>c_source_code_limu_type-method ) ).
      ENDIF.
      IF object_type_range->low = zif_adcoset_c_global=>c_source_code_type-function_group.
        IF object_type_range->sign = 'I'.
          subobject_type_ranges = VALUE #( BASE subobject_type_ranges
                                           sign   = 'I'
                                           option = 'EQ'
                                           ( low =  zif_adcoset_c_global=>c_source_code_limu_type-function_module )
                                           ( low =  zif_adcoset_c_global=>c_source_code_limu_type-report_source_code ) ).
        ELSEIF object_type_range->sign = 'E'.
          subobject_type_ranges = VALUE #( BASE subobject_type_ranges
                                           sign   = 'E'
                                           option = 'EQ'
                                           ( low =  zif_adcoset_c_global=>c_source_code_limu_type-function_module ) ).
        ENDIF.
      ENDIF.
      IF     object_type_range->low  = zif_adcoset_c_global=>c_source_code_type-program
         AND object_type_range->sign = 'I'.
        subobject_type_ranges = VALUE #( BASE subobject_type_ranges
                                         sign   = 'I'
                                         option = 'EQ'
                                         ( low =  zif_adcoset_c_global=>c_source_code_limu_type-report_source_code ) ).
      ENDIF.
    ENDLOOP.

    search_ranges-object_type_range = VALUE #( BASE search_ranges-object_type_range
                                               ( LINES OF subobject_type_ranges ) ).
  ENDMETHOD.

  METHOD resolve_tr_request.
    CHECK search_ranges-tr_request_range IS NOT INITIAL.

    SELECT trkorr FROM e070
      WHERE strkorr IN @search_ranges-tr_request_range
      INTO TABLE @DATA(tr_tasks).

    search_ranges-tr_request_range = VALUE #( BASE search_ranges-tr_request_range FOR task IN tr_tasks
                                              ( sign = 'I' option = 'EQ' low = task ) ).
  ENDMETHOD.

  METHOD init_native_scope_query.
    CHECK native_scope_query IS INITIAL.

    native_scope_query = create_native_query( ).
    native_scope_query->set_select( value    = `programid pgmid, ` &&
                                               `objecttype obj_type, ` &&
                                               `objectname obj_name`
                                    cols     = VALUE #( ( 'PGMID' ) ( 'OBJ_TYPE' ) ( 'OBJ_NAME' ) )
                                    distinct = abap_true ).
    native_scope_query->set_order_by( `obj_name, pgmid, obj_type` ).
  ENDMETHOD.

  METHOD create_native_query.
    result = NEW #( ).
    result->set_from( 'ZADCOSET_TRSCO' ).
    result->add_range_to_where( ranges        = search_ranges-object_type_range
                                sql_fieldname = 'objecttype' ).
    result->add_range_to_where( ranges        = search_ranges-object_name_range
                                sql_fieldname = 'objectname' ).
    result->add_range_to_where( ranges        = search_ranges-tr_request_range
                                sql_fieldname = 'request' ).
  ENDMETHOD.
ENDCLASS.
