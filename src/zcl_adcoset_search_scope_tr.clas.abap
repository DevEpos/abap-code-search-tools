"! <p class="shorttext synchronized">Search scope implementation</p>
CLASS zcl_adcoset_search_scope_tr DEFINITION
  PUBLIC
  INHERITING FROM zcl_adcoset_search_scope_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        search_scope TYPE zif_adcoset_ty_global=>ty_search_scope.

    METHODS zif_adcoset_search_scope~next_package           REDEFINITION.
    METHODS zif_adcoset_search_scope~configure_package_size REDEFINITION.
    METHODS zif_adcoset_search_scope~has_next_package       REDEFINITION.

  PROTECTED SECTION.
    METHODS determine_count REDEFINITION.
    METHODS init_from_db    REDEFINITION.

  PRIVATE SECTION.
    "! Reader for packages of object scope
    DATA scope_pack_reader TYPE REF TO lif_adbc_scope_obj_reader.

    METHODS init_package_reader.
    "! Enhance the type filter with corresponding LIMU types
    METHODS add_subobj_type_to_filter.
    METHODS resolve_tr_request.
ENDCLASS.


CLASS zcl_adcoset_search_scope_tr IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    search_ranges = search_scope-ranges.
    init_package_reader( ).
    init( search_scope ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~has_next_package.
    result = scope_pack_reader->has_more_packages( ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~next_package.
    result = scope_pack_reader->read_next_package( ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~configure_package_size.
    super->zif_adcoset_search_scope~configure_package_size( max_objects    = max_objects
                                                            max_task_count = max_task_count ).
    scope_pack_reader->set_object_count( object_count ).
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

    DATA(selection_limit) = COND i( WHEN max_objects > 0
                                    THEN max_objects + 1
                                    ELSE 0 ).

    object_count = scope_pack_reader->count_scope_objects( selection_limit ).

    IF object_count = selection_limit.
      object_count = max_objects.
      more_objects_in_scope = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD init_from_db.
    super->init_from_db( search_scope = search_scope ).
    scope_pack_reader->set_object_count( object_count ).
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

  METHOD init_package_reader.
    " Fail safe if user somehow managed to get to this point
    IF NOT zcl_adcoset_db_support_util=>is_db_supported( ).
      MESSAGE a000(00) WITH |DB '{ sy-dbsys }' is not supported by ABAP Code Search|.
    ENDIF.

    IF scope_pack_reader IS INITIAL.
      scope_pack_reader = lcl_adbc_scope_reader_fac=>create_package_reader( search_range_provider = me
                                                                            paging_provider       = me  ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
