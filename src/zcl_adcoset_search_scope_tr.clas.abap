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
    METHODS zif_adcoset_search_scope~more_objects_in_scope  REDEFINITION.

  PROTECTED SECTION.
    METHODS determine_count REDEFINITION.

  PRIVATE SECTION.
    "! Reader for packages of object scope
    DATA scope_pack_reader TYPE REF TO lif_adbc_scope_obj_reader.

    METHODS init_package_reader.
ENDCLASS.


CLASS zcl_adcoset_search_scope_tr IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    search_ranges = search_scope-ranges.
    init_package_reader( ).
    init( search_scope ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~has_next_package.
    scope_pack_reader->has_more_packages( ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~next_package.
    result = scope_pack_reader->read_next_package( ).
*    DATA(max_rows) = package_size.
*    IF     current_offset IS INITIAL
*       AND ( object_count < package_size OR package_size = 0 ).
*      max_rows = object_count.
*    ENDIF.
*    TRY.
*        DATA(tr_objects) = get_tr_objects( max_rows ).
*
*        result = VALUE #( count   = lines( tr_objects )
*                          objects = NEW zcl_adcoset_tr_obj_processor( tr_objects    = tr_objects
*                                                                      search_ranges = search_ranges )->run( ) ).
*
*        current_offset = current_offset + result-count.
*
*        IF result-count < max_rows.
*          all_packages_read = abap_true.
*        ENDIF.
*
*      CATCH cx_sql_exception INTO DATA(err). " TODO: variable is assigned but never used (ABAP cleaner)
*        all_packages_read = abap_true.
*    ENDTRY.
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~more_objects_in_scope.
    result = scope_pack_reader->more_objects_in_scope( ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~configure_package_size.
    super->zif_adcoset_search_scope~configure_package_size( max_objects    = max_objects
                                                            max_task_count = max_task_count ).

    scope_pack_reader->set_object_count( object_count ).
    scope_pack_reader->set_package_size( package_size ).
  ENDMETHOD.

  METHOD determine_count.
*    DATA(selection_limit) = COND i( WHEN max_objects > 0
*                                    THEN max_objects + 1
*                                    ELSE 0 ).
*
*    IF     search_ranges-object_type_range IS INITIAL
*       AND search_ranges-object_name_range IS INITIAL
*       AND search_ranges-tr_request_range  IS INITIAL.
*      RETURN.
*    ENDIF.

*    init_package_reader( ).
    object_count = scope_pack_reader->count_scope_objects( ).

*    IF object_count = selection_limit.
*      object_count = max_objects.
*      more_objects_in_scope = abap_true.
*    ENDIF.
  ENDMETHOD.

  METHOD init_package_reader.
    " Fail safe if user somehow managed to get to this point
    IF NOT zcl_adcoset_db_support_util=>is_db_supported( ).
      MESSAGE a000(00) WITH |DB '{ sy-dbsys }' is not supported by ABAP Code Search|.
    ENDIF.

    IF scope_pack_reader IS INITIAL.
      scope_pack_reader = lcl_adbc_scope_reader_fac=>create_package_reader( search_ranges  = search_ranges
                                                                            current_offset = current_offset
                                                                            max_objects    = max_objects ).
*      scope_pack_reader->set_object_count( object_count ).
      scope_pack_reader->set_package_size( package_size ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
