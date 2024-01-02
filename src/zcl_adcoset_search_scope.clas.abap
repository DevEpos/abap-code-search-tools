"! <p class="shorttext synchronized">Search scope implementation</p>
CLASS zcl_adcoset_search_scope DEFINITION
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
    DATA dyn_from_clause TYPE string.
    DATA tags_dyn_where_cond TYPE string.
    DATA appl_comp_dyn_where_cond TYPE string.

    "! Reader for packages of object scope
    DATA scope_pack_reader TYPE REF TO lif_adbc_scope_obj_reader.

    METHODS config_dyn_where_clauses.
    METHODS init_package_reader.
ENDCLASS.


CLASS zcl_adcoset_search_scope IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    init( search_scope ).
    init_package_reader( ).
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
    scope_pack_reader->set_package_size( package_size ).
  ENDMETHOD.

  METHOD determine_count.
    config_dyn_where_clauses( ).
    resolve_packages( ).

    DATA(selection_limit) = COND i( WHEN max_objects > 0
                                    THEN max_objects + 1
                                    ELSE 0 ).

    SELECT COUNT(*)
      FROM (dyn_from_clause)
      WHERE (tags_dyn_where_cond)
        AND obj~objecttype IN @search_ranges-object_type_range
        AND obj~objectname IN @search_ranges-object_name_range
        AND obj~developmentpackage IN @search_ranges-package_range
        AND obj~owner IN @search_ranges-owner_range
        AND obj~createddate IN @search_ranges-created_on_range
        AND (appl_comp_dyn_where_cond)
      INTO @object_count
      UP TO @selection_limit ROWS.

    IF object_count = selection_limit.
      object_count = max_objects.
      more_objects_in_scope = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD init_package_reader.
    " Fail safe if user somehow managed to get to this point
    IF NOT zcl_adcoset_db_support_util=>is_db_supported( ).
      MESSAGE a000(00) WITH |DB '{ sy-dbsys }' is not supported by ABAP Code Search|.
    ENDIF.

    IF scope_pack_reader IS INITIAL.
      scope_pack_reader = lcl_adbc_scope_reader_fac=>create_package_reader( search_ranges  = search_ranges
                                                                            current_offset = current_offset ).
      scope_pack_reader->set_object_count( object_count ).
      scope_pack_reader->set_package_size( package_size ).
    ENDIF.
  ENDMETHOD.

  METHOD init_from_db.
    super->init_from_db( search_scope = search_scope  ).
    config_dyn_where_clauses( ).
  ENDMETHOD.

  METHOD config_dyn_where_clauses.
    dyn_from_clause = `ZADCOSET_SRCDOBJ AS obj `.

    IF search_ranges-tag_id_range IS NOT INITIAL.
      tags_dyn_where_cond = `tgobj~tag_id in @search_ranges-tag_id_range`.

      " HINT: An object could be tagged twice and then it would appear
      "       more than once in the result -> this would result in possibly processing
      "       an object twice
      "       --> add group by clause if tags are supplied (possibly the only solution)
      dyn_from_clause = dyn_from_clause &&
        |INNER JOIN { zcl_adcoset_extensions_util=>get_current_tgobj_table( ) } AS tgobj | &&
        `ON  obj~ObjectName = tgobj~object_name ` &&
        `AND obj~ObjectType = tgobj~object_type `.
    ENDIF.

    IF search_ranges-appl_comp_range IS NOT INITIAL.
      dyn_from_clause = dyn_from_clause &&
        `INNER JOIN tdevc AS pack ` &&
        `ON obj~devclass = pack~devclass ` &&
        `INNER JOIN df14l AS appl ` &&
        `ON pack~component = appl~fctr_id `.

      appl_comp_dyn_where_cond = `appl~ps_posid IN @search_ranges-appl_comp_range`.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
