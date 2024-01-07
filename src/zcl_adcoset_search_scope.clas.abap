"! <p class="shorttext synchronized">Search scope implementation</p>
CLASS zcl_adcoset_search_scope DEFINITION
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
    METHODS init_from_db    REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF cds_field_name,
        programid          TYPE string VALUE 'programid',
        objecttype         TYPE string VALUE 'objecttype',
        objectname         TYPE string VALUE 'objectname',
        developmentpackage TYPE string VALUE 'developmentpackage',
        owner              TYPE string VALUE 'owner',
        createddate        TYPE string VALUE 'createddate',
      END OF cds_field_name.
    CONSTANTS:
      BEGIN OF table_field_name,
        tag_id      TYPE string VALUE 'tag_id',
        ps_posid    TYPE string VALUE 'ps_posid',
        object_name TYPE string VALUE 'object_name',
        object_type TYPE string VALUE 'object_type',
        devclass    TYPE string VALUE 'devclass',
        fctr_id     TYPE string VALUE 'fctr_id',
        component   TYPE string VALUE 'component',
      END OF table_field_name.
    CONSTANTS:
      BEGIN OF field_alias,
        name         TYPE string VALUE 'name',
        type         TYPE string VALUE 'type',
        package_name TYPE string VALUE 'package_name',
      END OF field_alias.
    CONSTANTS:
      BEGIN OF tab_alias,
        obj   TYPE string VALUE 'obj',
        tgobj TYPE string VALUE 'tgobj',
        appl  TYPE string VALUE 'appl',
        pack  TYPE string VALUE 'pack',
      END OF tab_alias.
    CONSTANTS:
      BEGIN OF column_name,
        obj_type     TYPE dbobject_d VALUE 'TYPE',
        obj_name     TYPE dbobject_d VALUE 'NAME',
        owner        TYPE dbobject_d VALUE 'OWNER',
        package_name TYPE dbobject_d VALUE 'PACKAGE_NAME',
      END OF column_name.

    DATA dyn_from_clause TYPE string.
    DATA tags_dyn_where_cond TYPE string.
    DATA appl_comp_dyn_where_cond TYPE string.

    DATA native_scope_query TYPE REF TO zcl_adcoset_nsql_sscope_query.

    METHODS config_dyn_where_clauses.
    METHODS init_native_scope_query.
ENDCLASS.


CLASS zcl_adcoset_search_scope IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    init( search_scope ).
  ENDMETHOD.

  METHOD zif_adcoset_search_scope~next_package.
    DATA(max_rows) = get_max_rows( ).

    init_native_scope_query( ).
    native_scope_query->set_limit( max_rows ).
    native_scope_query->set_offset( current_offset ).

    IF NOT native_scope_query->execute_query( REF #( result-objects ) ).
      all_packages_read = abap_true.
      RETURN.
    ENDIF.

    result-count = lines( result-objects ).
    current_offset = current_offset + result-count.

    IF result-count < max_rows.
      all_packages_read = abap_true.
    ENDIF.
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

  METHOD init_native_scope_query.
    IF native_scope_query IS NOT INITIAL.
      RETURN.
    ENDIF.

    native_scope_query = NEW #( ).
    native_scope_query->set_select(
        cols        = VALUE #( tab_alias = tab_alias-obj
                               ( name = cds_field_name-objecttype alias = field_alias-type )
                               ( name = cds_field_name-objectname alias = field_alias-name )
                               ( name = cds_field_name-owner )
                               ( name = cds_field_name-developmentpackage alias = field_alias-package_name ) )
        target_cols = VALUE #( ( column_name-obj_type  )
                               ( column_name-obj_name )
                               ( column_name-owner  )
                               ( column_name-package_name ) ) ).
    DATA(from_clause) = `ZADCOSET_SRCDOBJ ` && tab_alias-obj && ` `.
    IF search_ranges-tag_id_range IS NOT INITIAL.
      from_clause = from_clause && ` ` &&
        |INNER JOIN { zcl_adcoset_extensions_util=>get_current_tgobj_table( ) } | && tab_alias-tgobj &&
        | ON { tab_alias-obj }.{ cds_field_name-objectname } = { tab_alias-tgobj }.{ table_field_name-object_name }| &&
        | AND { tab_alias-obj }.{ cds_field_name-objecttype } = { tab_alias-tgobj }.{ table_field_name-object_type } |.
    ENDIF.

    IF search_ranges-appl_comp_range IS NOT INITIAL.
      from_clause = from_clause && ` ` &&
        | INNER JOIN tdevc { tab_alias-pack } ON { tab_alias-obj }.{ cds_field_name-developmentpackage } = { tab_alias-pack }.{ table_field_name-devclass } | &&
        | INNER JOIN df14l { tab_alias-appl } ON { tab_alias-pack }.{ table_field_name-component } = { tab_alias-appl }.{ table_field_name-fctr_id } |.
    ENDIF.

    native_scope_query->set_from( from_clause ).
    native_scope_query->set_order_by( VALUE #( ( tab_alias = tab_alias-obj name = cds_field_name-programid  ) ) ).

    native_scope_query->add_range_to_where( ranges   = search_ranges-object_type_range
                                            col_info = VALUE #( tab_alias = tab_alias-obj
                                                                name      = cds_field_name-objecttype ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-object_name_range
                                            col_info = VALUE #( tab_alias = tab_alias-obj
                                                                name      = cds_field_name-objectname ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-package_range
                                            col_info = VALUE #( tab_alias = tab_alias-obj
                                                                name      = cds_field_name-developmentpackage ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-owner_range
                                            col_info = VALUE #( tab_alias = tab_alias-obj
                                                                name      = cds_field_name-owner ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-created_on_range
                                            col_info = VALUE #( tab_alias = tab_alias-obj
                                                                name      = cds_field_name-createddate ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-tag_id_range
                                            col_info = VALUE #( tab_alias = tab_alias-tgobj
                                                                name      = table_field_name-tag_id ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-appl_comp_range
                                            col_info = VALUE #( tab_alias = tab_alias-appl
                                                                name      = table_field_name-ps_posid ) ).
  ENDMETHOD.

  METHOD init_from_db.
    super->init_from_db( search_scope = search_scope  ).
    config_dyn_where_clauses( ).
  ENDMETHOD.

  METHOD config_dyn_where_clauses.
    dyn_from_clause = `ZADCOSET_SRCDOBJ AS ` && tab_alias-obj && ` `.

    IF search_ranges-tag_id_range IS NOT INITIAL.
      tags_dyn_where_cond = tab_alias-tgobj && `~` && table_field_name-tag_id && ` IN @search_ranges-tag_id_range`.

      " HINT: An object could be tagged twice and then it would appear
      "       more than once in the result -> this would result in possibly processing
      "       an object twice
      "       --> add group by clause if tags are supplied (possibly the only solution)
      dyn_from_clause = dyn_from_clause &&
        |INNER JOIN { zcl_adcoset_extensions_util=>get_current_tgobj_table( ) } AS { tab_alias-tgobj } | &&
        | ON { tab_alias-obj }~{ cds_field_name-objectname } = { tab_alias-tgobj }~{ table_field_name-object_name } | &&
        | AND { tab_alias-obj }~{ cds_field_name-objecttype } = { tab_alias-tgobj }~{ table_field_name-object_type } |.

    ENDIF.

    IF search_ranges-appl_comp_range IS NOT INITIAL.
      dyn_from_clause = dyn_from_clause &&
        | INNER JOIN tdevc AS { tab_alias-pack } | &&
        | ON { tab_alias-obj }~{ table_field_name-devclass } = { tab_alias-pack }~{ table_field_name-devclass } | &&
        | INNER JOIN df14l AS { tab_alias-appl } | &&
        | ON { tab_alias-pack }~{ table_field_name-component } = { tab_alias-appl }~{ table_field_name-fctr_id } |.

      appl_comp_dyn_where_cond = `appl~ps_posid IN @search_ranges-appl_comp_range`.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
