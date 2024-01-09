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
      BEGIN OF c_cds_field_name,
        programid          TYPE string VALUE 'programid',
        objecttype         TYPE string VALUE 'objecttype',
        objectname         TYPE string VALUE 'objectname',
        developmentpackage TYPE string VALUE 'developmentpackage',
        owner              TYPE string VALUE 'owner',
        createddate        TYPE string VALUE 'createddate',
      END OF c_cds_field_name.
    CONSTANTS:
      BEGIN OF c_table_field_name,
        tag_id      TYPE string VALUE 'tag_id',
        ps_posid    TYPE string VALUE 'ps_posid',
        object_name TYPE string VALUE 'object_name',
        object_type TYPE string VALUE 'object_type',
        devclass    TYPE string VALUE 'devclass',
      END OF c_table_field_name.
    CONSTANTS:
      BEGIN OF c_field_alias,
        name         TYPE adbc_name VALUE 'NAME',
        type         TYPE adbc_name VALUE 'TYPE',
        package_name TYPE adbc_name VALUE 'PACKAGE_NAME',
        owner        TYPE adbc_name VALUE 'OWNER',
      END OF c_field_alias.
    CONSTANTS:
      BEGIN OF c_tab_alias,
        obj   TYPE string VALUE 'obj',
        tgobj TYPE string VALUE 'tgobj',
        appl  TYPE string VALUE 'appl',
      END OF c_tab_alias.

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
        cols        = VALUE #( tab_alias = c_tab_alias-obj
                               ( name = c_cds_field_name-objecttype alias = c_field_alias-type )
                               ( name = c_cds_field_name-objectname alias = c_field_alias-name )
                               ( name = c_cds_field_name-owner )
                               ( name = c_cds_field_name-developmentpackage alias = c_field_alias-package_name ) )
        target_cols = VALUE #( ( c_field_alias-type  )
                               ( c_field_alias-name )
                               ( c_field_alias-owner  )
                               ( c_field_alias-package_name ) ) ).

    DATA(from_clause) = `ZADCOSET_ISRCOBJ obj`.
    IF search_ranges-tag_id_range IS NOT INITIAL.
      from_clause = from_clause && ` ` &&
        |INNER JOIN { zcl_adcoset_extensions_util=>get_current_tgobj_table( ) } tgobj | &&
        `ON  obj.objectname = tgobj.object_name ` &&
        `AND obj.objecttype = tgobj.object_type `.
    ENDIF.

    IF search_ranges-appl_comp_range IS NOT INITIAL.
      from_clause = from_clause && ` ` &&
        `INNER JOIN tdevc pack ON obj.developmentpackage = pack.devclass ` &&
        `INNER JOIN df14l appl ON pack.component = appl.fctr_id `.
    ENDIF.

    native_scope_query->set_from( from_clause ).
    native_scope_query->set_order_by( VALUE #( ( tab_alias = c_tab_alias-obj name = c_cds_field_name-programid  ) ) ).

    native_scope_query->add_range_to_where( ranges   = search_ranges-object_type_range
                                            col_info = VALUE #( tab_alias = c_tab_alias-obj
                                                                name      = c_cds_field_name-objecttype ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-object_name_range
                                            col_info = VALUE #( tab_alias = c_tab_alias-obj
                                                                name      = c_cds_field_name-objectname ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-package_range
                                            col_info = VALUE #( tab_alias = c_tab_alias-obj
                                                                name      = c_cds_field_name-developmentpackage ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-owner_range
                                            col_info = VALUE #( tab_alias = c_tab_alias-obj
                                                                name      = c_cds_field_name-owner ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-created_on_range
                                            col_info = VALUE #( tab_alias = c_tab_alias-obj
                                                                name      = c_cds_field_name-createddate ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-tag_id_range
                                            col_info = VALUE #( tab_alias = c_tab_alias-tgobj
                                                                name      = c_table_field_name-tag_id ) ).
    native_scope_query->add_range_to_where( ranges   = search_ranges-appl_comp_range
                                            col_info = VALUE #( tab_alias = c_tab_alias-appl
                                                                name      = c_table_field_name-ps_posid ) ).
  ENDMETHOD.

  METHOD init_from_db.
    super->init_from_db( search_scope = search_scope  ).
    config_dyn_where_clauses( ).
  ENDMETHOD.

  METHOD config_dyn_where_clauses.
    dyn_from_clause = `ZADCOSET_ISRCOBJ AS obj `.

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
