*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_adbc_scope_reader_fac IMPLEMENTATION.
  METHOD create_package_reader.
    " see domain DBSYS_TYPE_SELECTOR for possible values
    CASE sy-dbsys.
      WHEN 'ORACLE'.
        result = NEW lcl_oracle_scope_obj_reader( search_ranges   = search_ranges
                                                  paging_provider = paging_provider ).
      WHEN 'HDB'.
        result = NEW lcl_hdb_scope_obj_reader( search_ranges   = search_ranges
                                               paging_provider = paging_provider ).
      WHEN 'MSSQL'.
        result = NEW lcl_mssql_scope_obj_reader( search_ranges   = search_ranges
                                                 paging_provider = paging_provider ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_oracle_scope_obj_reader IMPLEMENTATION.
  METHOD constructor.
    super->constructor( paging_provider = paging_provider
                        search_ranges   = search_ranges ).
  ENDMETHOD.

  METHOD build_limit_clause.
    limit_clause = |FETCH FIRST { max_rows } ROWS ONLY|.
  ENDMETHOD.

  METHOD build_offset_clause.
    offset_clause = |OFFSET { current_offset } ROWS |.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_hdb_scope_obj_reader IMPLEMENTATION.
  METHOD constructor.
    super->constructor( paging_provider = paging_provider
                        search_ranges   = search_ranges ).
  ENDMETHOD.

  METHOD combine_clauses.
    " HANA expects the LIMIT clause before the offset clause
    result = select_clause && from_clause && where_clause && order_by_clause && limit_clause && offset_clause.
  ENDMETHOD.

  METHOD build_limit_clause.
    limit_clause = |LIMIT { max_rows } |.
  ENDMETHOD.

  METHOD build_offset_clause.
    offset_clause = |OFFSET { current_offset }|.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_mssql_scope_obj_reader IMPLEMENTATION.
  METHOD constructor.
    super->constructor( paging_provider = paging_provider
                        search_ranges   = search_ranges ).
  ENDMETHOD.

  METHOD combine_clauses.
    " columns are case sensitive, so uppercase everything
    result = to_upper( select_clause && from_clause && where_clause && order_by_clause && offset_clause && limit_clause ).
  ENDMETHOD.

  METHOD build_limit_clause.
    limit_clause = |FETCH NEXT { max_rows } ROWS ONLY|.
  ENDMETHOD.

  METHOD build_offset_clause.
    offset_clause = |OFFSET { current_offset } ROWS |.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adbc_scope_obj_reader_base IMPLEMENTATION.
  METHOD constructor.
    me->search_ranges   = search_ranges.
    me->paging_provider = paging_provider.
    adbc_stmnt_cols = VALUE #( ( CONV adbc_name( 'TYPE' ) )
                               ( CONV adbc_name( 'NAME' ) )
                               ( CONV adbc_name( 'OWNER' ) )
                               ( CONV adbc_name( 'PACKAGE_NAME' ) ) ).

    build_query_clauses( ).
  ENDMETHOD.

  METHOD lif_adbc_scope_obj_reader~read_next_package.
    current_offset = paging_provider->get_skip( ).
    max_rows = paging_provider->get_top( ).

    build_offset_clause( ).
    build_limit_clause( ).

    DATA(query) = combine_clauses( ).

    TRY.
        DATA(result_set) = NEW cl_sql_statement( )->execute_query( query ).

        result_set->set_param_table( itab_ref             = REF #( result-objects )
                                     corresponding_fields = adbc_stmnt_cols ).
        result_set->next_package( ).

        result-count = lines( result-objects ).
        current_offset = current_offset + result-count.

        IF result-count < max_rows.
          all_packages_read = abap_true.
        ENDIF.
      CATCH cx_sql_exception INTO DATA(err). " TODO: variable is assigned but never used (ABAP cleaner)
        all_packages_read = abap_true.
    ENDTRY.
  ENDMETHOD.

  METHOD lif_adbc_scope_obj_reader~set_object_count.
    object_count = value.
  ENDMETHOD.

  METHOD lif_adbc_scope_obj_reader~has_more_packages.
    result = xsdbool( all_packages_read = abap_false AND current_offset < object_count ).
  ENDMETHOD.

  METHOD build_query_clauses.
    build_select_clause( ).
    build_where_clause( ).
    build_from_clause( ).
    build_order_by_clause( ).
  ENDMETHOD.

  METHOD build_where_clause.
    add_range_to_where( EXPORTING ranges        = search_ranges-object_type_range
                                  sql_fieldname = 'obj.objecttype'
                        CHANGING  where         = where_clause ).
    add_range_to_where( EXPORTING ranges        = search_ranges-object_name_range
                                  sql_fieldname = 'obj.objectname'
                        CHANGING  where         = where_clause ).
    add_range_to_where( EXPORTING ranges        = search_ranges-package_range
                                  sql_fieldname = 'obj.developmentpackage'
                        CHANGING  where         = where_clause ).
    add_range_to_where( EXPORTING ranges        = search_ranges-owner_range
                                  sql_fieldname = 'obj.owner'
                        CHANGING  where         = where_clause ).
    add_range_to_where( EXPORTING ranges        = search_ranges-created_on_range
                                  sql_fieldname = 'obj.createddate'
                        CHANGING  where         = where_clause ).
    add_range_to_where( EXPORTING ranges        = search_ranges-tag_id_range
                                  sql_fieldname = 'tgobj.tag_id'
                        CHANGING  where         = where_clause ).
    add_range_to_where( EXPORTING ranges        = search_ranges-appl_comp_range
                                  sql_fieldname = 'appl.ps_posid'
                        CHANGING  where         = where_clause ).

    IF where_clause IS NOT INITIAL.
      where_clause = |{ where_clause } |.
    ENDIF.
  ENDMETHOD.

  METHOD build_from_clause.
    from_clause = `FROM ZADCOSET_SRCDOBJ obj `.
    IF search_ranges-tag_id_range IS NOT INITIAL.
      from_clause = from_clause &&
        |INNER JOIN { zcl_adcoset_extensions_util=>get_current_tgobj_table( ) } tgobj | &&
        `ON  obj.objectname = tgobj.object_name ` &&
        `AND obj.objecttype = tgobj.object_type `.
    ENDIF.

    IF search_ranges-appl_comp_range IS NOT INITIAL.
      from_clause = from_clause &&
        `INNER JOIN tdevc pack ON obj.developmentpackage = pack.devclass ` &&
        `INNER JOIN df14l appl ON pack.component = appl.fctr_id `.
    ENDIF.
  ENDMETHOD.

  METHOD build_order_by_clause.
    order_by_clause = `ORDER BY obj.programid `.
  ENDMETHOD.

  METHOD build_select_clause.
    select_clause = `SELECT obj.objecttype type, ` &&
                    `obj.objectname name, ` &&
                    `obj.owner, ` &&
                    `obj.developmentpackage package_name `.
  ENDMETHOD.

  METHOD combine_clauses.
    result = select_clause && from_clause && where_clause && order_by_clause && offset_clause && limit_clause.
  ENDMETHOD.

  METHOD add_range_to_where.
    DATA(ranges_as_where) = conv_range_to_where( ranges        = ranges
                                                 data_type     = data_type
                                                 sql_fieldname = sql_fieldname ).

    IF ranges_as_where IS INITIAL.
      RETURN.
    ENDIF.

    IF where IS INITIAL.
      where = |WHERE { ranges_as_where }|.
    ELSE.
      where = |{ where }{ c_sql_and }{ ranges_as_where }|.
    ENDIF.
  ENDMETHOD.

  METHOD conv_range_to_where.
    CHECK ranges IS NOT INITIAL.

    split_including_excluding( EXPORTING ranges    = CORRESPONDING #( ranges )
                               IMPORTING including = DATA(including)
                                         excluding = DATA(excluding) ).

    DATA(incl_conditions) = conv_conditions( conditions    = including
                                             sql_fieldname = sql_fieldname
                                             data_type     = data_type
                                             negate        = abap_false ).
    DATA(excl_conditions) = conv_conditions( conditions    = excluding
                                             sql_fieldname = sql_fieldname
                                             data_type     = data_type
                                             negate        = abap_true ).
    result = incl_conditions.
    IF result IS INITIAL.
      result = excl_conditions.
    ELSEIF excl_conditions IS NOT INITIAL.
      result = |{ result }{ c_sql_and }{ excl_conditions }|.
    ENDIF.
  ENDMETHOD.

  METHOD split_including_excluding.
    LOOP AT ranges ASSIGNING FIELD-SYMBOL(<range_entry>).
      IF <range_entry>-sign = 'I'.
        including = VALUE #( BASE including
                             ( <range_entry> ) ).
      ELSEIF <range_entry>-sign = 'E'.
        excluding = VALUE #( BASE excluding
                             ( <range_entry> ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD conv_conditions.
    DATA new_cond TYPE string.

    CHECK conditions IS NOT INITIAL.

    DATA(combine_operator) = COND #( WHEN negate = abap_true THEN c_sql_and ELSE c_sql_or ).

    LOOP AT conditions ASSIGNING FIELD-SYMBOL(<condition>).
      new_cond = NEW lcl_cond_builder( fieldname = sql_fieldname
                                       negate    = negate
                                       abap_cond = <condition>
                                       data_type = data_type )->build( ).

      IF result IS NOT INITIAL.
        result = |{ result }{ combine_operator }{ new_cond } |.
      ELSE.
        result = |{ new_cond }|.
      ENDIF.
    ENDLOOP.

    IF negate = abap_false.
      result = |({ result })|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_cond_builder IMPLEMENTATION.
  METHOD constructor.
    me->fieldname = fieldname.
    me->data_type = data_type.
    me->negate    = negate.
    me->abap_cond = abap_cond.
  ENDMETHOD.

  METHOD build.
    IF negate = abap_true.
      negate_selopt_option( ).
    ENDIF.

    IF    abap_cond-option = c_selopt_option-not_contains_pattern
       OR abap_cond-option = c_selopt_option-not_between.
      not = c_sql_not.
    ENDIF.

    CASE abap_cond-option.
      WHEN c_selopt_option-between OR
           c_selopt_option-not_between.
        operator1 = c_sql_between.
        operator2 = c_sql_and.

        val1 = to_sql_val( abap_cond-low ).
        val2 = to_sql_val( abap_cond-high ).

      WHEN c_selopt_option-contains_pattern OR
           c_selopt_option-not_contains_pattern.
        operator1 = c_sql_comparator-like.
        val1 = to_sql_val( conv_like_pattern_to_sql( abap_cond-low ) ).

      WHEN OTHERS.
        operator1 = option_to_sql_operator( ).
        val1 = to_sql_val( abap_cond-low ).
    ENDCASE.

    result = |{ not }{ fieldname } { operator1 }{ val1 } { operator2 }{ val2 }|.
  ENDMETHOD.

  METHOD negate_selopt_option.
    abap_cond-option = SWITCH #( abap_cond-option
                                 WHEN c_selopt_option-between              THEN c_selopt_option-not_between
                                 WHEN c_selopt_option-not_between          THEN c_selopt_option-between
                                 WHEN c_selopt_option-equals               THEN c_selopt_option-not_equals
                                 WHEN c_selopt_option-not_equals           THEN c_selopt_option-equals
                                 WHEN c_selopt_option-lesser_than          THEN c_selopt_option-greater_equal
                                 WHEN c_selopt_option-lesser_equal         THEN c_selopt_option-greater_than
                                 WHEN c_selopt_option-greater_equal        THEN c_selopt_option-lesser_than
                                 WHEN c_selopt_option-greater_than         THEN c_selopt_option-lesser_equal
                                 WHEN c_selopt_option-contains_pattern     THEN c_selopt_option-not_contains_pattern
                                 WHEN c_selopt_option-not_contains_pattern THEN c_selopt_option-contains_pattern ).
  ENDMETHOD.

  METHOD option_to_sql_operator.
    result = SWITCH #( abap_cond-option
                       WHEN c_selopt_option-equals        THEN c_sql_comparator-equals
                       WHEN c_selopt_option-not_equals    THEN c_sql_comparator-not_equals
                       WHEN c_selopt_option-lesser_than   THEN c_sql_comparator-lesser_than
                       WHEN c_selopt_option-lesser_equal  THEN c_sql_comparator-lesser_equal
                       WHEN c_selopt_option-greater_equal THEN c_sql_comparator-greater_equal
                       WHEN c_selopt_option-greater_than  THEN c_sql_comparator-greater_than ).
  ENDMETHOD.

  METHOD to_sql_val.
    result = |{ cl_abap_dyn_prg=>quote( abap_val ) }|.
  ENDMETHOD.

  METHOD conv_like_pattern_to_sql.
    IF value CA '%_#'.
      operator2 = c_sql_escape_w_char.
    ENDIF.

    " 1) escape all '%' with '#%'
    result = replace( val  = value
                      sub  = '%'
                      with = '#%'
                      occ  = 0 ).
    " 2) escape all '#' with '##'
    result = replace( val  = result
                      sub  = '#'
                      with = '##'
                      occ  = 0 ).
    " 3) escape all '_' with '#_'
    result = replace( val  = result
                      sub  = '_'
                      with = '#_'
                      occ  = 0 ).
    " 4) escape all '*' with '%'
    result = replace( val  = result
                      sub  = '*'
                      with = '%'
                      occ  = 0 ).
    " 5) escape all '+' with '_'
    result = replace( val  = result
                      sub  = '+'
                      with = '_'
                      occ  = 0 ).
  ENDMETHOD.
ENDCLASS.
