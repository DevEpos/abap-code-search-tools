*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_adbc_scope_reader_fac IMPLEMENTATION.
  METHOD create_package_reader.
    " see domain DBSYS_TYPE_SELECTOR for possible values
    CASE sy-dbsys.
      WHEN 'ORACLE'.
        result = NEW lcl_oracle_scope_obj_reader( search_ranges  = search_ranges
                                                  current_offset = current_offset ).
      WHEN 'HDB'.
        result = NEW lcl_hdb_scope_obj_reader( search_ranges  = search_ranges
                                               current_offset = current_offset ).
      WHEN 'MSSQL'.
        result = NEW lcl_mssql_scope_obj_reader( search_ranges  = search_ranges
                                                 current_offset = current_offset ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_oracle_scope_obj_reader IMPLEMENTATION.
  METHOD constructor.
    super->constructor( current_offset = current_offset
                        search_ranges  = search_ranges
                        max_objects    = max_objects ).
    build_offset_clause( ).
    build_limit_clause( ).
  ENDMETHOD.

  METHOD build_limit_clause.
    limit_clause = |FETCH FIRST { max_rows } ROWS ONLY|.
  ENDMETHOD.

  METHOD build_offset_clause.
    offset_clause = | OFFSET { current_offset } ROWS |.
  ENDMETHOD.

  METHOD build_with_statement.
    result =
    `WITH e071_aggr AS (` &&
       `SELECT DISTINCT programid pgmid,` &&
                       `objecttype obj_type,` &&
                       `objectname obj_name ` &&
         from_clause &&
         where_clause &&
        `) ` &&
     ` SELECT COUNT(*) FROM e071_aggr `.

    IF selection_limit IS NOT INITIAL.
      result = result && |FETCH FIRST { selection_limit } ROWS ONLY|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_hdb_scope_obj_reader IMPLEMENTATION.
  METHOD constructor.
    super->constructor( current_offset = current_offset
                        search_ranges  = search_ranges
                        max_objects    = max_objects ).
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

  METHOD build_with_statement.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_mssql_scope_obj_reader IMPLEMENTATION.
  METHOD constructor.
    super->constructor( current_offset = current_offset
                        search_ranges  = search_ranges
                        max_objects    = max_objects ).
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

  METHOD build_with_statement.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adbc_scope_obj_reader_base IMPLEMENTATION.
  METHOD constructor.
    me->search_ranges = search_ranges.
    resolve_tr_request( ).
    add_subobj_type_to_filter( ).
    me->current_offset = current_offset.
    me->max_objects    = max_objects.
    adbc_stmnt_cols = VALUE #( ( CONV adbc_name( 'PGMID' ) )
                               ( CONV adbc_name( 'OBJ_TYPE' ) )
                               ( CONV adbc_name( 'OBJ_NAME' ) ) ).

    build_query_clauses( ).
  ENDMETHOD.

  METHOD lif_adbc_scope_obj_reader~read_next_package.
    DATA tr_objects TYPE zif_adcoset_ty_global=>ty_std_tr_request_objects.
    DATA tr_objects_sorted TYPE zif_adcoset_ty_global=>ty_tr_request_objects.

    DATA(max_rows) = package_size.
    IF     current_offset IS INITIAL
       AND ( object_count < package_size OR package_size = 0 ).
      max_rows = object_count.
    ENDIF.

    DATA(query) = combine_clauses( ).
    TRY.
        DATA(result_set) = NEW cl_sql_statement( )->execute_query( query ).
        result_set->set_param_table( itab_ref             = REF #( tr_objects )
                                     corresponding_fields = adbc_stmnt_cols ).
        IF result_set->next_package( ) > 0.
          tr_objects_sorted = tr_objects.
          result = VALUE #( count   = lines( tr_objects )
                            objects = NEW zcl_adcoset_tr_obj_processor( tr_objects    = tr_objects_sorted
                                                                        search_ranges = search_ranges )->run( ) ).
        ENDIF.
        current_offset = current_offset + result-count.

        IF result-count < max_rows.
          all_packages_read = abap_true.
        ENDIF.

      CATCH cx_sql_exception.
        all_packages_read = abap_true.
    ENDTRY.
  ENDMETHOD.

  METHOD lif_adbc_scope_obj_reader~set_object_count.
    object_count = value.
  ENDMETHOD.

  METHOD lif_adbc_scope_obj_reader~set_package_size.
    me->package_size = value.
  ENDMETHOD.

  METHOD lif_adbc_scope_obj_reader~has_more_packages.
    result = xsdbool( all_packages_read = abap_false AND current_offset < object_count ).
  ENDMETHOD.

  METHOD lif_adbc_scope_obj_reader~more_objects_in_scope.
    result = more_objects_in_scope.
  ENDMETHOD.

  METHOD build_query_clauses.
    build_select_clause( ).
    build_where_clause( ).
    build_from_clause( ).
    build_order_by_clause( ).
  ENDMETHOD.

  METHOD build_where_clause.
    add_range_to_where( EXPORTING ranges        = search_ranges-object_type_range
                                  sql_fieldname = 'objecttype'
                        CHANGING  where         = where_clause ).
    add_range_to_where( EXPORTING ranges        = search_ranges-object_name_range
                                  sql_fieldname = 'objectname'
                        CHANGING  where         = where_clause ).
    add_range_to_where( EXPORTING ranges        = search_ranges-tr_request_range
                                  sql_fieldname = 'request'
                        CHANGING  where         = where_clause ).
    IF where_clause IS NOT INITIAL.
      where_clause = |{ where_clause } |.
    ENDIF.
  ENDMETHOD.

  METHOD build_from_clause.
    from_clause = `FROM ZADCOSET_TRSCO `.
  ENDMETHOD.

  METHOD build_order_by_clause.
    order_by_clause = `ORDER BY obj_name,` &&
                                `pgmid,` &&
                                `obj_type`.
  ENDMETHOD.

  METHOD build_select_clause.
    select_clause = `SELECT programid pgmid, ` &&
                    `objecttype obj_type, ` &&
                    `objectname obj_name `.
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

  METHOD lif_adbc_scope_obj_reader~count_scope_objects.
    DATA(selection_limit) = COND i( WHEN max_objects > 0
                                    THEN max_objects + 1
                                    ELSE 0 ).

    IF     search_ranges-object_type_range IS INITIAL
       AND search_ranges-object_name_range IS INITIAL
       AND search_ranges-tr_request_range  IS INITIAL.
      RETURN.
    ENDIF.

    DATA(query) = build_with_statement( selection_limit ).
    TRY.
        DATA(result_set) = NEW cl_sql_statement( )->execute_query( query ).
        result_set->set_param( data_ref = REF #( object_count ) ).
        result_set->next( ).

      CATCH cx_sql_exception INTO DATA(err). " TODO: variable is assigned but never used (ABAP cleaner)
        object_count = 0.
    ENDTRY.

    IF object_count = selection_limit.
      object_count = max_objects.
      more_objects_in_scope = abap_true.
    ENDIF.

    result = object_count.
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
      ELSEIF object_type_range->low = zif_adcoset_c_global=>c_source_code_type-function_group.
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
      ELSEIF     object_type_range->low  = zif_adcoset_c_global=>c_source_code_type-program
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
