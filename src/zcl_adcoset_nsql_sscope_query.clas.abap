"! <p class="shorttext synchronized">Scope Object Reader for native SQL</p>
CLASS zcl_adcoset_nsql_sscope_query DEFINITION
  PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS set_order_by
      IMPORTING
        !value TYPE string.

    METHODS set_select
      IMPORTING
        !value    TYPE string
        cols      TYPE adbc_column_tab OPTIONAL
        !distinct TYPE abap_bool       OPTIONAL.

    METHODS set_offset
      IMPORTING
        !value TYPE i.

    METHODS set_limit
      IMPORTING
        !value TYPE i OPTIONAL.

    METHODS set_from
      IMPORTING
        !value TYPE string.

    METHODS reset.

    METHODS add_range_to_where
      IMPORTING
        !ranges       TYPE STANDARD TABLE
        data_type     TYPE abap_typekind DEFAULT cl_abap_typedescr=>typekind_char
        sql_fieldname TYPE string.

    METHODS has_where_cond
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Executes the query and writes the results in the given table
    METHODS execute_query
      IMPORTING
        itab          TYPE REF TO data
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Executes the current query as CTE count query <br/>
    "! e.g. SELECT col1 FROM tab -> WITH count_cte as ( query ) select count(*) from count_cte
    METHODS execute_cte_count
      RETURNING
        VALUE(result) TYPE i.

  PROTECTED SECTION.
    DATA adbc_stmnt_cols TYPE adbc_column_tab.
    DATA select_clause TYPE string.
    DATA where_clause TYPE string.
    DATA order_by_clause TYPE string.
    DATA offset_clause TYPE string.
    DATA limit_clause TYPE string.
    DATA from_clause TYPE string.

    METHODS combine_clauses
      IMPORTING
        exclude_paging TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result)  TYPE string.

    METHODS conv_range_to_where
      IMPORTING
        !ranges       TYPE STANDARD TABLE
        data_type     TYPE abap_typekind
        sql_fieldname TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS split_including_excluding
      IMPORTING
        !ranges    TYPE zif_adcoset_ty_global=>ty_generic_range
      EXPORTING
        !including TYPE zif_adcoset_ty_global=>ty_generic_range
        !excluding TYPE zif_adcoset_ty_global=>ty_generic_range.

    METHODS conv_conditions
      IMPORTING
        conditions    TYPE zif_adcoset_ty_global=>ty_generic_range
        sql_fieldname TYPE string
        data_type     TYPE abap_typekind
        negate        TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
    METHODS get_paging
      RETURNING
        VALUE(result) TYPE string_table.

    METHODS conv_tab_to_string
      IMPORTING
        !tab          TYPE string_table
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS zcl_adcoset_nsql_sscope_query IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD reset.
    CLEAR: from_clause,
           select_clause,
           offset_clause,
           limit_clause,
           where_clause.
  ENDMETHOD.

  METHOD set_limit.
    IF value IS INITIAL.
      CLEAR limit_clause.
    ENDIF.
    limit_clause = SWITCH #( sy-dbsys
                             WHEN zif_adcoset_c_global=>c_dbsys-oracle THEN |FETCH FIRST { value } ROWS ONLY|
                             WHEN zif_adcoset_c_global=>c_dbsys-hana   THEN |LIMIT { value }|
                             WHEN zif_adcoset_c_global=>c_dbsys-ms_sql THEN |FETCH NEXT { value } ROWS ONLY| ).
  ENDMETHOD.

  METHOD set_offset.
    offset_clause = SWITCH #( sy-dbsys
                              WHEN zif_adcoset_c_global=>c_dbsys-oracle
                                OR zif_adcoset_c_global=>c_dbsys-ms_sql THEN |OFFSET { value } ROWS|
                              WHEN zif_adcoset_c_global=>c_dbsys-hana   THEN |OFFSET { value }| ).
  ENDMETHOD.

  METHOD set_from.
    from_clause = |FROM { value }|.
  ENDMETHOD.

  METHOD set_order_by.
    order_by_clause = |ORDER BY { value }|.
  ENDMETHOD.

  METHOD has_where_cond.
    result = xsdbool( where_clause IS NOT INITIAL ).
  ENDMETHOD.

  METHOD set_select.
    DATA select_parts TYPE string_table.

    select_parts = VALUE #( ( `SELECT` ) ).

    IF distinct = abap_true.
      APPEND `DISTINCT` TO select_parts.
    ENDIF.

    APPEND value TO select_parts.

    select_clause = concat_lines_of( table = select_parts
                                     sep   = ` ` ).

    adbc_stmnt_cols = cols.
  ENDMETHOD.

  METHOD execute_query.
    DATA(query) = combine_clauses( ).
    TRY.
        DATA(result_set) = NEW cl_sql_statement( )->execute_query( query ).
        result_set->set_param_table( itab_ref             = itab
                                     corresponding_fields = adbc_stmnt_cols ).
        result = xsdbool( result_set->next_package( ) > 0 ).
      CATCH cx_sql_exception INTO DATA(lx_native_sql).
        zcl_adcoset_log=>add_exception( lx_native_sql ).
    ENDTRY.
  ENDMETHOD.

  METHOD execute_cte_count.
    DATA(query) = conv_tab_to_string( VALUE #( ( `WITH count_cte as(` )
                                               ( combine_clauses( exclude_paging = abap_true ) )
                                               ( `)` )
                                               ( `SELECT COUNT(*) FROM count_cte` ) ) ).

    TRY.
        DATA(result_set) = NEW cl_sql_statement( )->execute_query( query ).
        result_set->set_param( data_ref = REF #( result ) ).
        result_set->next( ).
      CATCH cx_sql_exception INTO DATA(lx_sql).
        zcl_adcoset_log=>add_exception( lx_sql ).
        result = 0.
    ENDTRY.
  ENDMETHOD.

  METHOD add_range_to_where.
    DATA(ranges_as_where) = conv_range_to_where( ranges        = ranges
                                                 data_type     = data_type
                                                 sql_fieldname = sql_fieldname ).

    IF ranges_as_where IS INITIAL.
      RETURN.
    ENDIF.

    IF where_clause IS INITIAL.
      where_clause = |WHERE { ranges_as_where }|.
    ELSE.
      where_clause = |{ where_clause }{ zcl_adcoset_nsql_cond_builder=>c_sql_and }{ ranges_as_where }|.
    ENDIF.
  ENDMETHOD.

  METHOD combine_clauses.
    DATA clauses TYPE string_table.

    clauses = VALUE #( ( select_clause )
                       ( from_clause )
                       ( where_clause )
                       ( order_by_clause ) ).
    IF exclude_paging = abap_false.
      clauses = VALUE #( BASE clauses
                         ( LINES OF get_paging( ) ) ).
    ENDIF.

    result = conv_tab_to_string( clauses ).
  ENDMETHOD.

  METHOD get_paging.
    IF sy-dbsys = 'HDB'.
      result = VALUE #( ( limit_clause )
                        ( offset_clause ) ).
    ELSE.
      result = VALUE #( ( offset_clause )
                        ( limit_clause ) ).
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
      result = |{ result }{ zcl_adcoset_nsql_cond_builder=>c_sql_and }{ excl_conditions }|.
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

    DATA(combine_operator) = COND #( WHEN negate = abap_true
                                     THEN zcl_adcoset_nsql_cond_builder=>c_sql_and
                                     ELSE zcl_adcoset_nsql_cond_builder=>c_sql_or ).

    LOOP AT conditions ASSIGNING FIELD-SYMBOL(<condition>).
      new_cond = NEW zcl_adcoset_nsql_cond_builder( fieldname = sql_fieldname
                                                    negate    = negate
                                                    abap_cond = <condition>
                                                    data_type = data_type )->build( ).

      IF result IS NOT INITIAL.
        result = |{ result }{ combine_operator }{ new_cond }|.
      ELSE.
        result = |{ new_cond }|.
      ENDIF.
    ENDLOOP.

    IF negate = abap_false.
      result = |({ result })|.
    ENDIF.
  ENDMETHOD.

  METHOD conv_tab_to_string.
    result = to_upper( concat_lines_of( table = tab
                                        sep   = ` ` ) ).
  ENDMETHOD.
ENDCLASS.
