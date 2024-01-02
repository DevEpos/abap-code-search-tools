*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CONSTANTS c_sql_or TYPE string VALUE ` OR `.
CONSTANTS c_sql_and TYPE string VALUE ` AND `.
CONSTANTS c_sql_not TYPE string VALUE `NOT `.
CONSTANTS c_sql_between TYPE string VALUE `BETWEEN `.
CONSTANTS c_sql_escape_w_char TYPE string VALUE `ESCAPE '#' `.

CONSTANTS:
  BEGIN OF c_selopt_option,
    between              TYPE string VALUE 'BT',
    not_between          TYPE string VALUE 'NB',
    equals               TYPE string VALUE 'EQ',
    not_equals           TYPE string VALUE 'NE',
    greater_than         TYPE string VALUE 'GT',
    greater_equal        TYPE string VALUE 'GE',
    lesser_than          TYPE string VALUE 'LT',
    lesser_equal         TYPE string VALUE 'LE',
    contains_pattern     TYPE string VALUE 'CP',
    not_contains_pattern TYPE string VALUE 'NP',
  END OF c_selopt_option.

CONSTANTS:
  BEGIN OF c_sql_comparator,
    equals        TYPE string VALUE `= `,
    not_equals    TYPE string VALUE `<> `,
    greater_than  TYPE string VALUE `> `,
    greater_equal TYPE string VALUE `>= `,
    lesser_than   TYPE string VALUE `< `,
    lesser_equal  TYPE string VALUE `<= `,
    like          TYPE string VALUE `LIKE `,
  END OF c_sql_comparator.

TYPES ty_generic_range TYPE RANGE OF string.

INTERFACE lif_adbc_scope_obj_reader.
  METHODS set_object_count
    IMPORTING
      !value TYPE i.

  METHODS has_more_packages
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS count_scope_objects
    IMPORTING
      selection_limit TYPE i
    RETURNING
      VALUE(result)   TYPE i.

  METHODS read_next_package
    RETURNING
      VALUE(result) TYPE zif_adcoset_ty_global=>ty_scope_package.

  METHODS get_current_offset
    RETURNING
      VALUE(result) TYPE i.

ENDINTERFACE.


CLASS lcl_adbc_scope_reader_fac DEFINITION
  CREATE PRIVATE.

  PUBLIC SECTION.
    "! Creates package reader for reading the next block of objects
    "! in a Code Search Scope
    CLASS-METHODS create_package_reader
      IMPORTING
        search_range_provider TYPE REF TO zif_adcoset_search_sr_provider
        paging_provider       TYPE REF TO zif_adcoset_paging_provider
      RETURNING
        VALUE(result)         TYPE REF TO lif_adbc_scope_obj_reader.
ENDCLASS.


CLASS lcl_adbc_scope_obj_reader_base DEFINITION
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES lif_adbc_scope_obj_reader.

    METHODS constructor
      IMPORTING
        search_range_provider TYPE REF TO zif_adcoset_search_sr_provider
        paging_provider       TYPE REF TO zif_adcoset_paging_provider
        current_offset        TYPE i
        max_objects           TYPE i.

  PROTECTED SECTION.
    DATA adbc_stmnt_cols TYPE adbc_column_tab.
    DATA search_range_provider TYPE REF TO zif_adcoset_search_sr_provider.
    DATA paging_provider TYPE REF TO zif_adcoset_paging_provider.
    "! Restricts the maximum number of objects to select for the search
    DATA current_offset TYPE i.
    DATA object_count TYPE i.
    DATA max_rows TYPE i.
    DATA all_packages_read TYPE abap_bool.
    "! Restricts the maximum number of objects to select for the search
    DATA max_objects TYPE i.
    DATA select_clause TYPE string.
    DATA where_clause TYPE string.
    DATA order_by_clause TYPE string.
    DATA offset_clause TYPE string.
    DATA limit_clause TYPE string.
    DATA from_clause TYPE string.

    METHODS build_query_clauses.
    METHODS build_where_clause.
    METHODS build_select_clause.
    METHODS build_order_by_clause.
    METHODS build_from_clause.
    METHODS build_offset_clause ABSTRACT.
    METHODS build_limit_clause ABSTRACT.

    METHODS add_range_to_where
      IMPORTING
        !ranges       TYPE STANDARD TABLE
        data_type     TYPE abap_typekind DEFAULT cl_abap_typedescr=>typekind_char
        sql_fieldname TYPE string
      CHANGING
        !where        TYPE string.

    METHODS conv_range_to_where
      IMPORTING
        !ranges       TYPE STANDARD TABLE
        data_type     TYPE abap_typekind
        sql_fieldname TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS split_including_excluding
      IMPORTING
        !ranges    TYPE ty_generic_range
      EXPORTING
        !including TYPE ty_generic_range
        !excluding TYPE ty_generic_range.

    METHODS combine_clauses
      RETURNING
        VALUE(result) TYPE string.

    METHODS conv_conditions
      IMPORTING
        conditions    TYPE ty_generic_range
        sql_fieldname TYPE string
        data_type     TYPE abap_typekind
        negate        TYPE abap_bool
      RETURNING
        VALUE(result) TYPE string.

    METHODS build_with_statement ABSTRACT
      IMPORTING
        selection_limit TYPE i
      RETURNING
        VALUE(result)   TYPE string.
ENDCLASS.


CLASS lcl_cond_builder DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        fieldname TYPE string
        negate    TYPE abap_bool
        abap_cond TYPE LINE OF ty_generic_range
        data_type TYPE abap_typekind.

    METHODS build
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA abap_cond TYPE LINE OF ty_generic_range.
    DATA fieldname TYPE string.
    DATA negate TYPE abap_bool.
    DATA data_type TYPE abap_typekind.
    DATA operator1 TYPE string.
    DATA operator2 TYPE string.
    DATA not TYPE string.
    DATA val1 TYPE string.
    DATA val2 TYPE string.

    METHODS negate_selopt_option.

    METHODS to_sql_val
      IMPORTING
        abap_val      TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS option_to_sql_operator
      RETURNING
        VALUE(result) TYPE string.

    METHODS conv_like_pattern_to_sql
      IMPORTING
        !value        TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.


CLASS lcl_oracle_scope_obj_reader DEFINITION
  INHERITING FROM lcl_adbc_scope_obj_reader_base.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        search_range_provider TYPE REF TO zif_adcoset_search_sr_provider
        paging_provider       TYPE REF TO zif_adcoset_paging_provider.

  PROTECTED SECTION.
    METHODS build_offset_clause  REDEFINITION.
    METHODS build_limit_clause   REDEFINITION.
    METHODS build_with_statement REDEFINITION.
ENDCLASS.


CLASS lcl_hdb_scope_obj_reader DEFINITION
  INHERITING FROM lcl_adbc_scope_obj_reader_base.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        search_range_provider TYPE REF TO zif_adcoset_search_sr_provider
        paging_provider       TYPE REF TO zif_adcoset_paging_provider.

  PROTECTED SECTION.
    METHODS combine_clauses      REDEFINITION.
    METHODS build_offset_clause  REDEFINITION.
    METHODS build_limit_clause   REDEFINITION.
    METHODS build_with_statement REDEFINITION.
ENDCLASS.


CLASS lcl_mssql_scope_obj_reader DEFINITION
  INHERITING FROM lcl_adbc_scope_obj_reader_base.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        search_range_provider TYPE REF TO zif_adcoset_search_sr_provider
        paging_provider       TYPE REF TO zif_adcoset_paging_provider.

  PROTECTED SECTION.
    METHODS combine_clauses      REDEFINITION.
    METHODS build_offset_clause  REDEFINITION.
    METHODS build_limit_clause   REDEFINITION.
    METHODS build_with_statement REDEFINITION.
ENDCLASS.
