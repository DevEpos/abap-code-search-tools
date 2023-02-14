*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

CONSTANTS:
  c_sql_or            TYPE string VALUE ` OR `,
  c_sql_and           TYPE string VALUE ` AND `,
  c_sql_not           TYPE string VALUE `NOT `,
  c_sql_between       TYPE string VALUE `BETWEEN `,
  c_sql_escape_w_char TYPE string VALUE `ESCAPE '#' `,

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

TYPES:
  ty_generic_range TYPE RANGE OF string.

INTERFACE lif_adbc_scope_obj_reader.
  METHODS:
    read_next_package
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tadir_objects,
    set_object_count
      IMPORTING
        value TYPE i,
    set_package_size
      IMPORTING
        value TYPE i,
    has_more_packages
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDINTERFACE.

CLASS lcl_adbc_scope_reader_fac DEFINITION
  CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      "! Creates package reader for reading the next block of objects
      "! in a Code Search Scope
      create_package_reader
        IMPORTING
          search_ranges  TYPE zif_adcoset_ty_global=>ty_search_scope_ranges
          current_offset TYPE i
        RETURNING
          VALUE(result)  TYPE REF TO lif_adbc_scope_obj_reader,
      "! Returns true if the connected database is supported
      is_db_supported
        RETURNING
          VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS lcl_adbc_scope_obj_reader_base DEFINITION
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES lif_adbc_scope_obj_reader.
    METHODS:
      constructor.
  PROTECTED SECTION.
    DATA:
      adbc_stmnt_cols          TYPE adbc_column_tab,
      search_ranges            TYPE zif_adcoset_ty_global=>ty_search_scope_ranges,
      "! Restricts the maximum number of objects to select for the search
      current_offset           TYPE i,
      object_count             TYPE i,
      max_rows                 TYPE i,
      package_size             TYPE i,
      all_packages_read        TYPE abap_bool,
      "! needs custom configuration per DBMS
      appl_comp_dyn_where_cond TYPE string,
      select_clause            TYPE string,
      where_clause             TYPE string,
      order_by_clause          TYPE string,
      offset_clause            TYPE string,
      limit_clause             TYPE string,
      from_clause              TYPE string.

    METHODS:
      build_query_clauses,
      build_where_clause,
      build_select_clause,
      build_order_by_clause,
      build_from_clause,
      build_offset_clause ABSTRACT,
      build_limit_clause ABSTRACT,
      add_range_to_where
        IMPORTING
          ranges        TYPE STANDARD TABLE
          data_type     TYPE abap_typekind DEFAULT cl_abap_typedescr=>typekind_char
          sql_fieldname TYPE string
        CHANGING
          where         TYPE string,
      conv_range_to_where
        IMPORTING
          ranges        TYPE STANDARD TABLE
          data_type     TYPE abap_typekind
          sql_fieldname TYPE string
        RETURNING
          VALUE(result) TYPE string,
      split_including_excluding
        IMPORTING
          ranges    TYPE ty_generic_range
        EXPORTING
          including TYPE ty_generic_range
          excluding TYPE ty_generic_range,
      combine_clauses
        RETURNING
          VALUE(result) TYPE string,
      conv_conditions
        IMPORTING
          conditions    TYPE ty_generic_range
          sql_fieldname TYPE string
          data_type     TYPE abap_typekind
          negate        TYPE abap_bool
        RETURNING
          VALUE(result) TYPE string.
ENDCLASS.

CLASS lcl_cond_builder DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          fieldname TYPE string
          negate    TYPE abap_bool
          abap_cond TYPE LINE OF ty_generic_range
          data_type TYPE abap_typekind,
      build
        RETURNING
          VALUE(result) TYPE string.

  PRIVATE SECTION.
    DATA:
      abap_cond TYPE LINE OF ty_generic_range,
      fieldname TYPE string,
      negate    TYPE abap_bool,
      data_type TYPE abap_typekind,
      operator1 TYPE string,
      operator2 TYPE string,
      not       TYPE string,
      val1      TYPE string,
      val2      TYPE string,
      sql_cond  TYPE string.

    METHODS:
      negate_selopt_option,
      to_sql_val
        IMPORTING
          abap_val      TYPE string
        RETURNING
          VALUE(result) TYPE string,
      option_to_sql_operator
        RETURNING
          VALUE(result) TYPE string,
      conv_like_pattern_to_sql
        IMPORTING
          value         TYPE string
        RETURNING
          VALUE(result) TYPE string.
ENDCLASS.

CLASS lcl_oracle_scope_obj_reader DEFINITION
 INHERITING FROM lcl_adbc_scope_obj_reader_base.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          search_ranges  TYPE zif_adcoset_ty_global=>ty_search_scope_ranges
          current_offset TYPE i.
  PROTECTED SECTION.
    METHODS:
      build_offset_clause REDEFINITION,
      build_limit_clause REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.
