"! <p class="shorttext synchronized">Condition builder for Native SQL</p>
CLASS zcl_adcoset_nsql_cond_builder DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_sql_or TYPE string VALUE ` OR `.
    CONSTANTS c_sql_and TYPE string VALUE ` AND `.
    CONSTANTS c_sql_not TYPE string VALUE `NOT `.
    CONSTANTS c_sql_between TYPE string VALUE `BETWEEN `.
    CONSTANTS c_sql_escape_w_char TYPE string VALUE `ESCAPE '#' `.

    METHODS constructor
      IMPORTING
        fieldname TYPE string
        negate    TYPE abap_bool
        abap_cond TYPE LINE OF zif_adcoset_ty_global=>ty_generic_range
        data_type TYPE abap_typekind.

    METHODS build
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
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
        equals        TYPE string VALUE ` = `,
        not_equals    TYPE string VALUE ` <> `,
        greater_than  TYPE string VALUE ` > `,
        greater_equal TYPE string VALUE ` >= `,
        lesser_than   TYPE string VALUE ` < `,
        lesser_equal  TYPE string VALUE ` <= `,
        like          TYPE string VALUE ` LIKE `,
      END OF c_sql_comparator.

    DATA abap_cond TYPE LINE OF zif_adcoset_ty_global=>ty_generic_range.
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


CLASS zcl_adcoset_nsql_cond_builder IMPLEMENTATION.
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

    result = |{ not }{ fieldname }{ operator1 }{ val1 }{ operator2 }{ val2 }|.
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
    result = replace( val = value sub = '%' with = '#%' occ = 0 ).
    " 2) escape all '#' with '##'
    result = replace( val = result sub = '#' with = '##' occ = 0 ).
    " 3) escape all '_' with '#_'
    result = replace( val = result sub = '_' with = '#_' occ = 0 ).
    " 4) escape all '*' with '%'
    result = replace( val = result sub = '*' with = '%' occ = 0 ).
    " 5) escape all '+' with '_'
    result = replace( val = result sub = '+' with = '_' occ = 0 ).
  ENDMETHOD.
ENDCLASS.
