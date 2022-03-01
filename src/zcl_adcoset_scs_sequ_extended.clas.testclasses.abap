*"* use this source file for your ABAP unit test classes

CLASS lcl_test_helper DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CONSTANTS:
      c_source_1 TYPE i VALUE 1,
      c_source_2 TYPE i VALUE 2.
    CLASS-METHODS:
      create_substring_matchers
        IMPORTING
          patterns      TYPE zif_adcoset_ty_global=>ty_patterns
        RETURNING
          VALUE(result) TYPE zif_adcoset_pattern_matcher=>ty_ref_tab,
      get_source
        IMPORTING
          source_id     TYPE i
        RETURNING
          VALUE(result) TYPE REF TO zif_adcoset_source_code.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_test_helper IMPLEMENTATION.

  METHOD create_substring_matchers.

    LOOP AT patterns INTO DATA(pattern).
      TRY.
          result = VALUE #( BASE result
            ( zcl_adcoset_matcher_factory=>create_matcher(
                type          = zif_adcoset_c_global=>c_matcher_type-substring
                pattern       = pattern
                ignore_case   = abap_true ) ) ).
        CATCH zcx_adcoset_no_matcher cx_sy_regex.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_source.
    DATA: mtd_key        TYPE seocpdkey,
          source_content TYPE string_table.

    CASE source_id.

      WHEN c_source_1.
        mtd_key = VALUE #( clsname = 'ZCL_ADCOSET_SCS_SEQU_EXTENDED' cpdname = 'FIND_NEXT_FULL_MATCH' ).

      WHEN c_source_2.
        mtd_key = VALUE #( clsname = 'ZCL_ADCOSET_SCS_SEQU_EXTENDED' cpdname = 'FIND_NEXT_BOUNDARY_END' ).

    ENDCASE.

    DATA(include_name) = cl_oo_classname_service=>get_method_include( mtdkey = mtd_key ).
    READ REPORT include_name INTO source_content.
    result = NEW zcl_adcoset_source_code(
      source        = source_content
      comment_regex = '^(\*|\s*")' ).
  ENDMETHOD.

ENDCLASS.
CLASS ltcl_abap_unit DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      cut TYPE REF TO zcl_adcoset_scs_sequ_extended.

    METHODS:
      test_match_found1 FOR TESTING,
      test_no_match_found1 FOR TESTING,
      test_no_match_found2 FOR TESTING.
ENDCLASS.

CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD test_match_found1.
    cut = NEW zcl_adcoset_scs_sequ_extended(
      ignore_comment_lines = abap_true
      matchers             = lcl_test_helper=>create_substring_matchers( VALUE #(
        ( content = ' loop'       )
        ( content = ' assigning'  )
        ( content = '.'           )
        ( content = ' find_next_partial_match' flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match_start BIT-OR
                                                       zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match_end )
      ) )
    ).

    DATA(source_code) = lcl_test_helper=>get_source( lcl_test_helper=>c_source_2 ).

    DATA(matches) = cut->zif_adcoset_src_code_searcher~search( source_code ).

    cl_abap_unit_assert=>assert_not_initial( matches ).
  ENDMETHOD.


  METHOD test_no_match_found1.
    cut = NEW zcl_adcoset_scs_sequ_extended(
      ignore_comment_lines = abap_true
      matchers             = lcl_test_helper=>create_substring_matchers( VALUE #(
        ( content = ' loop'       flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-boundary_start )
        ( content = ' assigning' )
        ( content = '.'           flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-boundary_end )
        ( content = ' if '        flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-exclude )
        ( content = ' append '    flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match_start )
        ( content = '.'           flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match_end ) ) )
    ).

    DATA(source_code) = lcl_test_helper=>get_source( lcl_test_helper=>c_source_1 ).

    DATA(matches) = cut->zif_adcoset_src_code_searcher~search( source_code ).

    cl_abap_unit_assert=>assert_initial( matches ).
  ENDMETHOD.


  METHOD test_no_match_found2.
    cut = NEW zcl_adcoset_scs_sequ_extended(
      ignore_comment_lines = abap_true
      matchers             = lcl_test_helper=>create_substring_matchers( VALUE #(
        ( content = ' loop'       )
        ( content = ' assigning'  )
        ( content = '.'           )
        ( content = ' if ' flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-exclude )
        ( content = ' find_next_partial_match' flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match_start BIT-OR
                                                       zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match_end )
      ) )
    ).

    DATA(source_code) = lcl_test_helper=>get_source( lcl_test_helper=>c_source_2 ).

    DATA(matches) = cut->zif_adcoset_src_code_searcher~search( source_code ).

    cl_abap_unit_assert=>assert_initial( matches ).
  ENDMETHOD.

ENDCLASS.
