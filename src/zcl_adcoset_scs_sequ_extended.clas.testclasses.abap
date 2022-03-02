*"* use this source file for your ABAP unit test classes

CLASS lcl_test_helper DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_substring_matchers
        IMPORTING
          patterns      TYPE zif_adcoset_ty_global=>ty_patterns
        RETURNING
          VALUE(result) TYPE zif_adcoset_pattern_matcher=>ty_ref_tab,
      get_source
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
    DATA: source_content TYPE string_table.

    source_content = VALUE #(
      ( |  METHOD find_next_full_match.| )
      ( || )
      ( |    LOOP AT matchers ASSIGNING FIELD-SYMBOL(<matcher>).| )
      ( |      DATA(i) = sy-tabix.| )
      ( || )
      ( |      " # check if exclusion flag was set at matcher| )
      ( |      IF <matcher>->control_flags BIT-AND c_pattern_ctrl_flag-exclude =| )
      ( |          c_pattern_ctrl_flag-exclude.| )
      ( |        APPEND <matcher> TO exclusion_matchers.| )
      ( |        CONTINUE.| )
      ( |      ELSEIF <matcher>->control_flags BIT-AND c_pattern_ctrl_flag-boundary_end =| )
      ( |          c_pattern_ctrl_flag-boundary_end.| )
      ( |        current_match = boundary_end.| )
      ( |        current_line_offset = boundary_end-line.| )
      ( |        current_col_offset = boundary_end-offset.| )
      ( || )
      ( |        CLEAR: boundary_start,| )
      ( |               boundary_end.| )
      ( |        CONTINUE.| )
      ( |      ENDIF.| )
      ( || )
      ( |      SELECT *| )
      ( |        FROM mara INTO DATA(mara_res).| )
      ( || )
      ( || )
      ( |      LOOP AT mara_res INTO DATA(mara_res_line).| )
      ( |        SELECT SINGLE werks| )
      ( |          FROM marc| )
      ( |          WHERE matnr = @mara_res_line-matnr| )
      ( |          INTO @DATA(marc_res).| )
      ( |      ENDLOOP.| )
      ( || )
      ( |      previous_match = current_match.| )
      ( || )
      ( |      " # find match for the current matcher| )
      ( |      find_next_partial_match(| )
      ( |        EXPORTING| )
      ( |          matcher     = <matcher>| )
      ( |        IMPORTING| )
      ( |          match       = current_match| )
      ( |          line_offset = current_line_offset| )
      ( |          col_offset  = current_col_offset ).| )
      ( || )
      ( |      IF has_more_matches = abap_false.| )
      ( |        RETURN.| )
      ( |      ENDIF.| )
      ( | ENDLOOP.| )
      ( || )
      ( |  ENDMETHOD.| ) ).

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
      test_match_found2 FOR TESTING,
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
        ( content = 'control_flags' flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match )
      ) )
    ).

    DATA(matches) = cut->zif_adcoset_src_code_searcher~search( lcl_test_helper=>get_source( ) ).

    cl_abap_unit_assert=>assert_not_initial( matches ).
  ENDMETHOD.

  METHOD test_match_found2.
    cut = NEW zcl_adcoset_scs_sequ_extended(
      ignore_comment_lines = abap_true
      matchers             = lcl_test_helper=>create_substring_matchers( VALUE #(
        ( content = ' loop'       )
        ( content = ' assigning'  )
        ( content = '.'           )
        ( content = ' if ' flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-exclude )
        ( content = 'control_flags' flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match )
      ) )
    ).

    DATA(matches) = cut->zif_adcoset_src_code_searcher~search( lcl_test_helper=>get_source( ) ).

    cl_abap_unit_assert=>assert_initial( matches ).
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

    DATA(matches) = cut->zif_adcoset_src_code_searcher~search( lcl_test_helper=>get_source( ) ).

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
        ( content = 'control_flags' flags = zif_adcoset_c_pattern_matching=>c_pattern_ctrl_flag-match )
      ) )
    ).

    DATA(matches) = cut->zif_adcoset_src_code_searcher~search( lcl_test_helper=>get_source( ) ).

    cl_abap_unit_assert=>assert_initial( matches ).
  ENDMETHOD.

ENDCLASS.
