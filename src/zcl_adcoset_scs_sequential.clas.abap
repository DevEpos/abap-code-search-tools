"! <p class="shorttext synchronized" lang="en">Sequential Source Code Searcher</p>
CLASS zcl_adcoset_scs_sequential DEFINITION
  PUBLIC
  INHERITING FROM zcl_adcoset_scs_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_src_code_searcher.

    METHODS:
      constructor
        IMPORTING
          ignore_comment_lines TYPE abap_bool
          matchers             TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      has_more_matches TYPE abap_bool,
      line_offset      TYPE i,
      col_offset       TYPE i,
      current_match    TYPE match_result,
      match_start      TYPE match_result.
    METHODS:
      find_next_full_match
        CHANGING
          result TYPE zif_adcoset_ty_global=>ty_search_matches,
      find_next_partial_match
        IMPORTING
          matcher TYPE REF TO zif_adcoset_pattern_matcher.
ENDCLASS.



CLASS zcl_adcoset_scs_sequential IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      ignore_comment_lines = ignore_comment_lines
      matchers             = matchers ).
  ENDMETHOD.


  METHOD zif_adcoset_src_code_searcher~search.
    " set the source code attribute for this search call
    me->source_code = source_code.
    has_more_matches = abap_true.

    CLEAR: line_offset,
           col_offset,
           current_match,
           match_start.

    WHILE has_more_matches = abap_true.
      CLEAR current_match.

      find_next_full_match( CHANGING result = result ).
    ENDWHILE.

  ENDMETHOD.


  METHOD find_next_full_match.

    LOOP AT matchers ASSIGNING FIELD-SYMBOL(<matcher>).
      DATA(i) = sy-tabix.
      find_next_partial_match( matcher = <matcher> ).

      IF has_more_matches = abap_false.
        RETURN.
      ENDIF.

      IF i = 1.
        match_start = current_match.
      ENDIF.
    ENDLOOP.

    IF current_match IS NOT INITIAL AND
        match_start IS NOT INITIAL.
      result = VALUE #( BASE result
        ( start_line   = match_start-line
          start_column = match_start-offset
          end_line     = current_match-line
          end_column   = current_match-offset + current_match-length
          snippet      = source_code->content[ match_start-line ] ) ).
    ENDIF.

  ENDMETHOD.


  METHOD find_next_partial_match.
    TRY.
        current_match = matcher->find_next_match(
          source     = source_code->content
          start_line = line_offset
          offset     = col_offset ).
      CATCH zcx_adcoset_pattern_sh_error.
        has_more_matches = abap_false.
        RETURN.
    ENDTRY.

    IF current_match IS INITIAL.
      has_more_matches = abap_false.
      RETURN.
    ENDIF.

    IF ignore_comment_lines = abap_true AND
        source_code->comment_regex IS NOT INITIAL AND
        is_comment_line( source_code->content[ current_match-line ] ).
      has_more_matches = abap_false.
      CLEAR current_match.
      RETURN.
    ENDIF.

    line_offset = current_match-line.
    col_offset = current_match-offset + current_match-length.
  ENDMETHOD.

ENDCLASS.
