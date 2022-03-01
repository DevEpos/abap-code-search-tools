"! <p class="shorttext synchronized" lang="en">Sequential Source Code Searcher</p>
CLASS zcl_adcoset_scs_sequential DEFINITION
  PUBLIC
  INHERITING FROM zcl_adcoset_scs_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_src_code_searcher.

    METHODS:
      constructor
        IMPORTING
          ignore_comment_lines TYPE abap_bool
          matchers             TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
    DATA:
      has_more_matches    TYPE abap_bool,
      current_line_offset TYPE i,
      current_col_offset  TYPE i,
      current_match       TYPE match_result,
      match_start         TYPE match_result,
      match_end           TYPE match_result.

    METHODS:
      find_next_partial_match
        IMPORTING
          matcher            TYPE REF TO zif_adcoset_pattern_matcher
        EXPORTING
          match              TYPE match_result
          VALUE(line_offset) TYPE i
          VALUE(col_offset)  TYPE i,
      collect_sequential_match
        CHANGING
          matches TYPE zif_adcoset_ty_global=>ty_search_matches.
  PRIVATE SECTION.

    METHODS:
      find_next_full_match
        CHANGING
          matches TYPE zif_adcoset_ty_global=>ty_search_matches.
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

    CLEAR: current_line_offset,
           current_col_offset.

    WHILE has_more_matches = abap_true.
      CLEAR: current_match,
             match_start.

      find_next_full_match( CHANGING matches = result ).
    ENDWHILE.

  ENDMETHOD.


  METHOD find_next_full_match.

    LOOP AT matchers ASSIGNING FIELD-SYMBOL(<matcher>).
      DATA(i) = sy-tabix.
      find_next_partial_match(
        EXPORTING
          matcher     = <matcher>
        IMPORTING
          match       = current_match
          line_offset = current_line_offset
          col_offset  = current_col_offset ).

      IF has_more_matches = abap_false.
        RETURN.
      ENDIF.

      IF i = 1.
        match_start = current_match.
      ENDIF.
    ENDLOOP.

    match_end = current_match.

    collect_sequential_match( CHANGING matches = matches ).

  ENDMETHOD.


  METHOD find_next_partial_match.
    CLEAR: match,
           line_offset,
           col_offset.
    TRY.
        match = matcher->find_next_match(
          source     = source_code->content
          start_line = current_line_offset
          offset     = current_col_offset ).
      CATCH zcx_adcoset_pattern_sh_error.
        has_more_matches = abap_false.
        RETURN.
    ENDTRY.

    IF match IS INITIAL.
      has_more_matches = abap_false.
      RETURN.
    ENDIF.

    IF ignore_comment_lines = abap_true AND
        source_code->comment_regex IS NOT INITIAL AND
        is_comment_line( source_code->content[ current_match-line ] ).
      has_more_matches = abap_false.
      CLEAR match.
      RETURN.
    ENDIF.

    line_offset = current_match-line.
    col_offset = current_match-offset + current_match-length.
  ENDMETHOD.


  METHOD collect_sequential_match.

    IF match_start IS NOT INITIAL AND
        match_end IS NOT INITIAL.
      matches = VALUE #( BASE matches
        ( start_line   = match_start-line
          start_column = match_start-offset
          end_line     = match_end-line
          end_column   = match_end-offset + match_end-length
          snippet      = source_code->content[ match_start-line ] ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
