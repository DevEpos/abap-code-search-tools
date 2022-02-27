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
          ignore_comment_lines  TYPE abap_bool
          check_sequence_bounds TYPE abap_bool DEFAULT abap_false
          matchers              TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      has_more_matches      TYPE abap_bool,
      check_sequence_bounds TYPE abap_bool,
      current_line_offset   TYPE i,
      current_col_offset    TYPE i,
      current_match         TYPE match_result,
      end_boundary_matcher  TYPE REF TO zif_adcoset_pattern_matcher,
      match_start           TYPE match_result.

    METHODS:
      "! Finds the next sequential match by using boundary matching<br/>
      "! This means the first and last pattern will mark the offset in the
      "! source code that should contain all other patterns.
      find_next_full_match_w_bounds
        CHANGING
          matches TYPE zif_adcoset_ty_global=>ty_search_matches,
      "! Finds next sequential match by iterating over all patterns
      "! and returning only matches where the patterns occur in the given order
      find_next_full_match_wo_bounds
        CHANGING
          matches TYPE zif_adcoset_ty_global=>ty_search_matches,
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
ENDCLASS.



CLASS zcl_adcoset_scs_sequential IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      ignore_comment_lines = ignore_comment_lines
      matchers             = matchers ).

    IF check_sequence_bounds = abap_true AND
        lines( matchers ) > 2.
      me->check_sequence_bounds = abap_true.
      DATA(matcher_count) = lines( matchers ).
      end_boundary_matcher = matchers[ matcher_count ].
      DELETE me->matchers INDEX matcher_count.
    ENDIF.

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

      IF check_sequence_bounds = abap_true.
        find_next_full_match_w_bounds( CHANGING matches = result ).
      ELSE.
        find_next_full_match_wo_bounds( CHANGING matches = result ).
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD find_next_full_match_wo_bounds.

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

    collect_sequential_match( CHANGING matches = matches ).

  ENDMETHOD.


  METHOD find_next_full_match_w_bounds.

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

        " look for the end boundary
        find_next_partial_match(
          EXPORTING
            matcher = end_boundary_matcher
          IMPORTING
            match   = DATA(end_match) ).
        IF has_more_matches = abap_false.
          RETURN.
        ENDIF.
      ELSEIF current_match-line > end_match-line OR
          ( current_match-line = end_match-line AND current_match-offset >= end_match-offset ).
        " match was found but steps out of the end boundary
        current_line_offset = end_match-line.
        current_col_offset = end_match-offset + end_match-length.
        RETURN.
      ENDIF.
    ENDLOOP.

    " set the current match to the end boundary match
    current_match = end_match.

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

    IF current_match IS NOT INITIAL AND
            match_start IS NOT INITIAL.
      matches = VALUE #( BASE matches
        ( start_line   = match_start-line
          start_column = match_start-offset
          end_line     = current_match-line
          end_column   = current_match-offset + current_match-length
          snippet      = source_code->content[ match_start-line ] ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
