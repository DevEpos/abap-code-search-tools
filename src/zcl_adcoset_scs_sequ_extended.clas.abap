"! <p class="shorttext synchronized" lang="en">Extended Sequential matching</p>
"! Uses control flags at matcher level to run a more customized and
"! detailed search
CLASS zcl_adcoset_scs_sequ_extended DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_adcoset_scs_sequential
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_c_pattern_matching.
    METHODS:
      zif_adcoset_src_code_searcher~search
        REDEFINITION,
      constructor
        IMPORTING
          ignore_comment_lines TYPE abap_bool
          matchers             TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES:
      c_pattern_ctrl_flag     FOR zif_adcoset_c_pattern_matching~c_pattern_ctrl_flag,
      c_pattern_ctrl_sequence FOR zif_adcoset_c_pattern_matching~c_pattern_ctrl_sequence.

    DATA:
      has_custom_match_boundary TYPE abap_bool,
      is_ctrl_flags_found       TYPE abap_bool,
      previous_match            TYPE match_result,
      end_boundary_matcher      TYPE REF TO zif_adcoset_pattern_matcher,
      boundary_start            TYPE match_result,
      boundary_end              TYPE match_result,
      exclusion_matchers        TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.

    METHODS:
      find_next_full_match
        CHANGING
          matches TYPE zif_adcoset_ty_global=>ty_search_matches,
      has_matches_for_exclusions
        RETURNING
          VALUE(result) TYPE abap_bool,
      find_next_boundary_end
        IMPORTING
          matchers_index TYPE syst-tabix
        RETURNING
          VALUE(result)  TYPE abap_bool,
      is_any_match_in_range
        IMPORTING
          matcher       TYPE REF TO zif_adcoset_pattern_matcher
          start_line    TYPE i
          offset        TYPE i
          end_line      TYPE i
          end_offset    TYPE i
        RETURNING
          VALUE(result) TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_scs_sequ_extended IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      ignore_comment_lines = ignore_comment_lines
      matchers             = matchers ).


    LOOP AT matchers INTO DATA(matcher).
      IF matcher->control_flags BIT-AND c_pattern_ctrl_flag-match_start =
          c_pattern_ctrl_flag-match_start.
        has_custom_match_boundary = abap_true.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD zif_adcoset_src_code_searcher~search.
    " set the source code attribute for this search call
    me->source_code = source_code.
    has_more_matches = abap_true.

    CLEAR: current_line_offset,
           current_col_offset.

    WHILE has_more_matches = abap_true.
      CLEAR: current_match,
             previous_match,
             match_start,
             match_end,
             boundary_start,
             boundary_end,
             exclusion_matchers.

      find_next_full_match( CHANGING matches = result ).
    ENDWHILE.

  ENDMETHOD.


  METHOD find_next_full_match.

    LOOP AT matchers ASSIGNING FIELD-SYMBOL(<matcher>).
      DATA(i) = sy-tabix.

      " # check if exclusion flag was set at matcher
      IF <matcher>->control_flags BIT-AND c_pattern_ctrl_flag-exclude =
          c_pattern_ctrl_flag-exclude.
        APPEND <matcher> TO exclusion_matchers.
        CONTINUE.
      ELSEIF <matcher>->control_flags BIT-AND c_pattern_ctrl_flag-boundary_end =
          c_pattern_ctrl_flag-boundary_end.
        current_match = boundary_end.
        current_line_offset = boundary_end-line.
        current_col_offset = boundary_end-offset.

        CLEAR: boundary_start,
               boundary_end.
        CONTINUE.
      ENDIF.

      previous_match = current_match.

      " # find match for the current matcher
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

      " # check if exclusion table has entries
      IF exclusion_matchers IS NOT INITIAL.
        IF has_matches_for_exclusions( ).
          RETURN.
        ENDIF.
      ELSE.
        CLEAR exclusion_matchers.
      ENDIF.

      " # Check if match is inside the boundary - if currently there is a boundary active
      IF boundary_end IS NOT INITIAL AND
          current_match-line > boundary_end-line OR
          ( current_match-line = boundary_end-line AND current_match-offset >= boundary_end-offset ).
        " match was found but steps out of the end boundary
        current_line_offset = boundary_end-line.
        current_col_offset = boundary_end-offset + boundary_end-length.
        RETURN.
      ENDIF.

      " # check boundary start/end
      IF <matcher>->control_flags BIT-AND c_pattern_ctrl_flag-boundary_start =
          c_pattern_ctrl_flag-boundary_start.
        boundary_start = current_match.

        IF NOT find_next_boundary_end( matchers_index = i + 1 ).
          has_more_matches = abap_false.
          RETURN.
        ENDIF.
      ENDIF.

      " # check if custom match patterns were specified, if not
      "   the match will span from the first pattern matcher to the last
      IF i = 1.
        IF has_custom_match_boundary = abap_false.
          match_start = current_match.
        ENDIF.
      ENDIF.

      IF <matcher>->control_flags BIT-AND c_pattern_ctrl_flag-match_start =
          c_pattern_ctrl_flag-match_start.
        match_start = current_match.
      ENDIF.
      IF <matcher>->control_flags BIT-AND c_pattern_ctrl_flag-match_end =
          c_pattern_ctrl_flag-match_end.
        match_end = current_match.
      ENDIF.

    ENDLOOP.

    IF has_custom_match_boundary = abap_false.
      match_end = current_match.
    ENDIF.

    collect_sequential_match( CHANGING matches = matches ).

  ENDMETHOD.


  METHOD has_matches_for_exclusions.

    DATA(range_match_start) = previous_match.

    LOOP AT exclusion_matchers ASSIGNING FIELD-SYMBOL(<matcher>).

      IF is_any_match_in_range(
           matcher    = <matcher>
           start_line = range_match_start-line
           offset     = range_match_start-offset
           end_line   = current_match-line
           end_offset = current_match-offset ).

        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_next_boundary_end.

    LOOP AT matchers ASSIGNING FIELD-SYMBOL(<matcher>) FROM matchers_index.
      IF <matcher>->control_flags BIT-AND c_pattern_ctrl_flag-boundary_end =
          c_pattern_ctrl_flag-boundary_end.

        find_next_partial_match(
          EXPORTING
            matcher = <matcher>
          IMPORTING
            match   = boundary_end ).

        IF boundary_end IS NOT INITIAL.
          result = abap_true.
        ENDIF.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD is_any_match_in_range.
    TRY.
        DATA(matches) = matcher->find_matches_in_range(
          source     = source_code->content
          start_line = start_line
          offset     = offset
          end_line   = end_line
          end_offset = end_offset ).
      CATCH zcx_adcoset_pattern_sh_error.
        RETURN.
    ENDTRY.

    IF matches IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT matches ASSIGNING FIELD-SYMBOL(<match>).
      IF ignore_comment_lines = abap_true AND
          source_code->comment_regex IS NOT INITIAL AND
          is_comment_line( source_code->content[ <match>-line ] ).
        CONTINUE.
      ENDIF.
      result = abap_true.
      RETURN.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
