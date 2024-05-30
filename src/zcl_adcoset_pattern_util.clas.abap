"! <p class="shorttext synchronized">Utility for handling patterns</p>
CLASS zcl_adcoset_pattern_util DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_c_pattern_matching.

    CLASS-METHODS class_constructor.

    "! <p class="shorttext synchronized">Checks control flags in pattern sequence</p>
    CLASS-METHODS parse_pattern_sequence
      IMPORTING
        patterns      TYPE zif_adcoset_ty_global=>ty_patterns
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_patterns
      RAISING
        zcx_adcoset_static_error.

  PRIVATE SECTION.
    ALIASES c_pattern_ctrl_flag     FOR zif_adcoset_c_pattern_matching~c_pattern_ctrl_flag.
    ALIASES c_pattern_ctrl_sequence FOR zif_adcoset_c_pattern_matching~c_pattern_ctrl_sequence.

    TYPES:
      BEGIN OF ty_flag_to_sequence,
        control_sequence TYPE string,
        flag             TYPE zif_adcoset_ty_global=>ty_control_flags,
      END OF ty_flag_to_sequence.

    CLASS-DATA ctrl_sequences_regex TYPE string.
    CLASS-DATA any_ctrl_sequence_regex TYPE string.
    CLASS-DATA possible_ctrl_seq_range TYPE RANGE OF zif_adcoset_ty_global=>ty_control_flags.
    CLASS-DATA ctrl_flag_to_sequence_map TYPE HASHED TABLE OF ty_flag_to_sequence WITH UNIQUE KEY control_sequence.

    CLASS-METHODS parse_pattern
      CHANGING
        !pattern TYPE zif_adcoset_ty_global=>ty_pattern
      RAISING
        zcx_adcoset_static_error.

    CLASS-METHODS parse_patterns
      IMPORTING
        patterns      TYPE zif_adcoset_ty_global=>ty_patterns
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_patterns
      RAISING
        zcx_adcoset_static_error.

    CLASS-METHODS validate_sequence
      CHANGING
        patterns TYPE zif_adcoset_ty_global=>ty_patterns
      RAISING
        zcx_adcoset_static_error.

    CLASS-METHODS parse_sequences
      IMPORTING
        ctrl_sequence TYPE string
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_control_flags
      RAISING
        zcx_adcoset_static_error.
ENDCLASS.


CLASS zcl_adcoset_pattern_util IMPLEMENTATION.
  METHOD class_constructor.
    ctrl_flag_to_sequence_map = VALUE #( ( control_sequence = c_pattern_ctrl_sequence-boundary_start
                                           flag             = c_pattern_ctrl_flag-boundary_start )
                                         ( control_sequence = c_pattern_ctrl_sequence-boundary_end
                                           flag             = c_pattern_ctrl_flag-boundary_end )
                                         ( control_sequence = c_pattern_ctrl_sequence-match
                                           flag             = c_pattern_ctrl_flag-match )
                                         ( control_sequence = c_pattern_ctrl_sequence-match_start
                                           flag             = c_pattern_ctrl_flag-match_start )
                                         ( control_sequence = c_pattern_ctrl_sequence-match_end
                                           flag             = c_pattern_ctrl_flag-match_end )
                                         ( control_sequence = c_pattern_ctrl_sequence-exclude
                                           flag             = c_pattern_ctrl_flag-exclude ) ).

    possible_ctrl_seq_range = VALUE #(
        sign   = 'I'
        option = 'EQ'
        ( low = c_pattern_ctrl_flag-match_start BIT-OR c_pattern_ctrl_flag-boundary_start )
        ( low = c_pattern_ctrl_flag-match_end BIT-OR c_pattern_ctrl_flag-boundary_end ) ).

    ctrl_sequences_regex = |{ c_pattern_ctrl_sequence-boundary_start }\|| &&
                           |{ c_pattern_ctrl_sequence-boundary_end }\|| &&
                           |{ c_pattern_ctrl_sequence-match }\|| &&
                           |{ c_pattern_ctrl_sequence-match_start }\|| &&
                           |{ c_pattern_ctrl_sequence-match_end }\|| &&
                           c_pattern_ctrl_sequence-exclude.
    ctrl_sequences_regex = replace( val = ctrl_sequences_regex sub = '(' with = '\(' occ = 0 ).
    ctrl_sequences_regex = replace( val = ctrl_sequences_regex sub = ')' with = '\)' occ = 0 ).
    any_ctrl_sequence_regex = |({ ctrl_sequences_regex })|.
    ctrl_sequences_regex = |^({ ctrl_sequences_regex })+|.
  ENDMETHOD.

  METHOD parse_pattern_sequence.
    DATA(parsed_patterns) = parse_patterns( patterns ).
    validate_sequence( CHANGING patterns = parsed_patterns ).

    result = parsed_patterns.
  ENDMETHOD.

  METHOD parse_patterns.
    DATA is_sequence_found TYPE abap_bool.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(patterns_count) = lines( patterns ).

    LOOP AT patterns INTO DATA(pattern).
      parse_pattern( CHANGING pattern = pattern ).

      result = VALUE #( BASE result ( pattern ) ).

      IF is_sequence_found = abap_false AND pattern-flags IS NOT INITIAL.
        is_sequence_found = abap_true.
      ENDIF.
    ENDLOOP.

    IF is_sequence_found = abap_false.
      RETURN.
    ENDIF.

    validate_sequence( CHANGING patterns = result ).
  ENDMETHOD.

  METHOD parse_pattern.
    FIND FIRST OCCURRENCE OF REGEX ctrl_sequences_regex IN pattern-content
         RESPECTING CASE
         RESULTS DATA(grouped_match).
    IF sy-subrc <> 0.
      " check if pattern begins with unrecognized control sequence
      FIND FIRST OCCURRENCE OF REGEX '^(\(#[^\s)]+\))+' IN pattern-content
           RESPECTING CASE
           RESULTS grouped_match.
      IF sy-subrc = 0.
        RAISE EXCEPTION TYPE zcx_adcoset_static_error
          EXPORTING text = |Invalid control seqeuence in pattern '{ pattern-content }' detected|.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(ctrl_sequence) = substring( val = pattern-content len = grouped_match-length ).
    pattern-content = substring( val = pattern-content off = grouped_match-length ).

    DATA(ctrl_sequence_count) = count( val = ctrl_sequence regex = any_ctrl_sequence_regex case = abap_true ).
    IF ctrl_sequence_count > 1.
      pattern-flags = parse_sequences( ctrl_sequence ).
    ELSE.
      pattern-flags = ctrl_flag_to_sequence_map[ control_sequence = ctrl_sequence ]-flag.
    ENDIF.

    IF pattern-content IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING text = |Control sequence { ctrl_sequence } without a pattern is not possible|.
    ENDIF.
  ENDMETHOD.

  METHOD validate_sequence.
    DATA(excludes_count) = 0.
    DATA(boundary_start_index) = 0.
    DATA(match_start_index) = 0.
    DATA(match_found) = abap_false.
    DATA(single_match_seq_found) = abap_false.

    LOOP AT patterns ASSIGNING FIELD-SYMBOL(<pattern>) WHERE flags IS NOT INITIAL.
      IF <pattern>-flags BIT-AND c_pattern_ctrl_flag-boundary_start =
         c_pattern_ctrl_flag-boundary_start.
        IF boundary_start_index > 0.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING text = |Previous boundary sequence not closed with '{
                        c_pattern_ctrl_sequence-boundary_end }'|.
        ENDIF.
        boundary_start_index = sy-tabix.
      ELSEIF <pattern>-flags BIT-AND c_pattern_ctrl_flag-exclude =
             c_pattern_ctrl_flag-exclude.
        excludes_count = excludes_count + 1.
      ELSEIF <pattern>-flags BIT-AND c_pattern_ctrl_flag-boundary_end =
             c_pattern_ctrl_flag-boundary_end.
        IF boundary_start_index = 0.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING text = |No boundary sequence started with '{
                        c_pattern_ctrl_sequence-boundary_start }'|.
        ENDIF.
        " reset for next boundary
        boundary_start_index = 0.
      ELSEIF <pattern>-flags BIT-AND c_pattern_ctrl_flag-match =
             c_pattern_ctrl_flag-match.
        IF match_found = abap_true.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              text = |{ c_pattern_ctrl_sequence-match_start }/{ c_pattern_ctrl_sequence-match_end } and { c_pattern_ctrl_sequence-match } | &&
                     |are exclusive and can not occur in one pattern sequence|.
        ELSEIF single_match_seq_found = abap_true.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              text = |The sequence '{ c_pattern_ctrl_sequence-match }' can only occur one time in a pattern sequence|.
        ENDIF.
        single_match_seq_found = abap_true.
      ENDIF.

      IF <pattern>-flags BIT-AND c_pattern_ctrl_flag-match_start =
         c_pattern_ctrl_flag-match_start.
        IF match_found = abap_true.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING text = |The match start/end sequence can only occur one time in a pattern sequence|.
        ELSEIF single_match_seq_found = abap_true.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              text = |{ c_pattern_ctrl_sequence-match } and { c_pattern_ctrl_sequence-match_start }/{ c_pattern_ctrl_sequence-match_end } | &&
                     |are exclusive and can not occur in one pattern sequence|.
        ENDIF.
        match_start_index = sy-tabix.
        match_found = abap_true.
      ELSEIF <pattern>-flags BIT-AND c_pattern_ctrl_flag-match_end =
             c_pattern_ctrl_flag-match_end.
        IF match_found = abap_false.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING text = |No match sequence started with '{
                        c_pattern_ctrl_sequence-match_start }'|.
        ENDIF.
        match_start_index = 0.
      ENDIF.
    ENDLOOP.

    IF match_found = abap_true AND match_start_index > 0.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING text = |Match sequence not closed with '{
                    c_pattern_ctrl_sequence-match_end }'|.
    ENDIF.

    IF lines( patterns ) = excludes_count.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING text = |The sequence can not contain only excludes|.
    ENDIF.

    IF boundary_start_index > 0.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING text = |Boundary sequence not closed with '{
                    c_pattern_ctrl_sequence-boundary_end }'|.
    ENDIF.
  ENDMETHOD.

  METHOD parse_sequences.
    DATA(rest_sequence) = ctrl_sequence.

    WHILE rest_sequence IS NOT INITIAL.
      DATA(closing_bracket) = find( val = rest_sequence sub = ')' ) + 1.
      DATA(sequence) = rest_sequence(closing_bracket).
      result = result BIT-OR ctrl_flag_to_sequence_map[ control_sequence = sequence ]-flag.
      rest_sequence = rest_sequence+closing_bracket.
    ENDWHILE.

    IF result NOT IN possible_ctrl_seq_range.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING text = |Invalid control sequence combination in { ctrl_sequence }|.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
