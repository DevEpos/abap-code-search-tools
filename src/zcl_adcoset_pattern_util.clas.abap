"! <p class="shorttext synchronized" lang="en">Utility for handling patterns</p>
CLASS zcl_adcoset_pattern_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_c_pattern_matching.
    CLASS-METHODS:
      class_constructor,
      "! <p class="shorttext synchronized" lang="en">Checks control flags in pattern sequence</p>
      parse_pattern_sequence
        IMPORTING
          patterns      TYPE zif_adcoset_ty_global=>ty_patterns
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_patterns
        RAISING
          zcx_adcoset_static_error.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES:
      c_pattern_ctrl_flag     FOR zif_adcoset_c_pattern_matching~c_pattern_ctrl_flag,
      c_pattern_ctrl_sequence FOR zif_adcoset_c_pattern_matching~c_pattern_ctrl_sequence.

    TYPES:
      BEGIN OF ty_flag_to_sequence,
        control_sequence TYPE string,
        flag             TYPE zif_adcoset_ty_global=>ty_control_flags,
      END OF ty_flag_to_sequence.

    CLASS-DATA:
      ctrl_sequences_regex      TYPE string,
      any_ctrl_sequence_regex   TYPE string,
      ctrl_flag_to_sequence_map TYPE HASHED TABLE OF ty_flag_to_sequence WITH UNIQUE KEY control_sequence.

    CLASS-METHODS:
      parse_pattern
        CHANGING
          pattern TYPE zif_adcoset_ty_global=>ty_pattern
        RAISING
          zcx_adcoset_static_error,
      parse_patterns
        IMPORTING
          patterns      TYPE zif_adcoset_ty_global=>ty_patterns
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_patterns
        RAISING
          zcx_adcoset_static_error,
      validate_sequence
        CHANGING
          patterns TYPE zif_adcoset_ty_global=>ty_patterns
        RAISING
          zcx_adcoset_static_error.
ENDCLASS.



CLASS zcl_adcoset_pattern_util IMPLEMENTATION.

  METHOD class_constructor.
    ctrl_flag_to_sequence_map = VALUE #(
      ( control_sequence = c_pattern_ctrl_sequence-boundary_start
        flag             = c_pattern_ctrl_flag-boundary_start )
      ( control_sequence = c_pattern_ctrl_sequence-boundary_end
        flag             = c_pattern_ctrl_flag-boundary_end )
      ( control_sequence = c_pattern_ctrl_sequence-match_start
        flag             = c_pattern_ctrl_flag-match_start )
      ( control_sequence = c_pattern_ctrl_sequence-match_end
        flag             = c_pattern_ctrl_flag-match_end )
      ( control_sequence = c_pattern_ctrl_sequence-exclude
        flag             = c_pattern_ctrl_flag-exclude ) ).


    ctrl_sequences_regex = c_pattern_ctrl_sequence-boundary_start && `|` &&
                           c_pattern_ctrl_sequence-boundary_end && `|` &&
                           c_pattern_ctrl_sequence-match_start && `|` &&
                           c_pattern_ctrl_sequence-match_end && `|` &&
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

    DATA: parsed_patterns    LIKE result,
          one_sequence_found TYPE abap_bool.

    DATA(patterns_count) = lines( patterns ).

    LOOP AT patterns INTO DATA(pattern).
      parse_pattern( CHANGING pattern = pattern ).

      " exclusion sequence must not occur at start or end of sequence
      IF ( sy-tabix = 1 OR sy-tabix = patterns_count ) AND
          pattern-flags BIT-AND c_pattern_ctrl_flag-exclude = c_pattern_ctrl_flag-exclude.
        RAISE EXCEPTION TYPE zcx_adcoset_static_error
          EXPORTING
            text = |The control seqeuence { c_pattern_ctrl_sequence-exclude } must not occur in the| &&
                   | { COND string( WHEN sy-tabix = 1 THEN 'first' ELSE 'last' ) } pattern|.

      ENDIF.
      result = VALUE #( BASE result ( pattern ) ).

      IF one_sequence_found = abap_false AND pattern-flags IS NOT INITIAL.
        one_sequence_found = abap_true.
      ENDIF.
    ENDLOOP.

    IF one_sequence_found = abap_false.
      RETURN.
    ENDIF.

    validate_sequence( CHANGING patterns = result ).

  ENDMETHOD.


  METHOD parse_pattern.

    FIND FIRST OCCURRENCE OF REGEX ctrl_sequences_regex IN pattern-content
      RESPECTING CASE
      RESULTS DATA(grouped_match).
    IF sy-subrc <> 0 OR grouped_match IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ctrl_sequence) = substring( val = pattern-content len = grouped_match-length ).
    DATA(pattern_without_ctrl_seq) = substring( val = pattern-content off = grouped_match-length ).


    DATA(ctrl_sequence_count) = count( val = ctrl_sequence regex = any_ctrl_sequence_regex case = abap_true ).

    IF ctrl_sequence_count = 2.
      IF ctrl_sequence = c_pattern_ctrl_sequence-match_end && c_pattern_ctrl_sequence-match_start OR
          ctrl_sequence = c_pattern_ctrl_sequence-match_start && c_pattern_ctrl_sequence-match_end.
        pattern = VALUE #(
          content = pattern_without_ctrl_seq
          flags   = ctrl_flag_to_sequence_map[ control_sequence = c_pattern_ctrl_sequence-match_start ]-flag BIT-OR
                    ctrl_flag_to_sequence_map[ control_sequence = c_pattern_ctrl_sequence-match_end ]-flag ).
      ELSE.
        RAISE EXCEPTION TYPE zcx_adcoset_static_error
          EXPORTING
            text = |More than one control sequence at pattern '{ pattern-content }'|.
      ENDIF.
    ELSEIF ctrl_sequence_count > 1.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING
          text = |More than one control sequence at pattern '{ pattern-content }'|.
    ELSE.
      pattern = VALUE #(
        content = pattern_without_ctrl_seq
        flags   = ctrl_flag_to_sequence_map[ control_sequence = ctrl_sequence ]-flag ).
    ENDIF.

    IF pattern_without_ctrl_seq IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING
          text = |Control sequence { ctrl_sequence } without a pattern is invalid|.
    ENDIF.

  ENDMETHOD.


  METHOD validate_sequence.
    DATA(match_start_index) = 0.
    DATA(boundary_start_index) = 0.
    DATA(match_found) = abap_false.

    LOOP AT patterns ASSIGNING FIELD-SYMBOL(<pattern>) WHERE flags IS NOT INITIAL.
      IF <pattern>-flags BIT-AND c_pattern_ctrl_flag-boundary_start =
          c_pattern_ctrl_flag-boundary_start.
        IF boundary_start_index > 0.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              text = |Previous boundary sequence not closed with '{
                c_pattern_ctrl_sequence-boundary_end }'|.
        ENDIF.
        boundary_start_index = sy-tabix.
      ELSEIF <pattern>-flags BIT-AND c_pattern_ctrl_flag-boundary_end =
          c_pattern_ctrl_flag-boundary_end.
        IF boundary_start_index = 0.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              text = |No boundary sequence started with '{
                c_pattern_ctrl_sequence-boundary_start }'|.
        ENDIF.
        " reset for next boundary
        boundary_start_index = 0.
      ELSEIF <pattern>-flags BIT-AND c_pattern_ctrl_flag-match_start =
          c_pattern_ctrl_flag-match_start.
        IF match_start_index > 0.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              text = |Match sequence not closed with '{
                c_pattern_ctrl_sequence-match_end }'|.
        ELSEIF match_found = abap_true.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              text = |The match sequence can only occur one time in a pattern sequence|.
        ENDIF.

        match_found = abap_true.

        " Match can occur in a single pattern
        IF <pattern>-flags BIT-AND c_pattern_ctrl_flag-match_end =
          c_pattern_ctrl_flag-match_end.
        ELSE.
          match_start_index = sy-tabix.
        ENDIF.
      ELSEIF <pattern>-flags BIT-AND c_pattern_ctrl_flag-match_end =
                c_pattern_ctrl_flag-match_end.
        IF match_start_index = 0.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error
            EXPORTING
              text = |No match sequence started with '{
                c_pattern_ctrl_sequence-match_start }'|.
        ENDIF.

        match_start_index = 0.
      ENDIF.
    ENDLOOP.

    IF match_found = abap_false AND match_start_index > 0.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING
          text = |Match sequence not closed with '{
            c_pattern_ctrl_sequence-match_end }'|.
    ENDIF.

    IF boundary_start_index > 0.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING
          text = |Boundary sequence not closed with '{
            c_pattern_ctrl_sequence-boundary_end }'|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
