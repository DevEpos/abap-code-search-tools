"! <p class="shorttext synchronized">Matcher for POSIX standard regular expressions</p>
CLASS zcl_adcoset_posix_regex_matchr DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_pattern_matcher.

    METHODS constructor
      IMPORTING
        !pattern    TYPE zif_adcoset_ty_global=>ty_pattern
        ignore_case TYPE abap_bool
      RAISING
        cx_sy_regex.

  PRIVATE SECTION.
    ALIASES control_flags FOR zif_adcoset_pattern_matcher~control_flags.

    DATA pattern TYPE string.
    DATA ignore_case TYPE abap_bool.
ENDCLASS.


CLASS zcl_adcoset_posix_regex_matchr IMPLEMENTATION.
  METHOD constructor.
    me->pattern     = pattern-content.
    me->ignore_case = ignore_case.
    control_flags = pattern-flags.
    " will only be used to verify the regex pattern is valid
    NEW cl_abap_regex( pattern     = pattern-content
                       ignore_case = ignore_case ).
  ENDMETHOD.

  METHOD zif_adcoset_pattern_matcher~find_matches.
    " The class CL_ABAP_MATCHER cannot be used for the POSIX RegExp as they can still occur
    " exceptions inside the '_find' method that cannot be caught
    TRY.
        IF ignore_case = abap_true.
          FIND ALL OCCURRENCES OF REGEX pattern IN TABLE source IGNORING CASE RESULTS result.
        ELSE.
          FIND ALL OCCURRENCES OF REGEX pattern IN TABLE source RESPECTING CASE RESULTS result.
        ENDIF.
      CATCH cx_sy_regex_too_complex INTO DATA(regex_error).
        RAISE EXCEPTION TYPE zcx_adcoset_pattern_sh_error
          EXPORTING previous = regex_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_adcoset_pattern_matcher~find_matches_in_range.
    IF ignore_case = abap_true.
      FIND ALL OCCURRENCES OF REGEX pattern IN TABLE source
           FROM start_line OFFSET offset
           TO   end_line   OFFSET end_offset
           IGNORING CASE RESULTS result.
    ELSE.
      FIND ALL OCCURRENCES OF REGEX pattern IN TABLE source
           FROM start_line OFFSET offset
           TO   end_line   OFFSET end_offset
           RESPECTING CASE RESULTS result.
    ENDIF.
  ENDMETHOD.

  METHOD zif_adcoset_pattern_matcher~find_next_match.
    DATA(l_start_line) = COND #( WHEN start_line IS INITIAL THEN 1 ELSE start_line ).

    TRY.
        IF ignore_case = abap_true.
          IF end_line IS NOT INITIAL AND end_offset IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX pattern IN TABLE source
                 FROM l_start_line OFFSET offset
                 TO   end_line     OFFSET end_offset
                 IGNORING CASE
                 RESULTS result.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX pattern IN TABLE source
                 FROM l_start_line OFFSET offset
                 IGNORING CASE
                 RESULTS result.
          ENDIF.
        ELSE.
          IF end_line IS NOT INITIAL AND end_offset IS NOT INITIAL.
            FIND FIRST OCCURRENCE OF REGEX pattern IN TABLE source
                 FROM l_start_line OFFSET offset
                 TO   end_line     OFFSET end_offset
                 RESPECTING CASE
                 RESULTS result.
          ELSE.
            FIND FIRST OCCURRENCE OF REGEX pattern IN TABLE source
                 FROM l_start_line OFFSET offset
                 RESPECTING CASE
                 RESULTS result.
          ENDIF.
        ENDIF.
      CATCH cx_sy_regex_too_complex INTO DATA(regex_error).
        RAISE EXCEPTION TYPE zcx_adcoset_pattern_sh_error
          EXPORTING previous = regex_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

