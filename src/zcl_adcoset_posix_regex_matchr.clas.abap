"! <p class="shorttext synchronized" lang="en">Matcher for POSIX standard regular expressions</p>
CLASS zcl_adcoset_posix_regex_matchr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_pattern_matcher.

    METHODS:
      constructor
        IMPORTING
          pattern     TYPE string
          ignore_case TYPE abap_bool
        RAISING
          cx_sy_regex.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      pattern     TYPE string,
      ignore_case TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_posix_regex_matchr IMPLEMENTATION.


  METHOD constructor.
    me->pattern = pattern.
    me->ignore_case = ignore_case.
    " will only be used to verify the regex pattern is valid
    NEW cl_abap_regex(
      pattern     = pattern
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
          EXPORTING
            previous = regex_error.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~find_next_match.
    DATA(l_start_line) = COND #( WHEN start_line IS INITIAL THEN 1 ELSE start_line ).

    TRY.
        IF ignore_case = abap_true.
          FIND FIRST OCCURRENCE OF REGEX pattern IN TABLE source
            FROM l_start_line OFFSET offset
            IGNORING CASE
            RESULTS result.
        ELSE.
          FIND FIRST OCCURRENCE OF REGEX pattern IN TABLE source
            FROM l_start_line OFFSET offset
            RESPECTING CASE
            RESULTS result.
        ENDIF.
      CATCH cx_sy_regex_too_complex INTO DATA(regex_error).
        RAISE EXCEPTION TYPE zcx_adcoset_pattern_sh_error
          EXPORTING
            previous = regex_error.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

