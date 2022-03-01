"! <p class="shorttext synchronized" lang="en">Finds matches in a string for a given (RegExp) pattern</p>
CLASS zcl_adcoset_substring_matcher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_pattern_matcher.

    METHODS:
      constructor
        IMPORTING
          pattern     TYPE zif_adcoset_ty_global=>ty_pattern OPTIONAL
          ignore_case TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES control_flags FOR zif_adcoset_pattern_matcher~control_flags.

    DATA:
      pattern     TYPE string,
      ignore_case TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_substring_matcher IMPLEMENTATION.

  METHOD constructor.
    me->pattern = pattern-content.
    me->control_flags = pattern-flags.
    me->ignore_case = ignore_case.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~find_matches.
    IF ignore_case = abap_true.
      FIND ALL OCCURRENCES OF pattern IN TABLE source IGNORING CASE RESULTS result.
    ELSE.
      FIND ALL OCCURRENCES OF pattern IN TABLE source RESPECTING CASE RESULTS result.
    ENDIF.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~find_matches_in_range.
    IF ignore_case = abap_true.
      FIND ALL OCCURRENCES OF pattern IN TABLE source
        FROM start_line OFFSET offset
        TO   end_line   OFFSET end_offset
        IGNORING CASE RESULTS result.
    ELSE.
      FIND ALL OCCURRENCES OF pattern IN TABLE source
        FROM start_line OFFSET offset
        TO   end_line   OFFSET end_offset
        RESPECTING CASE RESULTS result.
    ENDIF.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~find_next_match.
    DATA(l_start_line) = COND #( WHEN start_line IS INITIAL THEN 1 ELSE start_line ).

    IF ignore_case = abap_true.
      IF end_line IS NOT INITIAL AND end_offset IS NOT INITIAL.
        FIND FIRST OCCURRENCE OF pattern IN TABLE source
          FROM l_start_line OFFSET offset
          TO   end_line     OFFSET end_offset
          IGNORING CASE
          RESULTS result.
      ELSE.
        FIND FIRST OCCURRENCE OF pattern IN TABLE source
          FROM l_start_line OFFSET offset
          IGNORING CASE
          RESULTS result.
      ENDIF.
    ELSE.
      IF end_line IS NOT INITIAL AND end_offset IS NOT INITIAL.
        FIND FIRST OCCURRENCE OF pattern IN TABLE source
          FROM l_start_line OFFSET offset
          TO   end_line     OFFSET end_offset
          RESPECTING CASE
          RESULTS result.
      ELSE.
        FIND FIRST OCCURRENCE OF pattern IN TABLE source
          FROM l_start_line OFFSET offset
          RESPECTING CASE
          RESULTS result.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
