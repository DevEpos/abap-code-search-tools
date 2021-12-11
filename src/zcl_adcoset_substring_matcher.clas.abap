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
          pattern     TYPE string
          ignore_case TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      pattern     TYPE string,
      ignore_case TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_substring_matcher IMPLEMENTATION.


  METHOD constructor.
    me->pattern = pattern.
    me->ignore_case = ignore_case.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~find_matches.
    IF ignore_case = abap_true.
      FIND ALL OCCURRENCES OF pattern IN TABLE source IGNORING CASE RESULTS result.
    ELSE.
      FIND ALL OCCURRENCES OF pattern IN TABLE source RESPECTING CASE RESULTS result.
    ENDIF.
  ENDMETHOD.


ENDCLASS.
