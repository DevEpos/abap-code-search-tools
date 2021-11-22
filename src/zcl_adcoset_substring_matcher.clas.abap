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
          pattern TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      pattern TYPE string.
ENDCLASS.



CLASS zcl_adcoset_substring_matcher IMPLEMENTATION.


  METHOD constructor.
    me->pattern = pattern.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~get_matches.

  ENDMETHOD.


ENDCLASS.
