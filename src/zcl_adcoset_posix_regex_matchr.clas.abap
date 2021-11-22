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
          pattern TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      pattern TYPE string.
ENDCLASS.



CLASS zcl_adcoset_posix_regex_matchr IMPLEMENTATION.


  METHOD constructor.
    me->pattern = pattern.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~get_matches.

  ENDMETHOD.


ENDCLASS.
