"! <p class="shorttext synchronized" lang="en">Factory to create matchers</p>
CLASS zcl_adcoset_matcher_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: ty_matcher_type TYPE c LENGTH 1.
    CONSTANTS:
      BEGIN OF c_matcher_type,
        substring   TYPE ty_matcher_type VALUE '1',
        posix_regex TYPE ty_matcher_type VALUE '2',
        "! Perl compatible regular expression pattern
        pcre        TYPE ty_matcher_type VALUE '3',
      END OF c_matcher_type.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates matcher for the given type and pattern</p>
      create_matcher
        IMPORTING
          type          TYPE ty_matcher_type
          pattern       TYPE string
        RETURNING
          VALUE(result) TYPE REF TO zif_adcoset_pattern_matcher
        RAISING
          zcx_adcoset_no_matcher.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_matcher_factory IMPLEMENTATION.


  METHOD create_matcher.
    result = SWITCH #( type
      WHEN c_matcher_type-posix_regex THEN
        NEW zcl_adcoset_posix_regex_matchr( pattern )
      WHEN c_matcher_type-pcre THEN
        NEW zcl_adcoset_pcre_matcher( pattern )
      WHEN c_matcher_type-substring THEN
        NEW zcl_adcoset_substring_matcher( pattern )
      ELSE
        THROW zcx_adcoset_no_matcher( ) ).
  ENDMETHOD.

ENDCLASS.
