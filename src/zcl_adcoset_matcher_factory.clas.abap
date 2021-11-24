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
          ignore_case   TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(result) TYPE REF TO zif_adcoset_pattern_matcher
        RAISING
          zcx_adcoset_no_matcher
          cx_sy_regex,

      "! <p class="shorttext synchronized" lang="en">Checks if PCRE is supported in the system</p>
      is_pcre_supported
        RETURNING
          VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      pcre_supported TYPE abap_bool VALUE abap_undefined.
ENDCLASS.



CLASS zcl_adcoset_matcher_factory IMPLEMENTATION.


  METHOD create_matcher.
    result = SWITCH #( type

      WHEN c_matcher_type-posix_regex THEN
        NEW zcl_adcoset_posix_regex_matchr(
          pattern     = pattern
          ignore_case = ignore_case )

      WHEN c_matcher_type-pcre THEN
        NEW zcl_adcoset_pcre_matcher(
          pattern     = pattern
          ignore_case = ignore_case )

      WHEN c_matcher_type-substring THEN
        NEW zcl_adcoset_substring_matcher(
          pattern     = pattern
          ignore_case = ignore_case )

      ELSE
        THROW zcx_adcoset_no_matcher( ) ).
  ENDMETHOD.

  METHOD is_pcre_supported.
    IF pcre_supported = abap_undefined.
      DATA(abap_regex_descr) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( 'CL_ABAP_REGEX' ) ).
      pcre_supported = xsdbool( line_exists( abap_regex_descr->methods[ name = 'CREATE_PCRE' ] ) ).
    ENDIF.

    result = pcre_supported.
  ENDMETHOD.

ENDCLASS.
