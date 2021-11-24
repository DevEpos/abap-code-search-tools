"! <p class="shorttext synchronized" lang="en">Matcher for PCRE</p>
CLASS zcl_adcoset_pcre_matcher DEFINITION
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
      regex       TYPE REF TO cl_abap_regex,
      pattern     TYPE string,
      ignore_case TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_pcre_matcher IMPLEMENTATION.


  METHOD constructor.
    me->pattern = pattern.
    me->ignore_case = ignore_case.

    CALL METHOD cl_abap_regex=>('CREATE_PCRE')
      EXPORTING
        ignore_case      = ignore_case
*       enable_jit       = abap_true
        enable_multiline = abap_false
*       no_submatches    = abap_false
*       newline_mode     = cl_abap_regex=>('C_NEWLINE_MODE-CRLFANY')
*       unicode_handling = cl_abap_regex=>('C_UNICODE_HANDLING-STRICT')
*       extended         = abap_true
*       dot_all          = abap_false
      RECEIVING
        regex            = regex.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~get_matches.
    ASSERT ( text IS NOT INITIAL AND table IS INITIAL ) OR
           ( text IS INITIAL AND table IS NOT INITIAL ).

    TRY.
        IF text IS NOT INITIAL.
          result = regex->create_matcher( text = text )->find_all( ).
        ELSE.
          result = regex->create_matcher( table = table )->find_all( ).
        ENDIF.
      CATCH cx_sy_matcher ##NO_HANDLER.
        " should not happen. The regex exceptions will be handled in the constructor
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
