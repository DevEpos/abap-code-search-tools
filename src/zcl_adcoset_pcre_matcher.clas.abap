"! <p class="shorttext synchronized" lang="en">Matcher for PCRE</p>
CLASS zcl_adcoset_pcre_matcher DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_pattern_matcher.

    CLASS-METHODS:
      class_constructor.

    METHODS:
      constructor
        IMPORTING
          pattern     TYPE string
          ignore_case TYPE abap_bool
          settings    TYPE zif_adcoset_ty_global=>ty_pcre_regex_settings OPTIONAL
        RAISING
          cx_sy_regex.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_single_mode_option TYPE string VALUE '(?s)'.

    DATA:
      regex       TYPE REF TO cl_abap_regex,
      pattern     TYPE string,
      ignore_case TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_pcre_matcher IMPLEMENTATION.

  METHOD class_constructor.
  ENDMETHOD.


  METHOD constructor.
    me->pattern = pattern.
    me->ignore_case = ignore_case.

    IF zcl_adcoset_pcre_util=>is_dot_all_param_existing( ).
      CALL METHOD cl_abap_regex=>('CREATE_PCRE')
        EXPORTING
          pattern     = pattern
          ignore_case = ignore_case
          extended    = xsdbool( settings-extended_mode_disabled = abap_false )
          dot_all     = xsdbool( settings-single_line_mode_enabled = abap_true )
        RECEIVING
          regex       = regex.
    ELSE.
      DATA(l_pattern) = pattern.

      " if for some reason the meta string for single line mode was already
      " prefixed to the pattern it will not be removed and single line mode will
      " still be active
      IF settings-single_line_mode_enabled = abap_true.
        IF find( val = l_pattern sub = c_single_mode_option ) <= 0.
          l_pattern = c_single_mode_option && l_pattern.
        ENDIF.
      ENDIF.

      CALL METHOD cl_abap_regex=>('CREATE_PCRE')
        EXPORTING
          pattern     = l_pattern
          ignore_case = ignore_case
          extended    = xsdbool( settings-extended_mode_disabled = abap_false )
        RECEIVING
          regex       = regex.
    ENDIF.
  ENDMETHOD.


  METHOD zif_adcoset_pattern_matcher~find_matches.
    TRY.
        result = regex->create_matcher( table = source )->find_all( ).
      CATCH cx_sy_matcher ##NO_HANDLER.
        " TODO: check system where PCRE is available if the same problems as in
        "       zcl_adcoset_posix_regex_matchr can occur
        " should not happen. The regex exceptions will be handled in the constructor
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
