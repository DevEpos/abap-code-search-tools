"! <p class="shorttext synchronized">Matcher for PCRE</p>
CLASS zcl_adcoset_pcre_matcher DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_pattern_matcher.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING
        !pattern    TYPE zif_adcoset_ty_global=>ty_pattern
        ignore_case TYPE abap_bool
        settings    TYPE zif_adcoset_ty_global=>ty_pcre_regex_settings OPTIONAL
      RAISING
        cx_sy_regex.

  PRIVATE SECTION.
    ALIASES control_flags FOR zif_adcoset_pattern_matcher~control_flags.

    CONSTANTS:
      BEGIN OF c_pcre_options,
        single_line_on TYPE string VALUE '(?s)',
        extended_off   TYPE string VALUE '(?-x)',
      END OF c_pcre_options.

    CLASS-DATA matcher_classname TYPE string.

    DATA regex TYPE REF TO cl_abap_regex.
    DATA pattern TYPE string.
    DATA pattern_for_seq_matching TYPE string.
    DATA ignore_case TYPE abap_bool.

    "! Generates a subroutine pool for finding matches in a table of strings. <br/><br/>
    "!
    "! The program is needed as the PCRE option is not available on all NW stacks and <br/>
    "! offset's cannot be passed to the SAP API for RegEx processing
    METHODS generate_matcher_program
      RETURNING
        VALUE(result) TYPE progname.
ENDCLASS.


CLASS zcl_adcoset_pcre_matcher IMPLEMENTATION.
  METHOD class_constructor.
  ENDMETHOD.

  METHOD constructor.
    me->pattern     = pattern-content.
    me->ignore_case = ignore_case.
    control_flags = pattern-flags.

    IF zcl_adcoset_pcre_util=>is_dot_all_param_existing( ).
      CALL METHOD cl_abap_regex=>('CREATE_PCRE')
        EXPORTING pattern     = pattern-content
                  ignore_case = ignore_case
                  extended    = xsdbool( settings-extended_mode_disabled = abap_false )
                  dot_all     = xsdbool( settings-single_line_mode_enabled = abap_true )
        RECEIVING regex       = regex.
    ELSE.
      DATA(l_pattern) = pattern-content.

      " if for some reason the meta string for single line mode was already
      " prefixed to the pattern it will not be removed and single line mode will
      " still be active
      IF settings-single_line_mode_enabled = abap_true.
        IF find( val = l_pattern
                 sub = c_pcre_options-single_line_on ) <= 0.
          l_pattern = c_pcre_options-single_line_on && l_pattern.
        ENDIF.
      ENDIF.

      CALL METHOD cl_abap_regex=>('CREATE_PCRE')
        EXPORTING pattern     = l_pattern
                  ignore_case = ignore_case
                  extended    = xsdbool( settings-extended_mode_disabled = abap_false )
        RECEIVING regex       = regex.
    ENDIF.

    " create pattern for sequential matching
    pattern_for_seq_matching = pattern-content.
    IF settings-extended_mode_disabled = abap_true.
      pattern_for_seq_matching = c_pcre_options-extended_off && pattern_for_seq_matching.
    ENDIF.

    IF settings-single_line_mode_enabled = abap_true.
      pattern_for_seq_matching = c_pcre_options-single_line_on && pattern_for_seq_matching.
    ENDIF.
  ENDMETHOD.

  METHOD zif_adcoset_pattern_matcher~find_matches.
    TRY.
        result = regex->create_matcher( table = source )->find_all( ).
      CATCH cx_sy_matcher ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_adcoset_pattern_matcher~find_matches_in_range.
    IF matcher_classname IS INITIAL.
      matcher_classname = |\\PROGRAM={ generate_matcher_program( ) }\\CLASS=LCL_PCRE_MATCHER|.
    ENDIF.

    CALL METHOD (matcher_classname)=>('FIND_MATCHES_IN_RANGE')
      EXPORTING source      = source
                pattern     = pattern_for_seq_matching
                ignore_case = ignore_case
                start_line  = start_line
                offset      = offset
                end_line    = end_line
                end_offset  = end_offset
      RECEIVING result      = result.
  ENDMETHOD.

  METHOD zif_adcoset_pattern_matcher~find_next_match.
    IF matcher_classname IS INITIAL.
      matcher_classname = |\\PROGRAM={ generate_matcher_program( ) }\\CLASS=LCL_PCRE_MATCHER|.
    ENDIF.

    CALL METHOD (matcher_classname)=>('FIND_NEXT_MATCH')
      EXPORTING source      = source
                pattern     = pattern_for_seq_matching
                ignore_case = ignore_case
                start_line  = start_line
                offset      = offset
                end_line    = end_line
                end_offset  = end_offset
      RECEIVING result      = result.
  ENDMETHOD.

  METHOD generate_matcher_program.
    DATA source_lines TYPE TABLE OF string.

    source_lines = VALUE #( ( |REPORT zpcre_matcher.| )
                            ( |CLASS lcl_pcre_matcher DEFINITION CREATE PRIVATE.| )
                            ( |  PUBLIC SECTION.| )
                            ( |    CLASS-METHODS find_matches_in_range| )
                            ( |      IMPORTING| )
                            ( |        source        TYPE string_table| )
                            ( |        pattern       TYPE string| )
                            ( |        ignore_case   TYPE abap_bool| )
                            ( |        start_line    TYPE i| )
                            ( |        offset        TYPE i| )
                            ( |        end_line      TYPE i| )
                            ( |        end_offset    TYPE i| )
                            ( |      RETURNING| )
                            ( |        VALUE(result) TYPE match_result_tab.| )
                            ( || )
                            ( |    CLASS-METHODS find_next_match| )
                            ( |      IMPORTING| )
                            ( |        source        TYPE string_table| )
                            ( |        pattern       TYPE string| )
                            ( |        ignore_case   TYPE abap_bool| )
                            ( |        start_line    TYPE i OPTIONAL| )
                            ( |        offset        TYPE i OPTIONAL| )
                            ( |        end_line      TYPE i OPTIONAL| )
                            ( |        end_offset    TYPE i OPTIONAL| )
                            ( |      RETURNING| )
                            ( |        VALUE(result) TYPE match_result.| )
                            ( |ENDCLASS.| )
                            ( || )
                            ( |CLASS lcl_pcre_matcher IMPLEMENTATION.| )
                            ( || )
                            ( |  METHOD find_matches_in_range.| )
                            ( |    IF ignore_case = abap_true.| )
                            ( |      FIND ALL OCCURRENCES OF PCRE pattern IN TABLE source| )
                            ( |        FROM start_line OFFSET offset| )
                            ( |        TO   end_line   OFFSET end_offset| )
                            ( |        IGNORING CASE RESULTS result.| )
                            ( |    ELSE.| )
                            ( |      FIND ALL OCCURRENCES OF PCRE pattern IN TABLE source| )
                            ( |        FROM start_line OFFSET offset| )
                            ( |        TO   end_line   OFFSET end_offset| )
                            ( |        RESPECTING CASE RESULTS result.| )
                            ( |    ENDIF.| )
                            ( |  ENDMETHOD.| )
                            ( || )
                            ( |  METHOD find_next_match.| )
                            ( |    DATA(l_start_line) = COND #( WHEN start_line IS INITIAL THEN 1 ELSE start_line ).| )
                            ( |    IF ignore_case = abap_true.| )
                            ( |      IF end_line IS NOT INITIAL and end_offset IS NOT INITIAL.| )
                            ( |        FIND FIRST OCCURRENCE OF PCRE pattern IN TABLE source| )
                            ( |          FROM l_start_line OFFSET offset| )
                            ( |          TO   end_line     OFFSET end_offset| )
                            ( |          IGNORING CASE| )
                            ( |          RESULTS result.| )
                            ( |      ELSE.| )
                            ( |        FIND FIRST OCCURRENCE OF PCRE pattern IN TABLE source| )
                            ( |          FROM l_start_line OFFSET offset| )
                            ( |          IGNORING CASE| )
                            ( |          RESULTS result.| )
                            ( |      ENDIF.| )
                            ( |    ELSE.| )
                            ( |      IF end_line IS NOT INITIAL and end_offset IS NOT INITIAL.| )
                            ( |        FIND FIRST OCCURRENCE OF PCRE pattern IN TABLE source| )
                            ( |          FROM l_start_line OFFSET offset| )
                            ( |          TO   end_line     OFFSET end_offset| )
                            ( |          RESPECTING CASE| )
                            ( |          RESULTS result.| )
                            ( |      ELSE.| )
                            ( |        FIND FIRST OCCURRENCE OF PCRE pattern IN TABLE source| )
                            ( |          FROM l_start_line OFFSET offset| )
                            ( |          RESPECTING CASE| )
                            ( |          RESULTS result.| )
                            ( |      ENDIF.| )
                            ( |    ENDIF.| )
                            ( |  ENDMETHOD.| )
                            ( || )
                            ( |ENDCLASS.| ) ).

    GENERATE SUBROUTINE POOL source_lines
             NAME result
             " TODO: variable is assigned but never used (ABAP cleaner)
             MESSAGE DATA(message).
  ENDMETHOD.
ENDCLASS.
