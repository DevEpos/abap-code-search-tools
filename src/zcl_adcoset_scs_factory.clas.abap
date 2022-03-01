"! <p class="shorttext synchronized" lang="en">Factory for creating source code searcher</p>
CLASS zcl_adcoset_scs_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Creates source code searcher</p>
      create
        IMPORTING
          matchers      TYPE zif_adcoset_pattern_matcher=>ty_ref_tab
          settings      TYPE zif_adcoset_ty_global=>ty_search_settings
        RETURNING
          VALUE(result) TYPE REF TO zif_adcoset_src_code_searcher.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_scs_factory IMPLEMENTATION.

  METHOD create.
    IF settings-sequential_matching = abap_true AND lines( matchers ) > 1.

      DATA(is_ctrl_flags_found) = abap_false.
      IF lines( matchers ) > 2.

        LOOP AT matchers INTO DATA(matcher).
          IF matcher->control_flags IS NOT INITIAL.
            is_ctrl_flags_found = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.

      ENDIF.

      IF is_ctrl_flags_found = abap_true.
        result = NEW zcl_adcoset_scs_sequ_extended(
          ignore_comment_lines = settings-ignore_comment_lines
          matchers             = matchers ).
      ELSE.
        result = NEW zcl_adcoset_scs_sequential(
          ignore_comment_lines = settings-ignore_comment_lines
          matchers             = matchers ).
      ENDIF.

    ELSE.
      result = NEW zcl_adcoset_scs_standard(
        match_all            = settings-match_all_patterns
        ignore_comment_lines = settings-ignore_comment_lines
        matchers             = matchers ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
