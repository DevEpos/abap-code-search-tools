"! <p class="shorttext synchronized" lang="en">Standard Source Code Searcher</p>
CLASS zcl_adcoset_scs_standard DEFINITION
  PUBLIC
  INHERITING FROM zcl_adcoset_scs_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_src_code_searcher.

    METHODS:
      constructor
        IMPORTING
          match_all            TYPE abap_bool
          ignore_comment_lines TYPE abap_bool
          matchers             TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      match_all TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_scs_standard IMPLEMENTATION.

  METHOD constructor.
    super->constructor(
      ignore_comment_lines = ignore_comment_lines
      matchers             = matchers ).
    me->match_all = match_all.
  ENDMETHOD.


  METHOD zif_adcoset_src_code_searcher~search.
    " set the source code attribute for this search call
    me->source_code = source_code.

    LOOP AT matchers INTO DATA(matcher).
      TRY.
          DATA(raw_matches) = matcher->find_matches( source_code->content ).
        CATCH zcx_adcoset_pattern_sh_error INTO DATA(error).
          zcl_adcoset_log=>add_exception( error ).
          IF match_all = abap_true.
            CLEAR result.
            RETURN.
          ENDIF.
      ENDTRY.
      IF raw_matches IS NOT INITIAL.
        DATA(enhanced_matches) = enhance_matches(
          raw_matches          = raw_matches
          ignore_comment_lines = ignore_comment_lines ).
      ENDIF.

      " not all matchers found a match so quit further searching
      IF enhanced_matches IS NOT INITIAL.
        result = VALUE #( BASE result ( LINES OF enhanced_matches ) ).
      ELSEIF match_all = abap_true.
        CLEAR result.
        RETURN.
      ENDIF.

      CLEAR:
        raw_matches,
        enhanced_matches.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
