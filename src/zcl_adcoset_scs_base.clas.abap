"! <p class="shorttext synchronized">Base class for source code searcher</p>
CLASS zcl_adcoset_scs_base DEFINITION
  PUBLIC ABSTRACT
  CREATE PROTECTED.

  PUBLIC SECTION.

  PROTECTED SECTION.
    DATA ignore_comment_lines TYPE abap_bool.
    DATA matchers TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.
    DATA source_code TYPE REF TO zif_adcoset_source_code.

    METHODS constructor
      IMPORTING
        ignore_comment_lines TYPE abap_bool
        matchers             TYPE zif_adcoset_pattern_matcher=>ty_ref_tab.

    METHODS enhance_matches
      IMPORTING
        raw_matches          TYPE match_result_tab
        ignore_comment_lines TYPE abap_bool
      RETURNING
        VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_matches.

    METHODS get_line_index
      IMPORTING
        !offset       TYPE match_result-offset
      RETURNING
        VALUE(result) TYPE zif_adcoset_source_code=>ty_line_index.

    METHODS get_match_end_line
      IMPORTING
        start_line    TYPE zif_adcoset_source_code=>ty_line_index
        !offset       TYPE match_result-offset
        !length       TYPE match_result-length
      RETURNING
        VALUE(result) TYPE zif_adcoset_source_code=>ty_line_index.

    METHODS get_single_line_match
      IMPORTING
        raw_match            TYPE match_result
        ignore_comment_lines TYPE abap_bool
      RETURNING
        VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_match.

    METHODS get_multi_line_match
      IMPORTING
        raw_match            TYPE match_result
        ignore_comment_lines TYPE abap_bool
      RETURNING
        VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_match.

    METHODS is_comment_line
      IMPORTING
        code_line     TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS range_contains_comment_line
      IMPORTING
        start_line    TYPE zif_adcoset_source_code=>ty_line_index
        end_line      TYPE zif_adcoset_source_code=>ty_line_index
      RETURNING
        VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_adcoset_scs_base IMPLEMENTATION.
  METHOD constructor.
    me->matchers             = matchers.
    me->ignore_comment_lines = ignore_comment_lines.
  ENDMETHOD.

  METHOD enhance_matches.
    DATA enhanced_match TYPE zif_adcoset_ty_global=>ty_search_match.

    LOOP AT raw_matches ASSIGNING FIELD-SYMBOL(<raw_match>).

      IF source_code->is_single_line_content = abap_false.
        enhanced_match = get_single_line_match( raw_match            = <raw_match>
                                                ignore_comment_lines = ignore_comment_lines ).
      ELSE.
        enhanced_match = get_multi_line_match( raw_match            = <raw_match>
                                               ignore_comment_lines = ignore_comment_lines ).
      ENDIF.

      IF enhanced_match IS NOT INITIAL.
        result = VALUE #( BASE result
                          ( enhanced_match ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_single_line_match.
    DATA(snippet) = source_code->content[ raw_match-line ].

    IF     ignore_comment_lines        = abap_true
       AND source_code->comment_regex IS NOT INITIAL
       AND is_comment_line( snippet ).
      RETURN.
    ENDIF.

    result = VALUE #( start_line   = raw_match-line
                      start_column = raw_match-offset
                      end_line     = raw_match-line
                      end_column   = raw_match-offset + raw_match-length
                      snippet      = snippet ).
  ENDMETHOD.

  METHOD get_multi_line_match.
    DATA(start_line) = get_line_index( offset = raw_match-offset ).

    DATA(end_line) = get_match_end_line( start_line = start_line
                                         offset     = raw_match-offset
                                         length     = raw_match-length ).

    DATA(start_col) = raw_match-offset - start_line-offset.
    DATA(end_col) = raw_match-offset + raw_match-length - end_line-offset.

    IF     ignore_comment_lines        = abap_true
       AND source_code->comment_regex IS NOT INITIAL
       AND range_contains_comment_line( start_line = start_line
                                        end_line   = end_line ).
      RETURN.
    ENDIF.

    result = VALUE #( start_line   = start_line-number
                      start_column = start_col
                      end_line     = end_line-number
                      end_column   = end_col
                      snippet      = substring( val = source_code->content[ 1 ]
                                                off = start_line-offset
                                                len = start_line-length )
                      long_snippet = COND #(
                        WHEN end_line-number > start_line-number
                        THEN substring( val = source_code->content[ 1 ]
                                        off = start_line-offset
                                        len = raw_match-length + ( raw_match-offset - start_line-offset ) ) ) ).
  ENDMETHOD.

  METHOD get_line_index.
    LOOP AT source_code->line_indexes ASSIGNING FIELD-SYMBOL(<line_index>) WHERE offset > offset.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      result = source_code->line_indexes[ <line_index>-number - 1 ].
    ELSE.
      " offset must be in the last row
      result = source_code->line_indexes[ source_code->line_count ].
    ENDIF.
  ENDMETHOD.

  METHOD get_match_end_line.
    IF start_line-number = source_code->line_count.
      result = start_line.
    ELSE.
      DATA(match_end) = offset + length.
      DATA(next_line) = source_code->line_indexes[ start_line-number + 1 ].
      IF match_end <= next_line-offset.
        result = start_line.
      ELSE.
        result = get_line_index( offset = match_end ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD is_comment_line.
    result = xsdbool( find( val   = code_line
                            regex = source_code->comment_regex ) <> -1 ).
  ENDMETHOD.

  METHOD range_contains_comment_line.
    result =
      xsdbool( find( val   = source_code->content[ 1 ]
                     off   = start_line-offset
                     len   = end_line-offset + end_line-length - start_line-offset
                     regex = source_code->comment_regex  ) <> -1 ).
  ENDMETHOD.
ENDCLASS.
