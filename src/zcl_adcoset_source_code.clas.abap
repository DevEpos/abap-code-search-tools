"! <p class="shorttext synchronized" lang="en">Represents source code</p>
CLASS zcl_adcoset_source_code DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_adcoset_source_code.

    METHODS:
      constructor
        IMPORTING
          source        TYPE string_table
          line_indexes  TYPE zif_adcoset_source_code=>ty_line_indexes OPTIONAL
          comment_regex TYPE string OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      source         TYPE string_table,
      comment_regex  TYPE string,
      line_count     TYPE i,
      multiline_mode TYPE abap_bool,
      line_indexes   TYPE zif_adcoset_source_code=>ty_line_indexes.

    METHODS:
      enhance_matches
        IMPORTING
          raw_matches          TYPE match_result_tab
          ignore_comment_lines TYPE abap_bool
        RETURNING
          VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_matches,
      get_line_index
        IMPORTING
          offset        TYPE match_result-offset
        RETURNING
          VALUE(result) TYPE zif_adcoset_source_code=>ty_line_index,
      get_match_end_line
        IMPORTING
          start_line    TYPE zif_adcoset_source_code=>ty_line_index
          offset        TYPE match_result-offset
          length        TYPE match_result-length
        RETURNING
          VALUE(result) TYPE zif_adcoset_source_code=>ty_line_index,
      get_single_line_match
        IMPORTING
          raw_match            TYPE match_result
          ignore_comment_lines TYPE abap_bool
        RETURNING
          VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_match,
      get_multi_line_match
        IMPORTING
          raw_match            TYPE match_result
          ignore_comment_lines TYPE abap_bool
        RETURNING
          VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_match,
      is_comment_line
        IMPORTING
          code_line     TYPE string
        RETURNING
          VALUE(result) TYPE abap_bool,
      range_contains_comment_line
        IMPORTING
          start_line    TYPE zif_adcoset_source_code=>ty_line_index
          end_line      TYPE zif_adcoset_source_code=>ty_line_index
        RETURNING
          VALUE(result) TYPE abap_bool,
      find_sequential_matches
        IMPORTING
          matchers             TYPE zif_adcoset_pattern_matcher=>ty_ref_tab
          ignore_comment_lines TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_matches,
      find_non_sequential_matches
        IMPORTING
          matchers             TYPE zif_adcoset_pattern_matcher=>ty_ref_tab
          match_all            TYPE abap_bool OPTIONAL
          ignore_comment_lines TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(result)        TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDCLASS.



CLASS zcl_adcoset_source_code IMPLEMENTATION.


  METHOD constructor.
    ASSERT source IS NOT INITIAL.

    me->source = source.
    me->line_indexes = line_indexes.
    me->comment_regex = comment_regex.
    line_count = lines( line_indexes ).
    multiline_mode = xsdbool( line_count IS NOT INITIAL ).

  ENDMETHOD.


  METHOD zif_adcoset_source_code~find_matches.

    IF sequential_matching = abap_true and lines( matchers ) > 1.
      result = find_sequential_matches(
        matchers             = matchers
        ignore_comment_lines = ignore_comment_lines ).
    ELSE.
      result = find_non_sequential_matches(
        matchers             = matchers
        match_all            = match_all
        ignore_comment_lines = ignore_comment_lines ).
    ENDIF.
  ENDMETHOD.


  METHOD find_sequential_matches.
    DATA: line_offset   TYPE i,
          col_offset    TYPE i,
          current_match TYPE match_result,
          match_start   TYPE match_result,
          raw_matches   TYPE match_result_tab.

    DATA(has_more_matches) = abap_true.

    WHILE has_more_matches = abap_true.

      CLEAR: current_match,
             raw_matches.

      LOOP AT matchers ASSIGNING FIELD-SYMBOL(<matcher>).
        DATA(i) = sy-tabix.
        TRY.
            current_match = <matcher>->find_next_match(
              source     = source
              start_line = line_offset
              offset     = col_offset ).
          CATCH zcx_adcoset_pattern_sh_error.
            has_more_matches = abap_false.
            EXIT.
        ENDTRY.

        IF current_match IS INITIAL.
          has_more_matches = abap_false.
          EXIT.
        ENDIF.

        IF ignore_comment_lines = abap_true AND
            comment_regex IS NOT INITIAL AND
            is_comment_line( source[ current_match-line ] ).
          has_more_matches = abap_false.
          EXIT.
        ENDIF.

        IF i = 1.
          match_start = current_match.
        ENDIF.

        line_offset = current_match-line.
        col_offset = current_match-offset + current_match-length.
      ENDLOOP.

      raw_matches = VALUE #( BASE raw_matches ( current_match ) ).

      IF current_match IS NOT INITIAL AND
          match_start IS NOT INITIAL.
        result = VALUE #( BASE result
          ( start_line   = match_start-line
            start_column = match_start-offset
            end_line     = current_match-line
            end_column   = current_match-offset + current_match-length
            snippet      = source[ match_start-line ] ) ).
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD find_non_sequential_matches.

    LOOP AT matchers INTO DATA(matcher).
      TRY.
          DATA(raw_matches) = matcher->find_matches( source ).
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


  METHOD enhance_matches.
    DATA: enhanced_match TYPE zif_adcoset_ty_global=>ty_search_match.

    LOOP AT raw_matches ASSIGNING FIELD-SYMBOL(<raw_match>).

      IF multiline_mode = abap_false.
        enhanced_match = get_single_line_match(
          raw_match            = <raw_match>
          ignore_comment_lines = ignore_comment_lines ).
      ELSE.
        enhanced_match = get_multi_line_match(
          raw_match            = <raw_match>
          ignore_comment_lines = ignore_comment_lines ).
      ENDIF.

      IF enhanced_match IS NOT INITIAL.
        result = VALUE #( BASE result ( enhanced_match ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_single_line_match.
    DATA(snippet) = source[ raw_match-line ].

    IF ignore_comment_lines = abap_true AND
        comment_regex IS NOT INITIAL AND
        is_comment_line( snippet ).
      RETURN.
    ENDIF.

    result = VALUE #(
      start_line   = raw_match-line
      start_column = raw_match-offset
      end_line     = raw_match-line
      end_column   = raw_match-offset + raw_match-length
      snippet      = snippet ).
  ENDMETHOD.


  METHOD get_multi_line_match.
    DATA(start_line) = get_line_index( offset = raw_match-offset ).

    DATA(end_line) = get_match_end_line(
      start_line = start_line
      offset     = raw_match-offset
      length     = raw_match-length ).

    DATA(start_col) = raw_match-offset - start_line-offset.
    DATA(end_col) = raw_match-offset + raw_match-length - end_line-offset.

    IF ignore_comment_lines = abap_true AND
        comment_regex IS NOT INITIAL AND
        range_contains_comment_line(
          start_line = start_line
          end_line   = end_line ).
      RETURN.
    ENDIF.

    result = VALUE #(
      start_line   = start_line-number
      start_column = start_col
      end_line     = end_line-number
      end_column   = end_col
      " for now, only the start line of the match will be taken
      snippet      = substring(
        val  = source[ 1 ]
        off  = start_line-offset
        len  = start_line-length ) ).
  ENDMETHOD.


  METHOD get_line_index.

    LOOP AT line_indexes ASSIGNING FIELD-SYMBOL(<line_index>) WHERE offset > offset.
      EXIT.
    ENDLOOP.

    IF sy-subrc = 0.
      result = line_indexes[ <line_index>-number - 1 ].
    ELSE.
      " offset must be in the last row
      result = line_indexes[ line_count ].
    ENDIF.

  ENDMETHOD.


  METHOD get_match_end_line.
    IF start_line-number = line_count.
      result = start_line.
    ELSE.
      DATA(match_end) = offset + length.
      DATA(next_line) = line_indexes[ start_line-number + 1 ].
      IF match_end <= next_line-offset.
        result = start_line.
      ELSE.
        result = get_line_index(
          offset = match_end ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_comment_line.
    result = xsdbool( find( val = code_line regex = comment_regex ) <> -1 ).
  ENDMETHOD.


  METHOD range_contains_comment_line.
    result = xsdbool(
      find(
        val = source[ 1 ]
        off = start_line-offset
        len = end_line-offset + end_line-length - start_line-offset
        regex = comment_regex  ) <> -1 ).
  ENDMETHOD.

ENDCLASS.
