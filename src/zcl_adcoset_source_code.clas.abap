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
          source       TYPE string_table
          line_indexes TYPE zif_adcoset_source_code=>ty_line_indexes OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      source       TYPE string_table,
      line_count   TYPE i,
      line_indexes TYPE zif_adcoset_source_code=>ty_line_indexes.

    METHODS:
      enhance_matches
        IMPORTING
          raw_matches   TYPE match_result_tab
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_matches,
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
      get_single_source_match
        IMPORTING
          raw_match     TYPE match_result
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_match.
ENDCLASS.



CLASS zcl_adcoset_source_code IMPLEMENTATION.


  METHOD constructor.
    ASSERT source IS NOT INITIAL.

    me->source = source.
    me->line_indexes = line_indexes.
    line_count = lines( line_indexes ).
  ENDMETHOD.


  METHOD zif_adcoset_source_code~find_matches.

    LOOP AT matchers INTO DATA(matcher).
      DATA(raw_matches) = matcher->find_matches( source ).
      IF raw_matches IS NOT INITIAL.
        result = VALUE #( BASE result ( LINES OF enhance_matches( raw_matches ) ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD enhance_matches.
    CHECK raw_matches IS NOT INITIAL.

    LOOP AT raw_matches ASSIGNING FIELD-SYMBOL(<raw_match>).

      IF line_indexes IS INITIAL.
        result = VALUE #( BASE result
          ( start_line   = <raw_match>-line
            start_column = <raw_match>-offset
            end_line     = <raw_match>-line
            end_column   = <raw_match>-offset + <raw_match>-length
            snippet      = source[ <raw_match>-line ] ) ).
      ELSE.
        result = VALUE #( BASE result ( get_single_source_match( <raw_match> ) ) ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_single_source_match.
    DATA(start_line) = get_line_index( offset = raw_match-offset ).

    DATA(end_line) = get_match_end_line(
      start_line = start_line
      offset     = raw_match-offset
      length     = raw_match-length ).

    DATA(start_col) = raw_match-offset - start_line-offset.
    DATA(end_col) = raw_match-offset + raw_match-length - end_line-offset.

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


ENDCLASS.
