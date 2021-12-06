"! <p class="shorttext synchronized" lang="en">Utility for string processing</p>
CLASS zcl_adcoset_string_util DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Adjusts the line endings in the given source</p>
      adjust_line_endings
        IMPORTING
          text          TYPE string
          line_feed     TYPE string
        RETURNING
          VALUE(result) TYPE string,

      "! <p class="shorttext synchronized" lang="en">Transforms tabular source into single string</p>
      transform_to_string
        IMPORTING
          source_table TYPE string_table
          line_feed    TYPE string
        EXPORTING
          source_text  TYPE string
          indexes      TYPE zif_adcoset_source_code=>ty_line_indexes,

      "! <p class="shorttext synchronized" lang="en">Determines the line indexes in the source</p>
      determine_line_indexes
        IMPORTING
          source_text   TYPE string OPTIONAL
          source_table  TYPE string_table OPTIONAL
          line_feed     TYPE string
        RETURNING
          VALUE(result) TYPE zif_adcoset_source_code=>ty_line_indexes.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_string_util IMPLEMENTATION.


  METHOD adjust_line_endings.
    CHECK text IS NOT INITIAL.

    IF line_feed = |\r\n|.
      IF find( val = text sub = line_feed ) = -1.
        result = replace( val = text sub = |\n| with = line_feed occ = 0 ).
      ELSEIF find( val = text regex = `^\n|[^\r]\n` ) = -1.
        result = text.
      ELSE.
        result = replace( val = text sub = |\r\n| with = |\n| occ = 0 ).
        result = replace( val = result sub = |\n| with = |\r\n| occ = 0 ).
      ENDIF.
    ELSE.
      result = replace( val = text sub = |\r\n| with = |\n| occ = 0 ).
    ENDIF.
  ENDMETHOD.


  METHOD determine_line_indexes.
    DATA: l_source_table TYPE TABLE OF string.

    DATA(line_offset) = 1.

    DATA(line_feed_length) = strlen( line_feed ).

    IF source_text IS NOT INITIAL.
      SPLIT source_text AT line_feed INTO TABLE l_source_table.
    ELSE.
      l_source_table = source_table.
    ENDIF.

    LOOP AT l_source_table ASSIGNING FIELD-SYMBOL(<code_line>).
      DATA(line_number) = sy-tabix.
      result = VALUE #( BASE result
        ( number = sy-tabix
          offset = line_offset ) ).

      IF line_offset = 1.
        line_offset = 0.
      ENDIF.

      line_offset = line_offset + strlen( <code_line> ) + line_feed_length.
    ENDLOOP.

  ENDMETHOD.


  METHOD transform_to_string.

    CLEAR: source_text,
           indexes.

    indexes = determine_line_indexes(
      source_table = source_table
      line_feed    = line_feed ).

    source_text = concat_lines_of( table = source_table sep = line_feed ).

  ENDMETHOD.


ENDCLASS.
