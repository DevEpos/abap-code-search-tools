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
          text           TYPE string
          line_feed_type TYPE zif_adcoset_ty_global=>ty_line_feed_type
        RETURNING
          VALUE(result)  TYPE string,

      "! <p class="shorttext synchronized" lang="en">Splits string into table at given line feed type</p>
      split_at_line_feed
        IMPORTING
          text           TYPE string
          line_feed_type TYPE zif_adcoset_ty_global=>ty_line_feed_type
        RETURNING
          VALUE(result)  TYPE string_table,

      "! <p class="shorttext synchronized" lang="en">Concatenates lines with the given line feed type</p>
      concat_with_line_feed
        IMPORTING
          table          TYPE string_table
          line_feed_type TYPE zif_adcoset_ty_global=>ty_line_feed_type
        RETURNING
          VALUE(result)  TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_string_util IMPLEMENTATION.


  METHOD adjust_line_endings.
    CHECK text IS NOT INITIAL.

    " at this time it is assumed that a source will only contain one type of line ending
    IF line_feed_type = zif_adcoset_c_global=>c_line_feed_type-cr_lf.
      IF find( val = text sub = cl_abap_char_utilities=>cr_lf ) <= 0.
        result = replace( val = text sub = cl_abap_char_utilities=>newline with = cl_abap_char_utilities=>cr_lf occ = 0 ).
      ELSE.
        result = text.
      ENDIF.
    ELSE.
      result = replace( val = text sub = cl_abap_char_utilities=>cr_lf with = cl_abap_char_utilities=>newline occ = 0 ).
    ENDIF.
  ENDMETHOD.


  METHOD split_at_line_feed.
    CHECK text IS NOT INITIAL.

    IF line_feed_type = zif_adcoset_c_global=>c_line_feed_type-lf.
      SPLIT text AT cl_abap_char_utilities=>newline INTO TABLE result.
    ELSE.
      SPLIT text AT cl_abap_char_utilities=>cr_lf INTO TABLE result.
    ENDIF.
  ENDMETHOD.


  METHOD concat_with_line_feed.
    CHECK table IS NOT INITIAL.

    IF line_feed_type = zif_adcoset_c_global=>c_line_feed_type-lf.
      result = concat_lines_of( table = table sep = cl_abap_char_utilities=>newline ).
    ELSE.
      result = concat_lines_of( table = table sep = cl_abap_char_utilities=>cr_lf ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
