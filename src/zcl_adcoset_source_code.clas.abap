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
    ALIASES:
      content                FOR zif_adcoset_source_code~content,
      line_count             FOR zif_adcoset_source_code~line_count,
      line_indexes           FOR zif_adcoset_source_code~line_indexes,
      comment_regex          FOR zif_adcoset_source_code~comment_regex,
      is_single_line_content FOR zif_adcoset_source_code~is_single_line_content.
ENDCLASS.



CLASS zcl_adcoset_source_code IMPLEMENTATION.

  METHOD constructor.
    ASSERT source IS NOT INITIAL.

    me->content = source.
    me->line_indexes = line_indexes.
    me->comment_regex = comment_regex.
    line_count = lines( line_indexes ).
    is_single_line_content = xsdbool( line_count IS NOT INITIAL ).
  ENDMETHOD.

ENDCLASS.
