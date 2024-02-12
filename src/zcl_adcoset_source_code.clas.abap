"! <p class="shorttext synchronized">Represents source code</p>
CLASS zcl_adcoset_source_code DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_source_code.

    METHODS constructor
      IMPORTING
        !source       TYPE string_table
        line_indexes  TYPE zif_adcoset_source_code=>ty_line_indexes OPTIONAL
        comment_regex TYPE string                                   OPTIONAL.

  PRIVATE SECTION.
    ALIASES content                FOR zif_adcoset_source_code~content.
    ALIASES line_count             FOR zif_adcoset_source_code~line_count.
    ALIASES line_indexes           FOR zif_adcoset_source_code~line_indexes.
    ALIASES comment_regex          FOR zif_adcoset_source_code~comment_regex.
    ALIASES is_single_line_content FOR zif_adcoset_source_code~is_single_line_content.
ENDCLASS.


CLASS zcl_adcoset_source_code IMPLEMENTATION.
  METHOD constructor.
    ASSERT source IS NOT INITIAL.

    content = source.
    me->line_indexes  = line_indexes.
    me->comment_regex = comment_regex.
    line_count = lines( line_indexes ).
    is_single_line_content = xsdbool( line_count IS NOT INITIAL ).

    IF is_single_line_content = abap_true.
      zcl_adcoset_search_protocol=>add_loc( lines( line_indexes ) ).
    ELSE.
      zcl_adcoset_search_protocol=>add_loc( lines( source ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
