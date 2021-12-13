"! <p class="shorttext synchronized" lang="en">Source code reader for objects stored in TRDIR</p>
CLASS zcl_adcoset_scr_reposrc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_src_code_reader.

    METHODS:
      constructor
        IMPORTING
          is_multiline TYPE abap_bool
          line_feed    TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      c_default_comment_regex TYPE string VALUE '^(\*|\s*")',
      c_bdef_comment_regex    TYPE string VALUE '^\s*(//|/\*)'.

    DATA:
      line_feed    TYPE string,
      is_multiline TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_scr_reposrc IMPLEMENTATION.


  METHOD constructor.
    me->line_feed = line_feed.
    me->is_multiline = is_multiline.
  ENDMETHOD.


  METHOD zif_adcoset_src_code_reader~get_source_code.
    DATA: source TYPE string_table.

    READ REPORT name INTO source.
    IF sy-subrc <> 0 OR source IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adcoset_src_code_read.
    ENDIF.

    IF is_multiline = abap_true.
      zcl_adcoset_string_util=>transform_to_string(
        EXPORTING
          source_table = source
          line_feed    = line_feed
        IMPORTING
          source_text  = DATA(source_text)
          indexes      = DATA(indexes) ).

      source = VALUE #( ( source_text ) ).
    ENDIF.

    result = NEW zcl_adcoset_source_code(
      source        = source
      line_indexes  = indexes
      comment_regex = COND #(
        WHEN type = zif_adcoset_c_global=>c_source_code_type-behavior_definition THEN
          c_bdef_comment_regex
        ELSE
          c_default_comment_regex ) ).

  ENDMETHOD.

ENDCLASS.
