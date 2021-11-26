"! <p class="shorttext synchronized" lang="en">Source code reader for objects stored in TRDIR</p>
CLASS zcl_adcoset_scr_trdir DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_src_code_reader.

    METHODS:
      constructor
        IMPORTING
          line_feed_type TYPE zif_adcoset_ty_global=>ty_line_feed_type.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: line_feed_type TYPE zif_adcoset_ty_global=>ty_line_feed_type.
ENDCLASS.



CLASS zcl_adcoset_scr_trdir IMPLEMENTATION.


  METHOD constructor.
    me->line_feed_type = line_feed_type.
  ENDMETHOD.


  METHOD zif_adcoset_src_code_reader~get_source_table.
    READ REPORT name INTO result.
  ENDMETHOD.


  METHOD zif_adcoset_src_code_reader~get_source_text.
    DATA: source TYPE string_table.

    READ REPORT name INTO source.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = zcl_adcoset_string_util=>concat_with_line_feed(
      table          = source
      line_feed_type = line_feed_type ).
  ENDMETHOD.


ENDCLASS.
