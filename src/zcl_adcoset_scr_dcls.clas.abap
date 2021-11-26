"! <p class="shorttext synchronized" lang="en">Source code reader for CDS Access Controls</p>
CLASS zcl_adcoset_scr_dcls DEFINITION
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



CLASS zcl_adcoset_scr_dcls IMPLEMENTATION.


  METHOD constructor.
    me->line_feed_type = line_feed_type.
  ENDMETHOD.


  METHOD zif_adcoset_src_code_reader~get_source_table.

  ENDMETHOD.


  METHOD zif_adcoset_src_code_reader~get_source_text.

  ENDMETHOD.

ENDCLASS.
