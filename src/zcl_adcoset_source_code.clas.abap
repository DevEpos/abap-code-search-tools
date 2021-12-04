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
          source  TYPE string_table
          indexes TYPE zif_adcoset_source_code=>ty_line_indexes.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      source       TYPE string_table,
      indexes TYPE zif_adcoset_source_code=>ty_line_indexes.
ENDCLASS.



CLASS zcl_adcoset_source_code IMPLEMENTATION.


  METHOD constructor.
    me->source = source.
    me->indexes = indexes.
  ENDMETHOD.


  METHOD zif_adcoset_source_code~find_matches.

  ENDMETHOD.


ENDCLASS.
