"! <p class="shorttext synchronized" lang="en">Factory for creating search queries</p>
CLASS zcl_adcoset_search_query_fac DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Create query for parallel processing</p>
      create_query_for_parallel
        IMPORTING
          scope         TYPE REF TO zif_adcoset_search_scope
        RETURNING
          VALUE(result) TYPE REF TO zif_adcoset_search_query,
      "! <p class="shorttext synchronized" lang="en">Creates standard query</p>
      create_query
        IMPORTING
          scope         TYPE REF TO zif_adcoset_search_scope
        RETURNING
          VALUE(result) TYPE REF TO zif_adcoset_search_query.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_search_query_fac IMPLEMENTATION.


  METHOD create_query.

  ENDMETHOD.


  METHOD create_query_for_parallel.

  ENDMETHOD.


ENDCLASS.
