"! <p class="shorttext synchronized" lang="en">Search query for code search</p>
CLASS zcl_adcoset_search_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_search_query.

    METHODS:
      constructor
        IMPORTING
          scope TYPE REF TO zif_adcoset_search_scope.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      scope TYPE REF TO zif_adcoset_search_scope.
ENDCLASS.



CLASS zcl_adcoset_search_query IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.


  METHOD zif_adcoset_search_query~run.

  ENDMETHOD.


  METHOD zif_adcoset_search_query~get_results.

  ENDMETHOD.

ENDCLASS.
