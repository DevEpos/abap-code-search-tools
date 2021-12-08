"! <p class="shorttext synchronized" lang="en">Query for code search (for parallel processing)</p>
CLASS zcl_adcoset_parl_search_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_search_query,
      zif_adcoset_parl_result_recv.

    METHODS:
      constructor
        IMPORTING
          scope TYPE REF TO zif_adcoset_search_scope.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      scope TYPE REF TO zif_adcoset_search_scope.
ENDCLASS.



CLASS zcl_adcoset_parl_search_query IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.


  METHOD zif_adcoset_parl_result_recv~send_results.

  ENDMETHOD.


  METHOD zif_adcoset_search_query~run.

  ENDMETHOD.


  METHOD zif_adcoset_search_query~get_results.

  ENDMETHOD.

ENDCLASS.
