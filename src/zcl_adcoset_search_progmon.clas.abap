"! <p class="shorttext synchronized" lang="en">Progress Monitor for Code Search</p>
CLASS zcl_adcoset_search_progmon DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_search_progmon.

    CLASS-METHODS:
    "! <p class="shorttext synchronized" lang="en">Creates progress monitor</p>
    create
      IMPORTING
        max_results   TYPE i
        all_results   TYPE abap_bool
      RETURNING
        VALUE(result) TYPE REF TO zif_adcoset_search_progmon.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      max_results type i,
      all_results type i,
      match_count TYPE i,
      cancelled   TYPE abap_bool,
      done        TYPE abap_bool.
ENDCLASS.



CLASS zcl_adcoset_search_progmon IMPLEMENTATION.

  method create.
    data(monitor) = new zcl_adcoset_search_progmon( ).
    monitor->max_results = max_results.
    monitor->all_results = all_results.

    result = monitor.
  ENDMETHOD.


  METHOD zif_adcoset_search_progmon~add_match_count.
    me->match_count = me->match_count + match_count.
  ENDMETHOD.


  METHOD zif_adcoset_search_progmon~done.
    done = abap_true.
  ENDMETHOD.


  METHOD zif_adcoset_search_progmon~is_cancelled.
    result = cancelled.
  ENDMETHOD.


  METHOD zif_adcoset_search_progmon~is_done.
    result = done.
  ENDMETHOD.


  METHOD zif_adcoset_search_progmon~cancel.
    cancelled = abap_true.
  ENDMETHOD.

ENDCLASS.
