"! <p class="shorttext synchronized" lang="en">Protocol that collects information during code search</p>
CLASS zcl_adcoset_search_protocol DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Resets the protocol values</p>
      reset,
      "! <p class="shorttext synchronized" lang="en">Increments the count of searched objects</p>
      increment_searched_objs_count,
      "! <p class="shorttext synchronized" lang="en">Increments the count of searched sources</p>
      increment_searched_srcs_count,
      "! <p class="shorttext synchronized" lang="en">Increases count of searched sources with the given value</p>
      increase_searchd_sources_count
        IMPORTING
          value TYPE i,
      "! <p class="shorttext synchronized" lang="en">Increases count of searched objects with the given value</p>
      increase_searchd_objects_count
        IMPORTING
          value TYPE i,
      "! <p class="shorttext synchronized" lang="en">Returns count of searched objects</p>
      get_searched_object_count
        RETURNING
          VALUE(result) TYPE i,
      "! <p class="shorttext synchronized" lang="en">Returns count of searched sources</p>
      get_searched_sources_count
        RETURNING
          VALUE(result) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      searched_objects_count TYPE i,
      searched_sources_count TYPE i.
ENDCLASS.



CLASS zcl_adcoset_search_protocol IMPLEMENTATION.

  METHOD increment_searched_objs_count.
    searched_objects_count = searched_objects_count + 1.
  ENDMETHOD.


  METHOD increment_searched_srcs_count.
    searched_sources_count = searched_sources_count + 1.
  ENDMETHOD.


  METHOD reset.
    CLEAR:
      searched_objects_count,
      searched_sources_count.
  ENDMETHOD.


  METHOD get_searched_object_count.
    result = searched_objects_count.
  ENDMETHOD.


  METHOD get_searched_sources_count.
    result = searched_sources_count.
  ENDMETHOD.


  METHOD increase_searchd_sources_count.
    searched_sources_count = searched_sources_count + value.
  ENDMETHOD.


  METHOD increase_searchd_objects_count.
    searched_objects_count = searched_objects_count + value.
  ENDMETHOD.

ENDCLASS.
