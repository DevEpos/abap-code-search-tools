"! <p class="shorttext synchronized">Protocol that collects information during code search</p>
CLASS zcl_adcoset_search_protocol DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Resets the protocol values</p>
    CLASS-METHODS reset.

    "! <p class="shorttext synchronized">Adds Lines of Code to total searched LoC</p>
    CLASS-METHODS add_loc
      IMPORTING
        loc TYPE f.

    "! <p class="shorttext synchronized">Increments the count of searched objects</p>
    CLASS-METHODS increment_searched_objs_count
      IMPORTING
        searched_objs_count TYPE i OPTIONAL.

    "! <p class="shorttext synchronized">Increments the count of searched sources</p>
    CLASS-METHODS increment_searched_srcs_count.

    "! <p class="shorttext synchronized">Increases count of searched sources with the given value</p>
    CLASS-METHODS increase_searchd_sources_count
      IMPORTING
        !value TYPE i.

    "! <p class="shorttext synchronized">Increases count of searched objects with the given value</p>
    CLASS-METHODS increase_searchd_objects_count
      IMPORTING
        !value TYPE i.

    "! <p class="shorttext synchronized">Retrieves searched Lines of Code</p>
    CLASS-METHODS get_loc
      RETURNING
        VALUE(result) TYPE i.

    "! <p class="shorttext synchronized">Returns count of searched objects</p>
    CLASS-METHODS get_searched_object_count
      RETURNING
        VALUE(result) TYPE i.

    "! <p class="shorttext synchronized">Returns count of searched sources</p>
    CLASS-METHODS get_searched_sources_count
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.
    CLASS-DATA searched_objects_count TYPE i.
    CLASS-DATA loc TYPE f.
    CLASS-DATA searched_sources_count TYPE i.
ENDCLASS.


CLASS zcl_adcoset_search_protocol IMPLEMENTATION.
  METHOD add_loc.
    zcl_adcoset_search_protocol=>loc = zcl_adcoset_search_protocol=>loc + loc.
  ENDMETHOD.

  METHOD increment_searched_objs_count.
    IF searched_objs_count IS NOT INITIAL.
      searched_objects_count = searched_objects_count + searched_objs_count.
    ELSE.
      searched_objects_count = searched_objects_count + 1.
    ENDIF.
  ENDMETHOD.

  METHOD increment_searched_srcs_count.
    searched_sources_count = searched_sources_count + 1.
  ENDMETHOD.

  METHOD reset.
    CLEAR:
      searched_objects_count,
      searched_sources_count,
      loc.
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

  METHOD get_loc.
    result = loc.
  ENDMETHOD.
ENDCLASS.
