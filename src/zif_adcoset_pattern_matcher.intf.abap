"! <p class="shorttext synchronized" lang="en">Finds matches in a string for a given (RegExp) pattern</p>
INTERFACE zif_adcoset_pattern_matcher
  PUBLIC.

  TYPES: ty_ref_tab TYPE STANDARD TABLE OF REF TO zif_adcoset_pattern_matcher WITH EMPTY KEY.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves the matches in the given source</p>
    find_matches
      IMPORTING
        source        TYPE string_table
      RETURNING
        VALUE(result) TYPE match_result_tab
      RAISING
        zcx_adcoset_pattern_sh_error.
ENDINTERFACE.
