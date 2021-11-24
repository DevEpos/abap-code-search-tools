"! <p class="shorttext synchronized" lang="en">Finds matches in a string for a given (RegExp) pattern</p>
INTERFACE zif_adcoset_pattern_matcher
  PUBLIC.

  TYPES: ty_ref_tab TYPE STANDARD TABLE OF REF TO zif_adcoset_pattern_matcher WITH EMPTY KEY.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves the matches in the given string(s)</p>
    get_matches
      IMPORTING
        text          TYPE string OPTIONAL
        table         TYPE string_table OPTIONAL
      RETURNING
        VALUE(result) TYPE match_result_tab.
ENDINTERFACE.
