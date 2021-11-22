"! <p class="shorttext synchronized" lang="en">Finds matches in a string for a given (RegExp) pattern</p>
INTERFACE zif_adcoset_pattern_matcher
  PUBLIC.

  METHODS:
    get_matches

      RETURNING
        VALUE(result) TYPE match_result_tab.
ENDINTERFACE.
