"! <p class="shorttext synchronized" lang="en">Match results of code search</p>
INTERFACE zif_adcoset_match_results
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Enhances matches with identifier</p>
    enhance_matches
      IMPORTING
        identifier TYPE zif_adcoset_ty_global=>ty_match_identifier,
    "! <p class="shorttext synchronized" lang="en">Retrieves the matches</p>
    get_matches
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_matches.

ENDINTERFACE.
