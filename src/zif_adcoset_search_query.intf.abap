"! <p class="shorttext synchronized" lang="en">Search query for code search</p>
INTERFACE zif_adcoset_search_query
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Runs search query</p>
    run,

    "! <p class="shorttext synchronized" lang="en">Retrieve search results</p>
    get_results
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDINTERFACE.
