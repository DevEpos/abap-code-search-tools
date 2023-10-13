"! <p class="shorttext synchronized">Search query for code search</p>
INTERFACE zif_adcoset_search_query
  PUBLIC.

  "! <p class="shorttext synchronized">Runs search query</p>
  METHODS run.

  "! <p class="shorttext synchronized">Retrieve search results</p>
  METHODS get_results
    RETURNING
      VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_result_objects.
ENDINTERFACE.
