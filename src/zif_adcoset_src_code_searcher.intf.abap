"! <p class="shorttext synchronized" lang="en">Searches source code</p>
INTERFACE zif_adcoset_src_code_searcher
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Searches the given source code</p>
    search
      IMPORTING
        source_code   TYPE REF TO zif_adcoset_source_code
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDINTERFACE.
