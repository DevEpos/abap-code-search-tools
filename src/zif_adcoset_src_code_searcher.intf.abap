"! <p class="shorttext synchronized">Searches source code</p>
INTERFACE zif_adcoset_src_code_searcher
  PUBLIC.

  "! <p class="shorttext synchronized">Searches the given source code</p>
  METHODS search
    IMPORTING
      source_code   TYPE REF TO zif_adcoset_source_code
    RETURNING
      VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_matches.
ENDINTERFACE.
