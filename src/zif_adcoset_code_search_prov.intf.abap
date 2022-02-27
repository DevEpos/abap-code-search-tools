"! <p class="shorttext synchronized" lang="en">Code search provider</p>
INTERFACE zif_adcoset_code_search_prov
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Searches for matches in the given object</p>
    search
      IMPORTING
        object            TYPE zif_adcoset_ty_global=>ty_tadir_object
        src_code_reader   TYPE REF TO zif_adcoset_src_code_reader
        src_code_searcher TYPE REF TO zif_adcoset_src_code_searcher
      RETURNING
        VALUE(result)     TYPE zif_adcoset_ty_global=>ty_search_matches.

ENDINTERFACE.
