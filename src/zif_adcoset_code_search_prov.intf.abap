"! <p class="shorttext synchronized">Code search provider</p>
INTERFACE zif_adcoset_code_search_prov
  PUBLIC.

  "! <p class="shorttext synchronized">Searches for matches in the given object</p>
  METHODS search
    IMPORTING
      !object           TYPE zif_adcoset_ty_global=>ty_tadir_object_new
      src_code_reader   TYPE REF TO zif_adcoset_src_code_reader
      src_code_searcher TYPE REF TO zif_adcoset_src_code_searcher
    RETURNING
      VALUE(result)     TYPE zif_adcoset_ty_global=>ty_search_matches.

ENDINTERFACE.
