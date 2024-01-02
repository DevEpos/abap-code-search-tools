"! <p class="shorttext synchronized">Provider for Scope Search Ranges</p>
INTERFACE zif_adcoset_search_sr_provider
  PUBLIC.

  "! <p class="shorttext synchronized">Retrieves the ranges of this scope</p>
  METHODS get_scope_ranges DEFAULT IGNORE
    RETURNING
      VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.
ENDINTERFACE.
