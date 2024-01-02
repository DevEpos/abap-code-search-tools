"! <p class="shorttext synchronized">Paging Provider</p>
INTERFACE zif_adcoset_paging_provider
  PUBLIC.

  "! Retrieves the maximum number of rows to be selected
  METHODS get_top
    RETURNING
      VALUE(result) TYPE i.

  "! Retrieves the number rows that shall be skipped
  METHODS get_skip
    RETURNING
      VALUE(result) TYPE i.
ENDINTERFACE.
