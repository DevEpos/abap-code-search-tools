"! <p class="shorttext synchronized" lang="en">Progress monitor for Code Search</p>
"! Manages the progress of the code search
INTERFACE zif_adcoset_search_progmon
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Tests if the current search was cancelled</p>
    is_cancelled
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Sets the current search to cancelled</p>
    cancel,

    "! <p class="shorttext synchronized" lang="en">Adds the given match count to the overall count</p>
    add_match_count
      IMPORTING
        match_count TYPE i,

    "! <p class="shorttext synchronized" lang="en">Notifies that the search is done</p>
    done,

    "! <p class="shorttext synchronized" lang="en">Tests if the search is still ongoing</p>
    is_done
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDINTERFACE.
