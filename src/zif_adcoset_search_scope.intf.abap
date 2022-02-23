"! <p class="shorttext synchronized" lang="en">Defines the scope of a code search</p>
INTERFACE zif_adcoset_search_scope
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Retrieves number of objects in scope</p>
    count
      RETURNING
        VALUE(result) TYPE i,

    "! <p class="shorttext synchronized" lang="en">Retrieves the next package of objects in scope</p>
    next_package
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_tadir_objects,

    "! <p class="shorttext synchronized" lang="en">Returns the next offset available in the scope</p>
    next_offset DEFAULT IGNORE
      RETURNING
        VALUE(result) TYPE i,

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if another package exists</p>
    has_next_package
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Returns 'X' if more results would have been in scope</p>
    more_objects_in_scope DEFAULT IGNORE
      RETURNING
        VALUE(result) TYPE abap_bool,

    "! <p class="shorttext synchronized" lang="en">Configures the package size of the processor</p>
    "! The driving factor will be the available number of parallel tasks
    configure_package_size DEFAULT IGNORE
      IMPORTING
        max_objects TYPE i optional
        max_task_count TYPE i,

    "! <p class="shorttext synchronized" lang="en">Retrieves the ranges of this scope</p>
    get_scope_ranges DEFAULT IGNORE
      RETURNING
        VALUE(result) TYPE zif_adcoset_ty_global=>ty_search_scope_ranges.
ENDINTERFACE.
