"! <p class="shorttext synchronized">Defines the scope of a code search</p>
INTERFACE zif_adcoset_search_scope
  PUBLIC.

  INTERFACES zif_adcoset_search_sr_provider.

  ALIASES get_scope_ranges FOR zif_adcoset_search_sr_provider~get_scope_ranges.

  CONSTANTS c_min_parl_package_size TYPE i VALUE 10.
  CONSTANTS c_max_parl_package_size TYPE i VALUE 2500.
  CONSTANTS c_serial_package_size TYPE i VALUE 10000.

  "! <p class="shorttext synchronized">Retrieves number of objects in scope</p>
  METHODS count
    RETURNING
      VALUE(result) TYPE i.

  "! <p class="shorttext synchronized">Retrieves the next package of objects in scope</p>
  METHODS next_package
    RETURNING
      VALUE(result) TYPE zif_adcoset_ty_global=>ty_scope_package.

  "! <p class="shorttext synchronized">Returns 'X' if another package exists</p>
  METHODS has_next_package
    RETURNING
      VALUE(result) TYPE abap_bool.

  "! <p class="shorttext synchronized">Returns 'X' if more results would have been in scope</p>
  METHODS more_objects_in_scope DEFAULT IGNORE
    RETURNING
      VALUE(result) TYPE abap_bool.

  "! <p class="shorttext synchronized">Configures the package size of the processor</p>
  "! The driving factor will be the available number of parallel tasks
  METHODS configure_package_size DEFAULT IGNORE
    IMPORTING
      max_objects    TYPE i OPTIONAL
      max_task_count TYPE i.
ENDINTERFACE.
