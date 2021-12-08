"! <p class="shorttext synchronized" lang="en">Represents task to be run in parallel</p>
INTERFACE zif_adcoset_parl_task_runner
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Starts a new task</p>
    run
      IMPORTING
        input  TYPE any,
    "! <p class="shorttext synchronized" lang="en">Returns 'X' if there are enough tasks</p>
    has_enough_tasks
      RETURNING
        VALUE(result) TYPE abap_bool,
    "! <p class="shorttext synchronized" lang="en">Waits until all tasks are finished</p>
    wait_until_finished.

ENDINTERFACE.
