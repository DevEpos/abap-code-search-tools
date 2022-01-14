"! <p class="shorttext synchronized" lang="en">Represents task to be run in parallel</p>
INTERFACE zif_adcoset_parl_task_runner
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Starts a new task</p>
    run
      IMPORTING
        input TYPE any,
    "! <p class="shorttext synchronized" lang="en">Waits until all tasks are finished</p>
    wait_until_finished,

    "! <p class="shorttext synchronized" lang="en">Sets a new result receiver in the task runner</p>
    set_result_receiver
      IMPORTING
        receiver TYPE REF TO zif_adcoset_parl_result_recv,

    "! <p class="shorttext synchronized" lang="en">Returns the number of the maximum available tasks</p>
    get_max_tasks
      RETURNING
        VALUE(result) TYPE i.

ENDINTERFACE.
