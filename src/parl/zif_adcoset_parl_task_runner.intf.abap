"! <p class="shorttext synchronized">Represents task to be run in parallel</p>
INTERFACE zif_adcoset_parl_task_runner
  PUBLIC.

  "! <p class="shorttext synchronized">Starts a new task</p>
  METHODS run
    IMPORTING
      !input TYPE any.

  "! <p class="shorttext synchronized">Waits until all tasks are finished</p>
  METHODS wait_until_finished.

  "! <p class="shorttext synchronized">Sets a new result receiver in the task runner</p>
  METHODS set_result_receiver
    IMPORTING
      !receiver TYPE REF TO zif_adcoset_parl_result_recv.

  "! <p class="shorttext synchronized">Returns the number of the maximum available tasks</p>
  METHODS get_max_tasks
    RETURNING
      VALUE(result) TYPE i.

ENDINTERFACE.
