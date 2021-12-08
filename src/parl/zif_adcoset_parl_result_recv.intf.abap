"! <p class="shorttext synchronized" lang="en">Receives results from parallel tasks</p>
INTERFACE zif_adcoset_parl_result_recv
  PUBLIC.

  METHODS:
    "! <p class="shorttext synchronized" lang="en">Will be called when task is finished</p>
    send_results
      IMPORTING
        results TYPE any.
ENDINTERFACE.
