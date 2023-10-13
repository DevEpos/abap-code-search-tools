"! <p class="shorttext synchronized">Receives results from parallel tasks</p>
INTERFACE zif_adcoset_parl_result_recv
  PUBLIC.

  "! <p class="shorttext synchronized">Will be called when task is finished</p>
  METHODS send_results
    IMPORTING
      !results TYPE any.
ENDINTERFACE.
