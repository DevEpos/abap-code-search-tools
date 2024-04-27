"! <p class="shorttext synchronized">Runs parallel tasks</p>
CLASS zcl_adcoset_parl_task_runner DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES zif_adcoset_parl_task_runner.

    "! <p class="shorttext synchronized">Creates new task runner instance</p>
    "! The given handler class in param <em>HANDLER_CLASS</em> has to exist and must have
    "! a <strong>static method</strong> with name <em>HANDLER_METHOD</em>
    CLASS-METHODS new
      IMPORTING
        server_group     TYPE zif_adcoset_ty_global=>ty_server_group OPTIONAL
        task_prefix      TYPE string                                 OPTIONAL
        max_tasks        TYPE i                                      DEFAULT 10
        handler_class    TYPE string
        handler_method   TYPE seocpdname
        results_receiver TYPE REF TO zif_adcoset_parl_result_recv    OPTIONAL
      RETURNING
        VALUE(result)    TYPE REF TO zif_adcoset_parl_task_runner
      RAISING
        zcx_adcoset_static_error.

    METHODS on_end_of_task
      IMPORTING
        p_task TYPE clike.

  PRIVATE SECTION.
    CONSTANTS c_max_wait TYPE i VALUE 180.

    DATA server_group TYPE zif_adcoset_ty_global=>ty_server_group.
    DATA task_number TYPE i.
    DATA task_prefix TYPE string.
    DATA free_tasks TYPE i.
    DATA max_tasks TYPE i.
    DATA initialized TYPE abap_bool.
    DATA result_receiver TYPE REF TO zif_adcoset_parl_result_recv.
    DATA handler_def TYPE zif_adcoset_ty_global=>ty_parallel_handler.

    METHODS constructor
      IMPORTING
        server_group     TYPE zif_adcoset_ty_global=>ty_server_group
        max_tasks        TYPE i
        task_prefix      TYPE string
        handler_class    TYPE string
        handler_method   TYPE seocpdname
        results_receiver TYPE REF TO zif_adcoset_parl_result_recv
      RAISING
        zcx_adcoset_static_error.

    METHODS wait_until_free_task.
ENDCLASS.


CLASS zcl_adcoset_parl_task_runner IMPLEMENTATION.
  METHOD new.
    DATA(task_runner) = NEW zcl_adcoset_parl_task_runner( server_group     = server_group
                                                          task_prefix      = task_prefix
                                                          max_tasks        = max_tasks
                                                          handler_class    = handler_class
                                                          handler_method   = handler_method
                                                          results_receiver = results_receiver ).

    result = task_runner.
  ENDMETHOD.

  METHOD zif_adcoset_parl_task_runner~get_max_tasks.
    result = max_tasks.
  ENDMETHOD.

  METHOD zif_adcoset_parl_task_runner~set_result_receiver.
    me->result_receiver = receiver.
  ENDMETHOD.

  METHOD zif_adcoset_parl_task_runner~run.
    DATA err_msg TYPE c LENGTH 100.
    DATA input_compressed TYPE xstring.
    DATA free TYPE i.

    IF initialized = abap_false.
      free_tasks = max_tasks.
      initialized = abap_true.
    ENDIF.

    ASSERT free_tasks > 0.

    task_number = task_number + 1.
    DATA(task_name) = |{ task_prefix }{ task_number }|.

    EXPORT data = input TO DATA BUFFER input_compressed.

    DO.
      CALL FUNCTION 'ZADCOSET_NEW_TASK'
        STARTING NEW TASK task_name
        DESTINATION IN GROUP server_group
        CALLING on_end_of_task ON END OF TASK
        EXPORTING  input                 = input_compressed
                   handler_class         = handler_def-classname
                   handler_method        = handler_def-method
        EXCEPTIONS system_failure        = 1 MESSAGE err_msg
                   communication_failure = 2 MESSAGE err_msg
                   resource_failure      = 3
                   OTHERS                = 4.
      IF sy-subrc = 3.
        free = free_tasks.
        WAIT UNTIL free_tasks <> free UP TO 1 SECONDS.
        CONTINUE.
      ELSEIF sy-subrc <> 0.
        ASSERT err_msg = '' AND 0 = 1.
      ENDIF.
      EXIT.
    ENDDO.

    free_tasks = free_tasks - 1.

    wait_until_free_task( ).
  ENDMETHOD.

  METHOD zif_adcoset_parl_task_runner~wait_until_finished.
    WAIT UNTIL free_tasks = max_tasks UP TO c_max_wait SECONDS.
  ENDMETHOD.

  METHOD constructor.
    DATA(max_group_tasks) = zcl_adcoset_parl_proc_utils=>determine_max_tasks( server_group ).
    IF max_tasks IS INITIAL OR max_tasks > max_group_tasks.
      me->max_tasks = max_group_tasks.
    ELSEIF max_tasks > 0.
      me->max_tasks = max_tasks.
    ELSE.
      me->max_tasks = 1.
    ENDIF.

    IF me->max_tasks <= 1.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error
        EXPORTING text = |Not enough tasks available for parallel processing|.
    ENDIF.

    me->task_prefix  = COND #(
        WHEN task_prefix IS INITIAL THEN |ADCOSET_{ sy-datum }| ELSE task_prefix ).
    me->server_group = server_group.

    handler_def = zcl_adcoset_parl_proc_utils=>get_parallel_handler( handler_class  = handler_class
                                                                     handler_method = handler_method ).
    result_receiver = results_receiver.
  ENDMETHOD.

  METHOD on_end_of_task.
    " TODO: parameter P_TASK is never used (ABAP cleaner)

    DATA error_msg TYPE c LENGTH 200.
    DATA output_buffered TYPE xstring.

    RECEIVE RESULTS FROM FUNCTION 'ZADCOSET_NEW_TASK'
      IMPORTING  output                = output_buffered
      EXCEPTIONS error                 = 1
                 communication_failure = 2 MESSAGE error_msg
                 system_failure        = 3 MESSAGE error_msg.

    IF sy-subrc <> 0.
    ELSE.
      IF output_buffered IS NOT INITIAL.
        DATA output_typed TYPE REF TO data.

        CREATE DATA output_typed TYPE HANDLE handler_def-output_param-type_handle.
        ASSIGN output_typed->* TO FIELD-SYMBOL(<output>).

        IMPORT data = <output> FROM DATA BUFFER output_buffered.

        " TODO: what to do if no receiver is set???
        IF result_receiver IS BOUND.
          result_receiver->send_results( <output> ).
        ENDIF.
      ENDIF.
    ENDIF.

    free_tasks = free_tasks + 1.
  ENDMETHOD.

  METHOD wait_until_free_task.
    WAIT UNTIL free_tasks > 0 UP TO c_max_wait SECONDS.
  ENDMETHOD.
ENDCLASS.
