"! <p class="shorttext synchronized" lang="en">Query for code search (with parallel processing)</p>
CLASS zcl_adcoset_parl_search_query DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_adcoset_search_query,
      zif_adcoset_parl_result_recv.

    METHODS:
      constructor
        IMPORTING
          scope       TYPE REF TO zif_adcoset_search_scope
          task_runner TYPE REF TO zif_adcoset_parl_task_runner
          settings    TYPE zif_adcoset_ty_global=>ty_search_settings_int.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      scope          TYPE REF TO zif_adcoset_search_scope,
      task_runner    TYPE REF TO zif_adcoset_parl_task_runner,
      settings       TYPE zif_adcoset_ty_global=>ty_search_settings_int,
      search_results TYPE zif_adcoset_ty_global=>ty_search_result_objects.
ENDCLASS.



CLASS zcl_adcoset_parl_search_query IMPLEMENTATION.


  METHOD constructor.
    ASSERT:
      scope IS BOUND,
      task_runner IS BOUND.

    me->scope = scope.
    me->task_runner = task_runner.
    me->settings = settings.

    " registers this instance as result receiver
    me->task_runner->set_result_receiver( me ).
  ENDMETHOD.


  METHOD zif_adcoset_parl_result_recv~send_results.
    FIELD-SYMBOLS: <package_result> TYPE zif_adcoset_ty_global=>ty_search_package_result.
    ASSIGN results TO <package_result>.
    IF sy-subrc = 0.
      INSERT LINES OF <package_result>-result_objects INTO TABLE search_results.
      zcl_adcoset_log=>add_all_messages( <package_result>-messages ).
      zcl_adcoset_search_protocol=>increase_searchd_objects_count( <package_result>-searched_objects_count ).
      zcl_adcoset_search_protocol=>increase_searchd_sources_count( <package_result>-searched_sources_count ).
      zcl_adcoset_search_protocol=>add_loc( <package_result>-loc ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_adcoset_search_query~run.

    WHILE scope->has_next_package( ).
      DATA(package) = scope->next_package( ).

      " process new package asynchronously
      task_runner->run(
        input = VALUE zif_adcoset_ty_global=>ty_search_package(
          settings = settings
          objects  = package ) ).
    ENDWHILE.

    task_runner->wait_until_finished( ).

  ENDMETHOD.


  METHOD zif_adcoset_search_query~get_results.
    result = search_results.
  ENDMETHOD.

ENDCLASS.
