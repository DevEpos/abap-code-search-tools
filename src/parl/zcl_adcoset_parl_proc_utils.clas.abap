"! <p class="shorttext synchronized">Utilities for parallel processing</p>
CLASS zcl_adcoset_parl_proc_utils DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    " <p class="shorttext synchronized" lang="en">Retrieves parallel handler definition</p>
    CLASS-METHODS get_parallel_handler
      IMPORTING
        handler_class  TYPE string
        handler_method TYPE seocpdname
      RETURNING
        VALUE(result)  TYPE zif_adcoset_ty_global=>ty_parallel_handler
      RAISING
        zcx_adcoset_static_error.

    "! <p class="shorttext synchronized">Determines the maximum number of threads for group</p>
    CLASS-METHODS determine_max_tasks
      IMPORTING
        server_group  TYPE rzlli_apcl OPTIONAL
      RETURNING
        VALUE(result) TYPE i.

    "! <p class="shorttext synchronized">Asserts if aRFC call is active</p>
    CLASS-METHODS assert_async_rfc_call.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_server_group_info,
        group     TYPE rzlli_apcl,
        max_tasks TYPE i,
      END OF ty_server_group_info.

    CONSTANTS c_handler_importing_param TYPE abap_parmname VALUE 'INPUT'.
    CONSTANTS c_handler_exporting_param TYPE abap_parmname VALUE 'OUTPUT'.
    CONSTANTS c_max_allowed_tasks TYPE i VALUE 32.

    CLASS-DATA group_infos TYPE HASHED TABLE OF ty_server_group_info WITH UNIQUE KEY group.
ENDCLASS.


CLASS zcl_adcoset_parl_proc_utils IMPLEMENTATION.
  METHOD determine_max_tasks.
    DATA group_info TYPE ty_server_group_info.

    ASSIGN group_infos[ group = server_group ] TO FIELD-SYMBOL(<group>).
    IF sy-subrc = 0.
      result = <group>-max_tasks.
    ELSE.

      group_info-group = server_group.

      CALL FUNCTION 'SPBT_INITIALIZE'
        EXPORTING  group_name                     = server_group
        IMPORTING  free_pbt_wps                   = group_info-max_tasks
        EXCEPTIONS invalid_group_name             = 1
                   internal_error                 = 2
                   pbt_env_already_initialized    = 3
                   currently_no_resources_avail   = 4
                   no_pbt_resources_found         = 5
                   cant_init_different_pbt_groups = 6
                   OTHERS                         = 7.
      IF sy-subrc <> 0.
        " fallback to running sequentially. If SPBT_INITIALIZE fails, check transactions
        " RZ12, SM50, SM21, SARFC
        group_info-max_tasks = 1.
      ENDIF.

      IF group_info-max_tasks > 1.
        group_info-max_tasks = group_info-max_tasks - 1.
      ENDIF.

      IF group_info-max_tasks <= 1.
        group_info-max_tasks = 1.
      ELSEIF group_info-max_tasks > c_max_allowed_tasks.
        group_info-max_tasks = c_max_allowed_tasks.
      ENDIF.

      INSERT group_info INTO TABLE group_infos.
      result = group_info-max_tasks.
    ENDIF.
  ENDMETHOD.

  METHOD get_parallel_handler.
    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = to_upper( handler_class )
                                         RECEIVING  p_descr_ref    = DATA(generic_handler_descr)
                                         EXCEPTIONS type_not_found = 1 ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adcoset_static_error.
    ENDIF.

    DATA(handler_descr) = CAST cl_abap_classdescr( generic_handler_descr ).

    TRY.
        DATA(handler_method_info) = handler_descr->methods[ name = handler_method ].
        " check handler visibility
        IF    handler_method_info-is_class   <> abap_true
           OR handler_method_info-visibility <> cl_abap_classdescr=>public.
          RAISE EXCEPTION TYPE zcx_adcoset_static_error.
        ENDIF.

        DATA(input_param) = handler_method_info-parameters[ name      = c_handler_importing_param
                                                            parm_kind = cl_abap_objectdescr=>importing ].
        DATA(output_param) = handler_method_info-parameters[ name      = c_handler_exporting_param
                                                             parm_kind = cl_abap_objectdescr=>exporting ].
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_adcoset_static_error.
    ENDTRY.

    result = VALUE #( classname    = to_upper( handler_class )
                      method       = to_upper( handler_method )
                      input_param  = VALUE #( name        = c_handler_importing_param
                                              type_handle = handler_descr->get_method_parameter_type(
                                                                p_method_name    = handler_method
                                                                p_parameter_name = c_handler_importing_param ) )
                      output_param = VALUE #( name        = c_handler_exporting_param
                                              type_handle = handler_descr->get_method_parameter_type(
                                                                p_method_name    = handler_method
                                                                p_parameter_name = c_handler_exporting_param ) ) ).
  ENDMETHOD.

  METHOD assert_async_rfc_call.
    DATA caller_async_type TYPE sy-batch.

    CALL FUNCTION 'RFC_GET_ATTRIBUTES'
      IMPORTING  caller_async_type = caller_async_type
      EXCEPTIONS OTHERS            = 0.

    ASSERT caller_async_type IS NOT INITIAL.
  ENDMETHOD.
ENDCLASS.
