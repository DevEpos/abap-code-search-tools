FUNCTION zadcoset_new_task.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(INPUT) TYPE  XSTRING
*"     VALUE(HANDLER_CLASS) TYPE  STRING
*"     VALUE(HANDLER_METHOD) TYPE  SEOCPDNAME
*"  EXPORTING
*"     VALUE(OUTPUT) TYPE  XSTRING
*"  EXCEPTIONS
*"      INVALID_HANDLER
*"      ERROR
*"----------------------------------------------------------------------
  TRY.
      DATA(handler_def) = zcl_adcoset_parl_proc_utils=>get_parallel_handler(
        handler_class  = handler_class
        handler_method = handler_method ).
    CATCH zcx_adcoset_static_error.
      MESSAGE |{ handler_class }=>{ handler_method } is no valid handler method| TYPE 'E'
        RAISING invalid_handler.
  ENDTRY.

  DATA: typed_input  TYPE REF TO data,
        typed_output TYPE REF TO data.

  CREATE DATA typed_input TYPE HANDLE handler_def-input_param-type_handle.
  CREATE DATA typed_output TYPE HANDLE handler_def-output_param-type_handle.

  ASSIGN typed_input->* TO FIELD-SYMBOL(<input>).
  ASSIGN typed_output->* TO FIELD-SYMBOL(<output>).

  IMPORT data = <input> FROM DATA BUFFER input.

  TRY.
      CALL METHOD (handler_class)=>(handler_method)
        EXPORTING
          input  = <input>
        IMPORTING
          output = <output>.

      EXPORT data = <output> TO DATA BUFFER output.

    CATCH cx_sy_dyn_call_error
          zcx_adcoset_static_error INTO DATA(call_error).
      MESSAGE call_error->get_text( ) TYPE 'E'
        RAISING error.
  ENDTRY.
ENDFUNCTION.
