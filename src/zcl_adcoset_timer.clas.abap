"! <p class="shorttext synchronized" lang="en">Duration for measurements</p>
CLASS zcl_adcoset_timer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      ty_metric   TYPE c LENGTH 1.

    CONSTANTS:
      BEGIN OF c_metric,
        "! milliseconds
        ms TYPE ty_metric VALUE '0',
        "! seconds
        s  TYPE ty_metric VALUE '1',
      END OF c_metric.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Starts timer</p>
      start
        RETURNING
          VALUE(result) TYPE REF TO zcl_adcoset_timer,
      "! <p class="shorttext synchronized" lang="en">Stops timer</p>
      stop
        RETURNING
          VALUE(result) TYPE REF TO zcl_adcoset_timer,
      "! <p class="shorttext synchronized" lang="en">Retrieves duration in the given metric</p>
      get_duration
        IMPORTING
          metric        TYPE ty_metric DEFAULT zcl_adcoset_timer=>c_metric-s
        RETURNING
          VALUE(result) TYPE zif_adcoset_ty_global=>ty_duration,
      "! <p class="shorttext synchronized" lang="en">Retrieves duration as string</p>
      get_duration_string
        IMPORTING
          metric        TYPE ty_metric DEFAULT zcl_adcoset_timer=>c_metric-s
        RETURNING
          VALUE(result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      start_time    TYPE timestampl,
      duration_in_s TYPE zif_adcoset_ty_global=>ty_duration.
ENDCLASS.



CLASS zcl_adcoset_timer IMPLEMENTATION.

  METHOD get_duration.
    IF duration_in_s = -1.
      result = -1.
    ELSEIF metric = c_metric-s.
      result = duration_in_s.
    ELSEIF metric = c_metric-ms.
      result = duration_in_s * 1000.
    ENDIF.
  ENDMETHOD.

  METHOD get_duration_string.
    IF duration_in_s = -1.
      result = 'Error during duration determination'.
    ELSEIF metric = c_metric-s.
      result = |{ duration_in_s NUMBER = USER } s|.
    ELSEIF metric = c_metric-ms.
      result = |{ duration_in_s * 1000 NUMBER = USER DECIMALS = 0 } ms|.
    ENDIF.
  ENDMETHOD.

  METHOD start.
    GET TIME STAMP FIELD start_time.
    result = me.
  ENDMETHOD.

  METHOD stop.
    DATA: end_time TYPE timestampl.

    GET TIME STAMP FIELD end_time.
    TRY.
        duration_in_s = cl_abap_tstmp=>subtract(
          tstmp1 = end_time
          tstmp2 = start_time ).
      CATCH cx_parameter_invalid_range
            cx_parameter_invalid_type.
        duration_in_s = -1.
    ENDTRY.

    result = me.
  ENDMETHOD.

ENDCLASS.
