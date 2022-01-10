"! <p class="shorttext synchronized" lang="en">ADT Util</p>
CLASS zcl_adcoset_adt_request_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieve values of request parameter</p>
      get_request_param_values
        IMPORTING
          param_name     TYPE string
          default_values TYPE string_table OPTIONAL
          mandatory      TYPE abap_bool OPTIONAL
          upper_case     TYPE abap_bool OPTIONAL
          request        TYPE REF TO if_adt_rest_request
        RETURNING
          VALUE(results) TYPE string_table
        RAISING
          cx_adt_rest,
      "! <p class="shorttext synchronized" lang="en">Retrieve value of request parameter</p>
      get_request_param_value
        IMPORTING
          param_name    TYPE string
          default_value TYPE any OPTIONAL
          mandatory     TYPE abap_bool OPTIONAL
          upper_case    TYPE abap_bool OPTIONAL
          request       TYPE REF TO if_adt_rest_request
        RETURNING
          VALUE(result) TYPE string
        RAISING
          cx_adt_rest,
      "! <p class="shorttext synchronized" lang="en">Retrieve boolean parameter value from request</p>
      get_boolean_req_param
        IMPORTING
          param_name    TYPE string
          default_value TYPE abap_bool OPTIONAL
          request       TYPE REF TO if_adt_rest_request
        RETURNING
          VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_adt_request_util IMPLEMENTATION.


  METHOD get_request_param_value.
    IF mandatory = abap_true.
      request->get_uri_query_parameter(
        EXPORTING
          name      = param_name
          mandatory = abap_true
        IMPORTING
          value     = result ).
    ELSE.
      request->get_uri_query_parameter(
        EXPORTING
          name    = param_name
          default = default_value
        IMPORTING
          value   = result ).
    ENDIF.

    IF upper_case = abap_true.
      TRANSLATE result TO UPPER CASE.
    ENDIF.
  ENDMETHOD.


  METHOD get_request_param_values.
    IF mandatory = abap_true.
      request->get_uri_query_parameter_values(
        EXPORTING
          name      = param_name
          mandatory = abap_true
        IMPORTING
          values    = results ).
    ELSE.
      request->get_uri_query_parameter_values(
        EXPORTING
          name      = param_name
          default   = default_values
        IMPORTING
          values    = results ).
    ENDIF.

    IF upper_case = abap_true.

      LOOP AT results ASSIGNING FIELD-SYMBOL(<value>).
        TRANSLATE <value> TO UPPER CASE.
      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD get_boolean_req_param.
    TRY.
        DATA(value) = get_request_param_value(
          param_name = param_name
          request    = request ).
        IF value IS NOT INITIAL.
          value = to_lower( value ).
          result = COND #( WHEN value = 'true' OR value = 'x' THEN abap_true ).
        ELSE.
          result = default_value.
        ENDIF.
      CATCH cx_adt_rest.
    ENDTRY.
  ENDMETHOD.


ENDCLASS.
