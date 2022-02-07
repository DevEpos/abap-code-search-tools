"! <p class="shorttext synchronized" lang="en">ADT Util</p>
CLASS zcl_adcoset_adt_request_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Retrieve values of request parameter</p>
      get_query_parameter_values
        IMPORTING
          param_name      TYPE string
          value_separator TYPE string OPTIONAL
          mandatory       TYPE abap_bool OPTIONAL
          upper_case      TYPE abap_bool OPTIONAL
          request         TYPE REF TO if_adt_rest_request
        RETURNING
          VALUE(results)  TYPE string_table
        RAISING
          cx_adt_rest,
      "! <p class="shorttext synchronized" lang="en">Retrieve value of request parameter</p>
      get_query_parameter
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
      "! <p class="shorttext synchronized" lang="en">Retrieve value of integer request parameter</p>
      get_integer_query_parameter
        IMPORTING
          param_name    TYPE string
          default_value TYPE i OPTIONAL
          mandatory     TYPE abap_bool OPTIONAL
          request       TYPE REF TO if_adt_rest_request
        RETURNING
          VALUE(result) TYPE i
        RAISING
          cx_adt_rest,
      "! <p class="shorttext synchronized" lang="en">Retrieve boolean parameter value from request</p>
      get_boolean_query_parameter
        IMPORTING
          param_name    TYPE string
          default_value TYPE abap_bool OPTIONAL
          request       TYPE REF TO if_adt_rest_request
        RETURNING
          VALUE(result) TYPE abap_bool,

      "! <p class="shorttext synchronized" lang="en">Retrieves UUID request parameter</p>
      get_uuid_uri_query_parameter
        IMPORTING
          param_name    TYPE string
          mandatory     TYPE abap_bool OPTIONAL
          request       TYPE REF TO if_adt_rest_request
        RETURNING
          VALUE(result) TYPE uuid
        RAISING
          cx_adt_rest.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_adcoset_adt_request_util IMPLEMENTATION.


  METHOD get_query_parameter.
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


  METHOD get_integer_query_parameter.
    DATA(param_value) = get_query_parameter(
      param_name = param_name
      mandatory  = mandatory
      request    = request ).

    IF param_value IS INITIAL.
      result = default_value.
    ELSEIF param_value CN '0123456789'.
      RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
        EXPORTING
          text = |Non Integer value '{ param_value }' was passed to Parameter '{ param_name }'|.
    ELSE.
      result = param_value.
    ENDIF.

  ENDMETHOD.


  METHOD get_query_parameter_values.
    DATA: param_values LIKE results,
          tokens       TYPE string_table.

    IF mandatory = abap_true.
      request->get_uri_query_parameter_values(
        EXPORTING
          name      = param_name
          mandatory = abap_true
        IMPORTING
          values    = param_values ).
    ELSE.
      request->get_uri_query_parameter_values(
        EXPORTING
          name   = param_name
        IMPORTING
          values = param_values ).
    ENDIF.

    LOOP AT param_values ASSIGNING FIELD-SYMBOL(<value>).
      IF upper_case = abap_true.
        TRANSLATE <value> TO UPPER CASE.
      ENDIF.

      IF value_separator IS NOT INITIAL AND <value> CS value_separator.
        SPLIT <value> AT value_separator INTO TABLE tokens.
        results = VALUE #( BASE results ( LINES OF VALUE #( FOR token IN tokens ( token ) ) ) ).
      ELSE.
        results = VALUE #( BASE results ( <value> ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_boolean_query_parameter.
    TRY.
        DATA(value) = get_query_parameter(
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


  METHOD get_uuid_uri_query_parameter.
    DATA(uuid_c36_string) = get_query_parameter(
      param_name = param_name
      mandatory  = mandatory
      request    = request ).

    IF uuid_c36_string IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        REPLACE ALL OCCURRENCES OF '-' IN uuid_c36_string WITH space.
        cl_system_uuid=>convert_uuid_c32_static(
          EXPORTING
            uuid     = to_upper( uuid_c36_string )
          IMPORTING
            uuid_x16 = result ).
      CATCH cx_uuid_error INTO DATA(conversion_error).
        RAISE EXCEPTION TYPE zcx_adcoset_adt_rest
          EXPORTING
            previous = conversion_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
